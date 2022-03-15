#include <iron/full.h>
#include <unistd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdlib.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <pthread.h>
#include "foxlisp.h"
extern lisp_context * current_context;
   
lisp_value tcp_listen(lisp_value _port)
{
    int port = _port.integer;
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1)
		return nil;
	 struct sockaddr_in servaddr = {0};
    
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(port);

	 int optval = 1;
    setsockopt(sockfd, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval));

    if ((bind(sockfd, (struct sockaddr *)&servaddr, sizeof(servaddr))) != 0)
        return nil;

	 if ((listen(sockfd, 5)) != 0)
		return nil;
  	 return new_cons(get_symbol("tcp-listener"), integer(sockfd));
}

lisp_value tcp_connect(lisp_value _addr, lisp_value _port){
  type_assert(_addr, LISP_STRING);
  type_assert(_port, LISP_INTEGER);
  int port = _port.integer;
  const char * str = _addr.string;
  struct sockaddr_in servaddr = {0};
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd == -1)
	 return nil;
   
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = inet_addr(str);
  servaddr.sin_port = htons(port);
   
  if (connect(sockfd, (struct sockaddr *)&servaddr, sizeof(servaddr)) != 0)
	 return nil;

  return new_cons(get_symbol("tcp-client"), integer(sockfd));
}

static int get_fd(lisp_value value){
  if(value.type == LISP_CONS){
	 return get_fd(cdr(value));
  }
  if(value.type == LISP_INTEGER)
	 return value.integer;
  return -1;
}

lisp_value tcp_accept(lisp_value l)
{
  int sockfd = get_fd(l);
  if(sockfd == -1) return nil;
  struct sockaddr_in client = {0};
  int len = sizeof(client);
  int connfd = accept(sockfd, (struct sockaddr *)&client, &len);

  if (connfd < 0)
	 return new_cons(get_symbol("error"), integer(connfd));
  return new_cons(get_symbol("tcp-client"), integer(connfd));
}

lisp_value lisp_close(lisp_value con){
  var fd = get_fd(con);
  if(fd == -1)
	 return nil;
  close(fd);
  return t;
}

lisp_value lisp_read(lisp_value item, lisp_value buffer){
  var fd = get_fd(item);
  if(fd == -1){
    printf("NO FD\n");
	 return nil;
  }
  ssize_t r = read(fd, buffer.vector->data, buffer.vector->count * lisp_type_size(buffer.vector->default_value.type));
  printf("READ %p\n", r);
  if(r == -1) return nil;
  return integer(r);
}

lisp_value lisp_write(lisp_value item, lisp_value buffer){
  var fd = get_fd(item);
  if(fd == -1)
	 return nil;
  return integer(write(fd, buffer.vector->data, buffer.vector->count * lisp_type_size(buffer.vector->default_value.type))); 
}

void * lisp_perform_work(void * args){
  lisp_value eval = {.type = LISP_FUNCTION, .function = args};
  lisp_eval(current_context->globals, new_cons(eval, nil));
  return NULL;
}

lisp_value thread_start(lisp_value func){
  type_assert(func, LISP_FUNCTION);
  pthread_t thread;
  pthread_create(&thread, NULL, lisp_perform_work, func.function);
  
  return new_cons(get_symbol("thread"), native_pointer((void *) thread));
}

lisp_value thread_join(lisp_value func){
  type_assert(func, LISP_CONS);
  type_assert(cdr(func), LISP_NATIVE_POINTER);
  
  pthread_t thread = (pthread_t) cdr(func).native_pointer;
  pthread_join(thread, NULL);
  return nil;
}

lisp_value lisp_sleep(lisp_value time){
  var t = lisp_rational(time).rational;
  printf("Sleeping %f\n", t);

  iron_sleep(t);
  return nil;
}
