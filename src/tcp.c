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
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <poll.h>
extern lisp_context * current_context;
   
static lisp_value tcp_listen(lisp_value _port)
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

static lisp_value tcp_connect(lisp_value _addr, lisp_value _port){
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

static lisp_value tcp_set_nonblock(lisp_value l, lisp_value blocking){
  int sockfd = get_fd(l);
  bool blk = is_nil(blocking) == false;
  int status;
  if(!blk){
    status = fcntl(sockfd, F_SETFL, fcntl(sockfd, F_GETFL, 0) | O_NONBLOCK);
  }
  else
    status = fcntl(sockfd, F_SETFL, fcntl(sockfd, F_GETFL, 0) & ~O_NONBLOCK);
  if(status == -1){
    return nil;
  }
  return t;
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

lisp_value lisp_close_fd(lisp_value con){
  var fd = get_fd(con);
  if(fd == -1)
	 return nil;
  close(fd);
  return t;
}

lisp_value lisp_read_fd(lisp_value item, lisp_value buffer){
  var fd = get_fd(item);
  if(fd == -1){
    printf("NO FD\n");
	 return nil;
  }
  int r = read(fd, buffer.vector->data, buffer.vector->count * lisp_type_size(buffer.vector->default_value.type));
  if(r == -1)
    return nil;
  return integer(r);
}


lisp_value lisp_recv_fd(lisp_value item, lisp_value buffer){
  var fd = get_fd(item);
  if(fd == -1){
    printf("NO FD\n");
	 return nil;
  }
  // non-blocking receive.
  // first check if data is read (POLLIN)
  // if data is read, data is read.
  // if data is read and 0 bytes are read, 
  struct pollfd test = {0};
  test.events = POLLIN;
  test.fd = fd;
  int stat = poll(&test, 1, 0);
  if(stat == 0)
    return integer(0);
  int r = recv(fd, buffer.vector->data, buffer.vector->count * lisp_type_size(buffer.vector->default_value.type), 0);
  
  if(stat != 0 && r == 0)
    return nil;
  return integer(r);
}



lisp_value lisp_write_fd(lisp_value item, lisp_value buffer){
  var fd = get_fd(item);
  if(fd == -1)
	 return nil;

  ssize_t written = write(fd, buffer.vector->data, buffer.vector->count * lisp_type_size(buffer.vector->default_value.type));

  return integer(written); 
}

void * lisp_perform_work(void * args){
  lisp_value eval = {.type = LISP_FUNCTION, .function = args};
  lisp_eval(current_context->globals, new_cons(eval, nil));
  return NULL;
}
#ifndef WASM
pthread_t foxlisp_create_thread(void * (* f)(void * data), void * data);


lisp_value thread_start(lisp_value func){
  type_assert(func, LISP_FUNCTION);
  var thread = foxlisp_create_thread(lisp_perform_work, func.function);
  
  return new_cons(get_symbol("thread"), native_pointer((void *) thread));
}

lisp_value thread_join(lisp_value func){
  type_assert(func, LISP_CONS);
  type_assert(cdr(func), LISP_NATIVE_POINTER);
  
  pthread_t thread = (pthread_t) cdr(func).native_pointer;
  pthread_join(thread, NULL);
  return nil;
}


lisp_value thread_create_mutex(){
  pthread_mutex_t *lock = lisp_malloc(sizeof(*lock));
  *lock = (pthread_mutex_t){0};
  pthread_mutex_init(lock, NULL);
  return new_cons(get_symbol("mutex"), native_pointer(lock));
}

lisp_value thread_lock_mutex(lisp_value lock){
  type_assert(lock, LISP_CONS);
  type_assert(cdr(lock), LISP_NATIVE_POINTER);
  
  pthread_mutex_t * l = lock.native_pointer;
  pthread_mutex_lock(l);
  return t;
}


lisp_value thread_unlock_mutex(lisp_value lock){
  type_assert(lock, LISP_CONS);
  type_assert(cdr(lock), LISP_NATIVE_POINTER);
  
  pthread_mutex_t * l = lock.native_pointer;
  pthread_mutex_lock(l);
  return t;
}

lisp_value thread_destroy_mutex(lisp_value lock){
  type_assert(lock, LISP_CONS);
  type_assert(cdr(lock), LISP_NATIVE_POINTER);
  pthread_mutex_t * l = lock.native_pointer;
  pthread_mutex_destroy(l);
 return t;
}

#endif

lisp_value thread_sleep(lisp_value time){
  var t = lisp_rational(time).rational;
  
  iron_usleep((int)(t * 1000000.0));
  return nil;
}


void lrn(const char * l, int args, void * f);
void tcp_register(){

  lrn("tcp:listen", 1, tcp_listen);
  lrn("tcp:connect", 2, tcp_connect);
  lrn("tcp:accept", 1, tcp_accept);
  lrn("fd:close", 1, lisp_close_fd);
  lrn("fd:read", 2, lisp_read_fd);
  lrn("fd:write", 2, lisp_write_fd);
  lrn("fd:set-blocking", 2, tcp_set_nonblock);
  lrn("fd:recv", 2, lisp_recv_fd);

  #ifndef WASM
  lrn("thread:start", 1, thread_start);
  lrn("thread:join", 1, thread_join);
  lrn("thread:create-mutex", 0, thread_create_mutex);
  lrn("thread:lock-mutex", 1, thread_lock_mutex);
  lrn("thread:unlock-mutex", 1, thread_unlock_mutex);
  lrn("thread:destroy-mutex", 1, thread_destroy_mutex);
  #endif

  lrn("thread:sleep", 1, thread_sleep);
}
