#include <iron/full.h>
#include <iron/gl.h>
#include <iron/utf8.h>
#include <microio.h>
#include "foxlisp.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
static lisp_value lisp_pipe() {
  int fds[2];
  int r = pipe(fds);
  if (r == -1)
    return nil;
  return new_cons(integer(fds[0]), integer(fds[1]));
}

static lisp_value lisp_fork() { return integer(fork()); }

static lisp_value lisp_exec(lisp_value filename, lisp_value argv,
                            lisp_value env) {
  TYPE_ASSERT(filename, LISP_STRING);
  int arglen = lisp_length(argv).integer;
  int envlen = lisp_length(env).integer;
  char *args[arglen + 2];
  char *envs[envlen + 1];
  args[arglen + 1] = NULL;
  envs[envlen] = NULL;
  args[0] = filename.string;
  for (int i = 0; i < arglen; i++) {
    TYPE_ASSERT(car(argv), LISP_STRING);
    args[i + 1] = car(argv).string;
    argv = cdr(argv);
  }
  for (int i = 0; i < envlen; i++) {
    TYPE_ASSERT(car(env), LISP_STRING);
    envs[i] = car(env).string;
    env = cdr(env);
  }
  int ret = execvpe(filename.string, args, envs);
  if (ret == -1) {
    raise_string("Unable to start application");
  }
  return nil;
}

static lisp_value lisp_close(lisp_value fd) {
  TYPE_ASSERT(fd, LISP_INTEGER);
  close(fd.integer);
  return nil;
}

static lisp_value posix_read(lisp_value fd, lisp_value buffer) {
  int rd = read(fd.integer, buffer.vector->data,
                vector_length(buffer).integer * buffer.vector->elem_size);
  return integer(rd);
}

static lisp_value posix_write(lisp_value fd, lisp_value buffer) {
  int rd = write(fd.integer, buffer.vector->data,
                 vector_length(buffer).integer * buffer.vector->elem_size);
  return integer(rd);
}

static lisp_value posix_dup2(lisp_value fdnew, lisp_value fdold) {
  return integer(dup2(fdnew.integer, fdold.integer));
}

void lrn(const char *l, int args, void *f);
void lisp_process_module_init() {
  lrn("posix:pipe", 0, lisp_pipe);
  lrn("posix:fork", 0, lisp_fork);
  lrn("posix:exec", 3, lisp_exec);
  lrn("posix:close", 3, lisp_close);
  lrn("posix:read", 2, posix_read);
  lrn("posix:write", 2, posix_write);
  lrn("posix:dup2", 2, posix_dup2);
  lisp_register_value("posix:stderr", integer(2));
  lisp_register_value("posix:stdout", integer(1));
  lisp_register_value("posix:stdin", integer(0));
}
