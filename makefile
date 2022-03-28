OPT = -O0 -g3
LIB_SOURCES1 = main.c hashtable.c gc.c

LIB_SOURCES = $(addprefix src/, $(LIB_SOURCES1))
CC = gcc
TARGET = run
LIB_OBJECTS =$(LIB_SOURCES:.c=.o)
BCOBJECTS =$(LIB_SOURCES:.c=.bc)
LDFLAGS= -L. $(OPT) -rdynamic
LIBS= -lpthread -ldl libmicroio.a -lm
BCLIBS = -lpthread -ldl -lm
ALL= $(TARGET)
CFLAGS = -Isrc/  -I. -Iinclude/ -Igc/bdwgc/include/ -Ilibmicroio/include -std=gnu11 -c $(OPT) -Werror=implicit-function-declaration -Wformat=0 -D_GNU_SOURCE -fdiagnostics-color  -Wwrite-strings -Werror=maybe-uninitialized -DUSE_VALGRIND -DDEBUG -Wall -shared -fPIC

all: libmicroio.a
all: $(TARGET)

$(TARGET): $(LIB_OBJECTS) gc.o foxgl.so
	$(CC) $(LDFLAGS) $(LIB_OBJECTS) gc.o $(LIBS)  -o $(TARGET)

.FORCE:
iron/libiron.so: .FORCE
	make -C iron

libiron.so: iron/libiron.so
	cp iron/libiron.so libiron.so

libmicroio/libmicroio.a:
	make -C libmicroio

libmicroio.bc:
	emcc -Ilibmicroio/include -c libmicroio/src/microio.c -o libmicroio.bc

foxgl.so: src/foxgl.c src/tcp.c libiron.so
	gcc src/foxgl.c src/tcp.c -I.. -L.  -g3 -O2 -liron -fPIC -shared -o foxgl.so -Wl,-rpath,.

gc.o: gc/bdwgc/extra/gc.c
	gcc -DGC_PTHREADS -DGC_THREADS -Igc/libatomic_ops/src -c gc/bdwgc/extra/gc.c -o gc.o -O3 -Igc/bdwgc/include

gc.bc: gc/bdwgc/extra/gc.c
	emcc -Igc/libatomic_ops/src -c gc/bdwgc/extra/gc.c -o gc.bc -O3 -Igc/bdwgc/include

index.js: $(BCOBJECTS) gc.bc libmicroio.bc
	emcc $(LDFLAGS) $(BCOBJECTS) $(BCLIBS)  --preload-file fox2.lisp -s ASYNCIFY -s USE_PTHREADS=1 -s PTHREAD_POOL_SIZE=4 libmicroio.bc gc.bc -o $@ 

libmicroio.a:libmicroio/libmicroio.a
	cp libmicroio/libmicroio.a .

.c.o: $(HEADERS) $(LEVEL_CS)
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $@.depends

%.bc: %.c $(HEADERS)
	emcc $(CFLAGS) $< -o $@ -MMD -MF $@.depends

release: OPT = -O4 -g0
release: all

depend: h-depend
clean:
	rm -f $(LIB_OBJECTS) $(ALL) src/*.o.depends src/*.o src/*.bc
	rm libcrypto.bc libiron.bc
	rm libcrypto.a libiron.a
	make -C iron clean
.PHONY: test
test: $(TARGET)
	make -f makefile.test test

-include $(LIB_OBJECTS:.o=.o.depends)


