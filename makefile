OPT = -O3 -g0
LIB_SOURCES1 = main.c foxgl.c tcp.c foxal.c gc.c
LIB_SOURCES = $(addprefix src/, $(LIB_SOURCES1))
CC = gcc
TARGET = run
LIB_OBJECTS =$(LIB_SOURCES:.c=.o)
BCOBJECTS =$(LIB_SOURCES:.c=.bc)
LDFLAGS= -L. $(OPT) -rdynamic
LIBS= libmicroio.a -lm -lGL -lglfw3 -lX11 -lopenal
BCLIBS = -s USE_GLFW=3 -s WASM=1 -s USE_WEBGL2=1 -lm -lglfw3 -lGL -lopenal
BCFLAGS = -DWASM  
BCLDFLAGS= -s ALLOW_MEMORY_GROWTH=1
ALL= $(TARGET)
CFLAGS = -Isrc/  -I. -Iinclude/ -Igc/bdwgc/include/ -Ilibmicroio/include -std=gnu11 -c $(OPT) -Werror=implicit-function-declaration -Wformat=0 -D_GNU_SOURCE -fdiagnostics-color  -Wwrite-strings -Werror=maybe-uninitialized -DUSE_VALGRIND -DDEBUG -Wall -shared -fPIC

all: libmicroio.a
all: $(TARGET)

$(TARGET): $(LIB_OBJECTS) libiron.a gc.o
	$(CC) $(LDFLAGS) $(LIB_OBJECTS) -lpthread gc.o  libiron.a $(LIBS) -ldl  -o $(TARGET)

.FORCE:
iron/libiron.so: .FORCE
	make -C iron

libiron.a: iron/libiron.a
	cp iron/libiron.a libiron.a

libmicroio/libmicroio.a:
	make -C libmicroio

libmicroio.bc:
	emcc $(BCFLAGS) -Ilibmicroio/include -c libmicroio/src/microio.c -o libmicroio.bc

gc.o: gc/bdwgc/extra/gc.c
	gcc -DGC_PTHREADS -DGC_THREADS -Igc/libatomic_ops/src -c gc/bdwgc/extra/gc.c -o gc.o -O3 -Igc/bdwgc/include

gc.bc: gc/bdwgc/extra/gc.c
	emcc $(BCFLAGS) -Igc/libatomic_ops/src -c gc/bdwgc/extra/gc.c -o gc.bc -O3 -Igc/bdwgc/include

libiron.bc: iron/libiron.bc
	cp iron/libiron.bc .

index.js: $(BCOBJECTS) libmicroio.bc ld50.lisp foxgl.lisp libiron.bc
	emcc $(LDFLAGS) $(BCOBJECTS) $(BCLIBS) $(BCLDFLAGS) -s ALLOW_MEMORY_GROWTH=1  libiron.bc  -s ASYNCIFY -s libmicroio.bc -o $@  --embed-file ./ld50.lisp@ld50.lisp --embed-file ./lisp1.lisp@lisp1.lisp --embed-file ./vec2.lisp@vec2.lisp  --embed-file ./foxgl.lisp@foxgl.lisp --embed-file DejaVuSans.ttf

libmicroio.a:libmicroio/libmicroio.a
	cp libmicroio/libmicroio.a .

.c.o: $(HEADERS) $(LEVEL_CS)
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $@.depends

%.bc: %.c $(HEADERS)
	emcc $(BCFLAGS) $(CFLAGS) $< -o $@ -MMD -MF $@.depends

release: OPT = -O4 -g0
release: all

depend: h-depend
clean:
	rm -f $(LIB_OBJECTS) $(ALL) src/*.o.depends src/*.o src/*.bc
	rm libcrypto.bc libiron.bc
	rm libcrypto.a libiron.a
	rm foxgl.so
	make -C iron clean
.PHONY: test
test: $(TARGET)
	make -f makefile.test test

-include $(LIB_OBJECTS:.o=.o.depends)


