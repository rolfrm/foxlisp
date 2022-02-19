OPT = -O0 -g3
LIB_SOURCES1 = main.c hashtable.c

LIB_SOURCES = $(addprefix src/, $(LIB_SOURCES1))
CC = gcc
TARGET = run
LIB_OBJECTS =$(LIB_SOURCES:.c=.o)
LDFLAGS= -L. $(OPT)
LIBS= -lpthread -ldl libmicroio.a -lgc
ALL= $(TARGET)
CFLAGS = -Isrc/ -I. -Iinclude/ -I../../Desktop/gc/bdwgc/include/ -Ilibmicroio/include -std=gnu11 -c $(OPT) -Werror=implicit-function-declaration -Wformat=0 -D_GNU_SOURCE -fdiagnostics-color  -Wwrite-strings -msse4.2 -Werror=maybe-uninitialized -DUSE_VALGRIND -DDEBUG -Wall

all: libmicroio.a
all: $(TARGET)

$(TARGET): $(LIB_OBJECTS) 
	$(CC) $(LDFLAGS)   $(LIB_OBJECTS) $(LIBS)  -o $(TARGET)

.PHONY: iron/libiron.a

libmicroio/libmicroio.a:
	make -C libmicroio

libmicroio.a:libmicroio/libmicroio.a
	cp libmicroio/libmicroio.a .

.c.o: $(HEADERS) $(LEVEL_CS)
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $@.depends

release: OPT = -O4 -g0
release: all

wasm_release: OPT = -flto=thin -O3 -g0
wasm_release: wasm
wasm: CC = emcc
wasm: CFLAGS = -c -Isrc/ -I. -Iinclude/ -Isubmodule_openssl/include
wasm: LDFLAGS= -s WASM=1 -s USE_GLFW=3 --gc-sections 
wasm: LIBS= libiron.bc libcrypto.bc -lpthread -ldl
wasm: TARGET = run.js
wasm: libiron.bc
wasm: libcrypto.bc
wasm: $(TARGET)


depend: h-depend
clean:
	rm -f $(LIB_OBJECTS) $(ALL) src/*.o.depends src/*.o
	rm libcrypto.bc libiron.bc
	rm libcrypto.a libiron.a
	make -C iron clean
.PHONY: test
test: $(TARGET)
	make -f makefile.test test

-include $(LIB_OBJECTS:.o=.o.depends)


