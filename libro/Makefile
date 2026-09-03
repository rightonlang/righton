CC ?= gcc
CFLAGS ?= -O2 -fPIC -Wall -Wextra -Iinclude
AR ?= ar
SRC = src/libro.c
OBJ = libro.o
LIB = libro.a

all: $(LIB)

$(OBJ): $(SRC) include/libro.h
	$(CC) $(CFLAGS) -c $(SRC) -o $(OBJ)

$(LIB): $(OBJ)
	$(AR) rcs $(LIB) $(OBJ)

clean:
	rm -f $(OBJ) $(LIB)

install: $(LIB)
	mkdir -p /usr/local/lib
	cp $(LIB) /usr/local/lib/libro.a
	mkdir -p /usr/local/include
	cp include/libro.h /usr/local/include/libro.h

.PHONY: all clean install
