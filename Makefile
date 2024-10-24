# Copyright 2023 Echipa PCLP1

# compiler setup
CC=gcc
CFLAGS=-Wall -Wextra -std=c99

# define targets
TARGETS = Matrix_Simulator

build: $(TARGETS)

Matrix_Simulator: Matrix_Simulator.c
	$(CC) $(CFLAGS) -g Matrix_Simulator.c -o Matrix_Simulator

pack:
	zip -FSr 315CA_IoanGeorge-Victor_Tema2.zip README Makefile *.c *.h

clean:
	rm -f $(TARGETS)

.PHONY: pack clean
