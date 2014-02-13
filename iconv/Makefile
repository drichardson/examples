TARGETS=utf8-to-utf32

all: $(TARGETS)

utf8-to-utf32: utf8-to-utf32.c
	$(CC) -o $@ $^ -g -O0 -liconv

clean:
	$(RM) $(TARGETS)
