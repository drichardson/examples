TARGETS=utc_time

all: $(TARGETS)

utc_time: utc_time.c
	$(CC) $^ -o $@ -g -O0 -D_XOPEN_SOURCE=700 -D_BSD_SOURCE -Wall -Werror

clean:
	$(RM) $(TARGETS) 
	$(RM) -r *.dSYM


