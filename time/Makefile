TARGETS=utc_time localtime

all: $(TARGETS)

utc_time: utc_time.c
	$(CC) $^ -o $@ -g -O0 -D_XOPEN_SOURCE=700 -D_BSD_SOURCE -Wall -Werror

localtime: localtime.c
	$(CC) $^ -o $@ -g -O0 -Wall -Werror

clean:
	$(RM) $(TARGETS) 
	$(RM) -r *.dSYM


