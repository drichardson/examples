TARGETS=utc_time

all: $(TARGETS)

utc_time: utc_time.c
	$(CC) $^ -o $@ -g -O0 -Wall -Werror

clean:
	$(RM) $(TARGETS) 
	$(RM) -r *.dSYM


