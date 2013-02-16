targets := example1-fib example2-httpserver

all: packages $(targets)

packages:
	make -C fibonacci

example1-fib: example1-fib.go fibonacci/fibonacci.6
	6g example1-fib.go
	6l -o $@ example1-fib.6

example2-httpserver: example2-httpserver.6
	6l -o $@ $<

% : %.6
	6l -o $@ $<

%.6  : %.go
	6g $<

clean:
	$(RM) *.6 $(targets)
	make -C fibonacci clean
