targets := $(shell ../listTargets.sh)

all: $(targets)

%: %.go
	go build -o $@ $< 

clean:
	$(RM) $(targets)
