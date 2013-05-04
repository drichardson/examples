targets := $(shell ../listTargets.sh)

all: $(targets)

%: %.go
	go build $<

clean:
	$(RM) $(targets)
