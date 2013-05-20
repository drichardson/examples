targets := $(shell ../listTargets.sh)

all: $(targets)

%: %.go
	go build -gcflags "-N -l" $<

clean:
	$(RM) $(targets)
