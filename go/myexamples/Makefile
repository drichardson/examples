targets := $(shell ../listTargets.sh)

all: $(targets)

%: %.go
	go build -o $@ -gcflags "-N -l" $< 

clean:
	$(RM) $(targets)
