targets := $(shell ./listTargets.sh)

all: $(targets)

% : %.6
	6l -o $@ $<

%.6  : %.go
	6g $<

clean:
	$(RM) *.6 $(targets)
