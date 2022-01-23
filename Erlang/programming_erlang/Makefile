ERL=$(wildcard *.erl)
MODULES=$(basename ${ERL})
BEAM=$(addsuffix .beam,${MODULES})

all: ${BEAM}

%.beam: %.erl
	erlc $<

clean:
	$(RM) $(BEAM)
