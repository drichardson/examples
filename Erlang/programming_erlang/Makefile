ERL=$(wildcard *.erl)
MODULES=$(basename ${ERL})
BEAM=$(addsuffix .beam,${MODULES})

all: ${BEAM}

run_afile_client: afile_client.beam afile_server.beam
	erl -noshell -s afile_client run

%.beam: %.erl
	erlc $<

clean:
	$(RM) $(BEAM)
