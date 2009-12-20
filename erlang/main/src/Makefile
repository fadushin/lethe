# leave these lines alone
.SUFFIXES: .erl .beam .yrl

EBIN_DIR=../../target/main/ebin/

SRC_MODULES = xrpc uuid xtimer channel channel_manager

BIN_MODULES = $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})

${EBIN_DIR}%.beam : %.erl
	erlc -I ../include -o ${EBIN_DIR} $<

all: compile

compile: $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})

clean:	
	rm -f $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})
