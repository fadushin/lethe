# leave these lines alone
.SUFFIXES: .erl .beam .yrl

EBIN_DIR=../../target/test/ebin/

SRC_MODULES = test channel_test

BIN_MODULES = $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})

${EBIN_DIR}%.beam : %.erl
	erlc -I ../include -I ../../main/include -o ${EBIN_DIR} $<

all: compile

compile: $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})

clean:	
	rm -f $(addprefix ${EBIN_DIR}, ${SRC_MODULES:%=%.beam})
