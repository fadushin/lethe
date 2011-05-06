# 
# Copyright (c) dushin.net
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of dushin.net nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 

PRODUCT_NAME=lethe
VERSION=1.0-SNAPSHOT

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
APPFILES=$(patsubst $(SOURCE_DIR)/%.app, $(EBIN_DIR)/%.app, $(wildcard $(SOURCE_DIR)/*.app))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v

TEST_DIR=test
TEST_EBIN_DIR=test-ebin
TEST_SOURCES=$(wildcard $(TEST_DIR)/net_dushin_lethe*.erl)
TEST_MODULES=$(patsubst $(TEST_DIR)/%.erl, $(TEST_EBIN_DIR)/%.done, $(TEST_SOURCES))
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(TEST_EBIN_DIR)/%.beam, $(TEST_SOURCES))
TEST_ERLC_OPTS=-I $(INCLUDE_DIR) -o $(TEST_EBIN_DIR) -Wall +debug_info # +native -v

DIST_DIR=$(PRODUCT_NAME)-$(VERSION)

all: compile test dist

veryclean: clean test-clean dist-clean

# 
# Compilation targets
# 

compile: $(EBIN_DIR) $(TARGETS) $(APPFILES)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(TEST_EBIN_DIR)/%.beam: $(TEST_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(EBIN_DIR)/%.app: $(SOURCE_DIR)/%.app
	cp $< $@

$(EBIN_DIR) $(TEST_EBIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(EBIN_DIR) $(TEST_EBIN_DIR) $(DIST_DIR)


# 
# test targets
# 

test: test-compile $(TEST_MODULES)

test-compile: compile $(TEST_EBIN_DIR) $(TEST_TARGETS)

$(TEST_EBIN_DIR)/net_dushin_lethe_%_test.done: $(TEST_EBIN_DIR)/net_dushin_lethe_%_test.beam
	erl -noshell -pa ebin -pa test-ebin -s $(subst $(TEST_EBIN_DIR)/, , $(subst .done, , $@)) test -s erlang halt
	@touch $@

$(TEST_EBIN_DIR)/%.beam: $(TEST_DIR)/%.erl $(INCLUDES)
	erlc $(TEST_ERLC_OPTS) $<

test-clean:
	rm -rf $(TEST_EBIN_DIR)


# 
# dist targets
# 

dist: $(DIST_DIR).zip

$(DIST_DIR).zip: compile
	mkdir -p $(DIST_DIR)
	mkdir -p $(DIST_DIR)/logs
	mkdir -p $(DIST_DIR)/content
	cp -r ebin bin $(DIST_DIR)
	zip -r $(DIST_DIR).zip $(DIST_DIR)

dist-clean:
	rm -rf $(DIST_DIR) $(DIST_DIR).zip




# 
# 
# 

RUN_ERL_CMD=erl -boot start_sasl -pa ./ebin -s net_dushin_lethe_launcher start1 -yaws embedded true

run: dist
	$(RUN_ERL_CMD)
