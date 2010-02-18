## -*- makefile -*-

######################################################################
## Erlang

### edit this ###

#ERL_HOME=/opt/erlang/current/
#JAVA_HOME=/opt/java/sun/jdk/bin
APPLICATION=nerlo
TESTCOOKIE=123456

#################

include ../include/paths.mk

ifndef ERL_HOME
$(error ERL_HOME not defined)
endif
ifndef JAVA_HOME
$(error JAVA_HOME not defined)
endif

ERL := $(ERL_HOME)/bin/erl
ERLC := $(ERL_HOME)/bin/erlc

INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := ../ebin $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

ifdef no_tests
  ERLC_FLAGS += -DNOTEST
endif

ifdef hipe
  ERLC_FLAGS += +native
endif

EBIN_DIR := ../ebin
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

../ebin/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<
