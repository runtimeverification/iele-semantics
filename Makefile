# Common to all versions of K
# ===========================

UNAME_S := $(shell uname -s)
INSTALL := install -D

ifeq ($(UNAME_S),Darwin)
INSTALL=install
CPATH=/usr/local/include
export CPATH
endif

ifeq ($(BYTE),yes)
EXT=cmo
LIBEXT=cma
DLLEXT=cma
OCAMLC=c
LIBFLAG=-a
else
EXT=cmx
LIBEXT=cmxa
DLLEXT=cmxs
OCAMLC=opt -O3
LIBFLAG=-shared
endif

INSTALL_PREFIX := /usr
INSTALL_BIN    ?= $(INSTALL_PREFIX)/bin
INSTALL_LIB    ?= $(INSTALL_PREFIX)/lib/kiele

IELE_DIR  := .
BUILD_DIR := .build

KIELE_VERSION     ?= 0.2.0
KIELE_RELEASE_TAG ?= v$(KIELE_VERSION)-$(shell git rev-parse --short HEAD)

PLUGIN=$(abspath $(IELE_DIR))/plugin
PROTO=$(abspath $(IELE_DIR))/proto

IELE_BIN         := $(BUILD_DIR)$(INSTALL_BIN)
IELE_LIB         := $(BUILD_DIR)$(INSTALL_LIB)
IELE_RUNNER      := $(IELE_BIN)/kiele
IELE_ASSEMBLE    := $(IELE_LIB)/iele-assemble
IELE_INTERPRETER := $(IELE_LIB)/standalone/iele-testing-kompiled/iele-interpreter
IELE_CHECK       := $(IELE_LIB)/check/well-formedness-kompiled/interpreter
IELE_VM          := $(IELE_LIB)/node/iele-testing-kompiled/iele-vm
IELE_TEST_VM     := $(IELE_LIB)/iele-test-vm
IELE_TEST_CLIENT := $(IELE_LIB)/iele-test-client

KIELE = kiele

# We set SHELL here for Mac: https://stackoverflow.com/a/25506676
SHELL=/bin/bash

export PATH:=$(CURDIR)/$(IELE_LIB):$(CURDIR)/$(IELE_BIN):$(DESTDIR)$(INSTALL_LIB):$(PATH)

.PHONY: all clean distclean libff protobuf coverage secp256k1 cryptopp \
        build build-interpreter build-vm build-check build-haskell build-node build-testnode build-assembler \
		install install-interpreter install-vm install-kiele install-check uninstall \
        split-tests split-vm-tests split-blockchain-tests test-node test-iele-coverage test-generate-report \
        test-evm test-vm test-blockchain test-wellformed test-illformed test-bad-packet test-interactive test-sourcemap \
        test-iele test-iele-haskell test-iele-failing test-iele-slow test-iele-node assemble-iele-test test
.SECONDARY:

all: build split-tests

clean:
	rm -rf $(BUILD_DIR)

distclean: clean

# Dependencies
# ------------

ifndef SYSTEM_LIBFF

ifeq ($(UNAME_S),Darwin)
OPENSSL_ROOT     := $(shell brew --prefix openssl)
MACOS_CMAKE_OPTS := -DOPENSSL_ROOT_DIR=$(OPENSSL_ROOT) -DWITH_PROCPS=off
endif

libff_out := $(IELE_LIB)/libff/lib/libff.a

libff: $(libff_out)

$(libff_out): $(PLUGIN)/deps/libff/CMakeLists.txt
	@mkdir -p $(PLUGIN)/deps/libff/build
	cd $(PLUGIN)/deps/libff/build                                                                             \
	   && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$(INSTALL_LIB)/libff $(MACOS_CMAKE_OPTS) \
	   && make -s -j4                                                                                         \
	   && make install DESTDIR=../../../../$(BUILD_DIR)

endif # ifndef SYSTEM_LIBFF

protobuf_out := $(IELE_LIB)/plugin-node/proto/msg.pb.cc

protobuf: $(protobuf_out)

$(protobuf_out): $(PROTO)/proto/msg.proto
	mkdir -p $(dir $@)
	protoc --cpp_out=$(IELE_LIB)/plugin-node -I $(PROTO) $<
	cp $(PROTO)/world.h $(IELE_LIB)/plugin-node

ifndef SYSTEM_LIBSECP256K1

ifeq ($(UNAME_S),Darwin)
libsecp256k1_out := $(IELE_LIB)/libsecp256k1/lib/libsecp256k1.a
libsecp256k1_rm := $(addprefix $(IELE_LIB)/libsecp256k1/lib/,libsecp256k1.0.dylib libsecp256k1.dylib libsecp256k1.la)
libsecp256k1_destdir := $(abspath $(BUILD_DIR))
SECP256K1_ROOT := $(abspath $(IELE_LIB)/libsecp256k1)
endif

secp256k1: $(libsecp256k1_out)

$(libsecp256k1_out): $(PLUGIN)/deps/secp256k1/Makefile
	cd $(PLUGIN)/deps/secp256k1 \
	   && make                  \
	   && make install DESTDIR=$(libsecp256k1_destdir)
	rm $(libsecp256k1_rm)

$(PLUGIN)/deps/secp256k1/Makefile: $(PLUGIN)/deps/secp256k1/autogen.sh
	cd $(PLUGIN)/deps/secp256k1 \
	   && ./autogen.sh          \
	   && ./configure prefix=$(INSTALL_LIB)/libsecp256k1 --enable-module-recovery

endif # ifndef SYSTEM_LIBSECP256K1

ifndef SYSTEM_LIBCRYPTOPP

ifeq ($(UNAME_S),Darwin)
libcryptopp_out := $(IELE_LIB)/libcryptopp
CRYPTOPP_ROOT := $(abspath $(IELE_LIB)/libcryptopp)
endif

cryptopp: $(libcryptopp_out)

$(libcryptopp_out): $(PLUGIN)/deps/cryptopp/GNUmakefile
	cd $(PLUGIN)/deps/cryptopp \
	   && make libcryptopp.a   \
	   && make install PREFIX=$(INSTALL_LIB)/libcryptopp DESTDIR=../../../$(BUILD_DIR)

endif # ifndef SYSTEM_LIBCRYPTOPP

# Tests
# -----

CHECK         = git --no-pager diff --no-index --ignore-all-space -R
TEST_ASSEMBLE = $(IELE_DIR)/assemble-iele-test
TEST_BACKEND  = standalone
TEST_MODE     = NORMAL
TEST_SCHEDULE = DEFAULT
TEST_PORT     = 10000
TEST_ARGS     = --no-unparse
TEST_DIR      = $(IELE_DIR)/tests

test: split-tests test-vm test-iele test-iele-haskell test-iele-coverage test-wellformed test-illformed test-interactive test-node test-sourcemap test-generate-report

split-tests: split-vm-tests split-blockchain-tests

invalid_iele_tests_file=$(TEST_DIR)/failing.expected
invalid_iele_tests= $(addprefix $(IELE_DIR)/, $(shell cat $(invalid_iele_tests_file)))

split-vm-tests: $(patsubst $(TEST_DIR)/ethereum-tests/%.json,$(TEST_DIR)/%/make.timestamp, $(filter-out $(invalid_iele_tests), $(wildcard $(TEST_DIR)/ethereum-tests/VMTests/*/*.json)))

split-blockchain-tests: $(patsubst $(TEST_DIR)/ethereum-tests/%.json,$(TEST_DIR)/%/make.timestamp, $(filter-out $(invalid_iele_tests), $(wildcard $(TEST_DIR)/ethereum-tests/BlockchainTests/GeneralStateTests/*/*.json)))

vm_tests=$(wildcard $(TEST_DIR)/VMTests/*/*/*.iele.json)
blockchain_tests=$(wildcard $(TEST_DIR)/BlockchainTests/*/*/*/*.iele.json)
all_tests=$(vm_tests) $(blockchain_tests)
failing_tests = $(addprefix $(IELE_DIR)/,$(shell cat $(TEST_DIR)/failing.$(TEST_BACKEND)))
slow_tests    = $(addprefix $(IELE_DIR)/,$(shell cat $(TEST_DIR)/slow.$(TEST_BACKEND)))
skipped_tests=$(failing_tests) \
    $(wildcard $(TEST_DIR)/VMTests/vmPerformance/*/*.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/*/*/*_Frontier.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/*/*/*_Homestead.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/*/*/*_EIP150.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/*/*/*_EIP158.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/*/*/*_Constantinople.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/stQuadraticComplexityTest/*/*.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/stStaticCall/static_Call50000*/*.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/stStaticCall/static_Return50000*/*.iele.json) \
    $(wildcard $(TEST_DIR)/BlockchainTests/GeneralStateTests/stStaticCall/static_Call1MB1024Calldepth_d1g0v0/*.iele.json) \

coverage_tests=$(TEST_DIR)/iele/danse/ERC20/transfer_Caller-MoreThanBalance.iele.json

passing_tests=$(filter-out $(skipped_tests), $(all_tests))
passing_vm_tests=$(filter-out $(skipped_tests), $(vm_tests))
passing_blockchain_tests=$(filter-out $(skipped_tests), $(blockchain_tests))

passing_targets=$(passing_tests:=.test)
passing_vm_targets=$(passing_vm_tests:=.test)
passing_blockchain_targets=$(passing_blockchain_tests:=.test)

iele_tests=$(wildcard $(TEST_DIR)/iele/*/*/*.iele.json)
iele_assembled=$(iele_tests:=.test-assembled)
iele_quick_tests=$(filter-out $(slow_tests), $(iele_tests))
iele_passing_tests=$(filter-out $(failing_tests), $(iele_quick_tests))
iele_slow=$(slow_tests:=.test)
iele_failing=$(failing_tests:=.test)
iele_targets=$(iele_passing_tests:=.test)
iele_node_targets=$(iele_tests:=.nodetest)
iele_coverage_tests=$(coverage_tests:=.test-coverage)

iele_contracts=$(wildcard iele-examples/*.iele $(TEST_DIR)/iele/*/*/*.iele)
well_formed_contracts=$(filter-out $(wildcard $(TEST_DIR)/iele/*/ill-formed/*.iele), $(iele_contracts))
ill_formed_contracts=$(wildcard $(TEST_DIR)/iele/*/ill-formed/*.iele)
well_formedness_targets=$(well_formed_contracts:=.test-wellformed)
ill_formedness_targets=$(ill_formed_contracts:=.test-illformed)

sourcemap_tests=$(wildcard $(TEST_DIR)/sourcemaps/*.sourcemap)
sourcemap_targets=$(sourcemap_tests:=.test-sourcemap)

test-sourcemap: $(sourcemap_targets)

%.test-sourcemap: %.out
	$(CHECK) $< $*

%.sourcemap.out: $(IELE_ASSEMBLE)
	 --sourceMap iele-examples/$(shell basename $*).iele > $@

test-evm: test-vm test-blockchain
test-vm: $(passing_vm_targets)
test-blockchain: $(passing_blockchain_targets)
test-iele: $(iele_targets)
test-iele-haskell:
	$(MAKE) test-iele TEST_BACKEND=haskell
test-iele-slow: $(iele_slow)
test-iele-failing: $(iele_failing)
test-iele-node: $(iele_node_targets)
assemble-iele-test: $(iele_assembled)
test-wellformed: $(well_formedness_targets)
test-illformed: $(ill_formedness_targets)
test-iele-coverage: $(iele_coverage_tests)

test-bad-packet:
	netcat 127.0.0.1 $(TEST_PORT) -q 2 < $(TEST_DIR)/bad-packet
	netcat 127.0.0.1 $(TEST_PORT) -q 2 < $(TEST_DIR)/bad-packet-2
	iele-test-vm $(TEST_DIR)/iele/danse/sum/sum_zero.iele.json $(TEST_PORT)

test-interactive: iele-examples/erc20.iele $(TEST_DIR)/iele/danse/factorial/factorial_positive.iele.json.test-assembled
	kiele help
	kiele --help
	kiele version
	kiele --version
	kiele assemble iele-examples/erc20.iele
	echo
	kiele interpret $(TEST_DIR)/iele/danse/factorial/factorial_positive.iele.json.test-assembled
	kiele check --schedule DANSE iele-examples/erc20.iele
	# kiele vm

test-node: TEST_PORT=9001
test-node:
	$(TEST_DIR)/node-test.sh $(MAKEFLAGS) --port $(TEST_PORT)

report_tests := $(wildcard $(TEST_DIR)/reports/*/report.json)
test-generate-report: $(report_tests:.json=.html)
$(TEST_DIR)/reports/%/report.html: $(TEST_DIR)/reports/%/report.json
	cd $(dir $@) && kiele generate-report report.json -o report.html

$(TEST_DIR)/VMTests/%:  TEST_MODE     = VMTESTS
%.iele.test-wellformed: TEST_SCHEDULE = DANSE
%.iele.test-illformed:  TEST_SCHEDULE = DANSE

%.json.test: %.json.test-assembled
	$(KIELE) interpret --backend $(TEST_BACKEND) --mode $(TEST_MODE) --schedule $(TEST_SCHEDULE) $(TEST_ARGS) $<

%.json.test-coverage: %.json.test-assembled
	$(KIELE) interpret --backend $(TEST_BACKEND) --mode $(TEST_MODE) --schedule $(TEST_SCHEDULE) --coverage $< | grep -A 30 "<kiele-coverage>" > $*.json.coverage-out
	$(CHECK) $*.json.coverage-out $*.json.coverage-expected
	rm -rf $*.json.coverage-out

%.json.test-assembled: %.json
	$(TEST_ASSEMBLE) $< > $@

%.iele.test-wellformed: %.iele
	$(KIELE) check --schedule $(TEST_SCHEDULE) $<

%.iele.test-illformed: %.iele
	! $(KIELE) check --schedule $(TEST_SCHEDULE) $<

%.nodetest: %
	iele-test-vm $< $(TEST_PORT)

$(TEST_DIR)/%/make.timestamp: $(TEST_DIR)/ethereum-tests/%.json $(TEST_DIR)/evm-to-iele/evm-to-iele $(TEST_DIR)/evm-to-iele/evm-test-to-iele
	@echo "==   split: $@"
	mkdir -p $(dir $@)
	$(TEST_DIR)/split-test.py $< $(dir $@)
	touch $@

$(TEST_DIR)/evm-to-iele/evm-to-iele: $(wildcard $(TEST_DIR)/evm-to-iele/*.ml $(TEST_DIR)/evm-to-iele/*.mli)
	cd $(TEST_DIR)/evm-to-iele && eval `opam config env` && ocamlfind $(OCAMLC) -g ieleUtil.mli ieleUtil.ml evm.mli evm.ml iele.mli iele.ml conversion.mli conversion.ml main.ml -package zarith -package hex -linkpkg -o evm-to-iele

# Build Source Files
# ------------------

k_files:=$(addprefix $(IELE_DIR)/,iele-testing.md data.md iele.md iele-gas.md iele-binary.md plugin/plugin/krypto.md iele-syntax.md iele-node.md well-formedness.md iele-coverage.md)
checker_files:=$(addprefix $(IELE_DIR)/,iele-syntax.md well-formedness.md data.md)

# LLVM Builds
# -----------

ifeq ($(COVERAGE),k)
KOMPILE_FLAGS=--coverage
else ifeq ($(COVERAGE),ocaml)
PREDICATES=-predicates coverage
endif

ifneq ($(RELEASE),)
KOMPILE_FLAGS+=-O3
endif

LIB_PROCPS=-lprocps

ifeq ($(UNAME_S),Darwin)
MACOS_INCLUDE_OPTS := -I $(OPENSSL_ROOT)/include -I $(SECP256K1_ROOT)/include -I $(CRYPTOPP_ROOT)/include
MACOS_LINK_OPTS    := -L $(OPENSSL_ROOT)/lib -L $(SECP256K1_ROOT)/lib -L $(CRYPTOPP_ROOT)/lib
LIB_PROCPS=
endif

build-node: $(IELE_VM) $(IELE_RUNNER)
build-testnode : $(IELE_TEST_VM) $(IELE_TEST_CLIENT) $(IELE_RUNNER)

KOMPILE_INCLUDE_OPTS := $(addprefix -ccopt , -I $(PLUGIN)/plugin-c -I $(abspath $(IELE_LIB))/plugin-node -I $(abspath $(IELE_LIB))/plugin-node -I $(abspath $(IELE_LIB))/libff/include) -I $(IELE_DIR)
KOMPILE_LINK_OPTS    := $(addprefix -ccopt , -L /usr/local/lib -L $(abspath $(IELE_LIB))/libff/lib -lprotobuf -lff -lcryptopp -lsecp256k1 $(LIB_PROCPS) -lssl -lcrypto)
KOMPILE_CPP_FILES    := $(PLUGIN)/plugin-c/k.cpp $(PLUGIN)/plugin-c/crypto.cpp $(PROTO)/blockchain.cpp $(PROTO)/world.cpp $(PLUGIN)/plugin-c/blake2.cpp $(PLUGIN)/plugin-c/plugin_util.cpp
KOMPILE_CPP_OPTS     := $(addprefix -ccopt , $(KOMPILE_CPP_FILES))
ifeq ($(UNAME_S),Darwin)
KOMPILE_INCLUDE_OPTS += $(addprefix -ccopt , $(MACOS_INCLUDE_OPTS))
KOMPILE_LINK_OPTS    += $(addprefix -ccopt , $(MACOS_LINK_OPTS))
endif

$(IELE_CHECK): $(checker_files) $(protobuf_out) $(libff_out) $(libsecp256k1_out) $(libcryptopp_out)
	@mkdir -p $(dir $@)
	kompile --debug --main-module IELE-WELL-FORMEDNESS-STANDALONE --md-selector "(k & ! node) | standalone"                                                                       \
	    --syntax-module IELE-SYNTAX well-formedness.md --directory $(abspath $(IELE_LIB)/check) --hook-namespaces KRYPTO                                                          \
	    --gen-glr-bison-parser --bison-stack-max-depth 10000000                                                                                                                   \
	    --backend llvm -ccopt $(abspath $(protobuf_out)) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS) \
	    -ccopt -o -ccopt $(abspath $@)

$(IELE_LIB)/standalone/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! node) | standalone"
$(IELE_LIB)/node/iele-testing-kompiled/interpreter:       MD_SELECTOR="(k & ! standalone) | node"

$(IELE_LIB)/%/iele-testing-kompiled/interpreter: $(k_files) $(protobuf_out) $(libff_out) $(libsecp256k1_out) $(libcryptopp_out)
	@mkdir -p $(dir $@)
	kompile --debug --main-module IELE-TESTING --verbose --md-selector $(MD_SELECTOR)                                                                                  \
	    --syntax-module IELE-SYNTAX iele-testing.md --directory $(abspath $(IELE_LIB)/$*) --hook-namespaces "KRYPTO BLOCKCHAIN"                                        \
	    --backend llvm -ccopt $(abspath $(protobuf_out)) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 $(KOMPILE_FLAGS) \
	    -ccopt -o -ccopt $(abspath $@)

LLVM_KOMPILE_INCLUDE_OPTS := -I $(PLUGIN)/plugin-c/ -I $(abspath $(IELE_LIB))/plugin-node -I vm/c/ -I vm/c/iele/ -I $(abspath $(IELE_LIB))/libff/include
LLVM_KOMPILE_LINK_OPTS    := -L /usr/local/lib -L $(abspath $(IELE_LIB))/libff/lib -lff -lprotobuf -lgmp $(LIB_PROCPS) -lcryptopp -lsecp256k1 -lssl -lcrypto
ifeq ($(UNAME_S),Darwin)
LLVM_KOMPILE_INCLUDE_OPTS += $(MACOS_INCLUDE_OPTS)
LLVM_KOMPILE_LINK_OPTS    += $(MACOS_LINK_OPTS)
endif

build-kiele: $(IELE_RUNNER)

build-check: $(IELE_CHECK) $(IELE_RUNNER)

build-interpreter: $(IELE_INTERPRETER) $(IELE_RUNNER)

$(IELE_INTERPRETER): $(IELE_LIB)/standalone/iele-testing-kompiled/interpreter
	$(INSTALL) $< $@

build-vm: $(IELE_VM) $(IELE_RUNNER)

$(IELE_VM): $(IELE_LIB)/node/iele-testing-kompiled/interpreter $(wildcard vm/c/*.cpp vm/c/*.h) $(protobuf_out) $(libsecp256k1_out) $(libcryptopp_out)
	@mkdir -p $(dir $@)
	llvm-kompile $(IELE_LIB)/node/iele-testing-kompiled/definition.kore $(IELE_LIB)/node/iele-testing-kompiled/dt                                                        \
	    library vm/c/main.cpp vm/c/vm.cpp $(KOMPILE_CPP_FILES) $(abspath $(protobuf_out)) vm/c/iele/semantics.cpp $(LLVM_KOMPILE_INCLUDE_OPTS) $(LLVM_KOMPILE_LINK_OPTS) \
	    -o $(IELE_VM) -g

# Haskell Build
# -------------

haskell_dir            := $(IELE_LIB)/haskell
haskell_main_module    := IELE-TESTING
haskell_syntax_module  := IELE-SYNTAX
haskell_main_file      := iele-testing.md
haskell_main_filename  := $(basename $(notdir $(haskell_main_file)))
haskell_kompiled       := $(haskell_dir)/$(haskell_main_filename)-kompiled/definition.kore

build-haskell: $(haskell_kompiled) $(IELE_RUNNER)

$(haskell_kompiled): MD_SELECTOR="(k & ! node) | standalone"

$(haskell_kompiled): $(k_files)
	kompile --directory $(haskell_dir) --backend haskell --main-module $(haskell_main_module) --syntax-module $(haskell_syntax_module) --md-selector $(MD_SELECTOR) --hook-namespaces "KRYPTO JSON" $(haskell_main_file)

# IELE Assembler
# --------------

build-assembler: $(IELE_ASSEMBLE) $(IELE_RUNNER)

$(IELE_ASSEMBLE):
	cd iele-assemble && stack install --local-bin-path $(abspath $(IELE_LIB))

# Coverage Processing
# -------------------

coverage:
	kcovr $(BUILD_DIR)/node/iele-testing-kompiled $(BUILD_DIR)/standalone/iele-testing-kompiled $(BUILD_DIR)/check/well-formedness-kompiled -- $(filter-out krypto.md, $(source_files)) > $(BUILD_DIR)/coverage.xml

# Install
# -------

pyiele_files := pyiele/__init__.py              \
                pyiele/__main__.py              \
                pyiele/blackbox.py              \
                pyiele/config.py                \
                pyiele/fetchFunctionData.py     \
                pyiele/kieleCoverage.py         \
                pyiele/rlp.py                   \
                pyiele/rpc.py                   \
                pyiele/testrunner.py            \
                pyiele/transactionGeneration.py \
                pyiele/utils.py

build:                                               \
       $(IELE_ASSEMBLE)                              \
       $(IELE_CHECK)                                 \
       $(IELE_INTERPRETER)                           \
       $(IELE_NODE)                                  \
       $(IELE_RUNNER)                                \
       $(IELE_TEST_CLIENT)                           \
       $(IELE_TEST_VM)                               \
       $(IELE_VM)                                    \
       $(IELE_LIB)/kiele-generate-report.py          \
       $(IELE_LIB)/kore-json.py                      \
       $(IELE_LIB)/static-report.html                \
       $(IELE_LIB)/version                           \
       $(patsubst %, $(IELE_LIB)/%, $(pyiele_files)) \
       $(haskell_kompiled)

all_bin_sources := $(shell find $(IELE_BIN) -type f        \
                           | sed 's|^$(IELE_BIN)/||')
all_lib_sources := $(shell find $(IELE_LIB) -type f                                        \
                            -not -path "$(IELE_LIB)/*.cmi"                                 \
                            -not -path "$(IELE_LIB)/*.cmx"                                 \
                            -not -path "$(IELE_LIB)/*.ml"                                  \
                            -not -path "$(IELE_LIB)/*.mli"                                 \
                            -not -path "$(IELE_LIB)/*.o"                                   \
                            -not -path "$(IELE_LIB)/plugin-node/*"                         \
                            -not -path "$(IELE_LIB)/check/well-formedness-kompiled/dt/*"   \
                            -not -path "$(IELE_LIB)/node/iele-testing-kompiled/dt/*"       \
                            -not -path "$(IELE_LIB)/standalone/iele-testing-kompiled/dt/*" \
                            | sed 's|^$(IELE_LIB)/||')

iele_interpreter_libs := $(shell find $(IELE_LIB)/standalone -type f                       \
                            -not -path "$(IELE_LIB)/*.cmi"                                 \
                            -not -path "$(IELE_LIB)/*.cmx"                                 \
                            -not -path "$(IELE_LIB)/*.ml"                                  \
                            -not -path "$(IELE_LIB)/*.mli"                                 \
                            -not -path "$(IELE_LIB)/*.o"                                   \
                            -not -path "$(IELE_LIB)/standalone/iele-testing-kompiled/dt/*" \
                            | sed 's|^$(IELE_LIB)/||')

iele_check_libs := $(shell find $(IELE_LIB)/check -type f                                  \
                            -not -path "$(IELE_LIB)/*.cmi"                                 \
                            -not -path "$(IELE_LIB)/*.cmx"                                 \
                            -not -path "$(IELE_LIB)/*.ml"                                  \
                            -not -path "$(IELE_LIB)/*.mli"                                 \
                            -not -path "$(IELE_LIB)/*.o"                                   \
                            -not -path "$(IELE_LIB)/check/well-formedness-kompiled/dt/*"   \
                            | sed 's|^$(IELE_LIB)/||')

kiele_files := kiele-generate-report.py \
               kore-json.py             \
               static-report.html       \
               version

$(DESTDIR)$(INSTALL_BIN)/%: $(IELE_BIN)/%
	@mkdir -p $(dir $@)
	$(INSTALL) $< $@

$(DESTDIR)$(INSTALL_LIB)/%: $(IELE_LIB)/%
	@mkdir -p $(dir $@)
	$(INSTALL) $< $@

install-interpreter: $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(iele_interpreter_libs))

install-vm: $(patsubst $(IELE_LIB)/%, $(DESTDIR)$(INSTALL_LIB)/%, $(IELE_VM))

install-check: $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(iele_check_libs))

install-kiele: $(patsubst $(IELE_BIN)/%, $(DESTDIR)$(INSTALL_BIN)/%, $(IELE_RUNNER)) \
               $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(kiele_files))             \
               $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(pyiele_files))

install: $(patsubst %, $(DESTDIR)$(INSTALL_BIN)/%, $(all_bin_sources)) \
         $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(all_lib_sources))

uninstall:
	rm -rf $(DESTDIR)$(INSTALL_BIN)/kiele
	rm -rf $(DESTDIR)$(INSTALL_LIB)/kiele

release.md:
	echo "KIELE Release - $(KIELE_RELEASE_TAG)"  > $@
	echo                                        >> $@
	cat INSTALL.md                              >> $@

$(IELE_LIB)/version:
	@mkdir -p $(dir $@)
	echo "$(KIELE_RELEASE_TAG)" > $@

$(IELE_RUNNER): kiele
	@mkdir -p $(dir $@)
	$(INSTALL) $< $@

$(IELE_LIB)/%.py: %.py
	@mkdir -p $(dir $@)
	$(INSTALL) $< $@

$(IELE_LIB)/static-report.html: static-report.html
	@mkdir -p $(dir $@)
	$(INSTALL) $< $@

# Ocaml Builds
# ------------

$(BUILD_DIR)/plugin-ocaml/msg_types.ml: $(PROTO)/proto/msg.proto
	mkdir -p $(BUILD_DIR)/plugin-ocaml
	eval `opam config env` && ocaml-protoc $< -ml_out $(BUILD_DIR)/plugin-ocaml

$(IELE_TEST_VM): $(wildcard vm/*.ml vm/*.mli) $(BUILD_DIR)/plugin-ocaml/msg_types.ml $(IELE_VM)
	@mkdir -p $(IELE_LIB)
	cp vm/*.ml vm/*.mli $(BUILD_DIR)/plugin-ocaml/*.ml $(BUILD_DIR)/plugin-ocaml/*.mli $(IELE_LIB)
	cd $(IELE_LIB) && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-vm msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleVmTest.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string

$(IELE_TEST_CLIENT): $(wildcard vm/*.ml vm/*.mli) $(BUILD_DIR)/plugin-ocaml/msg_types.ml
	@mkdir -p $(IELE_LIB)
	cp vm/*.ml vm/*.mli $(BUILD_DIR)/plugin-ocaml/*.ml $(BUILD_DIR)/plugin-ocaml/*.mli $(IELE_LIB)
	cd $(IELE_LIB) && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-client msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleApi.mli ieleApi.ml ieleApiClient.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string $(PREDICATES)
