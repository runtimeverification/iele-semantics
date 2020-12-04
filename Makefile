# Common to all versions of K
# ===========================

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
OCAMLC=opt -O3 -cclib -Wl,-rpath=/usr/local/lib
LIBFLAG=-shared
endif

ifeq ($(COVERAGE),k)
KOMPILE_FLAGS=--coverage
else ifeq ($(COVERAGE),ocaml)
BISECT=-package bisect_ppx
PREDICATES=-predicates coverage
endif

BUILD_DIR      := $(abspath .build)
KORE_SUBMODULE := $(BUILD_DIR)/kore
BUILD_LOCAL    := $(BUILD_DIR)/local
LOCAL_LIB      := $(BUILD_LOCAL)/lib
LOCAL_INCLUDE  := $(BUILD_LOCAL)/include

PLUGIN=$(abspath plugin)
PROTO=$(abspath proto)

IELE_BIN         := $(BUILD_DIR)/bin
IELE_LIB         := $(BUILD_DIR)/lib/kiele
IELE_ASSEMBLE    := $(IELE_BIN)/iele-assemble
IELE_INTERPRETER := $(IELE_BIN)/iele-interpreter
IELE_CHECK       := $(IELE_BIN)/iele-check
IELE_VM          := $(IELE_BIN)/iele-vm
IELE_TEST_VM     := $(IELE_BIN)/iele-test-vm
IELE_TEST_CLIENT := $(IELE_BIN)/iele-test-client

export PATH:=$(IELE_BIN):$(PATH)

.PHONY: all clean distclean libff protobuf \
        build build build-haskell build-node build-testnode build-coverage \
        split-tests split-vm-tests split-blockchain-tests \
        test-evm test-vm test-blockchain test-iele test-iele-node test-wellformed test-bad-packet
.SECONDARY:

all: build split-tests

clean:
	rm -rf $(BUILD_DIR)

distclean: clean

# Dependencies
# ------------

libff_out := $(LOCAL_LIB)/libff.a

libff: $(libff_out)

$(libff_out): $(PLUGIN)/deps/libff/CMakeLists.txt
	@mkdir -p $(PLUGIN)/deps/libff/build
	cd $(PLUGIN)/deps/libff/build                                                   \
	   && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$(BUILD_LOCAL) \
	   && make -s -j4                                                               \
	   && make install

protobuf_out := $(BUILD_DIR)/plugin-node/proto/msg.pb.cc

protobuf: $(protobuf_out)

$(protobuf_out): $(PROTO)/proto/msg.proto
	mkdir -p $(BUILD_DIR)/plugin-node
	protoc --cpp_out=$(BUILD_DIR)/plugin-node -I $(PROTO) $<

# Tests
# -----

TEST          = kiele
TEST_ASSEMBLE = ./assemble-iele-test
TEST_BACKEND  = standalone
TEST_MODE     = NORMAL
TEST_SCHEDULE = DEFAULT
TEST_PORT     = 10000

split-tests: split-vm-tests split-blockchain-tests

invalid_iele_tests_file=tests/failing.expected
invalid_iele_tests= $(shell cat $(invalid_iele_tests_file))

split-vm-tests: $(patsubst tests/ethereum-tests/%.json,tests/%/make.timestamp, $(filter-out $(invalid_iele_tests), $(wildcard tests/ethereum-tests/VMTests/*/*.json)))

split-blockchain-tests: $(patsubst tests/ethereum-tests/%.json,tests/%/make.timestamp, $(filter-out $(invalid_iele_tests), $(wildcard tests/ethereum-tests/BlockchainTests/GeneralStateTests/*/*.json)))

vm_tests=$(wildcard tests/VMTests/*/*/*.iele.json)
blockchain_tests=$(wildcard tests/BlockchainTests/*/*/*/*.iele.json)
all_tests=$(vm_tests) $(blockchain_tests)
failing_tests = $(shell cat tests/failing.$(TEST_BACKEND))
skipped_tests=$(wildcard tests/VMTests/vmPerformance/*/*.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/*/*/*_Frontier.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/*/*/*_Homestead.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/*/*/*_EIP150.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/*/*/*_EIP158.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/*/*/*_Constantinople.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/stQuadraticComplexityTest/*/*.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/stStaticCall/static_Call50000*/*.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/stStaticCall/static_Return50000*/*.iele.json) \
   $(wildcard tests/BlockchainTests/GeneralStateTests/stStaticCall/static_Call1MB1024Calldepth_d1g0v0/*.iele.json) \

passing_tests=$(filter-out $(failing_tests), $(filter-out $(skipped_tests), $(all_tests)))
passing_vm_tests=$(filter-out $(failing_tests), $(filter-out $(skipped_tests), $(vm_tests)))
passing_blockchain_tests=$(filter-out $(failing_tests), $(filter-out $(skipped_tests), $(blockchain_tests)))
passing_targets=$(passing_tests:=.test)
passing_vm_targets=$(passing_vm_tests:=.test)
passing_blockchain_targets=$(passing_blockchain_tests:=.test)

iele_tests=$(wildcard tests/iele/*/*/*.iele.json)
iele_passing_tests=$(filter-out $(failing_tests), $(iele_tests))
iele_targets=$(iele_passing_tests:=.test)
iele_node_targets=$(iele_tests:=.nodetest)

iele_contracts=$(wildcard iele-examples/*.iele tests/iele/*/*/*.iele)
well_formed_contracts=$(filter-out $(wildcard tests/iele/*/ill-formed/*.iele), $(iele_contracts))
well_formedness_targets=$(well_formed_contracts:=.test-wellformed)

test-evm: test-vm test-blockchain
test-vm: $(passing_vm_targets)
test-blockchain: $(passing_blockchain_targets)
test-iele: $(iele_targets)
test-iele-node: $(iele_node_targets)
test-wellformed: $(well_formedness_targets)

test-bad-packet:
	netcat 127.0.0.1 $(TEST_PORT) -q 2 < tests/bad-packet
	netcat 127.0.0.1 $(TEST_PORT) -q 2 < tests/bad-packet-2
	iele-test-vm tests/iele/danse/sum/sum_zero.iele.json $(TEST_PORT)

tests/VMTests/%:        TEST_MODE = VMTESTS
%.iele.test-wellformed: TEST_SCHEDULE = DANSE

%.json.test: %.json.test-assembled
	$(TEST) interpret --backend $(TEST_BACKEND) --mode $(TEST_MODE) --schedule $(TEST_SCHEDULE) --no-unparse $<

%.json.test-assembled: %.json
	$(TEST_ASSEMBLE) $< > $@

%.iele.test-wellformed: %.iele
	$(TEST) check --backend check --mode $(TEST_MODE) --schedule $(TEST_SCHEDULE) $<

%.nodetest: %
	iele-test-vm $< $(TEST_PORT)

tests/%/make.timestamp: tests/ethereum-tests/%.json tests/evm-to-iele/evm-to-iele tests/evm-to-iele/evm-test-to-iele
	@echo "==   split: $@"
	mkdir -p $(dir $@)
	tests/split-test.py $< $(dir $@)
	touch $@

tests/evm-to-iele/evm-to-iele: $(wildcard tests/evm-to-iele/*.ml tests/evm-to-iele/*.mli)
	cd tests/evm-to-iele && eval `opam config env` && ocamlfind $(OCAMLC) -g ieleUtil.mli ieleUtil.ml evm.mli evm.ml iele.mli iele.ml conversion.mli conversion.ml main.ml -package zarith -package hex -linkpkg -o evm-to-iele

# Build Source Files
# ------------------

k_files:=iele-testing.md data.md iele.md iele-gas.md iele-binary.md plugin/plugin/krypto.md iele-syntax.md iele-node.md well-formedness.md
checker_files:=iele-syntax.md well-formedness.md data.md

# LLVM Builds
# -----------

build-node: $(IELE_VM)
build-testnode : $(IELE_TEST_VM) $(IELE_TEST_CLIENT)

KOMPILE=kompile

KOMPILE_INCLUDE_OPTS := -ccopt -I -ccopt $(PLUGIN)/plugin-c -ccopt -I -ccopt $(PROTO) -ccopt -I -ccopt $(BUILD_DIR)/plugin-node -ccopt -I -ccopt $(LOCAL_INCLUDE)
KOMPILE_LINK_OPTS    := -ccopt -L -ccopt /usr/local/lib -ccopt -L -ccopt $(LOCAL_LIB) -ccopt -lprotobuf -ccopt -lff -ccopt -lcryptopp -ccopt -lsecp256k1 -ccopt -lprocps
KOMPILE_CPP_FILES    := $(PLUGIN)/plugin-c/k.cpp $(PLUGIN)/plugin-c/crypto.cpp $(PROTO)/blockchain.cpp $(PROTO)/world.cpp $(PLUGIN)/plugin-c/blake2.cpp $(PLUGIN)/plugin-c/plugin_util.cpp
KOMPILE_CPP_OPTS     := $(addprefix -ccopt , $(KOMPILE_CPP_FILES))

build-coverage:
	kcovr $(BUILD_DIR)/node/iele-testing-kompiled $(BUILD_DIR)/standalone/iele-testing-kompiled $(BUILD_DIR)/check/well-formedness-kompiled -- $(filter-out krypto.md, $(source_files)) > $(BUILD_DIR)/coverage.xml

$(BUILD_DIR)/check/well-formedness-kompiled/interpreter: $(checker_files) $(protobuf_out) $(libff_out)
	$(KOMPILE) --debug --main-module IELE-WELL-FORMEDNESS-STANDALONE --md-selector "(k & ! node) | standalone" \
	                                --syntax-module IELE-SYNTAX well-formedness.md --directory $(BUILD_DIR)/check --hook-namespaces KRYPTO \
	                                --backend llvm -ccopt $(protobuf_out) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

$(BUILD_DIR)/standalone/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! node) | standalone"
$(BUILD_DIR)/node/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! standalone) | node"

$(BUILD_DIR)/%/iele-testing-kompiled/interpreter: $(k_files) $(protobuf_out) $(libff_out)
	@echo "== kompile: $@"
	$(KOMPILE) --debug --main-module IELE-TESTING --verbose --md-selector $(MD_SELECTOR) \
					--syntax-module IELE-SYNTAX iele-testing.md --directory $(BUILD_DIR)/$* --hook-namespaces "KRYPTO BLOCKCHAIN" \
	                --backend llvm -ccopt $(protobuf_out) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

LLVM_KOMPILE_INCLUDE_OPTS := -I $(PLUGIN)/plugin-c/ -I $(PROTO) -I $(BUILD_DIR)/plugin-node -I vm/c/ -I vm/c/iele/ -I $(LOCAL_INCLUDE)
LLVM_KOMPILE_LINK_OPTS    := -L /usr/local/lib -L $(LOCAL_LIB) -lff -lprotobuf -lgmp -lprocps -lcryptopp -lsecp256k1

$(IELE_CHECK): $(BUILD_DIR)/check/well-formedness-kompiled/interpreter
	@mkdir -p $(IELE_BIN)
	cp $< $@

$(IELE_INTERPRETER): $(BUILD_DIR)/standalone/iele-testing-kompiled/interpreter
	@mkdir -p $(IELE_BIN)
	cp $< $@

$(IELE_VM): $(BUILD_DIR)/node/iele-testing-kompiled/interpreter $(wildcard vm/c/*.cpp vm/c/*.h) $(protobuf_out)
	@mkdir -p $(IELE_BIN)
	llvm-kompile $(BUILD_DIR)/node/iele-testing-kompiled/definition.kore $(BUILD_DIR)/node/iele-testing-kompiled/dt library vm/c/main.cpp vm/c/vm.cpp $(KOMPILE_CPP_FILES) $(protobuf_out) vm/c/iele/semantics.cpp $(LLVM_KOMPILE_INCLUDE_OPTS) $(LLVM_KOMPILE_LINK_OPTS) -o $(IELE_VM) -g

# Haskell Build
# -------------

haskell_dir            := $(BUILD_DIR)/haskell
haskell_main_module    := IELE-TESTING
haskell_syntax_module  := IELE-SYNTAX
haskell_main_file      := iele-testing.md
haskell_main_filename  := $(basename $(notdir $(haskell_main_file)))
haskell_kompiled       := $(haskell_dir)/$(haskell_main_filename)-kompiled/definition.kore

build-haskell: $(haskell_kompiled)

$(haskell_kompiled): MD_SELECTOR="(k & ! node) | standalone"

$(haskell_kompiled):
	$(KOMPILE) --directory $(haskell_dir) --backend haskell --main-module $(haskell_main_module) --syntax-module $(haskell_syntax_module) --md-selector $(MD_SELECTOR) --hook-namespaces "KRYPTO JSON" $(haskell_main_file)

# IELE Assembler
# --------------

$(IELE_ASSEMBLE):
	cd iele-assemble && stack install --local-bin-path $(IELE_BIN)

# Install
# -------

KIELE_VERSION     ?= 0.2.0
KIELE_RELEASE_TAG ?= v$(KIELE_VERSION)-$(shell git rev-parse --short HEAD)

INSTALL_PREFIX := /usr/local
INSTALL_BIN    ?= $(DESTDIR)$(INSTALL_PREFIX)/bin
INSTALL_LIB    ?= $(DESTDIR)$(INSTALL_PREFIX)/lib/kiele

install_bins := iele-vm iele-test-vm iele-assemble iele-interpreter iele-check kiele

install_libs := version kore-json.py haskell/iele-testing-kompiled check/well-formedness-kompiled standalone/iele-testing-kompiled

$(IELE_BIN)/kiele: kiele
	@mkdir -p $(IELE_BIN)
	cp $< $@

$(IELE_LIB)/version:
	@mkdir -p $(IELE_LIB)
	echo "$(KIELE_RELEASE_TAG)" > $@

$(IELE_LIB)/standalone/iele-testing-kompiled: $(BUILD_DIR)/standalone/iele-testing-kompiled/interpreter
	@mkdir -p $(IELE_LIB)/standalone
	cp -r $(dir $<) $@

$(IELE_LIB)/haskell/iele-testing-kompiled: $(haskell_kompiled)
	@mkdir -p $(IELE_LIB)/haskell
	cp -r $(dir $<) $@

$(IELE_LIB)/check/well-formedness-kompiled: $(BUILD_DIR)/check/well-formedness-kompiled/interpreter
	@mkdir -p $(IELE_LIB)/check
	cp -r $(dir $<) $@

$(IELE_LIB)/kore-json.py: kore-json.py
	@mkdir -p $(IELE_LIB)
	cp $< $@

$(INSTALL_BIN)/%: $(IELE_BIN)/%
	install -D $< $@

$(INSTALL_LIB)/%: $(IELE_LIB)/%
	install -D $< $@

build: $(patsubst %, $(IELE_BIN)/%, $(install_bins)) $(patsubst %, $(IELE_LIB)/%, $(install_libs))
install: $(patsubst %, $(INSTALL_BIN)/%, $(install_bins)) $(patsubst %, $(INSTALL_LIB)/%, $(install_libs))

uninstall:
	rm $(patsubst %, $(INSTALL_BIN)/%, $(install_bins))
	rm $(patsubst %, $(INSTALL_LIB)/%, $(install_libs))

release.md:
	echo "KIELE Release - $(KIELE_RELEASE_TAG)"  > $@
	echo                                        >> $@

# Ocaml Builds
# ------------

$(BUILD_DIR)/plugin-ocaml/msg_types.ml: $(PROTO)/proto/msg.proto
	mkdir -p $(BUILD_DIR)/plugin-ocaml
	eval `opam config env` && ocaml-protoc $< -ml_out $(BUILD_DIR)/plugin-ocaml

$(IELE_TEST_VM): $(wildcard vm/*.ml vm/*.mli) $(BUILD_DIR)/plugin-ocaml/msg_types.ml
	@mkdir -p $(IELE_BIN)
	cp vm/*.ml vm/*.mli $(BUILD_DIR)/plugin-ocaml/*.ml $(BUILD_DIR)/plugin-ocaml/*.mli $(IELE_BIN)
	cd $(IELE_BIN) && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-vm msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleVmTest.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string

$(IELE_TEST_CLIENT): $(wildcard vm/*.ml vm/*.mli) $(BUILD_DIR)/plugin-ocaml/msg_types.ml
	@mkdir -p $(IELE_BIN)
	cp vm/*.ml vm/*.mli $(BUILD_DIR)/plugin-ocaml/*.ml $(BUILD_DIR)/plugin-ocaml/*.mli $(IELE_BIN)
	cd $(IELE_BIN) && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-client msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleApi.mli ieleApi.ml ieleApiClient.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string $(PREDICATES)
