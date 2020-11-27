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

export PATH:=$(shell cd iele-assemble && stack path --local-install-root)/bin:${PATH}

BUILD_DIR      := $(abspath .build)
KORE_SUBMODULE := $(BUILD_DIR)/kore
BUILD_LOCAL    := $(BUILD_DIR)/local
LOCAL_LIB      := $(BUILD_LOCAL)/lib
LOCAL_INCLUDE  := $(BUILD_LOCAL)/include

PLUGIN=$(abspath plugin)
PROTO=$(abspath proto)

IELE_BIN      := .build/vm
IELE_ASSEMBLE := $(IELE_BIN)/iele-assemble
IELE_VM       := $(IELE_BIN)/iele-vm
IELE_TEST_VM  := $(IELE_BIN)/iele-test-vm

.PHONY: all clean distclean build build-haskell tangle defn proofs split-tests test vm-test blockchain-test deps k-deps ocaml-deps iele-test iele-test-haskell iele-test-node node testnode kore libff protobuf \
        install uninstall
.SECONDARY:

all: build split-vm-tests testnode

clean:
	rm -rf .build

distclean: clean
	cd .build/k && mvn clean
	cd .build/kore && stack clean

build: tangle .build/standalone/iele-testing-kompiled/interpreter $(IELE_VM) $(IELE_ASSEMBLE) .build/check/well-formedness-kompiled/interpreter build-haskell

llvm: tangle .build/llvm/iele-testing.kore

haskell: tangle .build/haskell/definition.kore

# Tangle from *.md files
# ----------------------

tangle: defn proofs

k_files:=iele-testing.md data.md iele.md iele-gas.md iele-binary.md plugin/plugin/krypto.md iele-syntax.md iele-node.md well-formedness.md
checker_files:=iele-syntax.md well-formedness.md data.md

node: $(IELE_VM)
testnode : $(IELE_TEST_VM) .build/vm/iele-test-client

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
	mkdir -p .build/plugin-node
	protoc --cpp_out=.build/plugin-node -I $(PROTO) $<

# Tests
# -----

split-tests: split-vm-tests split-blockchain-tests

invalid_iele_tests_file=tests/failing.expected
invalid_iele_tests= $(shell cat ${invalid_iele_tests_file})

split-vm-tests: \
		  $(patsubst tests/ethereum-tests/%.json,tests/%/make.timestamp, $(filter-out ${invalid_iele_tests}, $(wildcard tests/ethereum-tests/VMTests/*/*.json))) \

split-blockchain-tests: \
				  $(patsubst tests/ethereum-tests/%.json,tests/%/make.timestamp, $(filter-out ${invalid_iele_tests}, $(wildcard tests/ethereum-tests/BlockchainTests/GeneralStateTests/*/*.json))) \

vm_tests=$(wildcard tests/VMTests/*/*/*.iele.json)
blockchain_tests=$(wildcard tests/BlockchainTests/*/*/*/*.iele.json)
all_tests=${vm_tests} ${blockchain_tests}
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

passing_tests=$(filter-out ${skipped_tests}, ${all_tests})
passing_vm_tests=$(filter-out ${skipped_tests}, ${vm_tests})
passing_blockchain_tests=$(filter-out ${skipped_tests}, ${blockchain_tests})
passing_targets=${passing_tests:=.test}
passing_vm_targets=${passing_vm_tests:=.test}
passing_blockchain_targets=${passing_blockchain_tests:=.test}

iele_tests=$(wildcard tests/iele/*/*/*.iele.json)
iele_targets=${iele_tests:=.test}
iele_node_targets=${iele_tests:=.nodetest}

iele_haskell_failing=$(shell cat tests/iele/failing.haskell)
iele_haskell_passing=$(filter-out $(iele_haskell_failing), $(iele_tests))
iele_haskell_targets=${iele_haskell_passing:=.test-haskell}

iele_contracts=$(wildcard iele-examples/*.iele tests/iele/*/*/*.iele)
well_formed_contracts=$(filter-out $(wildcard tests/iele/*/ill-formed/*.iele), ${iele_contracts})
well_formedness_targets=${well_formed_contracts:=.test}

test: $(passing_targets) ${iele_targets} ${iele_node_targets} ${well_formedness_targets} test-bad-packet
vm-test: $(passing_vm_targets)
blockchain-test: $(passing_blockchain_targets)
iele-test: ${iele_targets}
iele-test-haskell: $(iele_haskell_targets)
iele-test-node: ${iele_node_targets}
well-formed-test: ${well_formedness_targets}

test-bad-packet:
	netcat 127.0.0.1 $(PORT) -q 2 < tests/bad-packet
	netcat 127.0.0.1 $(PORT) -q 2 < tests/bad-packet-2
	$(IELE_TEST_VM) tests/iele/danse/sum/sum_zero.iele.json $(PORT)

tests/VMTests/%.json.test: tests/VMTests/%.json | .build/standalone/iele-testing-kompiled/interpreter
	./vmtest $<
tests/BlockchainTests/%.json.test: tests/BlockchainTests/%.json | .build/standalone/iele-testing-kompiled/interpreter
	./blockchaintest $<
tests/iele/%.json.test: tests/iele/%.json | .build/standalone/iele-testing-kompiled/interpreter 
	./blockchaintest $<
tests/iele/%.json.test-haskell: tests/iele/%.json | $(haskell_kompiled)
	./vmtest-haskell $<

%.iele.test: %.iele | .build/check/well-formedness-kompiled/interpreter
	./check-iele $<

PORT?=10000
tests/iele/%.nodetest: tests/iele/% | testnode
	$(IELE_TEST_VM) $< $(PORT)

tests/%/make.timestamp: tests/ethereum-tests/%.json tests/evm-to-iele/evm-to-iele tests/evm-to-iele/evm-test-to-iele
	@echo "==   split: $@"
	mkdir -p $(dir $@)
	tests/split-test.py $< $(dir $@)
	touch $@

tests/evm-to-iele/evm-to-iele: $(wildcard tests/evm-to-iele/*.ml tests/evm-to-iele/*.mli)
	cd tests/evm-to-iele && eval `opam config env` && ocamlfind $(OCAMLC) -g ieleUtil.mli ieleUtil.ml evm.mli evm.ml iele.mli iele.ml conversion.mli conversion.ml main.ml -package zarith -package hex -linkpkg -o evm-to-iele

tests/ethereum-tests/%.json:
	@echo "==  git submodule: cloning upstreams test repository"
	git submodule update --init

# LLVM Builds
# -----------

KOMPILE=kompile

KOMPILE_INCLUDE_OPTS := -ccopt -I -ccopt $(PLUGIN)/plugin-c -ccopt -I -ccopt $(PROTO) -ccopt -I -ccopt $(BUILD_DIR)/plugin-node -ccopt -I -ccopt $(LOCAL_INCLUDE)
KOMPILE_LINK_OPTS    := -ccopt -L -ccopt /usr/local/lib -ccopt -L -ccopt $(LOCAL_LIB) -ccopt -lprotobuf -ccopt -lff -ccopt -lcryptopp -ccopt -lsecp256k1 -ccopt -lprocps
KOMPILE_CPP_FILES    := $(PLUGIN)/plugin-c/k.cpp $(PLUGIN)/plugin-c/crypto.cpp $(PROTO)/blockchain.cpp $(PROTO)/world.cpp $(PLUGIN)/plugin-c/blake2.cpp $(PLUGIN)/plugin-c/plugin_util.cpp
KOMPILE_CPP_OPTS     := $(addprefix -ccopt , $(KOMPILE_CPP_FILES))

coverage:
	kcovr .build/node/iele-testing-kompiled .build/standalone/iele-testing-kompiled .build/check/well-formedness-kompiled -- $(filter-out krypto.md, $(source_files)) > .build/coverage.xml

deps:

haskell-deps:
		cd $(KORE_SUBMODULE) && stack install --local-bin-path $(abspath $(KORE_SUBMODULE))/bin kore:exe:kore-exec

.build/check/well-formedness-kompiled/interpreter: $(checker_files) $(protobuf_out) $(libff_out)
	${KOMPILE} --debug --main-module IELE-WELL-FORMEDNESS-STANDALONE --md-selector "(k & ! node) | standalone" \
	                                --syntax-module IELE-SYNTAX well-formedness.md --directory .build/check --hook-namespaces KRYPTO \
	                                --backend llvm -ccopt $(protobuf_out) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

.build/standalone/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! node) | standalone"
.build/node/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! standalone) | node"

.build/%/iele-testing-kompiled/interpreter: $(k_files) $(protobuf_out) $(libff_out)
	@echo "== kompile: $@"
	${KOMPILE} --debug --main-module IELE-TESTING --verbose --md-selector ${MD_SELECTOR} \
					--syntax-module IELE-SYNTAX iele-testing.md --directory .build/$* --hook-namespaces "KRYPTO BLOCKCHAIN" \
	                --backend llvm -ccopt $(protobuf_out) $(KOMPILE_CPP_OPTS) $(KOMPILE_INCLUDE_OPTS) $(KOMPILE_LINK_OPTS) -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

LLVM_KOMPILE_INCLUDE_OPTS := -I $(PLUGIN)/plugin-c/ -I $(PROTO) -I $(BUILD_DIR)/plugin-node -I vm/c/ -I vm/c/iele/ -I $(LOCAL_INCLUDE)
LLVM_KOMPILE_LINK_OPTS    := -L /usr/local/lib -L $(LOCAL_LIB) -lff -lprotobuf -lgmp -lprocps -lcryptopp -lsecp256k1

.build/vm/iele-vm: .build/node/iele-testing-kompiled/interpreter $(wildcard vm/c/*.cpp vm/c/*.h) $(protobuf_out)
	mkdir -p .build/vm
	llvm-kompile .build/node/iele-testing-kompiled/definition.kore .build/node/iele-testing-kompiled/dt library vm/c/main.cpp vm/c/vm.cpp $(KOMPILE_CPP_FILES) $(protobuf_out) vm/c/iele/semantics.cpp $(LLVM_KOMPILE_INCLUDE_OPTS) $(LLVM_KOMPILE_LINK_OPTS) -o .build/vm/iele-vm -g

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
	cd compiler && stack install --local-bin-path $(CURDIR)/$(IELE_BIN)

# Install
# -------

KIELE_VERSION     ?= 0.2.0
KIELE_RELEASE_TAG ?= v$(KIELE_VERSION)-$(shell git rev-parse --short HEAD)

INSTALL_PREFIX := /usr/local
INSTALL_BIN    ?= $(DESTDIR)$(INSTALL_PREFIX)/bin
INSTALL_LIB    ?= $(DESTDIR)$(INSTALL_PREFIX)/lib/kiele

install_bins := kiele-vm kiele-test-vm kiele-assemble

install_libs := version

version:
	echo "$(KIELE_RELEASE_TAG)" > $@

kiele-vm: $(IELE_VM)
	cp --preserve=mode $< $@

kiele-test-vm: $(IELE_TEST_VM)
	cp --preserve=mode $< $@

kiele-assemble: $(IELE_ASSEMBLE)
	cp --preserve=mode $< $@

$(INSTALL_BIN)/%: %
	install -D $< $@

$(INSTALL_LIB)/%: %
	install -D $< $@

install: $(patsubst %, $(INSTALL_BIN)/%, $(install_bins)) $(patsubst %, $(INSTALL_LIB)/%, $(install_libs))

uninstall:
	rm $(patsubst %, $(INSTALL_BIN)/%, $(install_bins))
	rm $(patsubst %, $(INSTALL_LIB)/%, $(install_libs))

release.md:
	echo "Firefly Release - $(KIELE_RELEASE_TAG)"  > $@
	echo                                          >> $@

# Ocaml Builds
# ------------

.build/plugin-ocaml/msg_types.ml: $(PROTO)/proto/msg.proto
	mkdir -p .build/plugin-ocaml
	eval `opam config env` && ocaml-protoc $< -ml_out .build/plugin-ocaml

$(IELE_TEST_VM): $(wildcard vm/*.ml vm/*.mli) .build/plugin-ocaml/msg_types.ml
	mkdir -p .build/vm
	cp vm/*.ml vm/*.mli .build/plugin-ocaml/*.ml .build/plugin-ocaml/*.mli .build/vm
	cd .build/vm && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-vm msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleVmTest.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string

.build/vm/iele-test-client: $(wildcard vm/*.ml vm/*.mli) .build/plugin-ocaml/msg_types.ml
	mkdir -p .build/vm
	cp vm/*.ml vm/*.mli .build/plugin-ocaml/*.ml .build/plugin-ocaml/*.mli .build/vm
	cd .build/vm && eval `opam config env` && ocamlfind $(OCAMLC) -g -o iele-test-client msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml ieleClientUtils.ml ieleApi.mli ieleApi.ml ieleApiClient.ml -package dynlink -package zarith -package str -package uuidm -package unix -package rlp -package yojson -package hex -package cryptokit -package ocaml-protoc -linkpkg -linkall -thread -safe-string $(PREDICATES)
