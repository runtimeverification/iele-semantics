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

export PATH:=$(shell cd compiler && stack path --local-install-root)/bin:${PATH}

KORE_SUBMODULE:=.build/kore

.PHONY: all clean distclean build tangle defn proofs split-tests test vm-test blockchain-test deps k-deps ocaml-deps assembler iele-test iele-test-node node testnode install kore
.SECONDARY:

all: build split-vm-tests testnode

clean:
	rm -rf .build/standalone .build/llvm .build/node .build/check .build/plugin-node .build/plugin-standalone .build/vm compiler/.stack-work .build/haskell

distclean: clean
	cd .build/k && mvn clean
	cd .build/kore && stack clean

build: tangle .build/standalone/iele-testing-kompiled/interpreter .build/vm/iele-vm assembler .build/check/well-formedness-kompiled/interpreter

llvm: tangle .build/llvm/iele-testing.kore

haskell: tangle .build/haskell/definition.kore

assembler:
	cd compiler && stack build --install-ghc

install: assembler
	cd compiler && stack install
	cp .build/vm/iele-vm .build/vm/iele-test-client .build/vm/iele-test-vm ~/.local/bin

# Tangle from *.md files
# ----------------------

tangle: defn proofs

k_files:=iele-testing.md data.md iele.md iele-gas.md iele-binary.md krypto.md iele-syntax.md iele-node.md well-formedness.md
checker_files:=iele-syntax.md well-formedness.md data.md

.build/node/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to .build/tangle/tangle.lua --metadata=code:".k:not(.standalone),.node" $< > $@
.build/standalone/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to .build/tangle/tangle.lua --metadata=code:".k:not(.node),.standalone" $< > $@

node: .build/vm/iele-vm
testnode : .build/vm/iele-test-vm .build/vm/iele-test-client

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

iele_contracts=$(wildcard iele-examples/*.iele tests/iele/*/*/*.iele)
well_formed_contracts=$(filter-out $(wildcard tests/iele/*/ill-formed/*.iele), ${iele_contracts})
well_formedness_targets=${well_formed_contracts:=.test}

test: $(passing_targets) ${iele_targets} ${iele_node_targets} ${well_formedness_targets} test-bad-packet
vm-test: $(passing_vm_targets)
blockchain-test: $(passing_blockchain_targets)
iele-test: ${iele_targets}
iele-test-node: ${iele_node_targets}
well-formed-test: ${well_formedness_targets}

test-bad-packet:
	netcat 127.0.0.1 $(PORT) -q 2 < tests/bad-packet
	netcat 127.0.0.1 $(PORT) -q 2 < tests/bad-packet-2
	.build/vm/iele-test-vm tests/iele/albe/sum/sum_zero.iele.json $(PORT)

tests/VMTests/%.json.test: tests/VMTests/%.json | .build/standalone/iele-testing-kompiled/interpreter
	./vmtest $<
tests/BlockchainTests/%.json.test: tests/BlockchainTests/%.json | .build/standalone/iele-testing-kompiled/interpreter
	./blockchaintest $<
tests/iele/%.json.test: tests/iele/%.json | .build/standalone/iele-testing-kompiled/interpreter 
	./blockchaintest $<

%.iele.test: %.iele | .build/check/well-formedness-kompiled/interpreter
	./check-iele $<

PORT?=10000
tests/iele/%.nodetest: tests/iele/% | testnode
	.build/vm/iele-test-vm $< $(PORT)

tests/%/make.timestamp: tests/ethereum-tests/%.json tests/evm-to-iele/evm-to-iele tests/evm-to-iele/evm-test-to-iele
	@echo "==   split: $@"
	mkdir -p $(dir $@)
	tests/split-test.py $< $(dir $@)
	touch $@

tests/evm-to-iele/evm-to-iele: $(wildcard tests/evm-to-iele/*.ml tests/evm-to-iele/*.mli)
	cd tests/evm-to-iele && ocamlfind $(OCAMLC) -g ieleUtil.mli ieleUtil.ml evm.mli evm.ml iele.mli iele.ml conversion.mli conversion.ml main.ml -package zarith -package hex -linkpkg -o evm-to-iele

tests/ethereum-tests/%.json:
	@echo "==  git submodule: cloning upstreams test repository"
	git submodule update --init

KOMPILE=kompile

coverage:
	kcovr .build/node/iele-testing-kompiled .build/standalone/iele-testing-kompiled .build/check/well-formedness-kompiled -- $(filter-out krypto.md, $(source_files)) > .build/coverage.xml

deps:

haskell-deps:
		cd $(KORE_SUBMODULE) && stack install --local-bin-path $(abspath $(KORE_SUBMODULE))/bin kore:exe:kore-exec

PLUGIN=$(abspath plugin)

.build/plugin-node/proto/msg.pb.cc: ${PLUGIN}/plugin/proto/msg.proto
	mkdir -p .build/plugin-node
	protoc --cpp_out=.build/plugin-node -I ${PLUGIN}/plugin ${PLUGIN}/plugin/proto/msg.proto

.build/check/well-formedness-kompiled/interpreter: $(checker_files) .build/plugin-node/proto/msg.pb.cc
	${KOMPILE} --debug --main-module IELE-WELL-FORMEDNESS-STANDALONE --md-selector "(k & ! node) | standalone" \
	                                --syntax-module IELE-SYNTAX well-formedness.md --directory .build/check \
	                                --backend llvm -ccopt ${PLUGIN}/plugin-c/crypto.cpp -ccopt ${PLUGIN}/plugin-c/blockchain.cpp -ccopt ${PLUGIN}/plugin-c/world.cpp -ccopt `pwd`/.build/plugin-node/proto/msg.pb.cc -ccopt -I -ccopt ${PLUGIN}/plugin-c -ccopt -I -ccopt `pwd`/.build/plugin-node -ccopt -L -ccopt /usr/local/lib \
				       	-ccopt -lprotobuf -ccopt -lff -ccopt -lcryptopp -ccopt -lsecp256k1 -ccopt -lprocps -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

.build/standalone/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! node) | standalone"
.build/node/iele-testing-kompiled/interpreter: MD_SELECTOR="(k & ! standalone) | node"

.build/%/iele-testing-kompiled/interpreter: $(k_files) .build/plugin-node/proto/msg.pb.cc
	@echo "== kompile: $@"
	${KOMPILE} --debug --main-module IELE-TESTING --verbose --md-selector ${MD_SELECTOR} \
					--syntax-module IELE-SYNTAX iele-testing.md --directory .build/$* \
	                                --backend llvm -ccopt ${PLUGIN}/plugin-c/crypto.cpp -ccopt ${PLUGIN}/plugin-c/blockchain.cpp -ccopt ${PLUGIN}/plugin-c/world.cpp -ccopt `pwd`/.build/plugin-node/proto/msg.pb.cc -ccopt -I -ccopt ${PLUGIN}/plugin-c -ccopt -I -ccopt `pwd`/.build/plugin-node -ccopt -L -ccopt /usr/local/lib \
				       	-ccopt -lprotobuf -ccopt -lff -ccopt -lcryptopp -ccopt -lsecp256k1 -ccopt -lprocps -ccopt -g -ccopt -std=c++14 -ccopt -O2 $(KOMPILE_FLAGS)

.build/vm/iele-vm: .build/node/iele-testing-kompiled/interpreter $(wildcard plugin/vm-c/*.cpp plugin/vm-c/*.h) .build/plugin-node/proto/msg.pb.cc
	mkdir -p .build/vm
	llvm-kompile .build/node/iele-testing-kompiled/definition.kore IELE-TESTING library ${PLUGIN}/vm-c/main.cpp ${PLUGIN}/vm-c/vm.cpp -I ${PLUGIN}/plugin-c/ -I .build/plugin-node -L /usr/local/lib ${PLUGIN}/plugin-c/*.cpp .build/plugin-node/proto/msg.pb.cc -lff -lprotobuf -lgmp -lprocps -lcryptopp -lsecp256k1 -I ${PLUGIN}/vm-c/ -I ${PLUGIN}/vm-c/iele/ ${PLUGIN}/vm-c/iele/semantics.cpp -o .build/vm/iele-vm -g
