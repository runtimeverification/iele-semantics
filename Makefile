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
OCAMLC=opt -O3
LIBFLAG=-shared
endif

.PHONY: all clean build tangle defn proofs split-tests test vm-test blockchain-test deps k-deps ocaml-deps assembler iele-test iele-test-node node testnode
.SECONDARY: .build/standalone/ethereum-kompiled/constants.$(EXT) .build/node/ethereum-kompiled/constants.$(EXT)

all: build split-vm-tests

clean:
	rm -rf .build/standalone .build/node .build/plugin-node .build/plugin-standalone .build/vm

build: tangle .build/standalone/ethereum-kompiled/interpreter .build/vm/iele-vm assembler .build/check/well-formedness-kompiled/interpreter

assembler:
	cd compiler && stack build --install-ghc

# Tangle from *.md files
# ----------------------

tangle: defn proofs

k_files:=ethereum.k data.k iele.k iele-gas.k iele-binary.k krypto.k iele-syntax.k iele-node.k well-formedness.k
standalone_files:=$(patsubst %,.build/standalone/%,$(k_files))
node_files:=$(patsubst %,.build/node/%,$(k_files))
checker_files:=.build/standalone/iele-syntax.k .build/standalone/well-formedness.k
defn_files=$(standalone_files) $(node_files)

defn: $(defn_files)

export LUA_PATH=$(shell pwd)/.build/tangle/?.lua;;


.build/node/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to .build/tangle/tangle.lua --metadata=code:".k:not(.standalone),.node" $< > $@
.build/standalone/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to .build/tangle/tangle.lua --metadata=code:".k:not(.node),.standalone" $< > $@

node: .build/vm/iele-vm
testnode : .build/vm/iele-test-vm

proof_dir=tests/proofs
proof_files=
proofs: $(proof_files)

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

iele_tests=$(wildcard tests/iele/*/*.iele.json)
iele_targets=${iele_tests:=.test}
iele_node_targets=${iele_tests:=.nodetest}

iele_contracts=$(wildcard iele-examples/*.iele tests/iele/*/*.iele)
well_formedness_targets=${iele_contracts:=.test}

test: $(passing_targets) ${iele_targets} ${iele_node_targets} ${well_formedness_targets}
vm-test: $(passing_vm_targets)
blockchain-test: $(passing_blockchain_targets)
iele-test: ${iele_targets}
iele-test-node: ${iele_node_targets}
well-formed-test: ${well_formedness_targets}

tests/VMTests/%.json.test: tests/VMTests/%.json | build
	./vmtest $<
tests/BlockchainTests/%.json.test: tests/BlockchainTests/%.json | build
	./blockchaintest $<
tests/iele/%.json.test: tests/iele/%.json | build
	./blockchaintest $<

%.iele.test: %.iele | build
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

KOMPILE=tests/ci/rv-k/k-distribution/target/release/k/bin/kompile

deps: k-deps ocaml-deps
k-deps:
	cd tests/ci/rv-k && mvn package

ocaml-deps:
	opam init
	opam repository add k "tests/ci/rv-k/k-distribution/target/release/k/lib/opam" || opam repository set-url k "tests/ci/rv-k/k-distribution/target/release/k/lib/opam"
	opam update
	opam switch 4.03.0+k
	eval `opam config env` && opam install -y mlgmp zarith uuidm cryptokit secp256k1.0.3.2 bn128 hex ocaml-protoc rlp yojson

.build/%/ethereum-kompiled/constants.$(EXT): $(defn_files)
	@echo "== kompile: $@"
	${KOMPILE} --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module IELE-SYNTAX .build/$*/ethereum.k --directory .build/$* \
					--hook-namespaces "KRYPTO MANTIS" --gen-ml-only -O3 --non-strict
	cd .build/$*/ethereum-kompiled && ocamlfind $(OCAMLC) -c -g constants.ml -package gmp -package zarith -safe-string

.build/check/well-formedness-kompiled/interpreter: $(checker_files)
	${KOMPILE} --debug --main-module IELE-WELL-FORMEDNESS-STANDALONE \
	                                --syntax-module IELE-SYNTAX .build/standalone/well-formedness.k --directory .build/check \
	                                --gen-ml-only -O3 --non-strict
	cd .build/check/well-formedness-kompiled && ocamlfind $(OCAMLC) -c -g -package gmp -package zarith -package uuidm -safe-string constants.ml prelude.ml plugin.ml parser.mli parser.ml lexer.ml run.ml
	cd .build/check/well-formedness-kompiled && ocamlfind $(OCAMLC) -c -g -w -11-26 -package gmp -package zarith -package uuidm -safe-string realdef.ml -match-context-rows 2
	cd .build/check/well-formedness-kompiled && ocamlfind $(OCAMLC) $(LIBFLAG) -o realdef.$(DLLEXT) realdef.$(EXT)
	cd .build/check/well-formedness-kompiled && ocamlfind $(OCAMLC) -g -o interpreter constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) run.$(EXT) interpreter.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -linkpkg -linkall -safe-string

.build/plugin-%/semantics.$(LIBEXT): $(wildcard plugin/plugin/*.ml plugin/plugin/*.mli) .build/%/ethereum-kompiled/constants.$(EXT)
	mkdir -p .build/plugin-$*
	cp plugin/plugin/*.ml plugin/plugin/*.mli .build/plugin-$*
	ocaml-protoc plugin/plugin/proto/*.proto -ml_out .build/plugin-$*
	cd .build/plugin-$* && ocamlfind $(OCAMLC) -c -g -I ../$*/ethereum-kompiled msg_types.mli msg_types.ml msg_pb.mli msg_pb.ml threadLocal.mli threadLocal.ml world.mli world.ml caching.mli caching.ml MANTIS.ml KRYPTO.ml -package cryptokit -package secp256k1 -package bn128 -package ocaml-protoc -safe-string -thread
	cd .build/plugin-$* && ocamlfind $(OCAMLC) -a -o semantics.$(LIBEXT) KRYPTO.$(EXT) msg_types.$(EXT) msg_pb.$(EXT) threadLocal.$(EXT) world.$(EXT) caching.$(EXT) MANTIS.$(EXT) -thread
	ocamlfind remove iele-semantics-plugin-$*
	ocamlfind install iele-semantics-plugin-$* plugin/plugin/META .build/plugin-$*/semantics.* .build/plugin-$*/*.cmi .build/plugin-$*/*.$(EXT)

.build/%/ethereum-kompiled/interpreter: .build/plugin-%/semantics.$(LIBEXT)
	ocamllex .build/$*/ethereum-kompiled/lexer.mll
	ocamlyacc .build/$*/ethereum-kompiled/parser.mly
	cd .build/$*/ethereum-kompiled && ocamlfind $(OCAMLC) -c -g -package gmp -package zarith -package uuidm -safe-string prelude.ml plugin.ml parser.mli parser.ml lexer.ml run.ml -thread
	cd .build/$*/ethereum-kompiled && ocamlfind $(OCAMLC) -c -g -w -11-26 -package gmp -package zarith -package uuidm -package iele-semantics-plugin-$* -safe-string realdef.ml -match-context-rows 2
	cd .build/$*/ethereum-kompiled && ocamlfind $(OCAMLC) $(LIBFLAG) -o realdef.$(DLLEXT) realdef.$(EXT)
	cd .build/$*/ethereum-kompiled && ocamlfind $(OCAMLC) -g -o interpreter constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) run.$(EXT) interpreter.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin-$* -linkpkg -linkall -thread -safe-string

.build/vm/iele-test-vm: .build/node/ethereum-kompiled/interpreter $(wildcard plugin/vm/*.ml plugin/vm/*.mli)
	mkdir -p .build/vm
	cp plugin/vm/*.ml plugin/vm/*.mli .build/vm
	cd .build/vm && ocamlfind $(OCAMLC) -g -I ../node/ethereum-kompiled -o iele-test-vm constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) realdef.$(EXT) run.$(EXT) VM.mli VM.ml ieleTestClient.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin-node -package rlp -package yojson -package hex -linkpkg -linkall -thread -safe-string

.build/vm/iele-vm: .build/node/ethereum-kompiled/interpreter $(wildcard plugin/vm/*.ml plugin/vm/*.mli)
	mkdir -p .build/vm
	cp plugin/vm/*.ml plugin/vm/*.mli .build/vm
	cd .build/vm && ocamlfind $(OCAMLC) -g -I ../node/ethereum-kompiled -o iele-vm constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) realdef.$(EXT) run.$(EXT) VM.mli VM.ml vmNetworkServer.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin-node -package rlp -package yojson -package hex -linkpkg -linkall -thread -safe-string
