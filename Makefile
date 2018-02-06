K_VERSION=rvk

# Common to all versions of K
# ===========================

.PHONY: all clean build tangle defn proofs split-tests test vm-test blockchain-test deps assembler iele-test iele-test-node

all: build split-vm-tests

clean:
	rm -rf .build/rvk .build/plugin

build: tangle .build/${K_VERSION}/ethereum-kompiled/interpreter assembler

assembler:
	cd compiler && stack build --install-ghc

# Tangle from *.md files
# ----------------------

tangle: defn proofs

defn_dir=.build/${K_VERSION}
defn_files=${defn_dir}/ethereum.k ${defn_dir}/data.k ${defn_dir}/iele.k ${defn_dir}/iele-gas.k ${defn_dir}/iele-binary.k ${defn_dir}/krypto.k ${defn_dir}/iele-syntax.k ${defn_dir}/iele-node.k
defn: $(defn_files)

NODE?=standalone

.build/${K_VERSION}/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to tangle.lua --metadata=code:"k $(K_VERSION) $(NODE)" $< > $@

node: .build/vm/iele-vm

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

test: $(passing_targets) ${iele_targets}
vm-test: $(passing_vm_targets)
blockchain-test: $(passing_blockchain_targets)
iele-test: ${iele_targets}
iele-test-node: ${iele_node_targets}

tests/VMTests/%.test: tests/VMTests/% | build
	./vmtest $<
tests/BlockchainTests/%.test: tests/BlockchainTests/% | build
	./blockchaintest $<
tests/iele/%.test: tests/iele/% | build
	./blockchaintest $<
tests/iele/%.nodetest: tests/iele/% | node
	.build/vm/iele-vm $<

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

deps:
	cd tests/ci/rv-k && mvn package
	opam init
	opam repository add k "tests/ci/rv-k/k-distribution/target/release/k/lib/opam" || opam repository set-url k "tests/ci/rv-k/k-distribution/target/release/k/lib/opam"
	opam update
	opam switch 4.03.0+k
	opam install mlgmp zarith uuidm cryptokit secp256k1 bn128 hex ocaml-protoc rlp yojson

ifeq ($(BYTE),yes)
EXT=cmo
LIBEXT=cma
OCAMLC=c
else
EXT=cmx
LIBEXT=cmxa
OCAMLC=opt -O3
endif

.build/rvk/ethereum-kompiled/constants.$(EXT): $(defn_files)
	@echo "== kompile: $@"
	${KOMPILE} --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module IELE-SYNTAX $< --directory .build/rvk \
					--hook-namespaces "KRYPTO MANTIS" --gen-ml-only -O3 --non-strict
	ocamlfind $(OCAMLC) -c -g .build/rvk/ethereum-kompiled/constants.ml -package gmp -package zarith -safe-string

.build/plugin/semantics.$(LIBEXT): $(wildcard iele-semantics-plugin/*.ml iele-semantics-plugin/*.mli) .build/rvk/ethereum-kompiled/constants.$(EXT)
	mkdir -p .build/plugin
	cp iele-semantics-plugin/*.ml iele-semantics-plugin/*.mli .build/plugin
	ocaml-protoc iele-semantics-plugin/proto/*.proto -ml_out .build/plugin
	cd .build/plugin && ocamlfind $(OCAMLC) -c -g -I ../rvk/ethereum-kompiled msg_types.mli msg_types.ml world.mli world.ml caching.mli caching.ml MANTIS.ml KRYPTO.ml -package cryptokit -package secp256k1 -package bn128 -safe-string
	cd .build/plugin && ocamlfind $(OCAMLC) -a -o semantics.$(LIBEXT) KRYPTO.$(EXT) msg_types.$(EXT) world.$(EXT) caching.$(EXT) MANTIS.$(EXT)
	ocamlfind remove iele-semantics-plugin
	ocamlfind install iele-semantics-plugin iele-semantics-plugin/META .build/plugin/semantics.* .build/plugin/*.cmi .build/plugin/*.$(EXT)

.build/rvk/ethereum-kompiled/interpreter: .build/plugin/semantics.$(LIBEXT)
	ocamllex .build/rvk/ethereum-kompiled/lexer.mll
	ocamlyacc .build/rvk/ethereum-kompiled/parser.mly
	cd .build/rvk/ethereum-kompiled && ocamlfind $(OCAMLC) -c -g -package gmp -package zarith -package uuidm -safe-string prelude.ml plugin.ml parser.mli parser.ml lexer.ml run.ml
	cd .build/rvk/ethereum-kompiled && ocamlfind $(OCAMLC) -c -g -w -11-26 -package gmp -package zarith -package uuidm -package iele-semantics-plugin -safe-string realdef.ml -match-context-rows 2
ifeq ($(BYTE),yes)
	cd .build/rvk/ethereum-kompiled && ocamlfind c -a -o realdef.cma realdef.cmo
else
	cd .build/rvk/ethereum-kompiled && ocamlfind opt -O3 -shared -o realdef.cmxs realdef.cmx
endif
	cd .build/rvk/ethereum-kompiled && ocamlfind $(OCAMLC) -g -o interpreter constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) run.$(EXT) interpreter.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin -linkpkg -linkall -safe-string

.build/vm/iele-vm: NODE=node
.build/vm/iele-vm: build $(wildcard iele-vm/*.ml iele-vm/*.mli)
	mkdir -p .build/vm
	cp iele-vm/*.ml iele-vm/*.mli .build/vm
	cd .build/vm && ocamlfind $(OCAMLC) -g -I ../rvk/ethereum-kompiled -o iele-vm constants.$(EXT) prelude.$(EXT) plugin.$(EXT) parser.$(EXT) lexer.$(EXT) realdef.$(EXT) run.$(EXT) ieleVM.mli ieleVM.ml ieleTestClient.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin -package rlp -package yojson -package hex -linkpkg -linkall -safe-string
