K_VERSION=rvk

# Common to all versions of K
# ===========================

.PHONY: all clean build tangle defn proofs split-tests test vm-test blockchain-test deps assembler

all: build split-vm-tests

clean:
	rm -rf .build

build: tangle .build/${K_VERSION}/ethereum-kompiled/extras/timestamp assembler

assembler:
	cd compiler && stack build --install-ghc

# Tangle from *.md files
# ----------------------

tangle: defn proofs

defn_dir=.build/${K_VERSION}
defn_files=${defn_dir}/ethereum.k ${defn_dir}/data.k ${defn_dir}/iele.k ${defn_dir}/iele-binary.k ${defn_dir}/krypto.k ${defn_dir}/iele-syntax.k
defn: $(defn_files)

.build/${K_VERSION}/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc --from markdown --to tangle.lua --metadata=code:"k $(K_VERSION)" $< > $@

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

test: $(passing_targets)
vm-test: $(passing_vm_targets)
blockchain-test: $(passing_blockchain_targets)

tests/VMTests/%.test: tests/VMTests/% | build
	./vmtest $<
	touch $@
tests/BlockchainTests/%.test: tests/BlockchainTests/% | build
	./blockchaintest $<
	touch $@

tests/%/make.timestamp: tests/ethereum-tests/%.json tests/evm-to-iele/evm-to-iele
	@echo "==   split: $@"
	mkdir -p $(dir $@)
	tests/split-test.py $< $(dir $@)
	touch $@

tests/evm-to-iele/evm-to-iele: $(wildcard tests/evm-to-iele/*.ml tests/evm-to-iele/*.mli)
	cd tests/evm-to-iele && ocamlfind opt -g ieleUtil.mli ieleUtil.ml evm.mli evm.ml iele.mli iele.ml conversion.mli conversion.ml main.ml -package zarith -package hex -linkpkg -o evm-to-iele

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
	opam install mlgmp zarith uuidm cryptokit secp256k1 bn128 hex

.build/rvk/ethereum-kompiled/extras/timestamp: .build/rvk/ethereum-kompiled/interpreter
.build/rvk/ethereum-kompiled/interpreter: $(defn_files) KRYPTO.ml
	@echo "== kompile: $@"
	${KOMPILE} --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module ETHEREUM-SIMULATION $< --directory .build/rvk \
					--hook-namespaces KRYPTO --gen-ml-only -O3 --non-strict
	ocamlfind opt -c .build/rvk/ethereum-kompiled/constants.ml -package gmp -package zarith
	ocamlfind opt -c -I .build/rvk/ethereum-kompiled KRYPTO.ml -package cryptokit -package secp256k1 -package bn128
	ocamlfind opt -a -o semantics.cmxa KRYPTO.cmx
	ocamlfind remove iele-semantics-plugin
	ocamlfind install iele-semantics-plugin META semantics.cmxa semantics.a KRYPTO.cmi KRYPTO.cmx
	${KOMPILE} --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module ETHEREUM-SIMULATION $< --directory .build/rvk \
					--hook-namespaces KRYPTO --packages iele-semantics-plugin -O3 --non-strict
	cd .build/rvk/ethereum-kompiled && ocamlfind opt -o interpreter constants.cmx prelude.cmx plugin.cmx parser.cmx lexer.cmx run.cmx interpreter.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package iele-semantics-plugin -linkpkg -inline 20 -nodynlink -O3 -linkall
