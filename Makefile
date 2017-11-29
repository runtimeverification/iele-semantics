K_VERSION=rvk

# Common to all versions of K
# ===========================

.PHONY: all clean build tangle defn proofs split-tests test vm-test blockchain-test

all: build split-vm-tests

clean:
	rm -r .build
	find tests/proofs/ -name '*.k' -delete

build: tangle .build/${K_VERSION}/ethereum-kompiled/extras/timestamp

# Tangle from *.md files
# ----------------------

tangle: defn proofs

defn_dir=.build/${K_VERSION}
defn_files=${defn_dir}/ethereum.k ${defn_dir}/data.k ${defn_dir}/iele.k ${defn_dir}/iele-binary.k ${defn_dir}/krypto.k ${defn_dir}/iele-syntax.k
defn: $(defn_files)

.build/${K_VERSION}/%.k: %.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc-tangle --from markdown --to code-k --code ${K_VERSION} $< > $@

proof_dir=tests/proofs
proof_files=${proof_dir}/sum-to-n-spec.k \
			${proof_dir}/hkg/allowance-spec.k \
			${proof_dir}/hkg/approve-spec.k \
			${proof_dir}/hkg/balanceOf-spec.k \
			${proof_dir}/hkg/transfer-else-spec.k ${proof_dir}/hkg/transfer-then-spec.k \
			${proof_dir}/hkg/transferFrom-else-spec.k ${proof_dir}/hkg/transferFrom-then-spec.k \
			${proof_dir}/bad/hkg-token-buggy-spec.k

proofs: $(proof_files)

tests/proofs/sum-to-n-spec.k: proofs/sum-to-n.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc-tangle --from markdown --to code-k --code sum-to-n $< > $@

tests/proofs/hkg/%-spec.k: proofs/hkg.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc-tangle --from markdown --to code-k --code $* $< > $@

tests/proofs/bad/hkg-token-buggy-spec.k: proofs/token-buggy-spec.md
	@echo "==  tangle: $@"
	mkdir -p $(dir $@)
	pandoc-tangle --from markdown --to code-k --code k $< > $@

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

# UIUC K Specific
# ---------------

.build/uiuck/ethereum-kompiled/extras/timestamp: $(defn_files)
	@echo "== kompile: $@"
	kompile --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module ETHEREUM-SIMULATION $< --directory .build/uiuck

# RVK Specific
# ------------

.build/rvk/ethereum-kompiled/extras/timestamp: .build/rvk/ethereum-kompiled/interpreter
.build/rvk/ethereum-kompiled/interpreter: $(defn_files) KRYPTO.ml
	@echo "== kompile: $@"
	kompile --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module ETHEREUM-SIMULATION $< --directory .build/rvk \
					--hook-namespaces KRYPTO --gen-ml-only -O3 --non-strict
	ocamlfind opt -c .build/rvk/ethereum-kompiled/constants.ml -package gmp -package zarith
	ocamlfind opt -c -I .build/rvk/ethereum-kompiled KRYPTO.ml -package cryptokit -package secp256k1 -package bn128
	ocamlfind opt -a -o semantics.cmxa KRYPTO.cmx
	ocamlfind remove ethereum-semantics-plugin
	ocamlfind install ethereum-semantics-plugin META semantics.cmxa semantics.a KRYPTO.cmi KRYPTO.cmx
	kompile --debug --main-module ETHEREUM-SIMULATION \
					--syntax-module ETHEREUM-SIMULATION $< --directory .build/rvk \
					--hook-namespaces KRYPTO --packages ethereum-semantics-plugin -O3 --non-strict
	cd .build/rvk/ethereum-kompiled && ocamlfind opt -o interpreter constants.cmx prelude.cmx plugin.cmx parser.cmx lexer.cmx run.cmx interpreter.ml -package gmp -package dynlink -package zarith -package str -package uuidm -package unix -package ethereum-semantics-plugin -linkpkg -inline 20 -nodynlink -O3 -linkall
