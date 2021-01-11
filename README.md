IELE: Semantics of New Blockchain VM in K
==============================================

In this repository we provide a model of IELE in K.

### Structure

-   The file [iele-syntax.md](iele-syntax.md) contains the syntax definition of IELE, along with comments that guide the reader through the structure of the language and links to more detailed descriptions of various features.
    This file is a good starting point for getting familiar with the language.
-   The file [data.md](data.md) explains the basic data of IELE (including words and some datastructures over them).
    This data is defined functionally.
-   [iele.md](iele.md) is the file containing the semantics of IELE.
    This file contains the **configuration** (a map of the state), and a simple imperative execution machine which IELE lives on top of.
    It deals with the semantics of opcodes and parsing/unparsing/assembling/disassembling.
-   [iele-gas.md](iele-gas.md) describes gas price computations. [iele-gas-summary.md](iele-gas-summary.md) summarizes them in a format readable by those who don't know K. 
-   Finally, the file [Design.md](Design.md) discusses the design rationale of IELE.
    It also provides more detailed descriptions of various IELE features, as well as differences and similarities with EVM and LLVM.
-   [iele-testing.md](iele-testing.md) loads test-files from the [Ethereum Test Set](https://github.com/ethereum/tests) and executes them, checking that the output is correct.
    If the output is correct, the entire configuration is cleared.
    If any check fails, the configuration retains the failed check at the top of the `<k>` cell.
    [iele-testing.md](iele-testing.md) is also capable of executing tests of arbitrary IELE transactions (subject to the fact that this first release of IELE is still built on top of an ethereum-like network layer).
    The test format is based off of, but slightly different from, the ethereum BlockchainTest json test format.

Using the Definition
--------------------

### Installing/Building

See [INSTALL.md](INSTALL.md).

### Help/Version

Calling `kiele help` will and `kiele version` will output the user guide and the KIELE version, respectively.

### Assembler

The assembler takes textual IELE and produces IELE bytecode.
For examplle, on the following file `iele-examples/factorial.iele`:

```iele
contract Factorial {

  define public @factorial(%n) {
    %lt = cmp lt %n, 0
    br %lt, throw

    %result = 1

  condition:
    %cond = cmp le %n, 0
    br %cond, after_loop

  loop_body:
    %result = mul %result, %n
    %n      = sub %n, 1
    br condition

  after_loop:
    ret %result

  throw:
    call @iele.invalid()
  }

  define @init() {}
}
```

You can run:

```sh
kiele assemble iele-examples/factorial.iele
```

To produce output:

```sh
0000004C6303690009666163746F7269616C6800010001618001100042650003026101036600006180011200446500020466000102001B610101030040640000660002F6000103660003FE6700000000
```

**NOTE**: Right now, if you use the NixOS package, you must call `iele-assemble` instead of `kiele assemble`.

### KIELE Node

You can run a KIELE node, specifying the port to run on and use the KIELE test harness to run some transactions through it.
To start the KIELE node, call (in one terminal):

```sh
kiele vm --port 9001
```

Then in a separate terminal, send a transaction through:

```sh
iele-test-vm tests/iele/danse/factorial/factorial_positive.iele.json 9001
```

You'll see the launched node record to the terminal a list of messages it sends and receives.

**NOTE**: The `iele-test-vm` executable is only available in a from-source build of KIELE.
          You can still launch and use the KIELE VM node without this executable.

### Interpreter

Tests in the Ethereum Test Suite format can be run directly by the IELE interpreter.

For example, from this repository, you can run:

```sh
make assemble-iele-test -j4
```

To prepare the Ethereum test-suite.
Then you can run an individual test, and see the post-state with:

```sh
kiele interpret tests/iele/danse/factorial/factorial_positive.iele.json.test-assembled
```

**NOTE**: Printing the post-state will not work unless you are using a from-source build of KIELE.
          Instead, you should inspect the exit code of `kiele interpret` to see if the test passed or failed, by adding option `--no-unparse` to skip printing the post-state.

### Well-Formedness Checker

The KIELE well-formedness checker will do some type-checking on the input IELE program, using exit code to indicate success/failure.
For example, we can type-check the ERC20 contract example `iele-examples/erc20.iele`:

```sh
kiele check --schedule DANSE iele-examples/erc20.iele
```

**NOTE**: The KIELE well-formedness checker requires that (i) you are using a local build of KIELE, and (ii) you have installed K as well.

Contacting Us
-------------

To reach us, you can either use our chat, or open issues against this repository:

-   [#IELE Element Channel](https://app.element.io/#/room/!LtQJkJbwuhxaMuWVOa:matrix.org) for chat.
-   [runtimeverification/iele-semantics](https://github.com/runtimeverification/iele-semantics) for issues/pull-requests.
