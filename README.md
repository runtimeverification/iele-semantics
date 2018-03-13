IELE: Semantics of New Cryptocurrency VM in K
==============================================

In this repository we provide a model of IELE in K.

### Structure

The file [iele-syntax.md](iele-syntax.md) contains the syntax definition of IELE, along with comments that guide the reader through the structure of the language and links to more detailed descriptions of various features. This file is a good starting point for getting familiar with the language.

The file [data.md](data.md) explains the basic data of IELE (including words and some datastructures over them).
This data is defined functionally.

[iele.md](iele.md) is the file containing the semantics of IELE.
This file contains the **configuration** (a map of the state), and a simple imperative execution machine which IELE lives on top of.
It deals with the semantics of opcodes, the gas cost of execution, and parsing/unparsing/assembling/disassembling.

Finally, the file [Design.md](Design.md) discusses the design rationale of IELE. It also provides more detailed descriptions of various IELE features, as well as differences and similarities with EVM and LLVM.

### Testing

[ethereum.md](ethereum.md) loads test-files from the [Ethereum Test Set](https://github.com/ethereum/tests) and executes them, checking that the output is correct.
If the output is correct, the entire configuration is cleared.
If any check fails, the configuration retains the failed check at the top of the `<k>` cell.

[ethereum.md](ethereum.md) is also capable of executing tests of arbitrary IELE transactions (subject to the fact that this first release of IELE is still
built on top of an ethereum-like network layer). The test format is based off of, but slightly different from, the ethereum BlockchainTest json test format.

The structure of the json files is as follows:

* The top level is a json object, containing a list of key value pairs whose keys are the names of the tests to be executed and whose values are json objects containing an individual test case. The top-level fields in each test case are described below:
  * "pre": a json object mapping account IDs (20-byte hexadecimal strings) to an object for each account representing the state of that account before the test starts.
  * "blocks": a list of blocks the test should execute, each containing the folowing fields:
    * "transactions": a list of transactions to be executed, each containing the following fields:
      * "nonce": the nonce of the sender of the transaction.
      * "function": the name of the function to be called. Should be the empty string if the "to" field is the empty string.
      * "gasLimit": the gas limit of the transaction.
      * "value": the amount of currency sent with the transaction.
      * "to": the account the transaction is calling a function on. If the transaction creates an account, this field should be the empty string.
      * "arguments": a list of integers representing the input arguments of the top-level function call of the transaction.
      * "data": the contract being uploaded. Should be the empty string if the "to" field is NOT the empty string.
      * "gasPrice": the price to be paid per unit of gas used.
      * "secretKey" OR ("v" AND "r" AND "s"): the signature of the transaction (either as a private key or as an ECDSA signature of the remaining fields) used to sign the transaction.
    * "results": a list of results of each transaction in the block, each containing the following fields:
      * "out": a list of integers representing the output of the top-level function call of the transaction.
      * "status": the exit status of the top-level function call of the transaction.
      * "gas": the amount of gas remaining after the transaction executes.
      * "logs": the keccak256 hash of the rlp-encoded log entries generated by the transaction.
      * "refund": the gas refund after the transaction executes.
    * "blockHeader": a json object containing the partial block header of the current block, with the following fields:
      * "gasLimit": The block gas limit.
      * "number": The block number.
      * "difficulty": The block difficulty.
      * "timestamp": The timestamp of the block.
      * "coinbase": The block's beneficiary.
  * "network": The current network rules. Currently only one ruleset is supported, "Albe".
  * "blockhashes": because we don't specify the entire blocks, it is necessary to explicitly specify the information seen by the iele.blockhash function. This should be a list of 32-byte hashes ending in 0x00 representing the parentHash of the genesis block.
  * "postState": a json object mapping accoutn IDs (20-byte hexadecimal strings) to an object for each account representing the state of that account expected after the test finishes.
  
* Each account listed in the "pre" or "postState" fields has the following fields:
  * "nonce": the nonce of the account.
  * "balance": the balance of currency in the account.
  * "storage": a json object mapping integer storage keys to integer storage values.
  * "code": either "", representing an empty account, a string beginning with "0x", indicating a binary representation of an account, or a filename containing a IELE contract to be assembled and used as the code of the account.
  
* Each integer is encocded as a string in hexadecimal representing the big-endian representation of the integer. The empty string also can be used to represent the integer zero.

For example pure-IELE tests, see `tests/iele`.

Using the Definition
--------------------

### Installing

See [INSTALL.md](INSTALL.md).

### Testing

To execute a VM test, run `./vmtest $file` where `$file` is a JSON file containing the specification of the test.
To execute a Blockchain test, run `./blockchaintest $file` where `$file` is a JSON file containing the specification of the test.

To execute all currently passing tests, run `make test`.

To run only the pure-IELE tests, run `make iele-test`.

For testing the interprocess VM API, first start the vm server
by running `.build/vm/iele-vm 10000 127.0.0.1` in another shell or
in the background.
With the server running, run `make iele-test-node` to run the pure-IELE
tests on the vm server, or run individual tests using `.build/vm/iele-vm-test`.

### Debugging

When executing a test, if a test fails, the intermediate state of the IELE VM is dumped on the command line. This can be used to provide
information about exactly what went wrong with the test, which can be used for debugging purposes. Generally speaking, each dumped state begins with
the string `` `<generatedTop>`(`<k>` `` followed by the current list of instructions to be executed by the VM in balanced parentheses. You can usually
determine what went wrong with the test by examining the first entry in the list (i.e., everything up to the first `~>`).

Some examples of common errors you may encounter are below, with each example followed by a description of the error.

```
loadTx(#token("966588469268559010541288244128342317224451555083","Int"))
```

In this example, either a transaction was signed incorrectly, or the sender of the transaction as specified by its signature does not exist.

```
`check__ETHEREUM-SIMULATION`(`_:__IELE-DATA`(#token("\"account\"","String"),`{_}_IELE-DATA`(`_,__IELE-DATA`(`_:__IELE-DATA`(#token("91343852333181432387730302044767688728495783936","Int"),`{_}_IELE-DATA`(`_,__IELE-DATA`(`_:__IELE-DATA`(#token("\"storage\"","String"),`_Map_`(`_|->_`(#token("0","Int"),#token("10001","Int")),`_|->_`(#token("2428090106599461928744973076844625336880384098059","Int"),#token("10000","Int")))),`.List{"_,__IELE-DATA"}`(.KList)))),`.List{"_,__IELE-DATA"}`(.KList)))))
```

Here account 91343852333181432387730302044767688728495783936 (in decimal) has different values for storage than expected by the test. Similar errors exist for the nonce, balance, and code of an account. You can refer to the `<storage>` cell in the dumped state for information on their actual values. The correct storage cell is nested within an `<account>` cell whose `<acctID>` is the address of the account in decimal.

```
`check__ETHEREUM-SIMULATION`(`_:__IELE-DATA`(#token("\"out\"","String"),`[_]_IELE-DATA`(`_,__IELE-DATA`(#token("\"0x01\"","String"),`.List{"_,__IELE-DATA"}`(.KList)))))
```

Here is a similar error, but the error states that the transaction returned an incorrect list of values. The actual return value can be found in the `<output>` cell.

If other errors occur that are not on this list, feel free to contact us on Gitter or Riot and we will be happy to assist in helping you understand. Future releases of IELE may also have improved debugging information making this easier to interpret.

Contacting Us
-------------

In the spirit of open source, community-driven development, we will be holding all IELE discussions on our channels
* [#IELE:matrix.org](https://riot.im/app/#/room/#IELE:matrix.org) on [Riot](https://riot.im)
* [runtimeverification/iele-semantics](https://gitter.im/runtimeverification/iele-semantics) on [Gitter](https://gitter.im)

Contributing
------------

Any pull requests into this repository will not be reviewed until at least some conditions are met.
Here we'll accumulate the standards that this repository is held to.

Code style guidelines, while somewhat subjective, will still be inspected before going to review.
In general, read the rest of the definition for examples about how to style new K code; we collect a few common flubs here.

Writing tests and more contract proofs is **always** appreciated.
Tests can come in the form of proofs done over contracts too :).

### Hard - Every Commit

These are hard requirements (**must** be met before review), and they **must** be true for **every** commit in the PR.

-   The build products which we store in the repository (K definition files and proof specification files) must be up-to-date with the files they are generated from.
    We do our development directly in the Markdown files and build the definition files (`*.k`) from them using [pandoc-tangle](https://github.com/ehildenb/pandoc-tangle).
    Not everyone wants to install `pandoc-tangle` or `pandoc`, so the build products are kept in the repository for people who just want to experiment quickly.

-   If a new feature is introduced in the PR, and later a bug is fixed in the new feature, the bug fix must be squashed back into the feature introduction.
    The *only* exceptions to this are if you want to document the bug because it was quite tricky or is something you believe should be fixed about K.
    In these exceptional cases, place the bug-fix commit directly after the feature introduction commit and leave useful commit messages.
    In addition, mark the feature introduction commit with `[skip-ci]` if tests will fail on that commit so that we know not to waste time testing it.

-   No tab characters, 4 spaces instead.
    Linux-style line endings; if you're on a Windows machine make sure to run `dos2unix` on the files.

### Hard - PR Tip

These are hard requirements (**must** be met before review), but they only have to be true for the tip of the PR before review.

-   Every test in the repository must pass.
    We will test this with `./tests/ci/with-k bothk ./Build test-all` (or `./tests/ci/with-k bothk ./Build partest-all` on parallel machines).

### Soft - Every Commit

These are soft requirements (review **may** start without these being met), and they will be considered for **every** commit in the PR.

-   Comments do not live in the K code blocks, but rather in the surrounding Markdown (unless there is a really good reason to localize the comment).

-   You should consider prefixing "internal" symbols (symbols that a user would not write in a program) with a hash (`#`).

-   Place a line of `-` after each block of syntax declarations.

    ```{.k}
        syntax Foo ::= "newSymbol"
     // --------------------------
        rule <k> newSymbol => . ... </k>
    ```

    Notice that if there are rules immediately following the syntax declaration, a commented-out line of `-` is inserted afterward.
    Notice that the width of the line of `-` matches that of the preceding line.

-   Place spaces around parentheses and commas in K's pretty functional-style syntax declarations.

    ```{.k}
        syntax Foo ::= newFunctionalSyntax ( Int , String )
     // ---------------------------------------------------
    ```

-   When multiple structurally-similar rules are present, line up as much as possible (and makes sense).

    ```{.k}
        rule <k> #do1       => . ... </k> <cell1> not-done => done        </cell1>
        rule <k> #do1Longer => . ... </k> <cell1> not-done => done-longer </cell1>

        rule <k> #do2     => . ... </k> <cell2> not-done => done2 </cell2>
        rule <k> #doShort => . ... </k> <cell2> nd       => done2 </cell2>
    ```

    This makes it simpler to make changes to entire groups of rules at a time using sufficiently modern editors.
    Notice that if we break alignment (eg. from the `#do1` group above to the `#do2` group), we put an extra line between the groups of rules.

-   Line up the `r` in `requires` with the `l` in `rule` (if it's not all on one line).
    Similarly, line up the end of `andBool` for extra side-conditions with the end of `requires`.

    ```{.k}
        rule <k> A => B ... </k>
             SOME_LARGE_CONFIGURATION

          requires A > 3
           andBool isPrime(A)
    ```
