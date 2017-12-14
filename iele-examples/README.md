# Iele examples

## Structure

* **erc20.iele**: Implements the ERC-20 token standard.
* **forwarder.iele**: Implements a forwarder that forwards any funds sent to
  the account it is deployed with to the account that created it.
* **forwardingWallet-copycreate.iele**: Showcases the dynamic contract creation
  feature of IELE. Implements a wallet that can serve deposit and withdrawal
  requests as well as setup new accounts that forward funds sent to them to
  this wallet.
* **forwardingWallet.iele**: Showcases the dynamic contract creation
  feature of IELE. Implements a forwarder that forwards any funds sent to
  its account to the account that created it.
* **simpleOpenAuction.iele**: Implements a simple open auction.

Tests for these contracts are under `tests/iele/`

## Running the tests

To run all the tests for these examples, run
```sh
make iele-test
```

To run individual tests use `./blockchaintest` on the corresponding
`.iele.json` file(s).

* The files in `tests/iele/ERC20` all test `erc20.iele`.
* The file `copycreate.iele.json` in `tests/iele/forwarder` tests
  `forwardingWallet-copycreate.iele` and `forwarder.iele`
while `create.iele.json` tests `forwardingWallet.iele` and `forwarder.iele`.
* The files in `tests/iele/auction` all test `simpleOpenAuction.iele`.

## Correctness proofs

Will be available soon.
