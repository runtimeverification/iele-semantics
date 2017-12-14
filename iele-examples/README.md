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

## Running the tests

To run all the tests, one can always do
```sh
make test
```

However, to run only the tests involving the examples above, run

```sh
make

#  erc20.iele
for f in `ls tests/iele/ERC20`; do echo $f; ./blockchaintest tests/iele/ERC20/$f || break; done

# forwarder.iele and forwardingWallet-copycreate.iele
./blockchaintest tests/iele/forwarder/copycreate.iele.json

# forwarder.iele and forwardingWallet.iele
./blockchaintest tests/iele/forwarder/create.iele.json

# simpleOpenAuction.iele
for f in `ls tests/iele/auction`; do echo $f; ./blockchaintest tests/iele/auction/$f || break; done
```

## Correctness proofs

Will be available soon.