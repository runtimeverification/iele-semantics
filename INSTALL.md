Installing IELE
===============

In a nutshell, to install IELE on an Ubuntu 16.04 machine, run:

```

sudo apt-get update
sudo apt-get install make gcc maven curl openjdk-8-jdk flex opam pkg-config libmpfr-dev autoconf libtool pandoc libssl-dev build-essential libffi-dev libpython-dev python-setuptools libssl-dev
git submodule update --init # Initialize submodules
curl -sSL https://get.haskellstack.org/ | sh # Install stack
cd .build/secp256k1 && ./autogen.sh && ./configure --enable-module-recovery --enable-module-ecdh --enable-experimental && make && sudo make install # install secp256k1 from bitcoin-core
cd ../..
cd .build/pyethereum && sudo python setup.py install # install pyethereum to sign "ethereum" transactions
cd ../..
make deps # Build dependencies not installed by package manager
eval `opam config env` # add OCAML installation to path
make # Build project

```

To test against a fragment of the ethereum test suite, run `make test`

Other Linux distros can install by installing the list of dependencies in the `apt-get install` command above and then following the rest of the instructions.

To install on Windows, first install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and then proceed with the installation for Ubuntu 16.04. Note that the installation will take significantly longer due to existing problems in the performance of disk-intensive processes in WSL, and you should expect it to over half an hour. However, once installed, it should run roughly the same speed as on native Linux.
