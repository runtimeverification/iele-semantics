Installing IELE
===============

In a nutshell, to install IELE on an Ubuntu 16.04 machine, run:

```

sudo apt-get update
sudo apt-get install make gcc maven curl openjdk-8-jdk flex opam pkg-config libmpfr-dev autoconf libtool pandoc libssl-dev build-essential libffi-dev libpython-dev python-setuptools
git submodule update --init # Initialize submodules
curl -sSL https://get.haskellstack.org/ | sh # Install stack
cd .build/secp256k1 && ./autogen.sh && ./configure --enable-module-recovery --enable-module-ecdh --enable-experimental && make && sudo make install # install secp256k1 from bitcoin-core
cd ../..
cd .build/pyethereum && sudo CFLAGS="$CFLAGS" LDFLAGS="$LDFLAGS" python setup.py install # install pyethereum to sign "ethereum" transactions
cd ../..
make deps # Build dependencies not installed by package manager
eval `opam config env` # add OCAML installation to path
make # Build project

```

To install on MacOS, after installing the command line tools package:

```

brew tap caskroom/cask caskroom/version
brew cask install java8
brew install maven opam pkg-config gmp mpfr automake libtool pandoc
export LDFLAGS="-L$(brew --prefix openssl)/lib" # needed by pyethereum
export CFLAGS="-I$(brew --prefix openssl)/include" # needed by pyethereum
```

Followed by the Ubuntu instructions beginning at `git submodule update --init`

--------------

To test against a fragment of the ethereum test suite, run `make test`

Other Linux distros can install by installing the list of dependencies in the `apt-get install` command above and then following the rest of the instructions.

To install on Windows, first install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and then proceed with the installation for Ubuntu 16.04. Note that the installation will take significantly longer due to existing problems in the performance of disk-intensive processes in WSL, and you should expect it to over half an hour. However, once installed, it should run roughly the same speed as on native Linux.

Developers
==========
If you have run the above setup, and a build fails because of missing dependences after checking out a different version of the code,
you can probably fix it by making sure the git submodules are up to date with `git submodule update --init` and then running `make deps`. If that fails, it should always work to repeat the full installation instructions above.
