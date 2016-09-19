#!/usr/bin/env bash

echo "Check and install stack..."
which stack > /dev/null || curl -sSL https://get.haskellstack.org/ | sh

echo "Setup stack..."
stack setup

echo "Build sshakyll and site..."
stack install

mkdir -p bin

cp -a $HOME/.local/bin/sshakyll bin
cp -a $HOME/.local/bin/site bin

echo "Build getPublicId..."
make -C sandstorm
cp -a sandstorm/bin/getPublicId bin
