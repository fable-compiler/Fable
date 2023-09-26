#!/bin/zsh

# dsm seems to incorrectly detect the architecture on Mac M1
# The if statement below is a workaround for this issue
# See https://github.com/Yakiyo/dsm/issues/26

if [[ $(uname -m) == 'aarch64' ]]; then
    dsm install --arch arm64 $1;
else
    dsm install $1;
fi