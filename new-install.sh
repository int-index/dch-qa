set -xe

cabal new-build monadfix-gen
cp $(cabal new-exec -- which monadfix-gen) $HOME/.local/bin
