set -xe

cabal new-build monadfix-lib-gen
cp $(cabal new-exec -- which monadfix-lib-gen) $HOME/.local/bin
