set -xe

cabal new-build dch-qa
cp $(cabal new-exec -- which dch-qa) $HOME/.local/bin
