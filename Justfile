set dotenv-load

x86:
  arch -x86_64 zsh

build:
  cabal install --installdir=bin --install-method=copy --overwrite-policy=always

test:
  cabal test --test-show-details=always
