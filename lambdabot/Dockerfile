FROM fpco/stack-build:lts-8.20
MAINTAINER Eric Zoerner
WORKDIR /root
RUN cabal update
# install haskell-src-exts first to help prevent out-of-memory errors
RUN cabal install haskell-src-exts-simple-1.19.0.0
RUN cabal install --constraint 'transformers installed' lambdabot djinn
RUN hoogle generate
RUN lambdabot -e "> 1 + 1"
CMD ["lambdabot"]
