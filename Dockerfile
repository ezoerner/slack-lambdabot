FROM fpco/stack-build:lts-8.20
MAINTAINER Eric Zoerner
COPY slack-lambdabot.cabal Setup.hs /root/
COPY src /root/src
WORKDIR /root
RUN cabal update
# install haskell-src-exts first to help prevent out-of-memory errors
RUN cabal install haskell-src-exts-simple-1.19.0.0
RUN cabal install --constraint 'transformers installed' lambdabot djinn
RUN cabal install
RUN hoogle generate
RUN lambdabot -e "> 1 + 1"
ARG api_token
ENV SLACK_API_TOKEN $api_token
CMD ["cabal", "exec", "slack-lambdabot"]
