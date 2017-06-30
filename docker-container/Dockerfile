FROM ubuntu:16.04
MAINTAINER Dan Fithian
RUN apt-get update -qq -y
RUN apt-get install -y libgmp10 ca-certificates netbase cabal-install ghc mueval
RUN cabal update
RUN cabal install random lambdabot-trusted containers bytestring base array
COPY slack-lambdabot /
ARG api_token
ENV SLACK_API_TOKEN $api_token
ENTRYPOINT ["/slack-lambdabot"]
