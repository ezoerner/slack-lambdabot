FROM ubuntu:16.04
MAINTAINER Dan Fithian
RUN apt-get update -qq -y
RUN apt-get install -y libgmp10 ca-certificates netbase cabal-install ghc mueval
RUN cabal update
RUN cabal install random lambdabot-trusted containers bytestring base array
COPY slack-lambdabot /
ENV SLACK_API_TOKEN xoxb-111447402885-B7W7gCbVNj4lRlpPx5Nwh4rT
ENTRYPOINT ["/slack-lambdabot"]
