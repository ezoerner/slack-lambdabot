# edit the FROM to be your lambda-cli image
FROM ezoerner/lambdabot-cli:latest
MAINTAINER Eric Zoerner
COPY slack-lambdabot.cabal Setup.hs /root/
COPY src /root/src
WORKDIR /root
RUN cabal install
CMD ["cabal", "exec", "slack-lambdabot"]
