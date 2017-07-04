ARG LBOT_IMAGE=ezoerner/lambdabot-cli:latest
FROM $LBOT_IMAGE
MAINTAINER Eric Zoerner
COPY slack-lambdabot.cabal Setup.hs /root/
COPY src /root/src
WORKDIR /root
RUN cabal install
ARG api_token
ENV SLACK_API_TOKEN $api_token
CMD ["cabal", "exec", "slack-lambdabot"]
