slack-lambdabot
===============

[Lambdabot](https://hackage.haskell.org/package/lambdabot) for
[Slack](https://slack.com)

Building
--------

Install [Docker](https://www.docker.com) if not already installed.

`slack-lambdabot` requires a Bot user OAuth access token ("api token").
Follow Slack's [Bot Users](https://api.slack.com/bot-users) guide to create one.

Build the project and docker image:

```
make API_TOKEN=<api_token> IMAGE_NAME=<username>/slack-lambdabot:<tag>
```

Running
-------

Run the docker image, e.g.

```
docker run -t <username>/slack-lambdabot:<tag>
```

Deploying
---------
_TBD: Instructions for deploying container to AWS ECS will be provided here._