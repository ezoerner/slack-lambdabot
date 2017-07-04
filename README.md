slack-lambdabot
===============

[Lambdabot](https://hackage.haskell.org/package/lambdabot) for
[Slack](https://slack.com)

Building
--------

Install [Docker](https://www.docker.com) if not already installed.

`slack-lambdabot` requires a Bot user OAuth access token ("api token").
Follow Slack's [Bot Users](https://api.slack.com/bot-users) guide to create one.

Build the docker image:

    make API_TOKEN=<api_token> IMAGE_NAME=<username>/slack-lambdabot:<tag>

Running
-------

Run the docker image, e.g.

```
make run IMAGE_NAME=<username>/slack-lambdabot:<tag>
```

Deploying
---------
_TBD: Instructions for deploying container to AWS ECS will be provided here._


Notes
-----
* I made a significant effort to build this project with stack instead of cabal,
but kept getting runtime errors from the mueval library. In the end it was
easier for me to get things working with cabal than to solve this problem.
