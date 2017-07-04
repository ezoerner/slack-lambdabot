slack-lambdabot
===============

[Lambdabot](https://hackage.haskell.org/package/lambdabot) for
[Slack](https://slack.com)

Building
--------

Install [Docker](https://www.docker.com) if not already installed.

`slack-lambdabot` requires a Bot user OAuth access token ("api token").
Follow Slack's [Bot Users](https://api.slack.com/bot-users) guide to create one.

The slack-lambdabot is built in two layers of docker images.
The first layer is lambdabot-cli, which can also be run standalone.
You can either use the public image `ezoerner/lambdabot-cli:latest`, or build your
own.

To build your own lambdabot-cli image:

    cd lambdabot-cli
    make IMAGE_NAME=<username>/lambdabot-cli:<tag>

To build the slack-lambdabot image:

**Note:** If you are using your own lambdabot-cli image, edit the FROM line in the Dockerfile
in the root of this project to match your image name.

Build the slack-lambdabot image:
    
    # if you are using the ezoerner/slack-lambdabot-cli image, leave out the
    # LB_CLI_IMAGE parameter
    cd <project root>
    make API_TOKEN=<api_token> IMAGE_NAME=<username>/slack-lambdabot:<tag> LB_CLI_IMAGE=<username>/lambdabot-cli:<tag>

Running
-------

Run the slack-lambdabot docker image:

```
make run IMAGE_NAME=<username>/slack-lambdabot:<tag>
```

Deploying
---------

The image can be deployed anywhere the docker image can be run, e.g.
AWS ECS, a Kubernetes platform, etc.
