# this should be overridden with full docker image name
IMAGE_NAME ?= slack-lambdabot:local
LB_CLI_IMAGE ?= ezoerner/lambdabot-cli:latest

all: build

setup:
	docker pull $(LB_CLI_IMAGE)

build: setup
	docker build -t=$(IMAGE_NAME) .

run: .check-token build
	docker run --env SLACK_API_TOKEN=$(API_TOKEN) -t --rm $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)

.check-token:
ifndef API_TOKEN
	$(error API_TOKEN is undefined)
endif
