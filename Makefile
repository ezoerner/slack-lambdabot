# this should be overridden with full docker image name
IMAGE_NAME ?= slack-lambdabot:local
LB_CLI_IMAGE ?= ezoerner/lambdabot-cli:latest

all: .check-token setup build

setup:
	docker pull $(LB_CLI_IMAGE)

build: .check-token
	docker build -t=$(IMAGE_NAME) --build-arg api_token=$(API_TOKEN) .

run:
	docker run -t --rm $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)

.check-token:
ifndef API_TOKEN
	$(error API_TOKEN is undefined)
endif
