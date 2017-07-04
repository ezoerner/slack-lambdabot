# this should be overridden with full docker image name, e.g. ezoerner/slack-lambdabot:1.0
IMAGE_NAME ?= lambdabot-cli

all: setup build

setup:
	docker pull fpco/stack-build:lts-8.20

build:
ifdef LBOT_IMAGE
	docker build -t=$(IMAGE_NAME) --build-arg LBOT_IMAGE=$(LBOT_IMAGE) .
else
	docker build -t=$(IMAGE_NAME) .
endif

run:
	docker run --name lambdabot-cli -it $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)

