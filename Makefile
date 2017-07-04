# this should be overridden with full docker image name, e.g. ezoerner/slack-lambdabot:1.0
IMAGE_NAME ?= slack-lambdabot:local

all: .check-token setup build

setup:
	docker pull fpco/stack-build:lts-8.20

build: .check-token
	docker build -t=$(IMAGE_NAME) --build-arg api_token=$(API_TOKEN) .

run:
	docker run -t $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)

.check-token:
ifndef API_TOKEN
	$(error API_TOKEN is undefined)
endif
