IMAGE_NAME ?= lambdabot-cli:local

all: build

setup:
	docker pull fpco/stack-build:lts-8.20

build: setup
	docker build -t=$(IMAGE_NAME) .

run: build
	docker run --name lambdabot-cli -it $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)