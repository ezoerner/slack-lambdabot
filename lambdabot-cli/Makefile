IMAGE_NAME ?= lambdabot-cli:local

all: setup build

setup:
	docker pull fpco/stack-build:lts-8.20

build:
	docker build -t=$(IMAGE_NAME) .

run:
	docker run --name lambdabot-cli -it $(IMAGE_NAME)

publish:
	docker push $(IMAGE_NAME)

