
all: setup build

setup:
	docker pull fpco/stack-build:lts-8.20

build:
ifdef LBOT_IMAGE
	docker build -t=ezoerner/lambdabot-cli:latest --build-arg LBOT_IMAGE=$(LBOT_IMAGE) .
else
	docker build -t=ezoerner/lambdabot-cli:latest .
endif

run:
	docker run --name lambdabot-cli -it ezoerner/lambdabot-cli:latest

publish:
	docker push ezoerner/lambdabot-cli:latest

