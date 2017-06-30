docker=docker-container
stack_bin_dir=$(shell stack path --local-install-root)/bin

# this should be overridden with full docker image name, e.g. ezoerner/slack-lambdabot:1.0
IMAGE_NAME ?= slack-lambdabot


all: check-token clean setup build container

clean:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack clean

setup:
	stack docker pull && stack --install-ghc setup

build:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack build

container: check-token build
	cp $(stack_bin_dir)/slack-lambdabot $(docker)
	cd $(docker) && docker build -t $(IMAGE_NAME) --build-arg api_token=$(API_TOKEN) .

publish: container
	docker push $(IMAGE_NAME)

check-token:
ifndef API_TOKEN
	$(error API_TOKEN is undefined)
endif
