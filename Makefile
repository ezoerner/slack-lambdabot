lts=6.26
ghc=7.10.3
docker=docker-container
stack_bin_dir=.stack-work/install/x86_64-linux-*/lts-$(lts)/$(ghc)/bin

docker_tag ?= local

all: clean setup build container

clean:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack clean

setup:
	stack docker pull && stack --install-ghc setup

build:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack build

container: build
	cp $(stack_bin_dir)/slack-lambdabot $(docker)
	cd $(docker) && docker build -t dfithian/slack-lambdabot:$(docker_tag) .

publish: container
	docker push dfithian/slack-lambdabot:$(docker_tag)
