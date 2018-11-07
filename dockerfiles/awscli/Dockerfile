FROM alpine:latest
LABEL maintainer "Gary Fenstamaker <gary.fenstamaker@washpost.com>"

RUN apk --no-cache add \
	ca-certificates \
	groff \
	less \
	python \
	py2-pip \
	&& pip install awscli \
	&& mkdir -p /root/.aws

ENTRYPOINT [ "aws" ]