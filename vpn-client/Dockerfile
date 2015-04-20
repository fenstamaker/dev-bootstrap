FROM fedora
MAINTAINER Gary Fenstamaker<gary@fenstamaker.com>

RUN yum -y update && \
    yum -y install openconnect && \
    yum -y install sudo && \
    yum -y install iproute && \
    yum -y install iputils

RUN useradd docker
RUN gpasswd wheel -a docker
RUN echo "docker:docker" | chpasswd

USER docker

CMD bash
