FROM haskell:8 AS builder

RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir /log

WORKDIR /duckling

ADD stack.yaml duckling.cabal ./

ENV LANG=C.UTF-8

RUN stack setup

ADD . .

# NOTE:`stack build` will use as many cores as are available to build
# in parallel. However, this can cause OOM issues as the linking step
# in GHC can be expensive. If the build fails, try specifying the
# '-j1' flag to force the build to run sequentially.
RUN stack install

ARG image
FROM $image

ENV LANG C.UTF-8

# fix for "No URLs in mirrorlist"
RUN if [ "$image" == "centos:8" ]; then; \
    sed -i 's/mirrorlist/#mirrorlist/g' /etc/yum.repos.d/CentOS-Linux-* && \
    sed -i 's|#baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/CentOS-Linux-*; fi

RUN yum -y update && \
    yum -y install pcre pcre-devel gmp tzdata && \
    yum -y clean all && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    cd /usr/lib64 && ln libpcrecpp.so.0.0.1 libpcre.so.3 && chmod 777 libpcre.so.3

COPY --from=builder /root/.local/bin/duckling-example-exe /usr/local/bin/

ARG GID
ARG UID
ARG username
ARG group
ARG exposed_port=9250

RUN groupadd -g $GID $group && \
    useradd -r -u $UID -g $group -m $username && \
    chown -R $UID:$GID "/home/$username"
USER $UID

WORKDIR "/home/$username"
RUN mkdir log && cd log && touch access.log && touch error.log

# build a shell script because the ENTRYPOINT command doesn't like using ENV or ARG
RUN printf "#!/bin/sh \n \
    duckling-example-exe  \
    -p $exposed_port  \
    --access-log=/home/$username/log/access.log  \
    --error-log=/home/$username/log/error.log" >> /home/$username/entrypoint.sh && \
    chmod +x ./entrypoint.sh

EXPOSE $exposed_port
ENTRYPOINT ["./entrypoint.sh"]
