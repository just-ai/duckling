FROM haskell:8

COPY . duckling

RUN mkdir /log

WORKDIR /duckling

RUN apt-get update

RUN apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends

RUN stack setup
RUN stack install

RUN mv /root/.local/bin/duckling-example-exe /usr/bin/duckling-example-exe
ENTRYPOINT ["/usr/bin/duckling-example-exe"]
