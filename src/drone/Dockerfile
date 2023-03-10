FROM erlang:alpine

RUN apk add bash gcc g++
RUN mkdir /drone
WORKDIR /drone

RUN mkdir /config
RUN mkdir /src

COPY config/* config/
COPY src/* src/
COPY rebar.lock .
COPY rebar.config .
COPY LICENSE .
COPY README.md .

RUN rebar3 compile
RUN rebar3 release

ENV RETRY_LIMIT=2

ENV DRONE_HUB_PROCESS=drone_hub
ENV DRONE_HUB_HOST=drone_hub@drone_hub_host

WORKDIR /drone/_build/default/rel/drone/bin
ENTRYPOINT ["./drone", "console","-noshell"]