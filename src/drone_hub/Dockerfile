FROM erlang:alpine

RUN apk add bash gcc g++ docker-cli
RUN mkdir /drone_hub
WORKDIR /drone_hub

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

ENV VELOCITY=5.0
ENV DRONE_SIZE=1.0
ENV FLY_HEIGHT=50.0
ENV NOTIFY_THRESHOLD=4

WORKDIR /drone_hub/_build/default/rel/drone_hub/bin
ENTRYPOINT ["./drone_hub", "console"]