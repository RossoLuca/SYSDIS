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

#ENTRYPOINT ["/bin/bash"]

WORKDIR /drone_hub/_build/default/rel/drone_hub/bin
ENTRYPOINT ["./drone_hub", "console"]
# ENTRYPOINT ["/bin/bash"]