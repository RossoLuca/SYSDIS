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

#ENTRYPOINT ["/bin/bash"]

WORKDIR /drone/_build/default/rel/drone/bin
ENTRYPOINT ["./drone", "console"]
#ENTRYPOINT ["/bin/bash"]