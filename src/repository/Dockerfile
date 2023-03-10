FROM erlang:alpine

RUN apk add bash
RUN mkdir /repository
WORKDIR /repository

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

ENV DB_PROCESS=db_listener

WORKDIR /repository/_build/default/rel/repository/bin
ENTRYPOINT ["./repository", "console"]