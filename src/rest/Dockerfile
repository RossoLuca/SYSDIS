FROM ubuntu:latest

RUN apt-get update
RUN apt-get install erlang git gcc g++ make wget curl bash rebar3 -y

RUN mkdir /rest
WORKDIR /rest

RUN wget https://erlang.mk/erlang.mk
RUN make -f erlang.mk bootstrap bootstrap-rel

RUN make

COPY /config/* /rest/config/
COPY /src/* /rest/src/
COPY rest.d /rest/
COPY Makefile /rest/
COPY relx.config /rest/

RUN rm -r _rel/
RUN rm -r deps/
RUN rm -r ebin/

WORKDIR /rest
RUN make

WORKDIR /rest/_rel/rest_release/bin/

EXPOSE 8080
ENTRYPOINT ["./rest_release-1", "console"]