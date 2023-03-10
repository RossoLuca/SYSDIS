FROM ubuntu:latest

RUN apt-get update
RUN apt-get install erlang git gcc g++ make wget curl bash rebar3 -y

RUN mkdir /ui_control_service
WORKDIR /ui_control_service

RUN wget https://erlang.mk/erlang.mk
RUN make -f erlang.mk bootstrap bootstrap-rel

RUN make

COPY /config/* /ui_control_service/config/
COPY /src/* /ui_control_service/src/
COPY ui_control_service.d /ui_control_service/
COPY Makefile /ui_control_service/
COPY relx.config /ui_control_service/

RUN rm -r _rel/
RUN rm -r deps/
RUN rm -r ebin/

WORKDIR /ui_control_service
RUN make

ENV REST_ENDPOINT=rest_host
ENV WATCH_DEFAULT_TIMEOUT=5000

WORKDIR /ui_control_service/_rel/ui_control_service_release/bin/

ENTRYPOINT ["./ui_control_service_release-1", "console"]