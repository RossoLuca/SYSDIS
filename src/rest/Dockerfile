FROM bitwalker/alpine-erlang:latest

RUN apk add erlang git gcc g++ make wget curl

RUN mkdir /rest
WORKDIR /rest

RUN wget https://erlang.mk/erlang.mk
RUN make -f erlang.mk bootstrap bootstrap-rel

RUN make

COPY /config/* /rest/config/
COPY /src/* /rest/src/

WORKDIR /rest
RUN make

WORKDIR /rest/_rel/rest_release/bin/

EXPOSE 8080
ENTRYPOINT ["./rest_release-1", "console"]