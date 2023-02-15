FROM erlang:latest
RUN mkdir /src
RUN mkdir /build
COPY db_initializer.erl db_supervisor.erl db_listener.erl records.hrl /src/
RUN erlc -o /build /src/db_listener.erl /src/db_supervisor.erl /src/db_initializer.erl
WORKDIR /build
#ENTRYPOINT ["erl", "-sname", "db", "-setcookie", "dis_sys", "-run", "db_supervisor", "start", "-noshell"]
#ENTRYPOINT ["erl", "-name", "db@127.0.0.1", "-setcookie", "dis_sys", "-run", "db_supervisor", "start"]
ENTRYPOINT ["erl", "-sname", "db", "-setcookie", "dis_sys", "-run", "db_supervisor", "start"]
