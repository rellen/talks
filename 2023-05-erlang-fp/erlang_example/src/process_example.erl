-module(process_example).
-export([start/0, ping/2, pong/0]).

start() ->
    Pid = spawn(fun() -> pong() end),
    spawn(process_example, ping, [3, Pid]).

ping(0, Pid) ->
    Pid ! finished,
    io:fwrite("Ping finished~n");

ping(N, Pid) ->
    Pid ! {ping, self()},
    receive
        pong -> io:fwrite("Ping received pong~n")
    end,
    ping(N - 1, Pid).

pong() ->
    receive
        finished -> io:fwrite("Pong finished~n");
        {ping, Ping_PID} ->
            Ping_PID ! pong,
            io:fwrite("Pong received ping~n"),
            pong()
    end.
