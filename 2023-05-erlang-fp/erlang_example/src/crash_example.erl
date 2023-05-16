-module(crash_example).

-export([start/0, ping/2, pong/0]).

start() ->
    Pid = spawn(crash_example, pong, []),
    spawn(crash_example, ping, [3, Pid]).

ping(0, _Pid) ->
    io:fwrite("Ping finished~n"),
    apply(crash_example, crash, []);

ping(N, Pid) ->
    %% Send a 'ping' message to the process with ID 'Pid'.
    Pid ! {ping, self()},
    receive
        pong ->
            io:fwrite("Ping received pong~n")
    end,
    ping(N - 1, Pid).

pong() ->
    receive
        finished ->
            io:fwrite("Pong finished~n");
        {ping, Ping_PID} ->
            %% Send a 'pong' message back to the 'ping' process.
            Ping_PID ! pong,
            io:fwrite("Pong received ping~n"),
            pong()
    end.
