defmodule ProcessExample do
  def start() do
    pid = spawn(&pong/0)
    spawn(fn -> ping(3, pid) end)
  end

  def ping(n, pid) when n > 0 do
    send(pid, {:ping, self()})

    receive do
      :pong -> IO.puts("Ping received pong")
    end

    ping(n - 1, pid)
  end

  def ping(0, pid) do
    send(pid, :finished)
    IO.puts("Ping finished")
  end

  def pong() do
    receive do
      :finished ->
        IO.puts("Pong finished")

      {:ping, pid} ->
        send(pid, :pong)
        IO.puts("Pong received ping")
        pong()
    end
  end
end
