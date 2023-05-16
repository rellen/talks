defmodule ProcessExampleTrap do
  def start() do
    IO.inspect(self(), label: "Shell PID is")
    Process.flag(:trap_exit, true)
    pid = spawn(&pong/0)
    spawn_link(fn -> ping(3, pid) end)

    receive do
      msg ->
        IO.inspect(msg, label: "Got message")
    end
  end

  def ping(n, pid) when n > 0 do
    send(pid, {:ping, self()})

    receive do
      :pong -> IO.puts("Ping received pong")
    end

    ping(n - 1, pid)
  end

  def ping(0, _pid) do
    apply(:foo, :bar, [])
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
