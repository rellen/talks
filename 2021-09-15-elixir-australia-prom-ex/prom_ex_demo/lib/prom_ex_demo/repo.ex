defmodule PromExDemo.Repo do
  use Ecto.Repo,
    otp_app: :prom_ex_demo,
    adapter: Ecto.Adapters.Postgres
end
