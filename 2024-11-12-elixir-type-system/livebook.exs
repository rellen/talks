# livebook.exs
Mix.install([
  {:livebook, "~> 0.11.0"},
  # Optional, but recommended for additional features
  {:kino, "~> 0.11.0"},
  {:vega_lite, "~> 0.1.8"},
  # Optional - for better markdown support
  {:earmark, "~> 1.4"},
  # Optional - for file system operations
  {:file_system, "~> 0.2.10"}
])

# Configuration options
port = System.get_env("LIVEBOOK_PORT", "8080") |> String.to_integer()
password = System.get_env("LIVEBOOK_PASSWORD", "")
home_dir = System.get_env("LIVEBOOK_HOME", File.cwd!())

# Start LiveBook
Application.put_env(:livebook, :home, home_dir)
Application.put_env(:livebook, :port, port)

unless password == "" do
  Application.put_env(:livebook, :password, password)
end

# Start the server
Livebook.application()
|> Enum.each(&Application.ensure_all_started/1)

# Keep the script running
Process.sleep(:infinity)
