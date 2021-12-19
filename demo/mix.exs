defmodule Demo.MixProject do
  use Mix.Project

  def compilers() do
    compilers = Mix.compilers()
    elixir_index = Enum.find_index(compilers, fn it -> it == :elixir end)
    List.insert_at(compilers, elixir_index, :agex)
  end

  def project do
    [
      app: :demo,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers() ++ [:agex],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
