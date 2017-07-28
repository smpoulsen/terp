defmodule Terp.Mixfile do
  use Mix.Project

  def project do
    [
      app: :terp,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
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
      {:combine, "~> 0.9.6"},
      {:rose_tree, "~> 0.2.0"}
    ]
  end
end
