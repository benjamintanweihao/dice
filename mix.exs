defmodule Dice.Mixfile do
  use Mix.Project

  def project do
    [app: :dice,
     version: "0.0.1",
     elixir: "~> 0.13.0",
     deps: deps]
  end

  def application do
    [ applications: [],
      mod: {Dice, []} ]
  end

  defp deps do
    [
      {:amnesia, github: "benjamintanweihao/amnesia"},
      {:exzmq, git: "https://github.com/benjamintanweihao/exzmq"},
      {:json, github: "cblage/elixir-json"}
    ]
  end
end
