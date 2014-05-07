defmodule Dice do
  use Application.Behaviour

  def start(_type, _args) do
    Dice.Supervisor.start_link
  end
end
