defmodule Dice.Server do
  use GenServer.Behaviour
  use Dice.Database

  alias Dice.Server

  @port 1155

  defstruct role: :slave

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def leader_pid do
    :global.whereis_name(__MODULE__)
  end

  def stop do
    :gen_server.cast(__MODULE__, :stop)
  end

  def put(key, value) do
    :gen_server.call(__MODULE__, {:put, key, value})
  end

  def get(key) do
    :gen_server.call(__MODULE__, {:get, key})
  end

  def remove(key) do
    :gen_server.call(__MODULE__, {:remove, key})
  end

  #############
  # Callbacks #
  #############

  def init(_) do
    IO.puts "==== INIT ===="
    case :global.register_name(__MODULE__, self) do
      :no ->
        # NOTE: :noproc is sent when the leader goes down. 
        # Has nice takeover properties.
        Process.link(leader_pid)
        {:ok, %Server{role: :slave}}

      :yes ->
        {:ok, %Server{role: :leader}, 0}
    end  
  end

  def terminate(_reason, _state) do
    Amnesia.stop
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_call({:put, key, value}, _from, state) do
    {:reply, Database.DiceCache.put(key, value), state}
  end

  def handle_call({:get, key}, _from, state) do
    {:reply, Database.DiceCache.get(key), state}
  end

  def handle_call({:remove, key}, _from, state) do
    {:reply,  Database.DiceCache.remove(key), state }
  end

  def handle_info(:timeout, %Server{role: :leader} = state) do
    # Manually start ZeroMQ
    Exzmq.App.start(nil, nil)

    {:ok, socket} = Exzmq.start([{:type, :rep}])
    Exzmq.bind(socket, :tcp, @port, [])
    loop(socket)
    {:noreply, state}
  end

  ######################
  # Internal Functions #
  ######################

  defp loop(socket) do
    case Exzmq.recv(socket) do
      {:ok, [msg]} ->
        IO.puts  "Received: #{msg}"
        
        case "#{msg}" |> JSON.decode do
          {:ok, hash} ->

            result = case hash["op"] do
                       "put" ->
                         Dice.Server.put hash["key"], hash["value"]
                       "get" ->
                         Dice.Server.get hash["key"] 
                       "remove" ->
                         Dice.Server.remove hash["key"] 
                       _ ->
                         :error
                      end

            Exzmq.send(socket, [[result: result] |> JSON.encode!])

          _ ->
            Exzmq.send(socket, [[result: []] |> JSON.encode!])
      
        end

      _ ->
        Exzmq.send(socket, [[result: []] |> JSON.encode!])
    end
    loop(socket)
  end

end
