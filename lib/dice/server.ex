defmodule Dice.Server do
  use GenServer.Behaviour
  use Amnesia

  defrecordp :dice_cache, key: nil, value: nil

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def stop do
    :gen_server.cast(__MODULE__, :stop)
  end

  def put(key, value) do
    # NOTE: I think this should be a cast instead
    :gen_server.call(__MODULE__, {:put, key, value})
  end

  def get(key) do
    :gen_server.call(__MODULE__, {:get, key})
  end

  def remove(key) do
    # NOTE: I think this should be a cast instead
    :gen_server.call(__MODULE__, {:remove, key})
  end

  #############
  # Callbacks #
  #############

  def init(_) do
    Amnesia.Table.wait([:dice_cache])
    {:ok, []}
  end

  def terminate(_reason, _state) do
    Amnesia.stop
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_call({:put, key, value}, _from, state) do
    record = dice_cache(key: key, value: value)
    {:atomic, result } = Amnesia.transaction do
                           case DiceCache.read(key) do
                             [] ->
                               record.write
                               :ok
                             [dice_cache(key: key, value: value)] ->
                               record.write
                               value
                           end
                         end
    {:reply, result, state}
  end

  def handle_call({:get, key}, _from, state) do
    case DiceCache.read!(key) do
      [dice_cache(key: key, value: value)] ->
        {:reply, value, state}
      _ ->
        {:reply, nil, state}
    end
  end

  def handle_call({:remove, key}, _from, state) do
    {:atomic, result} = Amnesia.transaction do
                          case DiceCache.read(key) do
                            [] -> 
                              :ok
                            [dice_cache(key: key, value: value)] ->
                              DiceCache.delete(key)
                              value
                          end
                        end
    {:reply, result, state}
  end

end
