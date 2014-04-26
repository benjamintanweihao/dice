use Amnesia

defdatabase Dice.Database do

  deftable DiceCache, [:key, :value] do

    def put(key, value) do
      record = DiceCache[key: key, value: value]
      Amnesia.transaction do
        case DiceCache.read(key) do
          DiceCache[key: _key, value: _value] ->
            record.update(value: value).write
            value
          _ ->
            record.write
            value
        end
      end
    end

    def get(key) do
      case DiceCache.read!(key) do
        DiceCache[key: _key, value: value] ->
          value
        _ ->
          nil
      end
    end

    def remove(key) do
      Amnesia.transaction do
        case DiceCache.read(key) do
          DiceCache[key: key, value: value] ->
            DiceCache.delete(key)
            value
          _ -> 
            :ok
        end
      end
    end

  end
end
