# DiCE – Distributed Cache in Elixir

This will be (haven't written anything yet) a more or less faithful port of Andrey Paramonov's [blog post](http://ndpar.blogspot.sg/2010/01/distributed-cache-in-erlang.html) on Distributed cache in Erlang to Elixir.

The main motivation is to learn Mnesia and getting to grips with distribution in Elixir.

This might eventually evolve into a [book](http://www.exotpbook.com) chapter/section on Distribution. And even further goal would be to demonstrate interop with other languages.

## Setting up the Nodes

```
% iex --sname master -S mix
% iex --sname slave1 -S mix
% iex --sname slave2 -S mix
```

One one machine (in this example, `master`):

```elixir
iex(master@benjamintan)1> Node.connect :'slave1@benjamintan'
iex(master@benjamintan)2> Node.connect :'slave2@benjamintan'
```

In `master`:

```elixir
iex(master@benjamintan)6> Node.list
[:slave1@benjamintan, :slave2@benjamintan]
```

In `slave1`:

```elixir
iex(slave1@benjamintan)1> Node.list
[:master@benjamintan, :slave2@benjamintan]
```

In `slave2`:

```elixir
iex(slave2@benjamintan)1> Node.list
[:master@benjamintan, :slave1@benjamintan]
```

## Creating the Schema

```elixir
iex(master@benjamintan)7> Amnesia.Schema.create([:'master@benjamintan', :'slave1@benjamintan', :'slave2@benjamintan'])
:ok
iex(master@benjamintan)8> Amnesia.Schema.create([:'master@benjamintan', :'slave1@benjamintan', :'slave2@benjamintan'])
{:error, {:master@benjamintan, {:already_exists, :master@benjamintan}}}
```

On _all_ nodes:

```elixir
iex> Amnesia.start
```
