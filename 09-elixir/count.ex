defmodule ParallelCounter do
    # Counts elements in a list in parallel
    def count_elements_parallel(list, predicate) when is_list(list) and is_function(predicate, 1) do
      num_chunks = System.schedulers_online()
      chunks = Enum.chunk_every(list, div(length(list), num_chunks) + 1)

      tasks = Enum.map(chunks, fn chunk ->
        Task.async(fn -> count_chunk(chunk, predicate) end)
      end)

      tasks
      |> Enum.map(&Task.await/1)
      |> Enum.sum()
    end

    # Counts elements in a list sequentially
    def count_elements_sequential(list, predicate) when is_list(list) and is_function(predicate, 1) do
      Enum.count(list, predicate)
    end

    # Counts elements in a chunk that satisfy the predicate
    defp count_chunk(chunk, predicate) do
      Enum.count(chunk, predicate)
    end
  end

  # Example usage
  list = Enum.to_list(1..1_000_000)
  predicate = fn x -> rem(x, 2) == 0 end  # Count even numbers

  # Measure time for parallel counting
  {parallel_time, parallel_count} = :timer.tc(fn ->
    ParallelCounter.count_elements_parallel(list, predicate)
  end)

  # Measure time for sequential counting
  {sequential_time, sequential_count} = :timer.tc(fn ->
    ParallelCounter.count_elements_sequential(list, predicate)
  end)

  # Print results
  IO.puts("Parallel Count of even numbers: #{parallel_count}")
  IO.puts("Time taken for parallel counting: #{parallel_time / 1_000_000} seconds")

  IO.puts("Sequential Count of even numbers: #{sequential_count}")
  IO.puts("Time taken for sequential counting: #{sequential_time / 1_000_000} seconds")
