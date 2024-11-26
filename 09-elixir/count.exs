defmodule WordLetterCounter do
  def run do
    IO.puts("Do you want to provide input from console or file? (type 'console' or 'file')")
    input_type = IO.gets("") |> String.trim()
    case input_type do
      "console" ->
        handle_console_input()
      "file" ->
        handle_file_input()
      _ ->
        IO.puts("Invalid input. Please type 'console' or 'file'.")
    end
  end
  defp handle_console_input do
    IO.puts("Please enter your text:")
    text = IO.gets("") |> String.trim()
    process_text(text, :console)
  end
  defp handle_file_input do
    IO.puts("Please enter the file path:")
    file_path = IO.gets("") |> String.trim()
    case File.read(file_path) do
      {:ok, content} ->
        process_text(content, :file)
      {:error, reason} ->
        IO.puts("Failed to read the file: #{reason}")
    end
  end
  defp process_text(text, output_type) do
    letters = count_letters(text)
    words = count_words(text)
    case output_type do
      :console ->
        IO.puts("Letter occurrences:")
        letters |> Enum.sort_by(fn {_k, v} -> -v end) |> Enum.each(&IO.puts("#{elem(&1, 0)} #{elem(&1, 1)}"))
        IO.puts("Word occurrences:")
        words |> Enum.sort_by(fn {_k, v} -> -v end) |> Enum.each(&IO.puts("#{elem(&1, 0)} #{elem(&1, 1)}"))
      :file ->
        write_to_file("letters.txt", letters)
        write_to_file("words.txt", words)
    end
  end
  defp count_letters(text) do
    text
    |> String.downcase()
    |> String.graphemes()
    |> Enum.filter(&(&1 =~ ~r/[a-z]/))
    |> Enum.frequencies()
  end
  defp count_words(text) do
    text
    |> String.downcase()
    |> String.split(~r/\s+/)
    |> Enum.frequencies()
  end
  defp write_to_file(filename, counts) do
    File.write!(filename, Enum.map_join(counts, "\n", fn {k, v} -> "#{k} #{v}" end))
    IO.puts("Results written to #{filename}")
  end
end
WordLetterCounter.run()
