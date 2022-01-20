defmodule Mix.Tasks.Compile.Agex do
  use Mix.Task.Compiler

  def compile(now) do
    File.rm_rf!("lib/agex")

    System.shell("java -jar ../target/scala-3.1.0/agex-assembly-0.1.0.jar agex lib/agex",
      into: IO.stream()
    )

    Mix.Tasks.Format.run(["lib/agex/*"])
    File.write!("_build/agex_manifest", to_string(now))
  end

  @impl true
  def run(args) do
    {last_modified_file, _} =
      System.shell(
        "find agex -type f -print0 | xargs -0 stat --format '%Y :%y %n' | sort -nr | cut -d: -f2- | head -1 | cut -d ' ' -f4"
      )

    {{y, m, d}, {h, min, s}} = File.stat!(String.trim(last_modified_file)).mtime

    {timestamp, _} =
      "#{y}#{m}#{d}#{h}#{min}#{s}"
      |> Integer.parse()
      |> IO.inspect(label: "agex input folder timestamp")

    case File.read("_build/agex_manifest") do
      {:ok, content} ->
        {compiled_timestamp, _} = Integer.parse(content)
        IO.inspect(compiled_timestamp, label: "compiled_timestamp")

        if compiled_timestamp < timestamp do
          IO.puts("Compiling...")
          compile(timestamp)
        else
          IO.puts("No agex files changed")
        end

      _ ->
        IO.puts("No manifest found")
        compile(timestamp)
    end

    :ok
  end

  def manifests() do
    "_build/agex_manifest"
  end

  @impl true
  def clean do
    File.rm_rf!("lib/agex")
    File.rm(manifests())
  end
end
