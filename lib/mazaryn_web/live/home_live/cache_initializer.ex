defmodule MazarynWeb.HomeLive.CacheInitializer do
  @moduledoc """
  Initializes ETS tables and cache systems for the application.

  This module should be called during application startup to ensure
  all required ETS tables are properly created.
  """

  require Logger

  @doc """
  Initializes all required ETS tables for the application.
  """
  def init_all_tables() do
    tables = [
      {:post_content_cache, [:named_table, :public, :set]},
      {:ipns_failures, [:named_table, :public, :set]},
      {:post_cache, [:named_table, :public, :set]}
    ]

    Enum.each(tables, fn {table_name, options} ->
      init_table(table_name, options)
    end)

    Logger.info("All ETS cache tables initialized successfully")
  end

  @doc """
  Initializes a specific ETS table if it doesn't exist.
  """
  def init_table(table_name, options \\ [:named_table, :public, :set]) do
    case :ets.info(table_name) do
      :undefined ->
        :ets.new(table_name, options)
        Logger.info("Initialized ETS table: #{table_name}")
        :ok
      _ ->
        Logger.debug("ETS table #{table_name} already exists")
        :ok
    end
  rescue
    e ->
      Logger.error("Failed to initialize ETS table #{table_name}: #{inspect(e)}")
      :error
  end

  @doc """
  Safely deletes from an ETS table, handling the case where table doesn't exist.
  """
  def safe_ets_delete(table_name, key) do
    case :ets.info(table_name) do
      :undefined ->
        Logger.debug("Attempted to delete from non-existent table #{table_name}")
        :ok
      _ ->
        try do
          :ets.delete(table_name, key)
          :ok
        rescue
          e ->
            Logger.error("Failed to delete from #{table_name}: #{inspect(e)}")
            :error
        end
    end
  end

  @doc """
  Safely inserts into an ETS table, initializing it if needed.
  """
  def safe_ets_insert(table_name, data, options \\ [:named_table, :public, :set]) do
    init_table(table_name, options)

    try do
      :ets.insert(table_name, data)
      :ok
    rescue
      e ->
        Logger.error("Failed to insert into #{table_name}: #{inspect(e)}")
        :error
    end
  end

  @doc """
  Safely looks up from an ETS table, handling the case where table doesn't exist.
  """
  def safe_ets_lookup(table_name, key) do
    case :ets.info(table_name) do
      :undefined ->
        Logger.debug("Attempted to lookup from non-existent table #{table_name}")
        []
      _ ->
        try do
          :ets.lookup(table_name, key)
        rescue
          e ->
            Logger.error("Failed to lookup from #{table_name}: #{inspect(e)}")
            []
        end
    end
  end
end
