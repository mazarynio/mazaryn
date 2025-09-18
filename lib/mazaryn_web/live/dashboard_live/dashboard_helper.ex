defmodule MazarynWeb.DashboardLive.DashboardHelper do

  def calculate_engagement_rate(likes, comments, views) when views > 0 do
    ((likes + comments) / views * 100) |> Float.round(2)
  end

  def calculate_engagement_rate(_, _, _), do: 0.0

  def get_level_badge_color(level) do
    cond do
      level >= 50 -> "bg-gradient-to-r from-purple-500 to-pink-500"
      level >= 40 -> "bg-gradient-to-r from-indigo-500 to-purple-500"
      level >= 30 -> "bg-gradient-to-r from-blue-500 to-indigo-500"
      level >= 20 -> "bg-gradient-to-r from-green-500 to-blue-500"
      level >= 10 -> "bg-gradient-to-r from-yellow-500 to-green-500"
      level >= 5 -> "bg-gradient-to-r from-orange-500 to-yellow-500"
      true -> "bg-gradient-to-r from-gray-500 to-gray-600"
    end
  end

  def get_next_level_xp(current_level) do
    base_xp = 1000
    growth_factor = 1.5

    (base_xp * :math.pow(growth_factor, current_level)) |> round()
  end

  def format_large_number(number) when number >= 1_000_000 do
    "#{Float.round(number / 1_000_000, 1)}M"
  end

  def format_large_number(number) when number >= 1_000 do
    "#{Float.round(number / 1_000, 1)}K"
  end

  def format_large_number(number), do: to_string(number)

  def calculate_growth_percentage(current, previous) when previous > 0 do
    growth = (current - previous) / previous * 100
    cond do
      growth > 0 -> "+#{Float.round(growth, 1)}%"
      growth < 0 -> "#{Float.round(growth, 1)}%"
      true -> "0%"
    end
  end

  def calculate_growth_percentage(_, _), do: "0%"

  def get_achievement_for_level(level) do
    cond do
      level >= 100 -> %{name: "Legend", color: "purple", icon: "crown"}
      level >= 75 -> %{name: "Master", color: "indigo", icon: "academic-cap"}
      level >= 50 -> %{name: "Expert", color: "blue", icon: "star"}
      level >= 25 -> %{name: "Advanced", color: "green", icon: "fire"}
      level >= 10 -> %{name: "Regular", color: "yellow", icon: "lightning-bolt"}
      level >= 5 -> %{name: "Active", color: "orange", icon: "trending-up"}
      true -> %{name: "Newbie", color: "gray", icon: "user"}
    end
  end

  def get_weekly_stats(user_id) do
    %{
      posts_this_week: 3,
      likes_this_week: 45,
      comments_this_week: 12,
      followers_this_week: 8,
      engagement_rate: 15.6
    }
  end

  def get_trending_hashtags do
    [
      %{tag: "#tech", posts: 1205},
      %{tag: "#ai", posts: 987},
      %{tag: "#social", posts: 756},
      %{tag: "#innovation", posts: 543},
      %{tag: "#mazaryn", posts: 432}
    ]
  end
end
