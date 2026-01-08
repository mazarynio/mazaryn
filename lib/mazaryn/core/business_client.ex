defmodule Core.BusinessClient do
  require Logger

  def create_business(user_id, business_data) do
    Logger.debug("BusinessClient.create_business - user_id: #{inspect(user_id)}")

    try do
      business_map = %{
        company_name:
          to_charlist_safe(business_data[:company_name] || business_data["company_name"] || ""),
        legal_name:
          to_charlist_safe(business_data[:legal_name] || business_data["legal_name"] || ""),
        industry: to_charlist_safe(business_data[:industry] || business_data["industry"] || ""),
        sub_industry:
          to_charlist_safe(business_data[:sub_industry] || business_data["sub_industry"] || ""),
        company_size:
          to_charlist_safe(
            business_data[:company_size] || business_data["company_size"] || "1-10"
          ),
        website: to_charlist_safe(business_data[:website] || business_data["website"] || ""),
        business_email:
          to_charlist_safe(
            business_data[:business_email] || business_data["business_email"] || ""
          ),
        business_phone:
          to_charlist_safe(
            business_data[:business_phone] || business_data["business_phone"] || ""
          ),
        business_type:
          to_charlist_safe(
            business_data[:business_type] || business_data["business_type"] || "B2C"
          ),
        business_description:
          to_charlist_safe(
            business_data[:business_description] || business_data["business_description"] || ""
          ),
        location: convert_location(business_data[:location] || business_data["location"])
      }

      result = :businessdb.create_business_concurrent(to_charlist_safe(user_id), business_map)

      case result do
        {:ok, business_id} when is_list(business_id) ->
          {:ok, List.to_string(business_id)}

        {:ok, business_id} when is_binary(business_id) ->
          {:ok, business_id}

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      error ->
        Logger.error("Exception in create_business: #{inspect(error)}")
        Logger.error("Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :exception}
    end
  end

  def get_business_by_id(business_id) do
    Logger.debug("BusinessClient.get_business_by_id - id: #{inspect(business_id)}")

    try do
      result = :businessdb.get_business_by_id(to_charlist_safe(business_id))

      case result do
        business when is_tuple(business) and tuple_size(business) > 0 ->
          business

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result from get_business_by_id: #{inspect(other)}")
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("Exception in get_business_by_id: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_businesses_by_user_id(user_id) do
    Logger.debug("BusinessClient.get_businesses_by_user_id - user_id: #{inspect(user_id)}")

    try do
      result = :businessdb.get_businesses_by_user_id(to_charlist_safe(user_id))

      case result do
        businesses when is_list(businesses) -> businesses
        {:error, :no_businesses_found} -> []
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_businesses_by_user_id: #{inspect(error)}")
        []
    end
  end

  def update_business(business_id, update_data) do
    Logger.debug("BusinessClient.update_business - id: #{inspect(business_id)}")

    try do
      erlang_map = convert_update_map(update_data)
      :businessdb.update_business(to_charlist_safe(business_id), erlang_map)
    rescue
      error ->
        Logger.error("Exception in update_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def delete_business(business_id) do
    Logger.debug("BusinessClient.delete_business - id: #{inspect(business_id)}")

    try do
      :businessdb.delete_business(to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in delete_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def verify_business(business_id) do
    Logger.debug("BusinessClient.verify_business - id: #{inspect(business_id)}")

    try do
      :businessdb.verify_business(to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in verify_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def unverify_business(business_id) do
    Logger.debug("BusinessClient.unverify_business - id: #{inspect(business_id)}")

    try do
      :businessdb.unverify_business(to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in unverify_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_team_member(business_id, user_id, role) do
    Logger.debug(
      "BusinessClient.add_team_member - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}"
    )

    try do
      :businessdb.add_team_member(
        to_charlist_safe(business_id),
        to_charlist_safe(user_id),
        to_charlist_safe(role)
      )
    rescue
      error ->
        Logger.error("Exception in add_team_member: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_team_member(business_id, user_id) do
    Logger.debug(
      "BusinessClient.remove_team_member - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}"
    )

    try do
      :businessdb.remove_team_member(to_charlist_safe(business_id), to_charlist_safe(user_id))
    rescue
      error ->
        Logger.error("Exception in remove_team_member: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def update_team_member_role(business_id, user_id, new_role) do
    Logger.debug(
      "BusinessClient.update_team_member_role - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}, new_role: #{inspect(new_role)}"
    )

    try do
      :businessdb.update_team_member_role(
        to_charlist_safe(business_id),
        to_charlist_safe(user_id),
        to_charlist_safe(new_role)
      )
    rescue
      error ->
        Logger.error("Exception in update_team_member_role: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_pending_invitation(business_id, user_id, role) do
    Logger.debug(
      "BusinessClient.add_pending_invitation - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}, role: #{inspect(role)}"
    )

    try do
      :businessdb.add_pending_invitation(
        to_charlist_safe(business_id),
        to_charlist_safe(user_id),
        to_charlist_safe(role)
      )
    rescue
      error ->
        Logger.error("Exception in add_pending_invitation: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_pending_invitation(business_id, user_id) do
    Logger.debug(
      "BusinessClient.remove_pending_invitation - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}"
    )

    try do
      :businessdb.remove_pending_invitation(
        to_charlist_safe(business_id),
        to_charlist_safe(user_id)
      )
    rescue
      error ->
        Logger.error("Exception in remove_pending_invitation: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def accept_invitation(business_id, user_id) do
    Logger.debug(
      "BusinessClient.accept_invitation - business_id: #{inspect(business_id)}, user_id: #{inspect(user_id)}"
    )

    try do
      :businessdb.accept_invitation(
        to_charlist_safe(business_id),
        to_charlist_safe(user_id)
      )
    rescue
      error ->
        Logger.error("Exception in accept_invitation: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_team_members(business_id) do
    Logger.debug("BusinessClient.get_team_members - business_id: #{inspect(business_id)}")

    try do
      result = :businessdb.get_team_members(to_charlist_safe(business_id))

      case result do
        members when is_list(members) -> members
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_team_members: #{inspect(error)}")
        []
    end
  end

  def add_post(business_id, post_id) do
    Logger.debug(
      "BusinessClient.add_post - business_id: #{inspect(business_id)}, post_id: #{inspect(post_id)}"
    )

    try do
      :businessdb.add_post(to_charlist_safe(business_id), to_charlist_safe(post_id))
    rescue
      error ->
        Logger.error("Exception in add_post: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_post(business_id, post_id) do
    Logger.debug(
      "BusinessClient.remove_post - business_id: #{inspect(business_id)}, post_id: #{inspect(post_id)}"
    )

    try do
      :businessdb.remove_post(to_charlist_safe(business_id), to_charlist_safe(post_id))
    rescue
      error ->
        Logger.error("Exception in remove_post: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_business_posts(business_id) do
    Logger.debug("BusinessClient.get_business_posts - business_id: #{inspect(business_id)}")

    try do
      result = :businessdb.get_business_posts(to_charlist_safe(business_id))

      case result do
        posts when is_list(posts) -> posts
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_business_posts: #{inspect(error)}")
        []
    end
  end

  def follow_business(user_id, business_id) do
    Logger.debug(
      "BusinessClient.follow_business - user_id: #{inspect(user_id)}, business_id: #{inspect(business_id)}"
    )

    try do
      :businessdb.follow_business(to_charlist_safe(user_id), to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in follow_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def unfollow_business(user_id, business_id) do
    Logger.debug(
      "BusinessClient.unfollow_business - user_id: #{inspect(user_id)}, business_id: #{inspect(business_id)}"
    )

    try do
      :businessdb.unfollow_business(to_charlist_safe(user_id), to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in unfollow_business: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def is_following_business(user_id, business_id) do
    Logger.debug(
      "BusinessClient.is_following_business - user_id: #{inspect(user_id)}, business_id: #{inspect(business_id)}"
    )

    try do
      :businessdb.is_following_business(to_charlist_safe(user_id), to_charlist_safe(business_id))
    rescue
      error ->
        Logger.error("Exception in is_following_business: #{inspect(error)}")
        false
    end
  end

  def search_businesses(company_name) do
    Logger.debug("BusinessClient.search_businesses - company_name: #{inspect(company_name)}")

    try do
      result = :businessdb.search_businesses(to_charlist_safe(company_name))

      case result do
        businesses when is_list(businesses) -> businesses
        {:error, :business_not_found} -> []
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in search_businesses: #{inspect(error)}")
        []
    end
  end

  def search_businesses_by_industry(industry) do
    Logger.debug("BusinessClient.search_businesses_by_industry - industry: #{inspect(industry)}")

    try do
      result = :businessdb.search_businesses_by_industry(to_charlist_safe(industry))

      case result do
        businesses when is_list(businesses) -> businesses
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in search_businesses_by_industry: #{inspect(error)}")
        []
    end
  end

  def get_analytics(business_id) do
    Logger.debug("BusinessClient.get_analytics - business_id: #{inspect(business_id)}")

    try do
      result = :businessdb.get_analytics(to_charlist_safe(business_id))

      case result do
        analytics when is_map(analytics) ->
          analytics

        {:error, reason} ->
          {:error, reason}

        _ ->
          %{}
      end
    rescue
      error ->
        Logger.error("Exception in get_analytics: #{inspect(error)}")
        %{}
    end
  end

  def update_analytics(business_id, analytics_data) do
    Logger.debug("BusinessClient.update_analytics - business_id: #{inspect(business_id)}")

    try do
      erlang_data = convert_update_map(analytics_data)
      :businessdb.update_analytics(to_charlist_safe(business_id), erlang_data)
    rescue
      error ->
        Logger.error("Exception in update_analytics: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def record_page_view(business_id, visitor_data) do
    Logger.debug("BusinessClient.record_page_view - business_id: #{inspect(business_id)}")

    try do
      erlang_data = convert_visitor_data(visitor_data)
      :businessdb.record_page_view(to_charlist_safe(business_id), erlang_data)
    rescue
      error ->
        Logger.error("Exception in record_page_view: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_business_job_postings(business_id) do
    Logger.debug(
      "BusinessClient.get_business_job_postings - business_id: #{inspect(business_id)}"
    )

    try do
      result = :businessdb.get_business_job_postings(to_charlist_safe(business_id))

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_business_job_postings: #{inspect(error)}")
        []
    end
  end

  def add_job_posting(business_id, job_id) do
    Logger.debug(
      "BusinessClient.add_job_posting - business_id: #{inspect(business_id)}, job_id: #{inspect(job_id)}"
    )

    try do
      :businessdb.add_job_posting(to_charlist_safe(business_id), to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in add_job_posting: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_job_posting(business_id, job_id) do
    Logger.debug(
      "BusinessClient.remove_job_posting - business_id: #{inspect(business_id)}, job_id: #{inspect(job_id)}"
    )

    try do
      :businessdb.remove_job_posting(to_charlist_safe(business_id), to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in remove_job_posting: #{inspect(error)}")
        {:error, :exception}
    end
  end

  defp convert_location(nil) do
    %{
      address: ~c"",
      city: ~c"",
      state: ~c"",
      country: ~c"",
      postal_code: ~c"",
      geo_coordinates: {0.0, 0.0},
      timezone: ~c""
    }
  end

  defp convert_location(location) when is_map(location) do
    %{
      address: to_charlist_safe(location[:address] || location["address"] || ""),
      city: to_charlist_safe(location[:city] || location["city"] || ""),
      state: to_charlist_safe(location[:state] || location["state"] || ""),
      country: to_charlist_safe(location[:country] || location["country"] || ""),
      postal_code: to_charlist_safe(location[:postal_code] || location["postal_code"] || ""),
      geo_coordinates: location[:geo_coordinates] || location["geo_coordinates"] || {0.0, 0.0},
      timezone: to_charlist_safe(location[:timezone] || location["timezone"] || "")
    }
  end

  defp convert_update_map(data) when is_map(data) do
    data
    |> Enum.map(fn {key, value} ->
      {key, convert_value(value)}
    end)
    |> Map.new()
  end

  defp convert_value(value) when is_binary(value), do: to_charlist_safe(value)

  defp convert_value(value) when is_list(value) and length(value) > 0 do
    case hd(value) do
      v when is_binary(v) -> Enum.map(value, &to_charlist_safe/1)
      _ -> value
    end
  end

  defp convert_value(value) when is_list(value), do: value
  defp convert_value(value) when is_map(value), do: convert_update_map(value)
  defp convert_value(value), do: value

  defp convert_visitor_data(data) when is_map(data) do
    %{
      timestamp: Map.get(data, :timestamp, DateTime.utc_now()),
      user_id: to_charlist_safe(Map.get(data, :user_id, "")),
      ip: to_charlist_safe(Map.get(data, :ip, "unknown"))
    }
  end

  defp to_charlist_safe(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist_safe(value) when is_list(value), do: value
  defp to_charlist_safe(value) when is_atom(value), do: Atom.to_charlist(value)
  defp to_charlist_safe(value) when is_integer(value), do: Integer.to_charlist(value)
  defp to_charlist_safe(value), do: to_charlist_safe(inspect(value))
end
