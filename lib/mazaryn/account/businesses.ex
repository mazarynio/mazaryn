defmodule Account.Businesses do
  alias Core.BusinessClient
  require Logger

  def create_business(user_id, business_data) do
    case BusinessClient.create_business(user_id, business_data) do
      {:ok, business_id} ->
        {:ok, business_id}

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from BusinessClient.create_business: #{inspect(other)}")
        {:error, :creation_failed}
    end
  end

  def get_business_by_id(business_id) do
    case BusinessClient.get_business_by_id(business_id) do
      business when is_tuple(business) and tuple_size(business) > 0 ->
        {:ok, business_record_to_map(business)}

      business when is_map(business) ->
        {:ok, map_to_business_map(business)}

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from get_business_by_id: #{inspect(other)}")
        {:error, :not_found}
    end
  end

  def get_businesses_by_user_id(user_id) do
    case BusinessClient.get_businesses_by_user_id(user_id) do
      businesses when is_list(businesses) ->
        Enum.map(businesses, fn
          business when is_tuple(business) -> business_record_to_map(business)
          business when is_map(business) -> map_to_business_map(business)
          _ -> empty_business_map()
        end)

      {:error, reason} ->
        Logger.error("Error getting businesses: #{inspect(reason)}")
        []

      [] ->
        []
    end
  end

  def update_business(business_id, update_data) do
    case BusinessClient.update_business(business_id, update_data) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from update_business: #{inspect(other)}")
        {:error, :update_failed}
    end
  end

  def delete_business(business_id) do
    case BusinessClient.delete_business(business_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from delete_business: #{inspect(other)}")
        {:error, :deletion_failed}
    end
  end

  def verify_business(business_id) do
    case BusinessClient.verify_business(business_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from verify_business: #{inspect(other)}")
        {:error, :verification_failed}
    end
  end

  def unverify_business(business_id) do
    case BusinessClient.unverify_business(business_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from unverify_business: #{inspect(other)}")
        {:error, :unverification_failed}
    end
  end

  def add_team_member(business_id, user_id, role \\ "member") do
    case BusinessClient.add_team_member(business_id, user_id, role) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from add_team_member: #{inspect(other)}")
        {:error, :add_member_failed}
    end
  end

  def remove_team_member(business_id, user_id) do
    case BusinessClient.remove_team_member(business_id, user_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from remove_team_member: #{inspect(other)}")
        {:error, :remove_member_failed}
    end
  end

  def update_team_member_role(business_id, user_id, new_role) do
    case BusinessClient.update_team_member_role(business_id, user_id, new_role) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from update_team_member_role: #{inspect(other)}")
        {:error, :update_role_failed}
    end
  end

  def add_pending_invitation(business_id, user_id, role) do
    case BusinessClient.add_pending_invitation(business_id, user_id, role) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from add_pending_invitation: #{inspect(other)}")
        {:error, :invitation_failed}
    end
  end

  def remove_pending_invitation(business_id, user_id) do
    case BusinessClient.remove_pending_invitation(business_id, user_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from remove_pending_invitation: #{inspect(other)}")
        {:error, :remove_invitation_failed}
    end
  end

  def accept_invitation(business_id, user_id) do
    case BusinessClient.accept_invitation(business_id, user_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from accept_invitation: #{inspect(other)}")
        {:error, :accept_invitation_failed}
    end
  end

  def get_team_members(business_id) do
    case BusinessClient.get_team_members(business_id) do
      members when is_list(members) ->
        Enum.map(members, fn
          {user_id, role, permissions} when is_tuple({user_id, role, permissions}) ->
            %{
              user_id: to_string_safe(user_id),
              role: to_string_safe(role),
              permissions: permissions
            }

          _ ->
            %{user_id: "", role: "", permissions: []}
        end)

      {:error, reason} ->
        Logger.error("Error getting team members: #{inspect(reason)}")
        []

      [] ->
        []
    end
  end

  def add_post(business_id, post_id) do
    case BusinessClient.add_post(business_id, post_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from add_post: #{inspect(other)}")
        {:error, :add_post_failed}
    end
  end

  def remove_post(business_id, post_id) do
    case BusinessClient.remove_post(business_id, post_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from remove_post: #{inspect(other)}")
        {:error, :remove_post_failed}
    end
  end

  def get_business_posts(business_id) do
    case BusinessClient.get_business_posts(business_id) do
      posts when is_list(posts) ->
        Enum.map(posts, &to_string_safe/1)

      {:error, reason} ->
        Logger.error("Error getting business posts: #{inspect(reason)}")
        []

      [] ->
        []
    end
  end

  def follow_business(user_id, business_id) do
    case BusinessClient.follow_business(user_id, business_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from follow_business: #{inspect(other)}")
        {:error, :follow_failed}
    end
  end

  def unfollow_business(user_id, business_id) do
    case BusinessClient.unfollow_business(user_id, business_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from unfollow_business: #{inspect(other)}")
        {:error, :unfollow_failed}
    end
  end

  def is_following_business?(user_id, business_id) do
    case BusinessClient.is_following_business(user_id, business_id) do
      true -> true
      false -> false
      _ -> false
    end
  end

  def search_businesses(company_name) do
    case BusinessClient.search_businesses(company_name) do
      businesses when is_list(businesses) ->
        Enum.map(businesses, fn
          business when is_tuple(business) -> business_record_to_map(business)
          business when is_map(business) -> map_to_business_map(business)
          _ -> empty_business_map()
        end)

      {:error, reason} ->
        Logger.error("Error searching businesses: #{inspect(reason)}")
        []

      [] ->
        []
    end
  end

  def search_businesses_by_industry(industry) do
    case BusinessClient.search_businesses_by_industry(industry) do
      businesses when is_list(businesses) ->
        Enum.map(businesses, fn
          business when is_tuple(business) -> business_record_to_map(business)
          business when is_map(business) -> map_to_business_map(business)
          _ -> empty_business_map()
        end)

      {:error, reason} ->
        Logger.error("Error searching businesses by industry: #{inspect(reason)}")
        []

      [] ->
        []
    end
  end

  def get_analytics(business_id) do
    case BusinessClient.get_analytics(business_id) do
      analytics when is_map(analytics) ->
        analytics

      {:error, reason} ->
        Logger.error("Error getting analytics: #{inspect(reason)}")
        %{}

      _ ->
        %{}
    end
  end

  def record_page_view(business_id, visitor_data) do
    case BusinessClient.record_page_view(business_id, visitor_data) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from record_page_view: #{inspect(other)}")
        {:error, :record_failed}
    end
  end

  def update_analytics(business_id, analytics_data) do
    case BusinessClient.update_analytics(business_id, analytics_data) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from update_analytics: #{inspect(other)}")
        {:error, :update_failed}
    end
  end

  def get_business_job_postings(business_id) do
    case BusinessClient.get_business_job_postings(business_id) do
      jobs when is_list(jobs) ->
        Enum.map(jobs, &job_to_map/1)

      {:error, reason} ->
        Logger.error("Error getting job postings: #{inspect(reason)}")
        []

      _ ->
        []
    end
  end

  def create_job_posting(business_id, job_data) do
    job_id = generate_id()

    case BusinessClient.add_job_posting(business_id, job_id) do
      :ok ->
        {:ok, job_id}

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from create_job_posting: #{inspect(other)}")
        {:error, :creation_failed}
    end
  end

  def update_job_posting(job_id, update_data) do
    :ok
  end

  def remove_job_posting(business_id, job_id) do
    case BusinessClient.remove_job_posting(business_id, job_id) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}

      other ->
        Logger.error("Unexpected result from remove_job_posting: #{inspect(other)}")
        {:error, :removal_failed}
    end
  end

  defp job_to_map(job) when is_map(job), do: job

  defp job_to_map(job_id) when is_binary(job_id) or is_list(job_id) do
    %{
      id: to_string_safe(job_id),
      title: "Job Posting",
      description: "Job description",
      location: "Location",
      employment_type: "Full-time",
      salary_range: nil,
      requirements: "",
      responsibilities: "",
      benefits: "",
      application_deadline: nil,
      status: "active"
    }
  end

  defp job_to_map(_), do: %{}

  defp generate_id do
    :crypto.strong_rand_bytes(16)
    |> Base.url_encode64(padding: false)
    |> binary_part(0, 16)
  end

  defp business_record_to_map(business) when is_tuple(business) and tuple_size(business) >= 14 do
    location = elem(business, 13)

    location_map =
      cond do
        is_map(location) ->
          %{
            address: Map.get(location, :address, "") |> to_string_safe(),
            city: Map.get(location, :city, "") |> to_string_safe(),
            state: Map.get(location, :state, "") |> to_string_safe(),
            country: Map.get(location, :country, "") |> to_string_safe(),
            postal_code: Map.get(location, :postal_code, "") |> to_string_safe(),
            geo_coordinates: Map.get(location, :geo_coordinates, {0.0, 0.0}),
            timezone: Map.get(location, :timezone, "") |> to_string_safe()
          }

        true ->
          %{
            address: "",
            city: "",
            state: "",
            country: "",
            postal_code: "",
            geo_coordinates: {0.0, 0.0},
            timezone: ""
          }
      end

    user_id_binary = elem(business, 2) |> to_string_safe()

    %{
      id: elem(business, 1) |> to_string_safe(),
      user_id: user_id_binary,
      company_name: elem(business, 3) |> safe_to_string(),
      legal_name: elem(business, 4) |> safe_to_string(),
      industry: elem(business, 5) |> safe_to_string(),
      sub_industry: elem(business, 6) |> safe_to_string(),
      company_size: elem(business, 7) |> safe_to_string(),
      website: elem(business, 8) |> safe_to_string(),
      business_email: elem(business, 9) |> safe_to_string(),
      business_phone: elem(business, 10) |> safe_to_string(),
      business_type: elem(business, 11) |> safe_to_string(),
      business_description: elem(business, 12) |> safe_to_string(),
      location: location_map
    }
  end

  defp business_record_to_map(business) when is_tuple(business) do
    Logger.warning("Business record has unexpected size: #{tuple_size(business)}")
    empty_business_map()
  end

  defp map_to_business_map(business) when is_map(business) do
    location = Map.get(business, :location, Map.get(business, "location", %{}))

    location_map =
      cond do
        is_map(location) ->
          %{
            address:
              Map.get(location, :address, Map.get(location, "address", "")) |> to_string_safe(),
            city: Map.get(location, :city, Map.get(location, "city", "")) |> to_string_safe(),
            state: Map.get(location, :state, Map.get(location, "state", "")) |> to_string_safe(),
            country:
              Map.get(location, :country, Map.get(location, "country", "")) |> to_string_safe(),
            postal_code:
              Map.get(location, :postal_code, Map.get(location, "postal_code", ""))
              |> to_string_safe(),
            geo_coordinates:
              Map.get(
                location,
                :geo_coordinates,
                Map.get(location, "geo_coordinates", {0.0, 0.0})
              ),
            timezone:
              Map.get(location, :timezone, Map.get(location, "timezone", "")) |> to_string_safe()
          }

        true ->
          %{
            address: "",
            city: "",
            state: "",
            country: "",
            postal_code: "",
            geo_coordinates: {0.0, 0.0},
            timezone: ""
          }
      end

    %{
      id: Map.get(business, :id, Map.get(business, "id", "")) |> to_string_safe(),
      user_id: Map.get(business, :user_id, Map.get(business, "user_id", "")) |> to_string_safe(),
      company_name:
        Map.get(business, :company_name, Map.get(business, "company_name", ""))
        |> to_string_safe(),
      legal_name:
        Map.get(business, :legal_name, Map.get(business, "legal_name", "")) |> to_string_safe(),
      industry:
        Map.get(business, :industry, Map.get(business, "industry", "")) |> to_string_safe(),
      sub_industry:
        Map.get(business, :sub_industry, Map.get(business, "sub_industry", ""))
        |> to_string_safe(),
      company_size:
        Map.get(business, :company_size, Map.get(business, "company_size", ""))
        |> to_string_safe(),
      website: Map.get(business, :website, Map.get(business, "website", "")) |> to_string_safe(),
      business_email:
        Map.get(business, :business_email, Map.get(business, "business_email", ""))
        |> to_string_safe(),
      business_phone:
        Map.get(business, :business_phone, Map.get(business, "business_phone", ""))
        |> to_string_safe(),
      business_type:
        Map.get(business, :business_type, Map.get(business, "business_type", ""))
        |> to_string_safe(),
      business_description:
        Map.get(business, :business_description, Map.get(business, "business_description", ""))
        |> to_string_safe(),
      location: location_map
    }
  end

  defp empty_business_map do
    %{
      id: "",
      user_id: "",
      company_name: "",
      legal_name: "",
      industry: "",
      sub_industry: "",
      company_size: "",
      website: "",
      business_email: "",
      business_phone: "",
      business_type: "",
      business_description: "",
      location: %{
        address: "",
        city: "",
        state: "",
        country: "",
        postal_code: "",
        geo_coordinates: {0.0, 0.0},
        timezone: ""
      }
    }
  end

  defp safe_to_string(value) when is_list(value) do
    try do
      List.to_string(value)
    rescue
      _ -> ""
    end
  end

  defp safe_to_string(value) when is_binary(value), do: value
  defp safe_to_string(value) when is_atom(value), do: Atom.to_string(value)
  defp safe_to_string(value) when is_integer(value), do: Integer.to_string(value)
  defp safe_to_string(_), do: ""

  defp to_string_safe(value) when is_list(value) do
    try do
      List.to_string(value)
    rescue
      _ -> ""
    end
  end

  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(value) when is_atom(value), do: Atom.to_string(value)
  defp to_string_safe(value) when is_integer(value), do: Integer.to_string(value)
  defp to_string_safe(_), do: ""
end
