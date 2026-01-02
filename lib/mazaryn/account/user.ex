defmodule Account.User do
  @moduledoc """
  Embedded schema to represent Account.User
  """
  use Ecto.Schema

  import Ecto.Changeset

  require Logger

  @optional_fields ~w(
    id
    p2p_node_address
    ipfs_key
    ai_user_id
    business_id
    username
    email
    password
    address
    knode
    private
    other_info
    media
    posts
    blog_post
    following
    follower
    blocked
    saved_posts
    reposts
    notif
    country
    avatar_url
    banner_url
    chat
    verified
    verification_token
    report
    level
    last_activity
    suspend
    datasets
    competitions
    data
    groups
  )a

  @required_attrs [
    :username,
    :email,
    :password
  ]

  embedded_schema do
    field(:form_disabled, :boolean)
    field(:p2p_node_address, :string)
    field(:ipfs_key, :string)
    field(:ai_user_id, :string)
    field(:business_id, {:array, :string}, default: [])
    field(:ads_id, {:array, :string}, default: [])
    field(:quantum_id, :string)
    field(:username, :string)
    field(:email, :string)
    field(:password, :string, virtual: true)
    field(:address, :string)
    field(:knode, {:array, :string}, default: [])
    field(:private, :boolean)
    field(:other_info, :map)
    field(:media, {:array, :string}, default: [])
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)

    field(:posts, {:array, :string}, default: [])
    field(:blog_post, {:array, :string}, default: [])
    field(:following, {:array, :string}, default: [])
    field(:follower, {:array, :string}, default: [])
    field(:blocked, {:array, :string}, default: [])
    field(:saved_posts, {:array, :string}, default: [])
    field(:reposts, {:array, :string}, default: [])

    field(:notif, {:array, :string}, default: [])

    field(:avatar_url, :string, default: "/images/default-user.svg")
    field(:banner_url, :string, default: "")
    field(:token_id, :string)
    field(:country, :string)
    field(:chat, {:array, :string}, default: [])
    field(:verified, :boolean, default: false)
    field(:verification_token, :string)
    field(:report, {:array, :string}, default: [])
    field(:level, :integer)
    field(:last_activity, :utc_datetime)
    field(:suspend, {:array, :string}, default: [])
    field(:datasets, {:array, :string}, default: [])
    field(:competitions, {:array, :string}, default: [])
    field(:groups, {:array, :string}, default: [])
    field(:data, :map)
  end

  def erl_changeset(
        {:user, id, p2p_node_address, ipfs_key, ai_user_id, business_id, ads_id, quantum_id,
         username, password, email, address, knode, media, posts, blog_post, notif, following,
         follower, blocked, saved_posts, reposts, other_info, private, date_created, date_updated,
         avatar_url, banner_url, token_id, chat, verified, report, level, last_activity, suspend,
         datasets, competitions, groups, data} = _user
      ) do
    avatar_url =
      case avatar_url do
        :undefined -> nil
        value -> value
      end

    banner_url =
      case banner_url do
        :undefined -> nil
        value -> value
      end

    token_id =
      case token_id do
        :undefined -> nil
        value -> value
      end

    verified =
      case verified do
        :undefined -> false
        value -> value
      end

    %__MODULE__{}
    |> change(%{
      id: id,
      p2p_node_address: p2p_node_address,
      ipfs_key: ipfs_key,
      ai_user_id: ai_user_id,
      business_id: business_id,
      ads_id: ads_id,
      quantum_id: quantum_id,
      username: username,
      password: password,
      email: email,
      address: address,
      knode: knode,
      media: media,
      posts: posts,
      blog_post: blog_post,
      following: following,
      follower: follower,
      blocked: blocked,
      saved_posts: saved_posts,
      reposts: reposts,
      other_info: Enum.into(other_info, %{}),
      private: private,
      date_created: date_created,
      date_updated: date_updated,
      avatar_url: avatar_url,
      banner_url: banner_url,
      token_id: token_id,
      notif: notif,
      chat: chat,
      verified: verified,
      report: report,
      level: level,
      last_activity: last_activity,
      suspend: suspend,
      datasets: datasets,
      competitions: competitions,
      groups: groups,
      data: data
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("User operation timed out")
    raise ArgumentError, "User operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("User operation failed with reason: #{reason}")
    raise ArgumentError, "User operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("User operation failed: #{inspect(reason)}")
    raise ArgumentError, "User operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("User operation returned nil")
    raise ArgumentError, "User not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid user data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:user, id, p2p_node_address, ipfs_key, ai_user_id, business_id, ads_id, quantum_id,
         username, password, email, address, knode, media, posts, blog_post, notif, following,
         follower, blocked, saved_posts, reposts, other_info, private, date_created, date_updated,
         avatar_url, banner_url, token_id, chat, verified, report, level, last_activity, suspend,
         datasets, competitions, groups, data} = _user
      ) do
    try do
      changeset =
        erl_changeset(
          {:user, id, p2p_node_address, ipfs_key, ai_user_id, business_id, ads_id, quantum_id,
           username, password, email, address, knode, media, posts, blog_post, notif, following,
           follower, blocked, saved_posts, reposts, other_info, private, date_created,
           date_updated, avatar_url, banner_url, token_id, chat, verified, report, level,
           last_activity, suspend, datasets, competitions, groups, data}
        )

      {:ok, changeset}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("User operation timed out")
    {:error, "User operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("User operation failed: #{inspect(reason)}")
    {:error, "User operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("User operation returned nil")
    {:error, "User not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid user data format"}
  end

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password,
      min: 8,
      max: 60,
      message: "Password must be between 8 and 20 characters"
    )
    |> create_password_hash()
  end

  def create_password_hash(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_password_hash(%Ecto.Changeset{} = changeset) do
    password_hash =
      changeset
      |> Ecto.Changeset.get_field(:password)
      |> :erlpass.hash()

    changeset
    |> Ecto.Changeset.put_change(:password, password_hash)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def generate_verification_token do
    :crypto.strong_rand_bytes(32)
    |> Base.url_encode64(padding: false)
  end

  def has_reposted?(%__MODULE__{reposts: reposts}, post_id) do
    post_id in reposts
  end

  def repost_count(%__MODULE__{reposts: reposts}) do
    length(reposts)
  end

  def add_repost(%__MODULE__{reposts: reposts} = user, post_id) do
    %{user | reposts: [post_id | reposts]}
  end

  def remove_repost(%__MODULE__{reposts: reposts} = user, post_id) do
    %{user | reposts: List.delete(reposts, post_id)}
  end
end
