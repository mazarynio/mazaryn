defmodule Core.Account.Business do
  @moduledoc """
  Embedded schema to represent Account.Business
  """
  use Ecto.Schema

  import Ecto.Changeset
  @optional_fields ~w(
    id
    ai_business_id
    ads_id
    company_size
    website
    business_phone
    tax_id
    verification_status
    business_type
    founding_date
    location
    operating_hours
    products_services
    social_media_links
    business_description
    logo_url
    banner_url
    followers_count
    following_count
    posts
    posts_count
    average_engagement_rate
    customer_support_email
    payment_info
    subscription_tier
    account_managers
    business_goals
    date_created
    date_updated
    brand_colors
    brand_voice_keywords
    certifications
    awards
    partnerships
    sustainability_initiatives
    corporate_values
    employee_count
    annual_revenue
    target_audience
    content_calendar
    crm_integration
    loyalty_program
    user_generated_content
    virtual_events
    augmented_reality_experiences
    chatbot_enabled
    blockchain_initiatives
    data_privacy_certifications
    ai_powered_features_enabled
    social_responsibility_score
    innovation_index
    customer_satisfaction_index
    crisis_management_plan
    digital_asset_portfolio
    metaverse_presence
    carbon_footprint_data
    diversity_inclusion_metrics
    talent_acquisition_channels
    knowledge_base_url
    api_integrations
    virtual_reality_spaces
    voice_interaction_profile
    biometric_authentication_methods
    iot_integration_points
    quantum_resistant_security_measures
    neurofeedback_enabled_services
    emotional_intelligence_score
    cultural_adaptation_strategies
    language_localization_status
    continual_learning_initiatives
    cross_platform_presence
    ethical_ai_commitment
    blockchain_integration
    ai_ethical_guidelines
    digital_transformation_score
    circular_economy_initiatives
    global_expansion_roadmap
    intellectual_property_portfolio
    regulatory_compliance_status
    supply_chain_transparency
    customer_data_management_practices
    digital_asset_management
    quantum_computing_readiness
    augmented_workforce_metrics
    ecosystem_partnerships
    cyber_resilience_score
    decentralized_autonomous_organization_status
    data
  )a

  @required_fields ~w(
    user_id
    company_name
    industry
    business_email
  )a

  embedded_schema do
    field(:ai_business_id, :string)
    field(:ads_id, :string)
    field(:company_size, :string)
    field(:website, :string)
    field(:business_phone, :string)
    field(:tex_id, :string)
    field(:verification_status, :string)
    field(:business_type, :string)
    field(:founding_date, :date)
    field(:location, :string)
    field(:operating_hours, :map)
    field(:products_services, {:array, :string})
    field(:social_media_links, {:array, :string})
    field(:business_description, :string)
    field(:logo_url, :string)
    field(:banner_url, :string)
    field(:followers_count, :integer)
    field(:following_count, :integer)
    field(:posts, {:array, :map})
    field(:posts_count, :integer)
    field(:average_engagement_rate, :float)
    field(:customer_support_email, :string)
    field(:payment_info, :map)
    field(:subscription_tier, :string)
    field(:account_managers, {:array, :string})
    field(:business_golas, :map)
    field(:data_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:brand_colors, {:array, :string})
    field(:brand_voice_keywords, {:array, :string})
    field(:certifications, {:array, :string})
    field(:awards, {:array, :string})
    field(:partnerships, {:array, :string})
    field(:sustainability_initiatives, :map)
    field(:corporate_values, :map)
    field(:employee_count, :integer)
    field(:annual_revenue, :integer)
    field(:target_audience, {:array, :string})
    field(:content_calendar, :map)
    field(:crm_integration, :map)
    field(:loyalty_program, :map)
    field(:user_generated_content, :map)
    field(:virtual_events, {:array, :map})
    field(:augmented_reality_experiences, :map)
    field(:chatbot_enabled, :boolean)
    field(:blockchain_initiatives, :map)
    field(:data_privacy_certifications, {:array, :string})
    field(:ai_powered_features_enabled, :boolean)
    field(:social_responsibility_score, :float)
    field(:innovation_index, :float)
    field(:customer_satisfaction_index, :float)
    field(:crisis_management_plan, :map)
    field(:digital_asset_portfolio, {:array, :map})
    field(:metaverse_presence, :map)
    field(:carbon_footprint_data, :map)
    field(:diversity_inclusion_metrics, :map)
    field(:talent_acquisition_channels, {:array, :string})
    field(:knowledge_base_url, :string)
    field(:api_integrations, {:array, :map})
    field(:virtual_reality_spaces, :map)
    field(:voice_interaction_profile, :map)
    field(:biometric_authentication_methods, {:array, :string})
    field(:iot_integration_points, {:array, :map})
    field(:quantum_resistant_security_measures, :map)
    field(:neurofeedback_enabled_services, :map)
    field(:emotional_intelligence_score, :float)
    field(:cultural_adaptation_strategies, :map)
    field(:language_localization_status, :map)
    field(:continual_learning_initiatives, :map)
    field(:cross_platform_presence, :map)
    field(:ethical_ai_commitment, :map)
    field(:blockchain_integration, :map)
    field(:ai_ethical_guidelines, :map)
    field(:digital_transformation_score, :float)
    field(:circular_economy_initiatives, :map)
    field(:global_expansion_roadmap, :map)
    field(:intellectual_property_portfolio, :map)
    field(:regulatory_compliance_status, :map)
    field(:supply_chain_transparency, :map)
    field(:customer_data_management_practices, :map)
    field(:digital_asset_management, :map)
    field(:quantum_computing_readiness, :map)
    field(:augmented_workforce_metrics, :map)
    field(:ecosystem_partnerships, {:array, :map})
    field(:cyber_resilience_score, :float)
    field(:decentralized_autonomous_organization_status, :map)
    field(:data, :map)
    field(:user_id, :integer)
    field(:company_name, :string)
    field(:industry, :string)
    field(:business_email, :string)
  end

  def erl_changeset(
        {:business, id, ai_business_id, ads_id, company_size, website, business_phone, tax_id,
         verification_status, business_type, founding_date, location, operating_hours,
         products_services, social_media_links, business_description, logo_url, banner_url,
         followers_count, following_count, posts, posts_count, average_engagement_rate,
         customer_support_email, payment_info, subscription_tier, account_managers,
         business_golas, data_created, date_updated, brand_colors, brand_voice_keywords,
         certifications, awards, partnerships, sustainability_initiatives, corporate_values,
         employee_count, annual_revenue, target_audience, content_calendar, crm_integration,
         loyalty_program, user_generated_content, virtual_events, augmented_reality_experiences,
         chatbot_enabled, blockchain_initiatives, data_privacy_certifications,
         ai_powered_features_enabled, social_responsibility_score, innovation_index,
         customer_satisfaction_index, crisis_management_plan, digital_asset_portfolio,
         metaverse_presence, carbon_footprint_data, diversity_inclusion_metrics,
         talent_acquisition_channels, knowledge_base_url, api_integrations,
         virtual_reality_spaces, voice_interaction_profile, biometric_authentication_methods,
         iot_integration_points, quantum_resistant_security_measures,
         neurofeedback_enabled_services, emotional_intelligence_score,
         cultural_adaptation_strategies, language_localization_status,
         continual_learning_initiatives, cross_platform_presence, ethical_ai_commitment,
         blockchain_integration, ai_ethical_guidelines, digital_transformation_score,
         circular_economy_initiatives, global_expansion_roadmap, intellectual_property_portfolio,
         regulatory_compliance_status, supply_chain_transparency,
         customer_data_management_practices, digital_asset_management,
         quantum_computing_readiness, augmented_workforce_metrics, ecosystem_partnerships,
         cyber_resilience_score, decentralized_autonomous_organization_status, data, user_id,
         company_name, industry, business_email}
      ) do
    %__MODULE__{}
    |> change(%{
      id: id,
      ai_business_id: ai_business_id,
      ads_id: ads_id,
      company_size: company_size,
      website: website,
      business_phone: business_phone,
      tax_id: tax_id,
      verification_status: verification_status,
      business_type: business_type,
      founding_date: founding_date,
      location: location,
      operating_hours: operating_hours,
      products_services: products_services,
      social_media_links: social_media_links,
      business_description: business_description,
      logo_url: logo_url,
      banner_url: banner_url,
      followers_count: followers_count,
      following_count: following_count,
      posts: posts,
      posts_count: posts_count,
      average_engagement_rate: average_engagement_rate,
      customer_support_email: customer_support_email,
      payment_info: payment_info,
      subscription_tier: subscription_tier,
      account_managers: account_managers,
      business_golas: business_golas,
      data_created: data_created,
      date_updated: date_updated,
      brand_colors: brand_colors,
      brand_voice_keywords: brand_voice_keywords,
      certifications: certifications,
      awards: awards,
      partnerships: partnerships,
      sustainability_initiatives: sustainability_initiatives,
      corporate_values: corporate_values,
      employee_count: employee_count,
      annual_revenue: annual_revenue,
      target_audience: target_audience,
      content_calendar: content_calendar,
      crm_integration: crm_integration,
      loyalty_program: loyalty_program,
      user_generated_content: user_generated_content,
      virtual_events: virtual_events,
      augmented_reality_experiences: augmented_reality_experiences,
      chatbot_enabled: chatbot_enabled,
      blockchain_initiatives: blockchain_initiatives,
      data_privacy_certifications: data_privacy_certifications,
      ai_powered_features_enabled: ai_powered_features_enabled,
      social_responsibility_score: social_responsibility_score,
      innovation_index: innovation_index,
      customer_satisfaction_index: customer_satisfaction_index,
      crisis_management_plan: crisis_management_plan,
      digital_asset_portfolio: digital_asset_portfolio,
      metaverse_presence: metaverse_presence,
      carbon_footprint_data: carbon_footprint_data,
      diversity_inclusion_metrics: diversity_inclusion_metrics,
      talent_acquisition_channels: talent_acquisition_channels,
      knowledge_base_url: knowledge_base_url,
      api_integrations: api_integrations,
      virtual_reality_spaces: virtual_reality_spaces,
      voice_interaction_profile: voice_interaction_profile,
      biometric_authentication_methods: biometric_authentication_methods,
      iot_integration_points: iot_integration_points,
      quantum_resistant_security_measures: quantum_resistant_security_measures,
      neurofeedback_enabled_services: neurofeedback_enabled_services,
      emotional_intelligence_score: emotional_intelligence_score,
      cultural_adaptation_strategies: cultural_adaptation_strategies,
      language_localization_status: language_localization_status,
      continual_learning_initiatives: continual_learning_initiatives,
      cross_platform_presence: cross_platform_presence,
      ethical_ai_commitment: ethical_ai_commitment,
      blockchain_integration: blockchain_integration,
      ai_ethical_guidelines: ai_ethical_guidelines,
      digital_transformation_score: digital_transformation_score,
      circular_economy_initiatives: circular_economy_initiatives,
      global_expansion_roadmap: global_expansion_roadmap,
      intellectual_property_portfolio: intellectual_property_portfolio,
      regulatory_compliance_status: regulatory_compliance_status,
      supply_chain_transparency: supply_chain_transparency,
      customer_data_management_practices: customer_data_management_practices,
      digital_asset_management: digital_asset_management,
      quantum_computing_readiness: quantum_computing_readiness,
      augmented_workforce_metrics: augmented_workforce_metrics,
      ecosystem_partnerships: ecosystem_partnerships,
      cyber_resilience_score: cyber_resilience_score,
      decentralized_autonomous_organization_status: decentralized_autonomous_organization_status,
      data: data,
      user_id: user_id,
      company_name: company_name,
      industry: industry,
      business_email: business_email
    })
  end

  def erl_changeset(_), do: %{}

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def update_changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, [:id, :content])
    |> validate_required([:id, :content])
  end

  def build(map) when map == %{}, do: %{}

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
