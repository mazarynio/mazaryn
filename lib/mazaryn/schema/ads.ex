defmodule Mazaryn.Schema.Ads do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Ads
  """

use Ecto.Schema

import Ecto.Changeset

@optional_fields ~w(
  id
  ai_ads_id
  business_account_id
  campaign_id
  date_created
  date_updated
  target_audience
  budget
  schedule
  placement
  performance_metrics
  a_b_testing
  dynamic_content
  interactive_elements
  augmented_reality_features
  shoppable_features
  compliance_check
  eco_friendly_badge =
  localization
  accessibility_features
  chatbot_integration
  user_generated_content
  social_proof_elements
  sequential_storytelling_id
  context_aware_adaptation
  voice_assistant_integration
  blockchain_verification
  neuroscience_tested
  virtual_reality_components
  voice_activated_elements
  biometric_response_tracking
  iot_integration
  blockchain_verified_metrics
  quantum_resistant_encryption
  neurofeedback_optimized
  emotional_ai_integration
  cultural_sensitivity_score
  language_localization
  continuous_learning_adaptation
  cross_reality_experience
  metaverse_ready
  microinteractions
  privacy_preserving_targeting
  adaptive_streaming_quality
  haptic_feedback_patterns
  scent_marketing_integration
  subconscious_priming_elements
  ambient_advertising_features
  cognitive_bias_leveraging
  sensory_branding_elements
  temporal_marketing_adaptation
  energy_efficient_display
  attention_economy_metrics
  digital_scarcity_elements
  choice_architecture_design
  psychographic_resonance_score
  subliminal_optimization
  data
)a

@required_fields ~w(
  user_id
  content
  ad_type
)a


embedded_schema do
  # Required fields
  field(:user_id, :string)
  field(:content, :string)
  field(:ad_type, :string)

  # Optional fields
  field(:ai_ads_id, :string)
  field(:business_account_id, :string)
  field(:campaign_id, :string)
  field(:date_created, :naive_datetime)
  field(:date_updated, :naive_datetime)
  field(:target_audience, :map)
  field(:budget, :decimal)
  field(:schedule, :map)
  field(:placement, :string)
  field(:performance_metrics, :map)
  field(:a_b_testing, :map)
  field(:dynamic_content, :map)
  field(:interactive_elements, :map)
  field(:augmented_reality_features, :map)
  field(:shoppable_features, :boolean)
  field(:compliance_check, :boolean)
  field(:eco_friendly_badge, :boolean)
  field(:localization, :map)
  field(:accessibility_features, :map)
  field(:chatbot_integration, :boolean)
  field(:user_generated_content, :map)
  field(:social_proof_elements, :map)
  field(:sequential_storytelling_id, :string)
  field(:context_aware_adaptation, :map)
  field(:voice_assistant_integration, :boolean)
  field(:blockchain_verification, :boolean)
  field(:neuroscience_tested, :boolean)
  field(:virtual_reality_components, :map)
  field(:voice_activated_elements, :map)
  field(:biometric_response_tracking, :map)
  field(:iot_integration, :map)
  field(:blockchain_verified_metrics, :map)
  field(:quantum_resistant_encryption, :boolean)
  field(:neurofeedback_optimized, :boolean)
  field(:emotional_ai_integration, :map)
  field(:cultural_sensitivity_score, :integer)
  field(:language_localization, :map)
  field(:continuous_learning_adaptation, :map)
  field(:cross_reality_experience, :map)
  field(:metaverse_ready, :boolean)
  field(:microinteractions, :map)
  field(:privacy_preserving_targeting, :boolean)
  field(:adaptive_streaming_quality, :map)
  field(:haptic_feedback_patterns, :map)
  field(:scent_marketing_integration, :boolean)
  field(:subconscious_priming_elements, :map)
  field(:ambient_advertising_features, :map)
  field(:cognitive_bias_leveraging, :map)
  field(:sensory_branding_elements, :map)
  field(:temporal_marketing_adaptation, :map)
  field(:energy_efficient_display, :boolean)
  field(:attention_economy_metrics, :map)
  field(:digital_scarcity_elements, :map)
  field(:choice_architecture_design, :map)
  field(:psychographic_resonance_score, :integer)
  field(:subliminal_optimization, :boolean)
  field(:data, :map)
end

  def erl_changeset(
        {:ads, id, ai_ads_id, business_account_id, campaign_id, date_created, date_updated, target_audience,
        budget, schedule, placement, performance_metrics, a_b_testing, dynamic_content,
        interactive_elements, augmented_reality_features, shoppable_features, compliance_check,
        eco_friendly_badge, localization, accessibility_features, chatbot_integration,
        user_generated_content, social_proof_elements, sequential_storytelling_id, context_aware_adaptation,
        voice_assistant_integration, blockchain_verification, neuroscience_tested, virtual_reality_components,
        voice_activated_elements, biometric_response_tracking, iot_integration, blockchain_verified_metrics,
        quantum_resistant_encryption, neurofeedback_optimized, emotional_ai_integration, cultural_sensitivity_score,
        language_localization, continuous_learning_adaptation, cross_reality_experience, metaverse_ready,
        microinteractions, privacy_preserving_targeting, adaptive_streaming_quality, haptic_feedback_patterns,
        scent_marketing_integration, subconscious_priming_elements, ambient_advertising_features,
        cognitive_bias_leveraging, sensory_branding_elements, temporal_marketing_adaptation,
        energy_efficient_display, attention_economy_metrics, digital_scarcity_elements, choice_architecture_design,
        psychographic_resonance_score, subliminal_optimization, data, user_id, content, ad_type}) do
        %__MODULE__{}
        |> change(%{
        id: id,
        ai_ads_id: ai_ads_id,
        business_account_id: business_account_id,
        campaign_id: campaign_id,
        date_created: date_created,
        date_updated: date_updated,
        target_audience: target_audience,
        budget: budget,
        schedule: schedule,
        placement: placement,
        performance_metrics: performance_metrics,
        a_b_testing: a_b_testing,
        dynamic_content: dynamic_content,
        interactive_elements: interactive_elements,
        augmented_reality_features: augmented_reality_features,
        shoppable_features: shoppable_features,
        compliance_check: compliance_check,
        eco_friendly_badge: eco_friendly_badge,
        localization: localization,
        accessibility_features: accessibility_features,
        chatbot_integration: chatbot_integration,
        user_generated_content: user_generated_content,
        social_proof_elements: social_proof_elements,
        sequential_storytelling_id: sequential_storytelling_id,
        context_aware_adaptation: context_aware_adaptation,
        voice_assistant_integration: voice_assistant_integration,
        blockchain_verification: blockchain_verification,
        neuroscience_tested: neuroscience_tested,
        virtual_reality_components: virtual_reality_components,
        voice_activated_elements: voice_activated_elements,
        biometric_response_tracking: biometric_response_tracking,
        iot_integration: iot_integration,
        blockchain_verified_metrics: blockchain_verified_metrics,
        quantum_resistant_encryption: quantum_resistant_encryption,
        neurofeedback_optimized: neurofeedback_optimized,
        emotional_ai_integration: emotional_ai_integration,
        cultural_sensitivity_score: cultural_sensitivity_score,
        language_localization: language_localization,
        continuous_learning_adaptation: continuous_learning_adaptation,
        cross_reality_experience: cross_reality_experience,
        metaverse_ready: metaverse_ready,
        microinteractions: microinteractions,
        privacy_preserving_targeting: privacy_preserving_targeting,
        adaptive_streaming_quality: adaptive_streaming_quality,
        haptic_feedback_patterns: haptic_feedback_patterns,
        scent_marketing_integration: scent_marketing_integration,
        subconscious_priming_elements: subconscious_priming_elements,
        ambient_advertising_features: ambient_advertising_features,
        cognitive_bias_leveraging: cognitive_bias_leveraging,
        sensory_branding_elements: sensory_branding_elements,
        temporal_marketing_adaptation: temporal_marketing_adaptation,
        energy_efficient_display: energy_efficient_display,
        attention_economy_metrics: attention_economy_metrics,
        digital_scarcity_elements: digital_scarcity_elements,
        choice_architecture_design: choice_architecture_design,
        psychographic_resonance_score: psychographic_resonance_score,
        subliminal_optimization: subliminal_optimization,
        data: data,
        user_id: user_id,
        content: content,
        ad_type: ad_type
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
