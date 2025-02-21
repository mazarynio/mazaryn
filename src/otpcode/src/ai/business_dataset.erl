-module(business_dataset).
-author("Zaryn Technologies").
-export([serialize_business/1, save_to_tfrecord/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_business({business, Id, UserId, AiBusinessId, AdsId, CompanyName, Industry, CompanySize, Website, BusinessEmail, BusinessPhone, TaxId, VerificationStatus, BusinessType, FoundingDate, Location, OperatingHours, ProductsServices, SocialMediaLinks, BusinessDescription, LogoUrl, BannerUrl, FollowersCount, FollowingCount, Posts, PostsCount, AverageEngagementRate, CustomerSupportEmail, PaymentInfo, SubscriptionTier, AccountManagers, BusinessGoals, DateCreated, DateUpdated, BrandColors, BrandVoiceKeywords, Certifications, Awards, Partnerships, SustainabilityInitiatives, CorporateValues, EmployeeCount, AnnualRevenue, TargetAudience, ContentCalendar, CrmIntegration, LoyaltyProgram, UserGeneratedContent, VirtualEvents, AugmentedRealityExperiences, ChatbotEnabled, BlockchainInitiatives, DataPrivacyCertifications, AiPoweredFeaturesEnabled, SocialResponsibilityScore, InnovationIndex, CustomerSatisfactionIndex, CrisisManagementPlan, DigitalAssetPortfolio, MetaversePresence, CarbonFootprintData, DiversityInclusionMetrics, TalentAcquisitionChannels, KnowledgeBaseUrl, ApiIntegrations, VirtualRealitySpaces, VoiceInteractionProfile, BiometricAuthenticationMethods, IotIntegrationPoints, QuantumResistantSecurityMeasures, NeurofeedbackEnabledServices, EmotionalIntelligenceScore, CulturalAdaptationStrategies, LanguageLocalizationStatus, ContinualLearningInitiatives, CrossPlatformPresence, EthicalAiCommitment, BlockchainIntegration, AiEthicalGuidelines, DigitalTransformationScore, CircularEconomyInitiatives, GlobalExpansionRoadmap, IntellectualPropertyPortfolio, RegulatoryComplianceStatus, SupplyChainTransparency, CustomerDataManagementPractices, DigitalAssetManagement, QuantumComputingReadiness, AugmentedWorkforceMetrics, EcosystemPartnerships, CyberResilienceScore, DecentralizedAutonomousOrganizationStatus, Data}) ->

    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

    ToBinary = fun
        (Value) when is_binary(Value) ->
            case is_valid_utf8(Value) of
                true -> Value; 
                false -> base64:encode(Value) 
            end;
        (Value) when is_list(Value) -> list_to_binary(Value); 
        (Value) when is_atom(Value) -> list_to_binary(atom_to_list(Value)); 
        (Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value)); 
        (Value) -> list_to_binary(lists:flatten(io_lib:format("~p", [Value]))) 
    end,

    #{
        <<"id">> => ToBinary(Id),
        <<"user_id">> => ToBinary(UserId),
        <<"ai_business_id">> => ToBinary(AiBusinessId),
        <<"ads_id">> => ToBinary(AdsId),
        <<"company_name">> => ToBinary(CompanyName),
        <<"industry">> => ToBinary(Industry),
        <<"company_size">> => ToBinary(CompanySize),
        <<"website">> => ToBinary(Website),
        <<"business_email">> => ToBinary(BusinessEmail),
        <<"business_phone">> => ToBinary(BusinessPhone),
        <<"tax_id">> => ToBinary(TaxId),
        <<"verification_status">> => ToBinary(VerificationStatus),
        <<"business_type">> => ToBinary(BusinessType),
        <<"founding_date">> => ToBinary(FoundingDate),
        <<"location">> => ToBinary(Location),
        <<"operating_hours">> => ToBinary(OperatingHours),
        <<"products_services">> => ToBinary(ProductsServices),
        <<"social_media_links">> => ToBinary(SocialMediaLinks),
        <<"business_description">> => ToBinary(BusinessDescription),
        <<"logo_url">> => ToBinary(LogoUrl),
        <<"banner_url">> => ToBinary(BannerUrl),
        <<"followers_count">> => ToBinary(FollowersCount),
        <<"following_count">> => ToBinary(FollowingCount),
        <<"posts">> => ToBinary(Posts),
        <<"posts_count">> => ToBinary(PostsCount),
        <<"average_engagement_rate">> => ToBinary(AverageEngagementRate),
        <<"customer_support_email">> => ToBinary(CustomerSupportEmail),
        <<"payment_info">> => ToBinary(PaymentInfo),
        <<"subscription_tier">> => ToBinary(SubscriptionTier),
        <<"account_managers">> => ToBinary(AccountManagers),
        <<"business_goals">> => ToBinary(BusinessGoals),
        <<"date_created">> => list_to_binary(TimestampStr),
        <<"date_updated">> => ToBinary(DateUpdated),
        <<"brand_colors">> => ToBinary(BrandColors),
        <<"brand_voice_keywords">> => ToBinary(BrandVoiceKeywords),
        <<"certifications">> => ToBinary(Certifications),
        <<"awards">> => ToBinary(Awards),
        <<"partnerships">> => ToBinary(Partnerships),
        <<"sustainability_initiatives">> => ToBinary(SustainabilityInitiatives),
        <<"corporate_values">> => ToBinary(CorporateValues),
        <<"employee_count">> => ToBinary(EmployeeCount),
        <<"annual_revenue">> => ToBinary(AnnualRevenue),
        <<"target_audience">> => ToBinary(TargetAudience),
        <<"content_calendar">> => ToBinary(ContentCalendar),
        <<"crm_integration">> => ToBinary(CrmIntegration),
        <<"loyalty_program">> => ToBinary(LoyaltyProgram),
        <<"user_generated_content">> => ToBinary(UserGeneratedContent),
        <<"virtual_events">> => ToBinary(VirtualEvents),
        <<"augmented_reality_experiences">> => ToBinary(AugmentedRealityExperiences),
        <<"chatbot_enabled">> => ToBinary(ChatbotEnabled),
        <<"blockchain_initiatives">> => ToBinary(BlockchainInitiatives),
        <<"data_privacy_certifications">> => ToBinary(DataPrivacyCertifications),
        <<"ai_powered_features_enabled">> => ToBinary(AiPoweredFeaturesEnabled),
        <<"social_responsibility_score">> => ToBinary(SocialResponsibilityScore),
        <<"innovation_index">> => ToBinary(InnovationIndex),
        <<"customer_satisfaction_index">> => ToBinary(CustomerSatisfactionIndex),
        <<"crisis_management_plan">> => ToBinary(CrisisManagementPlan),
        <<"digital_asset_portfolio">> => ToBinary(DigitalAssetPortfolio),
        <<"metaverse_presence">> => ToBinary(MetaversePresence),
        <<"carbon_footprint_data">> => ToBinary(CarbonFootprintData),
        <<"diversity_inclusion_metrics">> => ToBinary(DiversityInclusionMetrics),
        <<"talent_acquisition_channels">> => ToBinary(TalentAcquisitionChannels),
        <<"knowledge_base_url">> => ToBinary(KnowledgeBaseUrl),
        <<"api_integrations">> => ToBinary(ApiIntegrations),
        <<"virtual_reality_spaces">> => ToBinary(VirtualRealitySpaces),
        <<"voice_interaction_profile">> => ToBinary(VoiceInteractionProfile),
        <<"biometric_authentication_methods">> => ToBinary(BiometricAuthenticationMethods),
        <<"iot_integration_points">> => ToBinary(IotIntegrationPoints),
        <<"quantum_resistant_security_measures">> => ToBinary(QuantumResistantSecurityMeasures),
        <<"neurofeedback_enabled_services">> => ToBinary(NeurofeedbackEnabledServices),
        <<"emotional_intelligence_score">> => ToBinary(EmotionalIntelligenceScore),
        <<"cultural_adaptation_strategies">> => ToBinary(CulturalAdaptationStrategies),
        <<"language_localization_status">> => ToBinary(LanguageLocalizationStatus),
        <<"continual_learning_initiatives">> => ToBinary(ContinualLearningInitiatives),
        <<"cross_platform_presence">> => ToBinary(CrossPlatformPresence),
        <<"ethical_ai_commitment">> => ToBinary(EthicalAiCommitment),
        <<"blockchain_integration">> => ToBinary(BlockchainIntegration),
        <<"ai_ethical_guidelines">> => ToBinary(AiEthicalGuidelines),
        <<"digital_transformation_score">> => ToBinary(DigitalTransformationScore),
        <<"circular_economy_initiatives">> => ToBinary(CircularEconomyInitiatives),
        <<"global_expansion_roadmap">> => ToBinary(GlobalExpansionRoadmap),
        <<"intellectual_property_portfolio">> => ToBinary(IntellectualPropertyPortfolio),
        <<"regulatory_compliance_status">> => ToBinary(RegulatoryComplianceStatus),
        <<"supply_chain_transparency">> => ToBinary(SupplyChainTransparency),
        <<"customer_data_management_practices">> => ToBinary(CustomerDataManagementPractices),
        <<"digital_asset_management">> => ToBinary(DigitalAssetManagement),
        <<"quantum_computing_readiness">> => ToBinary(QuantumComputingReadiness),
        <<"augmented_workforce_metrics">> => ToBinary(AugmentedWorkforceMetrics),
        <<"ecosystem_partnerships">> => ToBinary(EcosystemPartnerships),
        <<"cyber_resilience_score">> => ToBinary(CyberResilienceScore),
        <<"decentralized_autonomous_organization_status">> => ToBinary(DecentralizedAutonomousOrganizationStatus),
        <<"data">> => ToBinary(Data)
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_tfrecord(Business, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_business(Business),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_business_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("Business saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.