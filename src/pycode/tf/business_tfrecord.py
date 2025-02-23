from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import tensorflow as tf
import logging
import os

# Suppress TensorFlow logging
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
logging.getLogger("tensorflow").setLevel(logging.ERROR)

router = APIRouter()

class BusinessDataRequest(BaseModel):
    id: str
    user_id: str
    ai_business_id: str
    ads_id: list
    company_name: str
    industry: str
    company_size: str
    website: str
    business_email: str
    business_phone: str
    tax_id: str
    verification_status: bool
    business_type: str
    founding_date: str
    location: dict
    operating_hours: list
    products_services: list
    social_media_links: dict
    business_description: str
    logo_url: str
    banner_url: str
    followers_count: int
    following_count: int
    posts: list
    posts_count: int
    average_engagement_rate: float
    customer_support_email: str
    payment_info: dict
    subscription_tier: str
    account_managers: list
    business_goals: list
    date_created: str
    date_updated: str
    brand_colors: list
    brand_voice_keywords: list
    certifications: list
    awards: list
    partnerships: list
    sustainability_initiatives: list
    corporate_values: list
    employee_count: int
    annual_revenue: float
    target_audience: list
    content_calendar: dict
    crm_integration: str
    loyalty_program: dict
    user_generated_content: list
    virtual_events: list
    augmented_reality_experiences: list
    chatbot_enabled: bool
    blockchain_initiatives: list
    data_privacy_certifications: list
    ai_powered_features_enabled: dict
    social_responsibility_score: float
    innovation_index: float
    customer_satisfaction_index: float
    crisis_management_plan: str
    digital_asset_portfolio: list
    metaverse_presence: dict
    carbon_footprint_data: dict
    diversity_inclusion_metrics: dict
    talent_acquisition_channels: list
    knowledge_base_url: str
    api_integrations: list
    virtual_reality_spaces: dict
    voice_interaction_profile: dict
    biometric_authentication_methods: list
    iot_integration_points: dict
    quantum_resistant_security_measures: list
    neurofeedback_enabled_services: list
    emotional_intelligence_score: float
    cultural_adaptation_strategies: dict
    language_localization_status: dict
    continual_learning_initiatives: list
    cross_platform_presence: dict
    ethical_ai_commitment: dict
    blockchain_integration: dict
    ai_ethical_guidelines: list
    digital_transformation_score: float
    circular_economy_initiatives: list
    global_expansion_roadmap: dict
    intellectual_property_portfolio: list
    regulatory_compliance_status: dict
    supply_chain_transparency: float
    customer_data_management_practices: dict
    digital_asset_management: dict
    quantum_computing_readiness: float
    augmented_workforce_metrics: dict
    ecosystem_partnerships: list
    cyber_resilience_score: float
    decentralized_autonomous_organization_status: dict
    data: dict
    filename: str  

@router.post("/convert_business_to_tfrecord")
def convert_business_to_tfrecord(request: BusinessDataRequest):
    try:
        example = tf.train.Example()

        for field, value in request.dict().items():
            if field == "filename":
                continue  
            if isinstance(value, str):
                example.features.feature[field].bytes_list.value.append(value.encode("utf-8"))
            elif isinstance(value, (list, dict)):
                example.features.feature[field].bytes_list.value.append(str(value).encode("utf-8"))
            elif isinstance(value, (int, float, bool)):
                example.features.feature[field].bytes_list.value.append(str(value).encode("utf-8"))

        serialized_data = example.SerializeToString()

        with tf.io.TFRecordWriter(request.filename) as writer:
            writer.write(serialized_data)

        return {"message": f"Business saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))