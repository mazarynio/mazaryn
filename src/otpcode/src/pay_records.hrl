-record(payment, {
    id,
    payer_id,                   % User ID or Business ID of the payer
    payee_id,                   % User ID or Business ID of the payee
    amount,
    currency,
    payment_type,               % e.g., "subscription", "ad_payment", "in_app_purchase", "transfer"
    payment_method = #{
        type => "",             % e.g., "credit_card", "bank_transfer", "crypto", "mobile_wallet"
        details => #{}          % Specific details based on the payment method
    },
    status,                     % e.g., "pending", "completed", "failed", "refunded"
    transaction_date,
    settlement_date,
    description,
    invoice_id,                 % If applicable
    recurring = false,          % For subscription payments
    fees = #{
        platform_fee => 0,
        payment_processor_fee => 0,
        tax => 0
    },
    metadata = #{},             % Additional data specific to the payment type
    refund_info = #{
        refunded_amount => 0,
        refund_date => null,
        refund_reason => ""
    },
    conversion_rates = #{},     % For cross-currency transactions
    risk_score = 0.0,           % Fraud risk assessment score
    verification_status = "",   % e.g., "verified", "pending", "failed"
    associated_content_id = "", % ID of associated post, ad, or product
    escrow_details = #{
        escrow_id => "",
        release_conditions => [],
        release_date => null
    },
    split_payment_info = [],    % For payments split among multiple recipients
    loyalty_points = #{
        points_earned => 0,
        points_redeemed => 0
    },
    tax_information = #{
        tax_rate => 0.0,
        tax_jurisdiction => "",
        tax_id => ""
    },
    compliance_checks = #{
        aml_check => "",        % Anti-Money Laundering check result
        kyc_verified => false   % Know Your Customer verification
    },
    blockchain_data = #{
        network => "",          % e.g., "Ethereum", "Bitcoin", "Solana"
        transaction_hash => "",
        block_number => 0,
        smart_contract_address => ""
    },
    instant_settlement = false, % Indicates if payment was settled instantly
    chargeback_info = #{
        chargeback_date => null,
        chargeback_reason => "",
        chargeback_status => ""
    },
    cross_border_info = #{
        source_country => "",
        destination_country => "",
        exchange_rate_applied => 0.0
    },
    user_device_info = #{},     % Device information for the transaction
    affiliate_data = #{
        affiliate_id => "",
        commission_rate => 0.0,
        commission_amount => 0
    },
    payment_link = "",          % Shareable payment link if applicable
    qr_code = "",               % QR code for the payment if applicable
    subscription_details = #{
        plan_id => "",
        billing_cycle => "",
        next_billing_date => null
    },
    data = #{}                  % For future expansions
}).

-record(ai_payment, {
    id,
    payment_id,                 % Reference to the main payment record
    fraud_detection = #{
        risk_score => 0.0,
        flagged_indicators => [],
        anomaly_detection_results => #{}
    },
    user_payment_behavior = #{
        typical_transaction_range => {0, 0},
        preferred_payment_methods => [],
        payment_frequency => 0.0
    },
    predictive_analytics = #{
        chargeback_probability => 0.0,
        expected_lifetime_value => 0.0,
        payment_default_risk => 0.0
    },
    dynamic_pricing = #{
        personalized_discount_rate => 0.0,
        price_sensitivity_score => 0.0
    },
    transaction_categorization = #{
        expense_category => "",
        business_category => ""
    },
    cross_selling_opportunities = [],
    payment_optimization = #{
        suggested_payment_method => "",
        cost_saving_recommendations => []
    },
    cash_flow_forecasting = #{
        short_term_forecast => [],
        long_term_forecast => []
    },
    sentiment_analysis = #{
        transaction_sentiment => 0.0,
        sentiment_factors => []
    },
    financial_health_indicators = #{
        spending_pattern_health => 0.0,
        savings_potential => 0.0,
        budget_adherence_score => 0.0
    },
    regulatory_compliance_check = #{
        compliance_score => 0.0,
        required_actions => []
    },
    market_basket_analysis = #{
        frequently_bought_together => [],
        upsell_opportunities => []
    },
    loyalty_program_optimization = #{
        recommended_rewards => [],
        points_usage_prediction => 0.0
    },
    crypto_market_analysis = #{
        price_volatility_assessment => 0.0,
        optimal_transaction_timing => ""
    },
    peer_comparison = #{
        percentile_rank => 0.0,
        benchmark_metrics => #{}
    },
    anomaly_explanation = #{
        detected_anomalies => [],
        probable_causes => []
    },
    smart_contract_analysis = #{
        contract_risk_assessment => 0.0,
        optimization_suggestions => []
    },
    payment_route_optimization = #{
        suggested_route => "",
        estimated_cost_savings => 0.0
    },
    identity_verification_confidence = 0.0,
    financial_crime_pattern_detection = #{
        detected_patterns => [],
        similarity_to_known_schemes => 0.0
    },
    customer_support_prediction = #{
        likelihood_of_support_need => 0.0,
        predicted_issues => []
    },
    real_time_currency_conversion_optimization = #{
        suggested_conversion_time => "",
        potential_savings => 0.0
    },
    blockchain_network_analysis = #{
        network_congestion_prediction => 0.0,
        optimal_gas_price => 0
    },
    ai_driven_financial_advice = [],
    payment_experience_score = 0.0,
    data = #{}                  % For future expansions
}).