-record(job_posting, {
    id,
    job_id,                     % Unique job identifier (e.g., "JOB-2024-001234")

    poster_type,                % "individual", "business"
    poster_id,                  % user_id or business_id
    posted_by_user_id,          % Actual user who created the posting
    business_id,                % Reference to business account (if applicable)

    title,                      % Job title
    description,                % Full job description
    short_description,          % Brief summary (max 200 chars)
    responsibilities = [],      % List of job responsibilities
    requirements = [],          % List of job requirements
    qualifications = [],        % Required qualifications
    preferred_qualifications = [], % Nice-to-have qualifications

    category,                   % "technology", "healthcare", "finance", "education", etc.
    sub_category,               % More specific classification
    industry,                   % Industry sector
    job_function,               % Primary job function
    seniority_level,            % "entry", "mid", "senior", "lead", "executive"
    employment_type,            % "full_time", "part_time", "contract", "temporary", "internship", "volunteer"
    work_schedule,              % "day_shift", "night_shift", "flexible", "rotating"

    location_type,              % "on_site", "remote", "hybrid"
    locations = [],             % List of {country, state, city, address, coordinates}
    remote_restrictions = [],   % Countries/regions where remote work is allowed
    relocation_assistance = false, % Relocation package available
    travel_requirement,         % "none", "occasional", "frequent", "constant"
    travel_percentage = 0,      % Percentage of time traveling (0-100)
    timezone_requirements = [], % Required/preferred timezones

    salary_range = #{
        min => 0.0,
        max => 0.0,
        currency => "USD",
        period => "yearly"      % "hourly", "daily", "weekly", "monthly", "yearly"
    },
    salary_negotiable = false,
    salary_visible = false,     % Show salary publicly
    equity_offered = false,     % Stock options/equity available
    equity_range = #{},         % Equity percentage/value range

    benefits = [],              % List of benefits (health, dental, vision, 401k, etc.)
    perks = [],                 % Additional perks (gym, meals, transport, etc.)
    bonus_structure = #{},      % Performance bonuses, signing bonus, etc.
    commission_structure = #{}, % Commission details for sales roles

    required_skills = [],       % Must-have skills
    preferred_skills = [],      % Nice-to-have skills
    skill_assessments = [],     % List of skill assessment IDs
    certifications_required = [], % Required certifications
    certifications_preferred = [], % Preferred certifications
    languages_required = [],    % Required languages with proficiency levels
    languages_preferred = [],   % Preferred languages

    experience_required = #{
        min_years => 0,
        max_years => 0,
        specific_domains => []  % Industry-specific experience
    },

    education_requirements = #{
        level => "",            % "high_school", "associate", "bachelor", "master", "phd"
        fields => [],           % Relevant fields of study
        required => false       % Is education mandatory
    },

    %% Application Process
    application_method,         % "platform", "external_url", "email"
    external_application_url,   % External application link
    application_email,          % Email for applications
    application_deadline,       % Deadline for applications
    application_questions = [], % Custom application questions
    resume_required = true,
    cover_letter_required = false,
    portfolio_required = false,
    video_introduction_required = false,

    screening_questions = [],   % Pre-screening questions
    skills_tests = [],          % Required skills tests
    background_check_required = false,
    drug_test_required = false,
    reference_check_required = false,
    min_references_required = 0,

    hiring_process_steps = [],  % List of interview/assessment stages
    estimated_time_to_hire,     % Days to complete hiring
    interview_types = [],       % "phone", "video", "in_person", "panel", "technical"
    number_of_positions = 1,    % Number of openings

    status,                     % "draft", "active", "paused", "closed", "filled", "cancelled"
    visibility,                 % "public", "private", "network_only", "verified_only"
    featured = false,           % Featured listing (premium)
    boosted = false,            % Boosted for more visibility (premium)
    featured_until,             % Featured expiration
    boosted_until,              % Boost expiration
    priority_level = 0,         % 0-10, higher = more visibility

    target_demographics = #{},  % Age range, experience level, etc.
    excluded_regions = [],      % Regions where job shouldn't be shown
    preferred_schools = [],     % Target specific universities/schools
    veteran_friendly = false,
    disability_friendly = false,
    lgbtq_friendly = false,
    diversity_statement,        % Company's diversity commitment

    views = 0,                  % Total views
    unique_views = 0,           % Unique user views
    applications_count = 0,     % Total applications received
    qualified_applications = 0, % Applications meeting criteria
    shortlisted_count = 0,      % Shortlisted candidates
    interviewed_count = 0,      % Candidates interviewed
    offers_made = 0,            % Job offers extended
    offers_accepted = 0,        % Accepted offers

    applications = [],          % List of application IDs
    shortlisted_candidates = [], % List of shortlisted application IDs
    rejected_candidates = [],   % List of rejected application IDs
    hired_candidates = [],      % List of hired user IDs

    avg_time_to_apply = 0,      % Average time users spend applying (seconds)
    conversion_rate = 0.0,      % View to application conversion
    source_tracking = #{},      % Where applicants came from

    job_description_ipfs_hash,  % IPFS hash for detailed description
    company_media_ipfs_hashes = [], % Company images/videos
    job_media_ipfs_hashes = [],     % Job-specific media
    documents_ipfs_hashes = #{},    % Supporting documents

    shares = 0,                 % Times job was shared
    saves = 0,                  % Times job was saved
    referrals = [],             % List of user referrals
    comments = [],              % Public comments/questions
    reactions = #{},            % User reactions

    equal_opportunity_employer = false,
    visa_sponsorship = false,   % Company sponsors work visas
    security_clearance_required = false,
    security_clearance_level,   % Level of clearance needed
    legal_disclaimers = [],     % Legal statements
    gdpr_compliant = true,
    data_retention_days = 90,   % Days to keep application data

    countries_allowed = [],     % Specific countries (empty = all)
    work_permit_required = #{}, % Per-country work permit requirements
    local_labor_law_compliant = true,
    salary_ranges_by_country = #{}, % Country-specific salary ranges
    currency_conversion_rates = #{}, % Current conversion rates

    tags = [],                  % Searchable tags
    keywords = [],              % SEO keywords
    search_score = 0.0,         % Search ranking score

    application_open_date,      % When applications open
    application_close_date,     % When applications close
    start_date,                 % Expected start date
    start_date_flexible = false,

    %% Notifications
    notification_settings = #{
        new_application => true,
        application_deadline_reminder => true,
        matched_candidate => true,
        low_application_count => true
    },

    %% Collaboration (for business accounts)
    hiring_team = [],           % List of {user_id, role, permissions}
    assigned_recruiters = [],   % Assigned recruiters

    %% AI & Matching
    ai_job_id,                  % Reference to AI analysis
    ai_match_criteria = #{},    % AI matching parameters
    auto_screening_enabled = false,
    ai_ranking_enabled = false,

    %% Metadata
    date_created,
    date_updated,
    date_published,
    last_activity,
    created_by_ip,
    expiration_date,
    auto_repost = false,        % Auto-repost when expired

    %% Custom & Extension
    custom_fields = #{},
    metadata = #{},
    data = #{}
}).


-record(resume, {
    id,
    user_id,

    resume_type,                % "platform_generated", "uploaded_pdf", "hybrid"
    is_primary = false,         % Primary resume for applications
    visibility,                 % "public", "private", "connections_only", "recruiters_only"
    status,                     % "draft", "active", "archived"

    full_name,
    professional_title,         % Current/desired job title
    headline,                   % Professional headline/tagline
    summary,                    % Professional summary

    email,
    phone,
    location = #{
        country => "",
        state => "",
        city => "",
        postal_code => "",
        willing_to_relocate => false
    },

    website,
    linkedin_url,
    github_url,
    portfolio_url,
    social_links = #{},         % Other social/professional links

    work_experience = [],       % List of experience records
    total_years_experience = 0,

    %% Education
    education = [],             % List of education records
    highest_degree,             % Highest education level

    %% Skills
    technical_skills = [],      % Technical skills with proficiency
    soft_skills = [],           % Soft skills
    tools_technologies = [],    % Tools & technologies
    skill_endorsements = #{},   % Map of skill to endorsers

    %% Languages
    languages = [],             % List of {language, proficiency_level, certifications}

    %% Certifications & Licenses
    certifications = [],        % Professional certifications
    licenses = [],              % Professional licenses

    %% Projects & Portfolio
    projects = [],              % Personal/professional projects
    publications = [],          % Published works
    patents = [],               % Patents held

    %% Achievements & Awards
    awards = [],                % Awards and honors
    achievements = [],          % Notable achievements

    %% Volunteer & Activities
    volunteer_experience = [],  % Volunteer work
    professional_memberships = [], % Professional organizations
    hobbies_interests = [],     % Personal interests

    %% Job Preferences
    job_preferences = #{
        desired_titles => [],
        desired_industries => [],
        desired_locations => [],
        remote_preference => "", % "remote_only", "hybrid", "on_site", "flexible"
        employment_types => [],
        min_salary => 0.0,
        salary_currency => "USD",
        willing_to_travel => false,
        relocation_willing => false,
        notice_period => "",    % "immediate", "2_weeks", "1_month", "negotiable"
        available_start_date => ""
    },

    %% IPFS Storage
    resume_pdf_ipfs_hash,       % Uploaded PDF resume
    cover_letter_ipfs_hash,     % Default cover letter
    portfolio_ipfs_hash,        % Portfolio document
    certifications_ipfs_hashes = #{}, % Certification documents

    references = [],            % Professional references
    references_available = true,

    %% Additional Documents
    transcripts_ipfs_hash,      % Academic transcripts
    recommendation_letters_ipfs_hashes = [], % Letters of recommendation

    %% Analytics
    views = 0,                  % Resume views
    downloads = 0,              % Resume downloads
    matches = 0,                % Job matches found
    applications_sent = 0,      % Applications using this resume
    recruiter_contacts = 0,     % Times contacted by recruiters

    %% AI & Optimization
    ai_resume_id,               % AI analysis reference
    ai_score = 0.0,             % Resume quality score (0-100)
    ai_suggestions = [],        % AI improvement suggestions
    keywords_extracted = [],    % AI-extracted keywords
    ats_compatibility_score = 0.0, % Applicant Tracking System compatibility

    %% Templates & Formatting
    template_id,                % Resume template used
    theme_colors = [],          % Color scheme
    font_family,                % Font selection

    %% Privacy & Security
    hide_contact_info = false,  % Hide contact until interest shown
    anonymous_mode = false,     % Show resume without identifying info
    block_companies = [],       % Companies that can't view resume

    %% Metadata
    date_created,
    date_updated,
    last_modified_section,      % Last updated section
    version = 1,                % Resume version number
    language = "en",            % Resume language

    %% Custom & Extension
    custom_sections = [],       % User-defined sections
    metadata = #{},
    data = #{}
}).

-record(work_experience, {
    id,
    company_name,
    company_business_id,        % Link to business account if on platform
    position_title,
    employment_type,            % "full_time", "part_time", "contract", etc.
    location = #{
        country => "",
        state => "",
        city => "",
        remote => false
    },
    start_date,                 % {Year, Month}
    end_date,                   % {Year, Month} or "present"
    current_position = false,
    duration_months = 0,
    description,
    responsibilities = [],
    achievements = [],
    technologies_used = [],
    team_size,
    reporting_to,
    media_ipfs_hashes = [],     % Work samples, certificates
    verification_status = false, % Verified by company
    data = #{}
}).

-record(education, {
    id,
    institution_name,
    degree,                     % "High School", "Associate", "Bachelor", "Master", "PhD"
    field_of_study,
    start_date,                 % {Year, Month}
    end_date,                   % {Year, Month} or "present"
    grade,                      % GPA or grade
    grade_type,                 % "GPA", "Percentage", "Class"
    activities = [],            % Extracurricular activities
    honors = [],                % Academic honors
    relevant_coursework = [],
    thesis_title,
    thesis_ipfs_hash,
    location = #{
        country => "",
        city => ""
    },
    verification_status = false,
    data = #{}
}).

%% Project Sub-record
-record(job_project, {
    id,
    title,
    description,
    role,
    start_date,
    end_date,
    current_project = false,
    technologies = [],
    url,
    github_url,
    media_ipfs_hashes = [],     % Screenshots, demos
    achievements = [],
    team_size,
    data = #{}
}).

%% ====================================================================
%% Job Application Records
%% ====================================================================

-record(job_application, {
    id,
    application_id,             % Unique application ID

    %% Relationships
    job_posting_id,
    applicant_user_id,
    resume_id,                  % Resume used for application

    %% Application Details
    status,                     % "submitted", "under_review", "shortlisted", "interviewing",
                                % "offer_extended", "accepted", "rejected", "withdrawn"

    %% Application Materials
    resume_ipfs_hash,           % Snapshot of resume at application time
    resume_pdf_ipfs_hash,       % PDF version
    cover_letter,               % Cover letter text
    cover_letter_ipfs_hash,     % Cover letter document
    portfolio_ipfs_hash,        % Portfolio
    additional_documents_ipfs_hashes = [], % Other documents
    video_introduction_ipfs_hash, % Video introduction

    %% Responses to Job-Specific Questions
    screening_question_answers = [], % List of {question_id, answer}
    custom_question_answers = [],    % Custom application questions

    %% Referral & Source
    referred_by_user_id,        % User who referred applicant
    referral_code,              % Referral tracking code
    application_source,         % "direct", "job_board", "social_media", "referral", "email"
    utm_parameters = #{},       % Tracking parameters

    %% Skills Assessment
    skills_test_results = [],   % List of {test_id, score, status}
    assessment_scores = #{},    % Various assessment scores
    technical_test_completed = false,

    %% Interview Process
    interview_stages = [],      % List of interview stage records
    current_stage,              % Current stage in hiring process
    interview_feedback = [],    % Interviewer feedback
    next_interview_scheduled,   % Next interview datetime

    %% Communication
    messages = [],              % Communication thread with employer
    last_message_date,
    unread_messages_count = 0,
    notifications_enabled = true,

    %% Offer Details (if applicable)
    offer_details = #{
        salary => 0.0,
        currency => "USD",
        equity => 0.0,
        benefits => [],
        start_date => "",
        offer_letter_ipfs_hash => "",
        offer_expiry_date => "",
        negotiation_history => []
    },

    %% Rejection Details
    rejection_reason,
    rejection_feedback,
    rejection_date,
    reapplication_allowed = true,
    reapplication_wait_days = 90,

    %% Analytics & Tracking
    viewed_by_employer = false,
    first_viewed_date,
    total_views = 0,
    time_in_each_stage = #{},   % Time spent in each stage
    response_time = 0,          % Hours to receive response

    %% AI & Matching
    ai_match_score = 0.0,       % AI-calculated match score (0-100)
    ai_ranking = 0,             % Ranking among all applicants
    ai_strengths = [],          % AI-identified strengths
    ai_concerns = [],           % AI-identified concerns
    auto_screened = false,
    auto_screen_result,         % "pass", "fail", "review_needed"

    %% Applicant Actions
    withdrawn = false,
    withdraw_reason,
    withdraw_date,

    %% Employer Actions
    flagged = false,            % Flagged for review
    flag_reason,
    notes = [],                 % Employer notes on candidate
    rating = 0.0,               % Employer rating (0-5)
    tags = [],                  % Tags for organization

    %% Compliance & Legal
    background_check_status,    % "pending", "in_progress", "cleared", "failed"
    background_check_date,
    reference_check_status,
    reference_check_results = [],
    right_to_work_verified = false,
    consent_given = true,       % Consent for data processing
    consent_date,

    %% Scheduling
    availability = [],          % Available time slots for interviews
    timezone,
    preferred_interview_method, % "phone", "video", "in_person"

    %% Metadata
    date_submitted,
    date_updated,
    last_activity,
    ip_address,
    device_info,
    geolocation,

    %% Custom & Extension
    custom_fields = #{},
    metadata = #{},
    data = #{}
}).

%% Interview Stage Sub-record
-record(interview_stage, {
    id,
    stage_name,                 % "phone_screen", "technical", "behavioral", "final"
    stage_number,
    status,                     % "scheduled", "completed", "cancelled", "no_show"
    interview_type,             % "phone", "video", "in_person", "panel"
    scheduled_date,
    duration_minutes,
    interviewers = [],          % List of interviewer IDs
    location,                   % Physical or virtual location
    meeting_link,
    dial_in_details,
    feedback = [],              % Interviewer feedback
    score = 0.0,
    notes,
    recording_ipfs_hash,        % Interview recording (if permitted)
    date_completed,
    data = #{}
}).

%% ====================================================================
%% Saved Jobs & Alerts
%% ====================================================================

-record(saved_job, {
    id,
    user_id,
    job_posting_id,
    date_saved,
    notes,                      % User notes on the job
    reminder_set = false,
    reminder_date,
    applied = false,
    application_id,
    data = #{}
}).

-record(job_alert, {
    id,
    user_id,
    alert_name,

    %% Search Criteria
    keywords = [],
    job_titles = [],
    companies = [],
    locations = [],
    remote_only = false,
    employment_types = [],
    experience_levels = [],
    salary_min = 0.0,
    salary_currency = "USD",
    industries = [],

    %% Alert Settings
    frequency,                  % "realtime", "daily", "weekly"
    notification_methods = [],  % "email", "push", "sms"
    max_alerts_per_period = 10,
    active = true,

    %% Matching
    ai_matching_enabled = false,
    min_match_score = 70.0,     % Minimum match percentage

    %% Analytics
    total_matches = 0,
    matches_viewed = 0,
    matches_applied = 0,

    %% Metadata
    date_created,
    date_updated,
    last_notification_sent,

    data = #{}
}).

%% ====================================================================
%% Recruiter & Talent Pool Records
%% ====================================================================

-record(talent_pool, {
    id,
    business_id,
    name,
    description,

    %% Pool Criteria
    criteria = #{
        skills => [],
        experience_years => #{min => 0, max => 999},
        education_levels => [],
        locations => [],
        industries => []
    },

    %% Members
    candidates = [],            % List of user IDs
    candidate_count = 0,

    %% Engagement
    active = true,
    auto_add_criteria,          % Automatically add matching candidates

    %% Analytics
    total_invites_sent = 0,
    response_rate = 0.0,
    hire_rate = 0.0,

    %% Metadata
    created_by_user_id,
    date_created,
    date_updated,

    data = #{}
}).

-record(candidate_search, {
    id,
    searcher_id,                % User or business ID
    searcher_type,              % "user", "business"

    %% Search Criteria
    keywords = [],
    skills_required = [],
    locations = [],
    min_experience_years = 0,
    max_experience_years = 999,
    education_level,
    current_companies = [],
    previous_companies = [],
    universities = [],
    certifications = [],
    languages = [],

    %% Availability
    available_only = false,     % Only show available candidates
    remote_candidates_only = false,

    %% Search Settings
    search_name,                % Saved search name
    saved = false,
    alert_enabled = false,

    %% Results
    results = [],               % List of matching resume IDs
    result_count = 0,
    contacted_candidates = [],

    %% Metadata
    date_created,
    last_run,

    data = #{}
}).

-record(recruiter_contact, {
    id,
    recruiter_id,               % Business or user ID
    candidate_user_id,
    resume_id,
    job_posting_id,             % Associated job (optional)

    %% Message
    subject,
    message,
    message_template_id,

    %% Status
    status,                     % "sent", "read", "replied", "interested", "not_interested"
    response,
    response_date,

    %% Follow-up
    follow_up_scheduled = false,
    follow_up_date,
    follow_up_sent = false,

    %% Metadata
    date_sent,
    date_read,

    data = #{}
}).

%% ====================================================================
%% Skills & Endorsements
%% ====================================================================

-record(skill, {
    id,
    name,
    category,                   % "technical", "soft", "language", "tool"
    description,
    related_skills = [],
    demand_score = 0.0,         % Market demand (0-100)
    avg_salary_impact = 0.0,    % Salary impact percentage

    data = #{}
}).

-record(skill_endorsement, {
    id,
    user_id,                    % User being endorsed
    skill_name,
    endorsed_by_user_id,        % User giving endorsement
    relationship,               % "colleague", "manager", "client", "teacher"
    date_endorsed,

    data = #{}
}).

-record(skill_assessment, {
    id,
    skill_name,
    user_id,

    %% Assessment Details
    assessment_type,            % "test", "certification", "project", "peer_review"
    provider,                   % Assessment provider
    score = 0.0,
    max_score = 100.0,
    proficiency_level,          % "beginner", "intermediate", "advanced", "expert"

    %% Certification
    certificate_ipfs_hash,
    certificate_url,
    certificate_number,
    issue_date,
    expiry_date,

    %% Verification
    verified = false,
    verification_source,

    %% Metadata
    date_completed,

    data = #{}
}).

%% ====================================================================
%% Company Reviews (for job seekers)
%% ====================================================================

-record(company_review, {
    id,
    business_id,
    reviewer_user_id,

    %% Review Details
    overall_rating = 0.0,       % 0.0 to 5.0
    ratings = #{
        work_life_balance => 0.0,
        compensation => 0.0,
        culture => 0.0,
        management => 0.0,
        career_growth => 0.0
    },

    %% Employment Details
    job_title,
    employment_status,          % "current", "former"
    employment_duration_months,
    location,

    %% Review Content
    review_title,
    pros,
    cons,
    advice_to_management,

    %% Verification
    verified_employee = false,
    anonymous = false,

    %% Engagement
    helpful_count = 0,
    not_helpful_count = 0,

    %% Status
    status,                     % "pending", "approved", "rejected"
    moderation_notes,

    %% Metadata
    date_created,
    date_updated,

    data = #{}
}).

%% ====================================================================
%% Interview Preparation & Resources
%% ====================================================================

-record(interview_prep, {
    id,
    user_id,
    job_application_id,

    %% Company Research
    company_research_notes,
    company_questions = [],     % Questions to ask interviewer

    %% Practice
    practice_questions = [],    % Practice interview questions
    practice_answers = [],      % User's answers
    mock_interview_recordings_ipfs_hashes = [],

    %% Resources
    resource_links = [],
    documents_ipfs_hashes = [],

    %% Preparation Checklist
    checklist = [],             % Preparation tasks
    completed_items = [],

    %% Metadata
    date_created,
    date_updated,

    data = #{}
}).

%% ====================================================================
%% Analytics & Insights
%% ====================================================================

-record(job_market_insights, {
    id,

    %% Market Data
    location,                   % Country/region
    industry,
    job_title,

    %% Statistics
    total_openings = 0,
    avg_salary = 0.0,
    salary_range = #{min => 0.0, max => 0.0},
    top_companies = [],
    top_skills_demanded = [],
    growth_rate = 0.0,          % YoY growth percentage

    %% Trends
    trending_up = [],           % Skills/roles trending up
    trending_down = [],         % Skills/roles trending down

    %% Metadata
    data_date,                  % Date of data snapshot
    source,

    data = #{}
}).

-record(user_job_analytics, {
    id,
    user_id,

    %% Application Analytics
    total_applications = 0,
    applications_this_month = 0,
    response_rate = 0.0,
    interview_rate = 0.0,
    offer_rate = 0.0,

    %% Job Search Performance
    profile_views = 0,
    resume_downloads = 0,
    recruiter_contacts = 0,

    %% Engagement
    jobs_viewed = 0,
    jobs_saved = 0,
    searches_performed = 0,

    %% Time Metrics
    avg_application_time = 0,   % Minutes
    avg_time_to_response = 0,   % Days
    avg_time_to_hire = 0,       % Days

    %% Skill Insights
    most_requested_skills = [], % Skills appearing in job requirements
    skill_gaps = [],            % Skills user lacks for desired roles

    %% Recommendations
    recommended_skills = [],
    recommended_certifications = [],

    %% Metadata
    last_calculated,

    data = #{}
}).

%% ====================================================================
%% Messaging & Communication
%% ====================================================================

-record(job_message, {
    id,
    thread_id,                  % Message thread identifier

    %% Participants
    sender_id,
    sender_type,                % "applicant", "recruiter", "hiring_manager"
    recipient_id,
    recipient_type,

    %% Context
    job_posting_id,
    application_id,

    %% Message Content
    subject,
    body,
    attachments_ipfs_hashes = [],

    %% Status
    read = false,
    read_date,
    archived = false,
    starred = false,

    %% Metadata
    date_sent,

    data = #{}
}).

%% ====================================================================
%% Compliance & Verification
%% ====================================================================

-record(background_check, {
    id,
    user_id,
    job_application_id,

    %% Check Details
    provider,                   % Background check service provider
    check_types = [],           % "criminal", "employment", "education", "credit"

    %% Results
    status,                     % "pending", "in_progress", "completed", "failed"
    overall_result,             % "clear", "review_needed", "discrepancies_found"
    findings = [],

    %% Specific Checks
    criminal_check_result,
    employment_verification_result,
    education_verification_result,
    reference_check_results = [],

    %% Consent
    consent_given = false,
    consent_date,
    consent_ipfs_hash,          % Signed consent form

    %% Reports
    report_ipfs_hash,

    %% Metadata
    requested_date,
    completed_date,
    expiry_date,

    data = #{}
}).

-record(employment_verification, {
    id,
    user_id,
    work_experience_id,

    %% Verification Details
    status,                     % "pending", "verified", "failed", "unresponsive"
    verified_by,                % Verifying organization
    verification_method,        % "email", "phone", "document", "third_party"

    %% Verified Information
    company_confirmed = false,
    title_confirmed = false,
    dates_confirmed = false,
    responsibilities_confirmed = false,

    %% Discrepancies
    discrepancies = [],

    %% Metadata
    requested_date,
    verified_date,
    verification_document_ipfs_hash,

    data = #{}
}).

%% ====================================================================
%% Referral System
%% ====================================================================

-record(job_referral, {
    id,
    referral_code,

    %% Referrer & Referee
    referrer_user_id,           % User making referral
    referee_user_id,            % User being referred
    referee_email,              % If referee not yet on platform

    %% Job & Application
    job_posting_id,
    application_id,

    %% Status
    status,                     % "sent", "signed_up", "applied", "interviewed", "hired"

    %% Rewards
    referrer_reward = 0.0,
    referee_reward = 0.0,
    reward_currency = "USD",
    reward_paid = false,
    reward_payment_date,

    %% Metadata
    date_referred,
    date_hired,

    data = #{}
}).

%% ====================================================================
%% Job Board Settings & Configuration
%% ====================================================================

-record(job_board_settings, {
    id,

    %% Global Settings
    platform_fee_percentage = 0.0,
    featured_job_price = 0.0,
    boost_job_price = 0.0,
    job_posting_duration_days = 30,

    %% Limits by Tier
    free_tier_limits = #{
        job_posts_per_month => 2,
        applications_per_job => 100,
        team_members => 1
    },

    basic_tier_limits = #{
        job_posts_per_month => 10,
        applications_per_job => 500,
        team_members => 5
    },

    professional_tier_limits = #{
        job_posts_per_month => 999,
        applications_per_job => 9999,
        team_members => 20
    },

    %% Compliance
    required_equal_opportunity_statement = true,
    gdpr_compliance_required = true,

    data = #{}
}).
