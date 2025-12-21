defmodule Mazaryn.Schema.Livestream do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:user_id, :string)
    field(:title, :string)
    field(:description, :string)
    field(:thumbnail_cid, :string)
    field(:preview_cids, {:array, :string}, default: [])
    field(:status, :string)
    field(:visibility, :string)
    field(:tags, {:array, :string}, default: [])
    field(:category, :string)
    field(:language, :string, default: "en")
    field(:stream_key, :string)
    field(:rtmp_url, :string)
    field(:rtmp_backup_url, :string)
    field(:playback_url, :string)
    field(:hls_url, :string)
    field(:dash_url, :string)
    field(:rust_stream_id, :string)
    field(:protocol, :string, default: "rtmp")
    field(:latency_mode, :string, default: "normal")
    field(:viewers_count, :integer, default: 0)
    field(:peak_viewers, :integer, default: 0)
    field(:total_unique_viewers, :integer, default: 0)
    field(:unique_viewers, {:array, :string}, default: [])
    field(:viewer_timeline, {:array, :map}, default: [])
    field(:current_bitrate, :integer, default: 0)
    field(:current_resolution, :string)
    field(:target_bitrate, :integer, default: 5_000_000)
    field(:target_resolution, :string, default: "1920x1080")
    field(:stream_quality_score, :float, default: 0.0)
    field(:dropped_frames, :integer, default: 0)
    field(:connection_quality, :string, default: "stable")
    field(:chat_enabled, :boolean, default: true)
    field(:chat_mode, :string, default: "normal")
    field(:chat_messages, {:array, :map}, default: [])
    field(:emote_only_mode, :boolean, default: false)
    field(:subscriber_only_chat, :boolean, default: false)
    field(:follower_only_chat, :boolean, default: false)
    field(:follower_only_duration, :integer, default: 0)
    field(:slow_mode, :boolean, default: false)
    field(:slow_mode_duration, :integer, default: 0)
    field(:banned_words, {:array, :string}, default: [])
    field(:chat_rules, {:array, :string}, default: [])
    field(:moderators, {:array, :string}, default: [])
    field(:vips, {:array, :string}, default: [])
    field(:banned_users, {:array, :map}, default: [])
    field(:reactions, :map, default: %{})
    field(:reaction_counts, :map, default: %{})
    field(:shares, :integer, default: 0)
    field(:saves, :integer, default: 0)
    field(:scheduled_for, :naive_datetime)
    field(:started_at, :naive_datetime)
    field(:ended_at, :naive_datetime)
    field(:duration_seconds, :integer, default: 0)
    field(:auto_record, :boolean, default: true)
    field(:recording_cid, :string)
    field(:recording_status, :string, default: "disabled")
    field(:vod_enabled, :boolean, default: true)
    field(:vod_cid, :string)
    field(:clips_enabled, :boolean, default: true)
    field(:user_generated_clips, {:array, :string}, default: [])
    field(:highlights, {:array, :map}, default: [])
    field(:auto_highlight_moments, {:array, :map}, default: [])
    field(:donations_enabled, :boolean, default: true)
    field(:donation_alerts, {:array, :map}, default: [])
    field(:donation_goals, {:array, :map}, default: [])
    field(:donations_total, :float, default: 0.0)
    field(:top_donors, {:array, :map}, default: [])
    field(:bits_enabled, :boolean, default: true)
    field(:bits_received, :integer, default: 0)
    field(:super_chat_enabled, :boolean, default: true)
    field(:super_chat_messages, {:array, :map}, default: [])
    field(:channel_points_enabled, :boolean, default: true)
    field(:point_rewards, {:array, :string}, default: [])
    field(:custom_rewards_redeemed, {:array, :map}, default: [])
    field(:subscriber_only_mode, :boolean, default: false)
    field(:subscriber_tiers, {:array, :map}, default: [])
    field(:subscriber_count, :integer, default: 0)
    field(:subscriber_badges, :map, default: %{})
    field(:raid_enabled, :boolean, default: true)
    field(:raid_source_stream_id, :string)
    field(:raid_target_stream_id, :string)
    field(:raid_viewer_count, :integer, default: 0)
    field(:host_enabled, :boolean, default: true)
    field(:hosted_channels, {:array, :string}, default: [])
    field(:simulcast_enabled, :boolean, default: false)
    field(:simulcast_destinations, {:array, :map}, default: [])
    field(:simulcast_platforms, {:array, :string}, default: [])
    field(:cross_platform_chat, :boolean, default: false)
    field(:interactive_elements, {:array, :map}, default: [])
    field(:polls_active, {:array, :map}, default: [])
    field(:interactive_hotspots, {:array, :map}, default: [])
    field(:gamification_elements, {:array, :map}, default: [])
    field(:achievements, {:array, :map}, default: [])
    field(:adaptive_bitrate_enabled, :boolean, default: true)
    field(:adaptive_bitrate_manifest, :string)
    field(:buffer_health_metrics, {:array, :map}, default: [])
    field(:stream_health_score, :float, default: 100.0)
    field(:encoding_settings, :map, default: %{})
    field(:transcoding_profiles, {:array, :map}, default: [])
    field(:dvr_enabled, :boolean, default: false)
    field(:dvr_window_seconds, :integer, default: 7200)
    field(:low_latency_enabled, :boolean, default: false)
    field(:ultra_low_latency, :boolean, default: false)
    field(:cdn_urls, :map, default: %{})
    field(:edge_servers, {:array, :string}, default: [])
    field(:multicast_enabled, :boolean, default: false)
    field(:p2p_enabled, :boolean, default: false)
    field(:peer_availability, {:array, :map}, default: [])
    field(:geo_restrictions, {:array, :string}, default: [])
    field(:age_restriction, :integer)
    field(:content_warnings, {:array, :string}, default: [])
    field(:mature_content, :boolean, default: false)
    field(:monetized, :boolean, default: false)
    field(:revenue_model, :string)
    field(:price_per_view, :float)
    field(:pay_per_view, :boolean, default: false)
    field(:subscription_required, :boolean, default: false)
    field(:ad_breaks_enabled, :boolean, default: true)
    field(:ad_break_timestamps, {:array, :integer}, default: [])
    field(:sponsor_info, :map, default: %{})
    field(:sponsored_stream, :boolean, default: false)
    field(:analytics_enabled, :boolean, default: true)
    field(:viewer_retention, {:array, :map}, default: [])
    field(:engagement_rate, :float, default: 0.0)
    field(:chat_activity_rate, :float, default: 0.0)
    field(:geographic_views, :map, default: %{})
    field(:device_breakdown, :map, default: %{})
    field(:referral_sources, :map, default: %{})
    field(:peak_concurrent_viewers_timestamp, :naive_datetime)
    field(:avg_watch_duration, :float, default: 0.0)
    field(:completion_rate, :float, default: 0.0)
    field(:return_viewer_rate, :float, default: 0.0)
    field(:new_follower_count, :integer, default: 0)
    field(:new_subscriber_count, :integer, default: 0)
    field(:clips_created_during_stream, :integer, default: 0)
    field(:viral_moments, {:array, :map}, default: [])
    field(:trending_score, :float, default: 0.0)
    field(:discovery_score, :float, default: 0.0)
    field(:recommended_to, {:array, :string}, default: [])
    field(:featured, :boolean, default: false)
    field(:featured_until, :naive_datetime)
    field(:platform_promoted, :boolean, default: false)
    field(:collab_mode, :boolean, default: false)
    field(:collaboration_participants, {:array, :string}, default: [])
    field(:multi_stream_enabled, :boolean, default: false)
    field(:co_streamers, {:array, :string}, default: [])
    field(:split_screen_layout, :string)
    field(:picture_in_picture, :boolean, default: false)
    field(:screen_share_enabled, :boolean, default: true)
    field(:webcam_enabled, :boolean, default: true)
    field(:audio_only_mode, :boolean, default: false)
    field(:spatial_audio, :boolean, default: false)
    field(:binaural_audio, :boolean, default: false)
    field(:audio_quality, :string, default: "high")
    field(:video_filters, {:array, :string}, default: [])
    field(:effects_used, {:array, :string}, default: [])
    field(:overlays, {:array, :map}, default: [])
    field(:custom_graphics, {:array, :string}, default: [])
    field(:green_screen_enabled, :boolean, default: false)
    field(:background_replacement_cid, :string)
    field(:virtual_background, :boolean, default: false)
    field(:ar_effects, {:array, :string}, default: [])
    field(:face_tracking, :boolean, default: false)
    field(:auto_framing, :boolean, default: false)
    field(:noise_suppression, :boolean, default: true)
    field(:echo_cancellation, :boolean, default: true)
    field(:auto_volume, :boolean, default: true)
    field(:accessibility_features, :map, default: %{})
    field(:closed_captions_enabled, :boolean, default: false)
    field(:auto_captions, :boolean, default: false)
    field(:caption_language, :string, default: "en")
    field(:sign_language_interpreter, :boolean, default: false)
    field(:audio_description, :boolean, default: false)
    field(:high_contrast_mode, :boolean, default: false)
    field(:metadata_tags, {:array, :string}, default: [])
    field(:searchable_keywords, {:array, :string}, default: [])
    field(:seo_optimized, :boolean, default: false)
    field(:thumbnail_ab_test, :boolean, default: false)
    field(:title_ab_test, :boolean, default: false)
    field(:notification_sent, :boolean, default: false)
    field(:notification_sent_to, {:array, :string}, default: [])
    field(:push_notifications_count, :integer, default: 0)
    field(:email_notifications_count, :integer, default: 0)
    field(:scheduled_reminders, {:array, :map}, default: [])
    field(:countdown_enabled, :boolean, default: false)
    field(:countdown_duration, :integer, default: 0)
    field(:pre_stream_lobby, :boolean, default: false)
    field(:post_stream_screen, :boolean, default: false)
    field(:end_screen_cid, :string)
    field(:replay_available, :boolean, default: true)
    field(:replay_cid, :string)
    field(:vod_available_until, :naive_datetime)
    field(:auto_delete_after, :integer, default: 0)
    field(:archive_enabled, :boolean, default: false)
    field(:archive_cid, :string)
    field(:nft_minted, :boolean, default: false)
    field(:nft_contract_address, :string)
    field(:nft_token_id, :string)
    field(:nft_metadata_cid, :string)
    field(:blockchain_proof_cid, :string)
    field(:token_gated, :boolean, default: false)
    field(:token_requirement, :string)
    field(:fan_tier_access, :map, default: %{})
    field(:early_access_enabled, :boolean, default: false)
    field(:exclusive_for_members, :boolean, default: false)
    field(:community_only, :boolean, default: false)
    field(:stream_series_id, :string)
    field(:episode_number, :integer)
    field(:season_number, :integer)
    field(:recurring_schedule, :string)
    field(:next_stream_date, :naive_datetime)
    field(:stream_template_id, :string)
    field(:brand_partnership, :boolean, default: false)
    field(:brand_partner_info, :map, default: %{})
    field(:product_placement, {:array, :map}, default: [])
    field(:affiliate_links, {:array, :map}, default: [])
    field(:merchandise_showcase, {:array, :map}, default: [])
    field(:reported, {:array, :string}, default: [])
    field(:moderation_status, :string)
    field(:ai_moderation_enabled, :boolean, default: true)
    field(:auto_ban_enabled, :boolean, default: false)
    field(:toxicity_filter_level, :string, default: "medium")
    field(:sentiment_analysis, :map, default: %{})
    field(:stream_title_changes, {:array, :map}, default: [])
    field(:category_changes, {:array, :map}, default: [])
    field(:quality_changes_log, {:array, :map}, default: [])
    field(:technical_issues_log, {:array, :map}, default: [])
    field(:viewer_feedback, {:array, :map}, default: [])
    field(:polls_results, {:array, :map}, default: [])
    field(:quiz_results, {:array, :map}, default: [])
    field(:predictions, {:array, :map}, default: [])
    field(:prediction_results, {:array, :map}, default: [])
    field(:betting_enabled, :boolean, default: false)
    field(:watch_parties, {:array, :string}, default: [])
    field(:synchronized_viewing_rooms, {:array, :string}, default: [])
    field(:virtual_theater_mode, :boolean, default: false)
    field(:watch_together_enabled, :boolean, default: false)
    field(:reaction_videos, {:array, :string}, default: [])
    field(:duet_enabled, :boolean, default: false)
    field(:stitch_enabled, :boolean, default: false)
    field(:remix_enabled, :boolean, default: false)
    field(:clip_of_stream_winner, :string)
    field(:mvp_viewer, :string)
    field(:most_active_chatter, :string)
    field(:biggest_donor, :string)
    field(:loyalty_points_distributed, :integer, default: 0)
    field(:xp_earned_by_viewers, :map, default: %{})
    field(:achievements_unlocked, {:array, :map}, default: [])
    field(:leaderboard_position, :integer)
    field(:competitive_ranking, :string)
    field(:esports_tournament, :boolean, default: false)
    field(:tournament_info, :map, default: %{})
    field(:match_id, :string)
    field(:game_being_played, :string)
    field(:game_metadata, :map, default: %{})
    field(:stream_companion_app, :boolean, default: false)
    field(:second_screen_experience, :boolean, default: false)
    field(:mobile_companion_features, {:array, :string}, default: [])
    field(:api_webhooks, {:array, :string}, default: [])
    field(:integration_enabled, {:array, :string}, default: [])
    field(:third_party_bots, {:array, :string}, default: [])
    field(:stream_deck_integration, :boolean, default: false)
    field(:obs_plugin_active, :boolean, default: false)
    field(:streamlabs_connected, :boolean, default: false)
    field(:automated_scenes, {:array, :map}, default: [])
    field(:scene_transitions, {:array, :string}, default: [])
    field(:production_quality_score, :float, default: 0.0)
    field(:professional_setup, :boolean, default: false)
    field(:studio_mode, :boolean, default: false)
    field(:multi_camera_setup, :boolean, default: false)
    field(:camera_angles, {:array, :string}, default: [])
    field(:instant_replay_available, :boolean, default: false)
    field(:slow_motion_capable, :boolean, default: false)
    field(:time_travel_enabled, :boolean, default: false)
    field(:dvr_controls, :map, default: %{})
    field(:emergency_backup_stream, :string)
    field(:failover_enabled, :boolean, default: false)
    field(:redundancy_servers, {:array, :string}, default: [])
    field(:uptime_percentage, :float, default: 100.0)
    field(:connection_drops, :integer, default: 0)
    field(:reconnection_attempts, :integer, default: 0)
    field(:stream_stability_score, :float, default: 100.0)
    field(:optimal_settings_applied, :boolean, default: false)
    field(:bandwidth_test_results, :map, default: %{})
    field(:network_conditions, :string, default: "stable")
    field(:isp_info, :string)
    field(:server_location, :string)
    field(:optimal_ingest_server, :string)
    field(:custom_rtmp_server, :string)
    field(:rtmps_enabled, :boolean, default: false)
    field(:srt_enabled, :boolean, default: false)
    field(:webrtc_enabled, :boolean, default: false)
    field(:stream_protocol_version, :string)
    field(:encryption_enabled, :boolean, default: false)
    field(:drm_protected, :boolean, default: false)
    field(:watermark_enabled, :boolean, default: false)
    field(:watermark_cid, :string)
    field(:copyright_music_detected, {:array, :string}, default: [])
    field(:copyright_claims, {:array, :map}, default: [])
    field(:dmca_safe, :boolean, default: true)
    field(:royalty_free_music, {:array, :string}, default: [])
    field(:licensed_music, {:array, :string}, default: [])
    field(:music_attribution, {:array, :map}, default: [])
    field(:original_content_certified, :boolean, default: false)
    field(:verification_badge, :boolean, default: false)
    field(:partner_status, :boolean, default: false)
    field(:affiliate_status, :boolean, default: false)
    field(:creator_tier, :string)
    field(:follower_milestone_reached, {:array, :integer}, default: [])
    field(:subscriber_milestone_reached, {:array, :integer}, default: [])
    field(:view_milestone_reached, {:array, :integer}, default: [])
    field(:stream_anniversary, :boolean, default: false)
    field(:special_events, {:array, :map}, default: [])
    field(:charity_stream, :boolean, default: false)
    field(:charity_info, :map, default: %{})
    field(:fundraising_goal, :float)
    field(:amount_raised, :float, default: 0.0)
    field(:donation_matching, :boolean, default: false)
    field(:collaboration_tree, {:array, :string}, default: [])
    field(:guest_appearances, {:array, :map}, default: [])
    field(:interview_mode, :boolean, default: false)
    field(:podcast_mode, :boolean, default: false)
    field(:educational_content, :boolean, default: false)
    field(:tutorial_stream, :boolean, default: false)
    field(:workshop_stream, :boolean, default: false)
    field(:qa_session, :boolean, default: false)
    field(:ama_mode, :boolean, default: false)
    field(:behind_the_scenes, :boolean, default: false)
    field(:irl_stream, :boolean, default: false)
    field(:outdoor_stream, :boolean, default: false)
    field(:mobile_streaming, :boolean, default: false)
    field(:travel_stream, :boolean, default: false)
    field(:location_tagging, {:array, :string}, default: [])
    field(:gps_coordinates, :string)
    field(:venue_info, :map, default: %{})
    field(:event_coverage, :boolean, default: false)
    field(:event_name, :string)
    field(:date_created, :naive_datetime)
    field(:data, :map, default: %{})
  end

  def erl_changeset(
        {:livestream, id, user_id, title, description, thumbnail_cid, preview_cids, status,
         visibility, tags, category, language, stream_key, rtmp_url, rtmp_backup_url,
         playback_url, hls_url, dash_url, rust_stream_id, protocol, latency_mode, viewers_count,
         peak_viewers, total_unique_viewers, unique_viewers, viewer_timeline, current_bitrate,
         current_resolution, target_bitrate, target_resolution, stream_quality_score,
         dropped_frames, connection_quality, chat_enabled, chat_mode, chat_messages,
         emote_only_mode, subscriber_only_chat, follower_only_chat, follower_only_duration,
         slow_mode, slow_mode_duration, banned_words, chat_rules, moderators, vips, banned_users,
         reactions, reaction_counts, shares, saves, scheduled_for, started_at, ended_at,
         duration_seconds, auto_record, recording_cid, recording_status, vod_enabled, vod_cid,
         clips_enabled, user_generated_clips, highlights, auto_highlight_moments,
         donations_enabled, donation_alerts, donation_goals, donations_total, top_donors,
         bits_enabled, bits_received, super_chat_enabled, super_chat_messages,
         channel_points_enabled, point_rewards, custom_rewards_redeemed, subscriber_only_mode,
         subscriber_tiers, subscriber_count, subscriber_badges, raid_enabled,
         raid_source_stream_id, raid_target_stream_id, raid_viewer_count, host_enabled,
         hosted_channels, simulcast_enabled, simulcast_destinations, simulcast_platforms,
         cross_platform_chat, interactive_elements, polls_active, interactive_hotspots,
         gamification_elements, achievements, adaptive_bitrate_enabled, adaptive_bitrate_manifest,
         buffer_health_metrics, stream_health_score, encoding_settings, transcoding_profiles,
         dvr_enabled, dvr_window_seconds, low_latency_enabled, ultra_low_latency, cdn_urls,
         edge_servers, multicast_enabled, p2p_enabled, peer_availability, geo_restrictions,
         age_restriction, content_warnings, mature_content, monetized, revenue_model,
         price_per_view, pay_per_view, subscription_required, ad_breaks_enabled,
         ad_break_timestamps, sponsor_info, sponsored_stream, analytics_enabled, viewer_retention,
         engagement_rate, chat_activity_rate, geographic_views, device_breakdown,
         referral_sources, peak_concurrent_viewers_timestamp, avg_watch_duration, completion_rate,
         return_viewer_rate, new_follower_count, new_subscriber_count,
         clips_created_during_stream, viral_moments, trending_score, discovery_score,
         recommended_to, featured, featured_until, platform_promoted, collab_mode,
         collaboration_participants, multi_stream_enabled, co_streamers, split_screen_layout,
         picture_in_picture, screen_share_enabled, webcam_enabled, audio_only_mode, spatial_audio,
         binaural_audio, audio_quality, video_filters, effects_used, overlays, custom_graphics,
         green_screen_enabled, background_replacement_cid, virtual_background, ar_effects,
         face_tracking, auto_framing, noise_suppression, echo_cancellation, auto_volume,
         accessibility_features, closed_captions_enabled, auto_captions, caption_language,
         sign_language_interpreter, audio_description, high_contrast_mode, metadata_tags,
         searchable_keywords, seo_optimized, thumbnail_ab_test, title_ab_test, notification_sent,
         notification_sent_to, push_notifications_count, email_notifications_count,
         scheduled_reminders, countdown_enabled, countdown_duration, pre_stream_lobby,
         post_stream_screen, end_screen_cid, replay_available, replay_cid, vod_available_until,
         auto_delete_after, archive_enabled, archive_cid, nft_minted, nft_contract_address,
         nft_token_id, nft_metadata_cid, blockchain_proof_cid, token_gated, token_requirement,
         fan_tier_access, early_access_enabled, exclusive_for_members, community_only,
         stream_series_id, episode_number, season_number, recurring_schedule, next_stream_date,
         stream_template_id, brand_partnership, brand_partner_info, product_placement,
         affiliate_links, merchandise_showcase, reported, moderation_status,
         ai_moderation_enabled, auto_ban_enabled, toxicity_filter_level, sentiment_analysis,
         stream_title_changes, category_changes, quality_changes_log, technical_issues_log,
         viewer_feedback, polls_results, quiz_results, predictions, prediction_results,
         betting_enabled, watch_parties, synchronized_viewing_rooms, virtual_theater_mode,
         watch_together_enabled, reaction_videos, duet_enabled, stitch_enabled, remix_enabled,
         clip_of_stream_winner, mvp_viewer, most_active_chatter, biggest_donor,
         loyalty_points_distributed, xp_earned_by_viewers, achievements_unlocked,
         leaderboard_position, competitive_ranking, esports_tournament, tournament_info, match_id,
         game_being_played, game_metadata, stream_companion_app, second_screen_experience,
         mobile_companion_features, api_webhooks, integration_enabled, third_party_bots,
         stream_deck_integration, obs_plugin_active, streamlabs_connected, automated_scenes,
         scene_transitions, production_quality_score, professional_setup, studio_mode,
         multi_camera_setup, camera_angles, instant_replay_available, slow_motion_capable,
         time_travel_enabled, dvr_controls, emergency_backup_stream, failover_enabled,
         redundancy_servers, uptime_percentage, connection_drops, reconnection_attempts,
         stream_stability_score, optimal_settings_applied, bandwidth_test_results,
         network_conditions, isp_info, server_location, optimal_ingest_server, custom_rtmp_server,
         rtmps_enabled, srt_enabled, webrtc_enabled, stream_protocol_version, encryption_enabled,
         drm_protected, watermark_enabled, watermark_cid, copyright_music_detected,
         copyright_claims, dmca_safe, royalty_free_music, licensed_music, music_attribution,
         original_content_certified, verification_badge, partner_status, affiliate_status,
         creator_tier, follower_milestone_reached, subscriber_milestone_reached,
         view_milestone_reached, stream_anniversary, special_events, charity_stream, charity_info,
         fundraising_goal, amount_raised, donation_matching, collaboration_tree,
         guest_appearances, interview_mode, podcast_mode, educational_content, tutorial_stream,
         workshop_stream, qa_session, ama_mode, behind_the_scenes, irl_stream, outdoor_stream,
         mobile_streaming, travel_stream, location_tagging, gps_coordinates, venue_info,
         event_coverage, event_name, date_created, data}
      ) do
    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      user_id: to_string(user_id),
      title: to_string(title),
      description: process_string(description),
      thumbnail_cid: process_string(thumbnail_cid),
      preview_cids: process_string_list(preview_cids),
      status: atom_to_string(status),
      visibility: atom_to_string(visibility),
      tags: process_string_list(tags),
      category: to_string(category),
      language: process_string(language),
      stream_key: to_string(stream_key),
      rtmp_url: to_string(rtmp_url),
      rtmp_backup_url: to_string(rtmp_backup_url),
      playback_url: process_string(playback_url),
      hls_url: process_string(hls_url),
      dash_url: process_string(dash_url),
      rust_stream_id: process_string(rust_stream_id),
      protocol: atom_to_string(protocol),
      latency_mode: atom_to_string(latency_mode),
      viewers_count: viewers_count,
      peak_viewers: peak_viewers,
      total_unique_viewers: total_unique_viewers,
      unique_viewers: process_string_list(unique_viewers),
      viewer_timeline: process_viewer_timeline(viewer_timeline),
      current_bitrate: current_bitrate,
      current_resolution: process_string(current_resolution),
      target_bitrate: target_bitrate,
      target_resolution: to_string(target_resolution),
      stream_quality_score: stream_quality_score,
      dropped_frames: dropped_frames,
      connection_quality: atom_to_string(connection_quality),
      chat_enabled: chat_enabled,
      chat_mode: atom_to_string(chat_mode),
      chat_messages: process_chat_messages(chat_messages),
      emote_only_mode: emote_only_mode,
      subscriber_only_chat: subscriber_only_chat,
      follower_only_chat: follower_only_chat,
      follower_only_duration: follower_only_duration,
      slow_mode: slow_mode,
      slow_mode_duration: slow_mode_duration,
      banned_words: process_string_list(banned_words),
      chat_rules: process_string_list(chat_rules),
      moderators: process_string_list(moderators),
      vips: process_string_list(vips),
      banned_users: process_banned_users(banned_users),
      reactions: process_reactions(reactions),
      reaction_counts: process_reaction_counts(reaction_counts),
      shares: shares,
      saves: saves,
      scheduled_for: handle_datetime(scheduled_for),
      started_at: handle_datetime(started_at),
      ended_at: handle_datetime(ended_at),
      duration_seconds: duration_seconds,
      auto_record: auto_record,
      recording_cid: process_string(recording_cid),
      recording_status: atom_to_string(recording_status),
      vod_enabled: vod_enabled,
      vod_cid: process_string(vod_cid),
      clips_enabled: clips_enabled,
      user_generated_clips: process_string_list(user_generated_clips),
      highlights: process_list(highlights),
      auto_highlight_moments: process_list(auto_highlight_moments),
      donations_enabled: donations_enabled,
      donation_alerts: process_list(donation_alerts),
      donation_goals: process_list(donation_goals),
      donations_total: donations_total,
      top_donors: process_list(top_donors),
      bits_enabled: bits_enabled,
      bits_received: bits_received,
      super_chat_enabled: super_chat_enabled,
      super_chat_messages: process_list(super_chat_messages),
      channel_points_enabled: channel_points_enabled,
      point_rewards: process_string_list(point_rewards),
      custom_rewards_redeemed: process_list(custom_rewards_redeemed),
      subscriber_only_mode: subscriber_only_mode,
      subscriber_tiers: process_list(subscriber_tiers),
      subscriber_count: subscriber_count,
      subscriber_badges: process_map(subscriber_badges),
      raid_enabled: raid_enabled,
      raid_source_stream_id: process_string(raid_source_stream_id),
      raid_target_stream_id: process_string(raid_target_stream_id),
      raid_viewer_count: raid_viewer_count,
      host_enabled: host_enabled,
      hosted_channels: process_string_list(hosted_channels),
      simulcast_enabled: simulcast_enabled,
      simulcast_destinations: process_list(simulcast_destinations),
      simulcast_platforms: process_string_list(simulcast_platforms),
      cross_platform_chat: cross_platform_chat,
      interactive_elements: process_list(interactive_elements),
      polls_active: process_list(polls_active),
      interactive_hotspots: process_list(interactive_hotspots),
      gamification_elements: process_list(gamification_elements),
      achievements: process_list(achievements),
      adaptive_bitrate_enabled: adaptive_bitrate_enabled,
      adaptive_bitrate_manifest: process_string(adaptive_bitrate_manifest),
      buffer_health_metrics: process_list(buffer_health_metrics),
      stream_health_score: stream_health_score,
      encoding_settings: process_map(encoding_settings),
      transcoding_profiles: process_list(transcoding_profiles),
      dvr_enabled: dvr_enabled,
      dvr_window_seconds: dvr_window_seconds,
      low_latency_enabled: low_latency_enabled,
      ultra_low_latency: ultra_low_latency,
      cdn_urls: process_map(cdn_urls),
      edge_servers: process_string_list(edge_servers),
      multicast_enabled: multicast_enabled,
      p2p_enabled: p2p_enabled,
      peer_availability: process_list(peer_availability),
      geo_restrictions: process_string_list(geo_restrictions),
      age_restriction: age_restriction,
      content_warnings: process_string_list(content_warnings),
      mature_content: mature_content,
      monetized: monetized,
      revenue_model: atom_to_string(revenue_model),
      price_per_view: price_per_view,
      pay_per_view: pay_per_view,
      subscription_required: subscription_required,
      ad_breaks_enabled: ad_breaks_enabled,
      ad_break_timestamps: process_integer_list(ad_break_timestamps),
      sponsor_info: process_map(sponsor_info),
      sponsored_stream: sponsored_stream,
      analytics_enabled: analytics_enabled,
      viewer_retention: process_list(viewer_retention),
      engagement_rate: engagement_rate,
      chat_activity_rate: chat_activity_rate,
      geographic_views: process_map(geographic_views),
      device_breakdown: process_map(device_breakdown),
      referral_sources: process_map(referral_sources),
      peak_concurrent_viewers_timestamp: handle_datetime(peak_concurrent_viewers_timestamp),
      avg_watch_duration: avg_watch_duration,
      completion_rate: completion_rate,
      return_viewer_rate: return_viewer_rate,
      new_follower_count: new_follower_count,
      new_subscriber_count: new_subscriber_count,
      clips_created_during_stream: clips_created_during_stream,
      viral_moments: process_list(viral_moments),
      trending_score: trending_score,
      discovery_score: discovery_score,
      recommended_to: process_string_list(recommended_to),
      featured: featured,
      featured_until: handle_datetime(featured_until),
      platform_promoted: platform_promoted,
      collab_mode: collab_mode,
      collaboration_participants: process_string_list(collaboration_participants),
      multi_stream_enabled: multi_stream_enabled,
      co_streamers: process_string_list(co_streamers),
      split_screen_layout: process_string(split_screen_layout),
      picture_in_picture: picture_in_picture,
      screen_share_enabled: screen_share_enabled,
      webcam_enabled: webcam_enabled,
      audio_only_mode: audio_only_mode,
      spatial_audio: spatial_audio,
      binaural_audio: binaural_audio,
      audio_quality: atom_to_string(audio_quality),
      video_filters: process_string_list(video_filters),
      effects_used: process_string_list(effects_used),
      overlays: process_list(overlays),
      custom_graphics: process_string_list(custom_graphics),
      green_screen_enabled: green_screen_enabled,
      background_replacement_cid: process_string(background_replacement_cid),
      virtual_background: virtual_background,
      ar_effects: process_string_list(ar_effects),
      face_tracking: face_tracking,
      auto_framing: auto_framing,
      noise_suppression: noise_suppression,
      echo_cancellation: echo_cancellation,
      auto_volume: auto_volume,
      accessibility_features: process_map(accessibility_features),
      closed_captions_enabled: closed_captions_enabled,
      auto_captions: auto_captions,
      caption_language: to_string(caption_language),
      sign_language_interpreter: sign_language_interpreter,
      audio_description: audio_description,
      high_contrast_mode: high_contrast_mode,
      metadata_tags: process_string_list(metadata_tags),
      searchable_keywords: process_string_list(searchable_keywords),
      seo_optimized: seo_optimized,
      thumbnail_ab_test: thumbnail_ab_test,
      title_ab_test: title_ab_test,
      notification_sent: notification_sent,
      notification_sent_to: process_string_list(notification_sent_to),
      push_notifications_count: push_notifications_count,
      email_notifications_count: email_notifications_count,
      scheduled_reminders: process_list(scheduled_reminders),
      countdown_enabled: countdown_enabled,
      countdown_duration: countdown_duration,
      pre_stream_lobby: pre_stream_lobby,
      post_stream_screen: post_stream_screen,
      end_screen_cid: process_string(end_screen_cid),
      replay_available: replay_available,
      replay_cid: process_string(replay_cid),
      vod_available_until: handle_datetime(vod_available_until),
      auto_delete_after: auto_delete_after,
      archive_enabled: archive_enabled,
      archive_cid: process_string(archive_cid),
      nft_minted: nft_minted,
      nft_contract_address: process_string(nft_contract_address),
      nft_token_id: process_string(nft_token_id),
      nft_metadata_cid: process_string(nft_metadata_cid),
      blockchain_proof_cid: process_string(blockchain_proof_cid),
      token_gated: token_gated,
      token_requirement: process_string(token_requirement),
      fan_tier_access: process_map(fan_tier_access),
      early_access_enabled: early_access_enabled,
      exclusive_for_members: exclusive_for_members,
      community_only: community_only,
      stream_series_id: process_string(stream_series_id),
      episode_number: episode_number,
      season_number: season_number,
      recurring_schedule: process_string(recurring_schedule),
      next_stream_date: handle_datetime(next_stream_date),
      stream_template_id: process_string(stream_template_id),
      brand_partnership: brand_partnership,
      brand_partner_info: process_map(brand_partner_info),
      product_placement: process_list(product_placement),
      affiliate_links: process_list(affiliate_links),
      merchandise_showcase: process_list(merchandise_showcase),
      reported: process_string_list(reported),
      moderation_status: atom_to_string(moderation_status),
      ai_moderation_enabled: ai_moderation_enabled,
      auto_ban_enabled: auto_ban_enabled,
      toxicity_filter_level: atom_to_string(toxicity_filter_level),
      sentiment_analysis: process_map(sentiment_analysis),
      stream_title_changes: process_list(stream_title_changes),
      category_changes: process_list(category_changes),
      quality_changes_log: process_list(quality_changes_log),
      technical_issues_log: process_list(technical_issues_log),
      viewer_feedback: process_list(viewer_feedback),
      polls_results: process_list(polls_results),
      quiz_results: process_list(quiz_results),
      predictions: process_list(predictions),
      prediction_results: process_list(prediction_results),
      betting_enabled: betting_enabled,
      watch_parties: process_string_list(watch_parties),
      synchronized_viewing_rooms: process_string_list(synchronized_viewing_rooms),
      virtual_theater_mode: virtual_theater_mode,
      watch_together_enabled: watch_together_enabled,
      reaction_videos: process_string_list(reaction_videos),
      duet_enabled: duet_enabled,
      stitch_enabled: stitch_enabled,
      remix_enabled: remix_enabled,
      clip_of_stream_winner: process_string(clip_of_stream_winner),
      mvp_viewer: process_string(mvp_viewer),
      most_active_chatter: process_string(most_active_chatter),
      biggest_donor: process_string(biggest_donor),
      loyalty_points_distributed: loyalty_points_distributed,
      xp_earned_by_viewers: process_map(xp_earned_by_viewers),
      achievements_unlocked: process_list(achievements_unlocked),
      leaderboard_position: leaderboard_position,
      competitive_ranking: process_string(competitive_ranking),
      esports_tournament: esports_tournament,
      tournament_info: process_map(tournament_info),
      match_id: process_string(match_id),
      game_being_played: process_string(game_being_played),
      game_metadata: process_map(game_metadata),
      stream_companion_app: stream_companion_app,
      second_screen_experience: second_screen_experience,
      mobile_companion_features: process_string_list(mobile_companion_features),
      api_webhooks: process_string_list(api_webhooks),
      integration_enabled: process_string_list(integration_enabled),
      third_party_bots: process_string_list(third_party_bots),
      stream_deck_integration: stream_deck_integration,
      obs_plugin_active: obs_plugin_active,
      streamlabs_connected: streamlabs_connected,
      automated_scenes: process_list(automated_scenes),
      scene_transitions: process_string_list(scene_transitions),
      production_quality_score: production_quality_score,
      professional_setup: professional_setup,
      studio_mode: studio_mode,
      multi_camera_setup: multi_camera_setup,
      camera_angles: process_string_list(camera_angles),
      instant_replay_available: instant_replay_available,
      slow_motion_capable: slow_motion_capable,
      time_travel_enabled: time_travel_enabled,
      dvr_controls: process_map(dvr_controls),
      emergency_backup_stream: process_string(emergency_backup_stream),
      failover_enabled: failover_enabled,
      redundancy_servers: process_string_list(redundancy_servers),
      uptime_percentage: uptime_percentage,
      connection_drops: connection_drops,
      reconnection_attempts: reconnection_attempts,
      stream_stability_score: stream_stability_score,
      optimal_settings_applied: optimal_settings_applied,
      bandwidth_test_results: process_map(bandwidth_test_results),
      network_conditions: atom_to_string(network_conditions),
      isp_info: process_string(isp_info),
      server_location: process_string(server_location),
      optimal_ingest_server: process_string(optimal_ingest_server),
      custom_rtmp_server: process_string(custom_rtmp_server),
      rtmps_enabled: rtmps_enabled,
      srt_enabled: srt_enabled,
      webrtc_enabled: webrtc_enabled,
      stream_protocol_version: process_string(stream_protocol_version),
      encryption_enabled: encryption_enabled,
      drm_protected: drm_protected,
      watermark_enabled: watermark_enabled,
      watermark_cid: process_string(watermark_cid),
      copyright_music_detected: process_string_list(copyright_music_detected),
      copyright_claims: process_list(copyright_claims),
      dmca_safe: dmca_safe,
      royalty_free_music: process_string_list(royalty_free_music),
      licensed_music: process_string_list(licensed_music),
      music_attribution: process_list(music_attribution),
      original_content_certified: original_content_certified,
      verification_badge: verification_badge,
      partner_status: partner_status,
      affiliate_status: affiliate_status,
      creator_tier: process_string(creator_tier),
      follower_milestone_reached: process_integer_list(follower_milestone_reached),
      subscriber_milestone_reached: process_integer_list(subscriber_milestone_reached),
      view_milestone_reached: process_integer_list(view_milestone_reached),
      stream_anniversary: stream_anniversary,
      special_events: process_list(special_events),
      charity_stream: charity_stream,
      charity_info: process_map(charity_info),
      fundraising_goal: fundraising_goal,
      amount_raised: amount_raised,
      donation_matching: donation_matching,
      collaboration_tree: process_string_list(collaboration_tree),
      guest_appearances: process_list(guest_appearances),
      interview_mode: interview_mode,
      podcast_mode: podcast_mode,
      educational_content: educational_content,
      tutorial_stream: tutorial_stream,
      workshop_stream: workshop_stream,
      qa_session: qa_session,
      ama_mode: ama_mode,
      behind_the_scenes: behind_the_scenes,
      irl_stream: irl_stream,
      outdoor_stream: outdoor_stream,
      mobile_streaming: mobile_streaming,
      travel_stream: travel_stream,
      location_tagging: process_string_list(location_tagging),
      gps_coordinates: process_string(gps_coordinates),
      venue_info: process_map(venue_info),
      event_coverage: event_coverage,
      event_name: process_string(event_name),
      date_created: handle_datetime(date_created),
      data: process_map(data)
    })
  end

  def erl_changeset(_), do: change(%__MODULE__{}, %{})

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil

  defp handle_datetime({{year, month, day}, {hour, minute, second}}) do
    case NaiveDateTime.new(year, month, day, hour, minute, second) do
      {:ok, datetime} -> datetime
      _ -> nil
    end
  end

  defp handle_datetime(datetime) when is_struct(datetime, NaiveDateTime), do: datetime
  defp handle_datetime(_), do: nil

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(list) when is_list(list), do: to_string(list)
  defp atom_to_string(_), do: nil

  defp process_string(:undefined), do: nil
  defp process_string(nil), do: nil
  defp process_string(value) when is_binary(value), do: value
  defp process_string(value) when is_list(value), do: to_string(value)
  defp process_string(_), do: nil

  defp process_string_list(list) when is_list(list) do
    Enum.map(list, fn
      item when is_binary(item) -> item
      item when is_list(item) -> to_string(item)
      item when is_atom(item) -> Atom.to_string(item)
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_string_list(:undefined), do: []
  defp process_string_list(nil), do: []
  defp process_string_list(_), do: []

  defp process_integer_list(list) when is_list(list), do: list
  defp process_integer_list(:undefined), do: []
  defp process_integer_list(nil), do: []
  defp process_integer_list(_), do: []

  defp process_map(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, acc ->
      string_key =
        case key do
          k when is_atom(k) -> Atom.to_string(k)
          k when is_binary(k) -> k
          k when is_list(k) -> to_string(k)
          k -> inspect(k)
        end

      Map.put(acc, string_key, value)
    end)
  end

  defp process_map(:undefined), do: %{}
  defp process_map(nil), do: %{}
  defp process_map(_), do: %{}

  defp process_list(list) when is_list(list), do: list
  defp process_list(:undefined), do: []
  defp process_list(nil), do: []
  defp process_list(_), do: []

  defp process_reactions(reactions) when is_map(reactions) do
    Enum.reduce(reactions, %{}, fn {type, ids}, acc ->
      processed_ids = process_string_list(ids)
      Map.put(acc, atom_to_string(type), processed_ids)
    end)
  end

  defp process_reactions(_), do: %{}

  defp process_reaction_counts(counts) when is_map(counts) do
    Enum.reduce(counts, %{}, fn {type, count}, acc ->
      Map.put(acc, atom_to_string(type), count)
    end)
  end

  defp process_reaction_counts(_), do: %{}

  defp process_viewer_timeline(timeline) when is_list(timeline) do
    Enum.map(timeline, fn
      {viewer_id, action, timestamp} ->
        %{
          viewer_id: to_string(viewer_id),
          action: atom_to_string(action),
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_viewer_timeline(_), do: []

  defp process_chat_messages(messages) when is_list(messages) do
    Enum.map(messages, fn
      {msg_id, user_id, username, message, timestamp} ->
        %{
          id: to_string(msg_id),
          user_id: to_string(user_id),
          username: to_string(username),
          message: to_string(message),
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_chat_messages(_), do: []

  defp process_banned_users(users) when is_list(users) do
    Enum.map(users, fn
      {user_id, reason, timestamp} ->
        %{
          user_id: to_string(user_id),
          reason: to_string(reason),
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_banned_users(_), do: []

  def changeset(livestream, attrs \\ %{}) do
    livestream
    |> cast(attrs, [
      :id,
      :user_id,
      :title,
      :description,
      :thumbnail_cid,
      :preview_cids,
      :status,
      :visibility,
      :tags,
      :category,
      :language,
      :stream_key,
      :rtmp_url,
      :rtmp_backup_url,
      :playback_url,
      :hls_url,
      :dash_url,
      :rust_stream_id,
      :protocol,
      :latency_mode,
      :viewers_count,
      :peak_viewers,
      :total_unique_viewers,
      :unique_viewers,
      :viewer_timeline,
      :current_bitrate,
      :current_resolution,
      :target_bitrate,
      :target_resolution,
      :stream_quality_score,
      :dropped_frames,
      :connection_quality,
      :chat_enabled,
      :chat_mode,
      :chat_messages,
      :emote_only_mode,
      :subscriber_only_chat,
      :follower_only_chat,
      :follower_only_duration,
      :slow_mode,
      :slow_mode_duration,
      :banned_words,
      :chat_rules,
      :moderators,
      :vips,
      :banned_users,
      :reactions,
      :reaction_counts,
      :shares,
      :saves,
      :scheduled_for,
      :started_at,
      :ended_at,
      :duration_seconds,
      :date_created,
      :data
    ])
  end

  def is_live?(%__MODULE__{status: "live"}), do: true
  def is_live?(_), do: false

  def is_scheduled?(%__MODULE__{status: "scheduled"}), do: true
  def is_scheduled?(_), do: false

  def is_ended?(%__MODULE__{status: "ended"}), do: true
  def is_ended?(_), do: false

  def format_duration(nil), do: "N/A"

  def format_duration(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    "#{hours}:#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  def format_duration(_), do: "N/A"

  def format_viewers(viewers) when is_integer(viewers) do
    cond do
      viewers >= 1_000_000 -> "#{Float.round(viewers / 1_000_000, 1)}M"
      viewers >= 1_000 -> "#{Float.round(viewers / 1_000, 1)}K"
      true -> Integer.to_string(viewers)
    end
  end

  def format_viewers(_), do: "0"

  def total_reactions(%__MODULE__{reaction_counts: counts}) when is_map(counts) do
    Enum.reduce(counts, 0, fn {_type, count}, acc -> acc + count end)
  end

  def total_reactions(_), do: 0
end
