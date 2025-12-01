defmodule Mazaryn.Schema.Video do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    # field(:id, :string)
    field(:ai_video_id, :string)
    field(:user_id, :string)
    field(:business_id, {:array, :string}, default: [])
    field(:file_url, :string)
    field(:ipfs_cid, :string)
    field(:thumbnail_cids, {:array, :string}, default: [])
    field(:preview_cid, :string)
    field(:original_filename, :string)
    field(:file_size_bytes, :integer)
    field(:status, :string)
    field(:original_resolution, :string)
    field(:available_qualities, {:array, :map}, default: [])
    field(:codec, :string)
    field(:bitrate, :integer)
    field(:frame_rate, :float)
    field(:aspect_ratio, :string)
    field(:title, :string)
    field(:description, :string)
    field(:duration_seconds, :integer)
    field(:thumbnail_url, :string)
    field(:thumbnail_timestamp, :integer, default: 0)
    field(:preview_urls, {:array, :string}, default: [])
    field(:chapters, {:array, :map}, default: [])
    field(:subtitles, {:array, :map}, default: [])
    field(:audio_tracks, {:array, :map}, default: [])
    field(:interactive_elements, {:array, :string}, default: [])
    field(:auto_generated_tags, {:array, :string}, default: [])
    field(:detected_objects, {:array, :map}, default: [])
    field(:scene_detection, {:array, :map}, default: [])
    field(:transcript_cid, :string)
    field(:content_warnings, {:array, :string}, default: [])
    field(:views, :integer, default: 0)
    field(:unique_views, :integer, default: 0)
    field(:watch_time_total, :integer, default: 0)
    field(:avg_watch_percentage, :float, default: 0.0)
    field(:likes, {:array, :string}, default: [])
    field(:reactions, :map, default: %{})
    field(:reaction_counts, :map, default: %{})
    field(:comments, {:array, :string}, default: [])
    field(:shares, :integer, default: 0)
    field(:saves, :integer, default: 0)
    field(:hashtags, {:array, :string}, default: [])
    field(:mentions, {:array, :string}, default: [])
    field(:location, :string)
    field(:privacy, :string)
    field(:allow_comments, :boolean, default: true)
    field(:allow_downloads, :boolean, default: false)
    field(:allow_remixes, :boolean, default: false)
    field(:monetized, :boolean, default: false)
    field(:revenue_model, :string)
    field(:price, :float)
    field(:ad_breaks, {:array, :map}, default: [])
    field(:sponsor_info, :map, default: %{})
    field(:viewer_retention, {:array, :map}, default: [])
    field(:geographic_views, :map, default: %{})
    field(:device_breakdown, :map, default: %{})
    field(:referral_sources, :map, default: %{})
    field(:peak_concurrent_viewers, :integer, default: 0)
    field(:is_live, :boolean, default: false)
    field(:live_stream_url, :string)
    field(:live_started_at, :naive_datetime)
    field(:live_ended_at, :naive_datetime)
    field(:live_viewers_current, :integer, default: 0)
    field(:live_chat_id, :string)
    field(:playlist_ids, {:array, :string}, default: [])
    field(:series_id, :string)
    field(:episode_number, :integer)
    field(:season_number, :integer)
    field(:reported, {:array, :string}, default: [])
    field(:moderation_status, :string)
    field(:age_restriction, :integer)
    field(:pin_info, {:array, :map}, default: [])
    field(:cdn_urls, :map, default: %{})
    field(:peer_availability, {:array, :map}, default: [])
    field(:date_created, :naive_datetime)
    field(:date_updated, :naive_datetime)
    field(:date_published, :naive_datetime)
    field(:last_viewed, :naive_datetime)
    field(:upload_device_info, :map, default: %{})
    field(:processing_time_ms, :integer)
    field(:video_embedding, :string)
    field(:visual_signature, :string)
    field(:sign_language_overlay_cid, :string)
    field(:audio_description_cid, :string)
    field(:accessibility_score, :float, default: 0.0)
    field(:vertical_version_cid, :string)
    field(:square_version_cid, :string)
    field(:story_version_cid, :string)
    field(:format_variants, :map, default: %{})
    field(:collaboration_tree, {:array, :string}, default: [])
    field(:remix_parent_id, :string)
    field(:remix_children_ids, {:array, :string}, default: [])
    field(:remix_permissions, :map, default: %{})
    field(:collaboration_revenue_split, {:array, :map}, default: [])
    field(:adaptive_bitrate_manifest, :string)
    field(:buffer_health_metrics, {:array, :map}, default: [])
    field(:stream_protocol, :string)
    field(:drm_info, :map, default: %{})
    field(:offline_download_cid, :string)
    field(:filming_equipment, :map, default: %{})
    field(:color_grading_lut_cid, :string)
    field(:behind_the_scenes_cid, :string)
    field(:production_notes, {:array, :string}, default: [])
    field(:predicted_virality_score, :float, default: 0.0)
    field(:optimal_posting_time, :naive_datetime)
    field(:suggested_improvements, {:array, :string}, default: [])
    field(:ab_test_variants, {:array, :map}, default: [])
    field(:nft_minted, :boolean, default: false)
    field(:nft_contract_address, :string)
    field(:nft_token_id, :string)
    field(:nft_metadata_cid, :string)
    field(:blockchain_proof_cid, :string)
    field(:limited_edition_count, :integer, default: 0)
    field(:semantic_search_keywords, {:array, :string}, default: [])
    field(:visual_search_embeddings, {:array, :map}, default: [])
    field(:audio_fingerprint_segments, {:array, :map}, default: [])
    field(:multimodal_search_index, :map, default: %{})
    field(:interactive_hotspots, {:array, :map}, default: [])
    field(:branching_narrative_tree, :map, default: %{})
    field(:live_poll_results, :map, default: %{})
    field(:gamification_elements, {:array, :map}, default: [])
    field(:synchronized_viewing_rooms, {:array, :string}, default: [])
    field(:stream_category, :string)
    field(:stream_tags, {:array, :string}, default: [])
    field(:raid_enabled, :boolean, default: true)
    field(:raid_target_channel, :string)
    field(:raid_viewer_count, :integer, default: 0)
    field(:host_enabled, :boolean, default: true)
    field(:hosted_channels, {:array, :string}, default: [])
    field(:subscriber_only_mode, :boolean, default: false)
    field(:subscriber_tiers, {:array, :map}, default: [])
    field(:subscriber_count, :integer, default: 0)
    field(:subscriber_badges, :map, default: %{})
    field(:emote_library, {:array, :string}, default: [])
    field(:bits_enabled, :boolean, default: true)
    field(:bits_received, :integer, default: 0)
    field(:donation_alerts, {:array, :map}, default: [])
    field(:donation_goals, {:array, :map}, default: [])
    field(:top_donors, {:array, :map}, default: [])
    field(:moderators, {:array, :string}, default: [])
    field(:vips, {:array, :string}, default: [])
    field(:chat_rules, {:array, :string}, default: [])
    field(:banned_words, {:array, :string}, default: [])
    field(:slow_mode, :boolean, default: false)
    field(:slow_mode_duration, :integer, default: 0)
    field(:subscriber_only_chat, :boolean, default: false)
    field(:follower_only_chat, :boolean, default: false)
    field(:follower_only_duration, :integer, default: 0)
    field(:emote_only_mode, :boolean, default: false)
    field(:channel_points_enabled, :boolean, default: true)
    field(:point_rewards, {:array, :string}, default: [])
    field(:points_distributed, :integer, default: 0)
    field(:custom_rewards_redeemed, {:array, :map}, default: [])
    field(:clips_enabled, :boolean, default: true)
    field(:user_generated_clips, {:array, :string}, default: [])
    field(:trending_clips, {:array, :string}, default: [])
    field(:clip_of_the_week, :string)
    field(:auto_highlight_moments, {:array, :map}, default: [])
    field(:simulcast_destinations, {:array, :map}, default: [])
    field(:cross_platform_chat, :boolean, default: false)
    field(:unified_chat_id, :string)
    field(:is_short_form, :boolean, default: false)
    field(:loop_count, :integer, default: 0)
    field(:average_loop_percentage, :float, default: 0.0)
    field(:sound_id, :string)
    field(:sound_usage_count, :integer, default: 0)
    field(:duet_enabled, :boolean, default: true)
    field(:duet_videos, {:array, :string}, default: [])
    field(:stitch_enabled, :boolean, default: true)
    field(:stitch_videos, {:array, :string}, default: [])
    field(:original_audio_cid, :string)
    field(:effects_used, {:array, :string}, default: [])
    field(:filters_applied, {:array, :string}, default: [])
    field(:green_screen_enabled, :boolean, default: false)
    field(:ar_effects, {:array, :string}, default: [])
    field(:trending_score, :float, default: 0.0)
    field(:fyp_appearances, :integer, default: 0)
    field(:sound_trending, :boolean, default: false)
    field(:hashtag_trending, {:array, :string}, default: [])
    field(:challenge_id, :string)
    field(:text_overlays, {:array, :map}, default: [])
    field(:voiceover_cid, :string)
    field(:speed_adjustments, {:array, :map}, default: [])
    field(:transitions, {:array, :string}, default: [])
    field(:stickers, {:array, :map}, default: [])
    field(:share_to_story_count, :integer, default: 0)
    field(:profile_views_from_video, :integer, default: 0)
    field(:follower_conversion_rate, :float, default: 0.0)
    field(:rewatches, :integer, default: 0)
    field(:analytics_insights, :map, default: %{})
    field(:audience_similar_accounts, {:array, :string}, default: [])
    field(:content_inspiration, {:array, :string}, default: [])
    field(:best_time_to_post, :naive_datetime)
    field(:commercial_music_license, :boolean, default: false)
    field(:music_attribution_required, :boolean, default: false)
    field(:original_content_certified, :boolean, default: false)
    field(:copyright_claims, {:array, :map}, default: [])
    field(:watch_pattern_signature, :string)
    field(:retention_breakpoints, {:array, :map}, default: [])
    field(:optimal_thumbnail_frame, :integer)
    field(:hook_effectiveness_score, :float, default: 0.0)
    field(:data, :map, default: %{})
  end

  def erl_changeset(
        {:video, id, ai_video_id, user_id, business_id, file_url, ipfs_cid, thumbnail_cids,
         preview_cid, original_filename, file_size_bytes, status, original_resolution,
         available_qualities, codec, bitrate, frame_rate, aspect_ratio, title, description,
         duration_seconds, thumbnail_url, thumbnail_timestamp, preview_urls, chapters, subtitles,
         audio_tracks, interactive_elements, auto_generated_tags, detected_objects,
         scene_detection, transcript_cid, content_warnings, views, unique_views, watch_time_total,
         avg_watch_percentage, likes, reactions, reaction_counts, comments, shares, saves,
         hashtags, mentions, location, privacy, allow_comments, allow_downloads, allow_remixes,
         monetized, revenue_model, price, ad_breaks, sponsor_info, viewer_retention,
         geographic_views, device_breakdown, referral_sources, peak_concurrent_viewers, is_live,
         live_stream_url, live_started_at, live_ended_at, live_viewers_current, live_chat_id,
         playlist_ids, series_id, episode_number, season_number, reported, moderation_status,
         age_restriction, pin_info, cdn_urls, peer_availability, date_created, date_updated,
         date_published, last_viewed, upload_device_info, processing_time_ms, video_embedding,
         visual_signature, sign_language_overlay_cid, audio_description_cid, accessibility_score,
         vertical_version_cid, square_version_cid, story_version_cid, format_variants,
         collaboration_tree, remix_parent_id, remix_children_ids, remix_permissions,
         collaboration_revenue_split, adaptive_bitrate_manifest, buffer_health_metrics,
         stream_protocol, drm_info, offline_download_cid, filming_equipment,
         color_grading_lut_cid, behind_the_scenes_cid, production_notes, predicted_virality_score,
         optimal_posting_time, suggested_improvements, ab_test_variants, nft_minted,
         nft_contract_address, nft_token_id, nft_metadata_cid, blockchain_proof_cid,
         limited_edition_count, semantic_search_keywords, visual_search_embeddings,
         audio_fingerprint_segments, multimodal_search_index, interactive_hotspots,
         branching_narrative_tree, live_poll_results, gamification_elements,
         synchronized_viewing_rooms, stream_category, stream_tags, raid_enabled,
         raid_target_channel, raid_viewer_count, host_enabled, hosted_channels,
         subscriber_only_mode, subscriber_tiers, subscriber_count, subscriber_badges,
         emote_library, bits_enabled, bits_received, donation_alerts, donation_goals, top_donors,
         moderators, vips, chat_rules, banned_words, slow_mode, slow_mode_duration,
         subscriber_only_chat, follower_only_chat, follower_only_duration, emote_only_mode,
         channel_points_enabled, point_rewards, points_distributed, custom_rewards_redeemed,
         clips_enabled, user_generated_clips, trending_clips, clip_of_the_week,
         auto_highlight_moments, simulcast_destinations, cross_platform_chat, unified_chat_id,
         is_short_form, loop_count, average_loop_percentage, sound_id, sound_usage_count,
         duet_enabled, duet_videos, stitch_enabled, stitch_videos, original_audio_cid,
         effects_used, filters_applied, green_screen_enabled, ar_effects, trending_score,
         fyp_appearances, sound_trending, hashtag_trending, challenge_id, text_overlays,
         voiceover_cid, speed_adjustments, transitions, stickers, share_to_story_count,
         profile_views_from_video, follower_conversion_rate, rewatches, analytics_insights,
         audience_similar_accounts, content_inspiration, best_time_to_post,
         commercial_music_license, music_attribution_required, original_content_certified,
         copyright_claims, watch_pattern_signature, retention_breakpoints,
         optimal_thumbnail_frame, hook_effectiveness_score, data}
      ) do
    processed_ipfs_cid = process_cid(ipfs_cid)
    processed_file_url = process_string(file_url)
    processed_thumbnail_cids = process_string_list(thumbnail_cids)
    processed_preview_cid = process_string(preview_cid)
    processed_original_filename = process_string(original_filename)
    processed_status = atom_to_string(status)
    processed_privacy = atom_to_string(privacy)
    processed_reactions = process_reactions(reactions)
    processed_reaction_counts = process_reaction_counts(reaction_counts)
    processed_comments = process_string_list(comments)
    processed_hashtags = process_string_list(hashtags)
    processed_mentions = process_string_list(mentions)
    processed_location = process_string(location)
    processed_revenue_model = atom_to_string(revenue_model)
    processed_ad_breaks = process_list_of_tuples(ad_breaks)
    processed_sponsor_info = process_map(sponsor_info)
    processed_viewer_retention = process_viewer_retention(viewer_retention)
    processed_geographic_views = process_map(geographic_views)
    processed_device_breakdown = process_map(device_breakdown)
    processed_referral_sources = process_map(referral_sources)
    processed_live_started_at = handle_datetime(live_started_at)
    processed_live_ended_at = handle_datetime(live_ended_at)
    processed_playlist_ids = process_string_list(playlist_ids)
    processed_series_id = process_string(series_id)
    processed_reported = process_string_list(reported)
    processed_moderation_status = atom_to_string(moderation_status)
    processed_pin_info = process_list(pin_info)
    processed_cdn_urls = process_map(cdn_urls)
    processed_peer_availability = process_list(peer_availability)
    processed_date_created = handle_datetime(date_created)
    processed_date_updated = handle_datetime(date_updated)
    processed_date_published = handle_datetime(date_published)
    processed_last_viewed = handle_datetime(last_viewed)
    processed_upload_device_info = process_map(upload_device_info)
    processed_video_embedding = process_string(video_embedding)
    processed_visual_signature = process_string(visual_signature)
    processed_sign_language_overlay_cid = process_string(sign_language_overlay_cid)
    processed_audio_description_cid = process_string(audio_description_cid)
    processed_vertical_version_cid = process_string(vertical_version_cid)
    processed_square_version_cid = process_string(square_version_cid)
    processed_story_version_cid = process_string(story_version_cid)
    processed_format_variants = process_map(format_variants)
    processed_collaboration_tree = process_string_list(collaboration_tree)
    processed_remix_parent_id = process_string(remix_parent_id)
    processed_remix_children_ids = process_string_list(remix_children_ids)
    processed_remix_permissions = process_map(remix_permissions)
    processed_collaboration_revenue_split = process_list(collaboration_revenue_split)
    processed_adaptive_bitrate_manifest = process_string(adaptive_bitrate_manifest)
    processed_buffer_health_metrics = process_list(buffer_health_metrics)
    processed_stream_protocol = atom_to_string(stream_protocol)
    processed_drm_info = process_map(drm_info)
    processed_offline_download_cid = process_string(offline_download_cid)
    processed_filming_equipment = process_map(filming_equipment)
    processed_color_grading_lut_cid = process_string(color_grading_lut_cid)
    processed_behind_the_scenes_cid = process_string(behind_the_scenes_cid)
    processed_production_notes = process_string_list(production_notes)
    processed_optimal_posting_time = handle_datetime(optimal_posting_time)
    processed_suggested_improvements = process_string_list(suggested_improvements)
    processed_ab_test_variants = process_list(ab_test_variants)
    processed_nft_contract_address = process_string(nft_contract_address)
    processed_nft_token_id = process_string(nft_token_id)
    processed_nft_metadata_cid = process_string(nft_metadata_cid)
    processed_blockchain_proof_cid = process_string(blockchain_proof_cid)
    processed_semantic_search_keywords = process_string_list(semantic_search_keywords)
    processed_visual_search_embeddings = process_list(visual_search_embeddings)
    processed_audio_fingerprint_segments = process_list(audio_fingerprint_segments)
    processed_multimodal_search_index = process_map(multimodal_search_index)
    processed_interactive_hotspots = process_list(interactive_hotspots)
    processed_branching_narrative_tree = process_map(branching_narrative_tree)
    processed_live_poll_results = process_map(live_poll_results)
    processed_gamification_elements = process_list(gamification_elements)
    processed_synchronized_viewing_rooms = process_string_list(synchronized_viewing_rooms)
    processed_stream_category = process_string(stream_category)
    processed_stream_tags = process_string_list(stream_tags)
    processed_raid_target_channel = process_string(raid_target_channel)
    processed_hosted_channels = process_string_list(hosted_channels)
    processed_subscriber_tiers = process_list(subscriber_tiers)
    processed_subscriber_badges = process_map(subscriber_badges)
    processed_emote_library = process_string_list(emote_library)
    processed_donation_alerts = process_list(donation_alerts)
    processed_donation_goals = process_list(donation_goals)
    processed_top_donors = process_list(top_donors)
    processed_moderators = process_string_list(moderators)
    processed_vips = process_string_list(vips)
    processed_chat_rules = process_string_list(chat_rules)
    processed_banned_words = process_string_list(banned_words)
    processed_point_rewards = process_string_list(point_rewards)
    processed_custom_rewards_redeemed = process_list(custom_rewards_redeemed)
    processed_user_generated_clips = process_string_list(user_generated_clips)
    processed_trending_clips = process_string_list(trending_clips)
    processed_clip_of_the_week = process_string(clip_of_the_week)
    processed_auto_highlight_moments = process_list(auto_highlight_moments)
    processed_simulcast_destinations = process_list(simulcast_destinations)
    processed_unified_chat_id = process_string(unified_chat_id)
    processed_sound_id = process_string(sound_id)
    processed_duet_videos = process_string_list(duet_videos)
    processed_stitch_videos = process_string_list(stitch_videos)
    processed_original_audio_cid = process_string(original_audio_cid)
    processed_effects_used = process_string_list(effects_used)
    processed_filters_applied = process_string_list(filters_applied)
    processed_ar_effects = process_string_list(ar_effects)
    processed_hashtag_trending = process_string_list(hashtag_trending)
    processed_challenge_id = process_string(challenge_id)
    processed_text_overlays = process_list(text_overlays)
    processed_voiceover_cid = process_string(voiceover_cid)
    processed_speed_adjustments = process_list(speed_adjustments)
    processed_transitions = process_string_list(transitions)
    processed_stickers = process_list(stickers)
    processed_analytics_insights = process_map(analytics_insights)
    processed_audience_similar_accounts = process_string_list(audience_similar_accounts)
    processed_content_inspiration = process_string_list(content_inspiration)
    processed_best_time_to_post = handle_datetime(best_time_to_post)
    processed_copyright_claims = process_list(copyright_claims)
    processed_watch_pattern_signature = process_string(watch_pattern_signature)
    processed_retention_breakpoints = process_list(retention_breakpoints)
    processed_optimal_thumbnail_frame = optimal_thumbnail_frame
    processed_data = process_map(data)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      ai_video_id: process_string(ai_video_id),
      user_id: to_string(user_id),
      business_id: process_string_list(business_id),
      file_url: processed_file_url,
      ipfs_cid: processed_ipfs_cid,
      thumbnail_cids: processed_thumbnail_cids,
      preview_cid: processed_preview_cid,
      original_filename: processed_original_filename,
      file_size_bytes: file_size_bytes,
      status: processed_status,
      original_resolution: process_string(original_resolution),
      available_qualities: process_available_qualities(available_qualities),
      codec: process_string(codec),
      bitrate: bitrate,
      frame_rate: frame_rate,
      aspect_ratio: process_string(aspect_ratio),
      title: to_string(title),
      description: process_string(description),
      duration_seconds: duration_seconds,
      thumbnail_url: process_string(thumbnail_url),
      thumbnail_timestamp: thumbnail_timestamp,
      preview_urls: process_string_list(preview_urls),
      chapters: process_chapters(chapters),
      subtitles: process_subtitles(subtitles),
      audio_tracks: process_audio_tracks(audio_tracks),
      interactive_elements: process_string_list(interactive_elements),
      auto_generated_tags: process_string_list(auto_generated_tags),
      detected_objects: process_detected_objects(detected_objects),
      scene_detection: process_scene_detection(scene_detection),
      transcript_cid: process_string(transcript_cid),
      content_warnings: process_string_list(content_warnings),
      views: views,
      unique_views: unique_views,
      watch_time_total: watch_time_total,
      avg_watch_percentage: avg_watch_percentage,
      likes: process_string_list(likes),
      reactions: processed_reactions,
      reaction_counts: processed_reaction_counts,
      comments: processed_comments,
      shares: shares,
      saves: saves,
      hashtags: processed_hashtags,
      mentions: processed_mentions,
      location: processed_location,
      privacy: processed_privacy,
      allow_comments: allow_comments,
      allow_downloads: allow_downloads,
      allow_remixes: allow_remixes,
      monetized: monetized,
      revenue_model: processed_revenue_model,
      price: price,
      ad_breaks: processed_ad_breaks,
      sponsor_info: processed_sponsor_info,
      viewer_retention: processed_viewer_retention,
      geographic_views: processed_geographic_views,
      device_breakdown: processed_device_breakdown,
      referral_sources: processed_referral_sources,
      peak_concurrent_viewers: peak_concurrent_viewers,
      is_live: is_live,
      live_stream_url: process_string(live_stream_url),
      live_started_at: processed_live_started_at,
      live_ended_at: processed_live_ended_at,
      live_viewers_current: live_viewers_current,
      live_chat_id: process_string(live_chat_id),
      playlist_ids: processed_playlist_ids,
      series_id: processed_series_id,
      episode_number: episode_number,
      season_number: season_number,
      reported: processed_reported,
      moderation_status: processed_moderation_status,
      age_restriction: age_restriction,
      pin_info: processed_pin_info,
      cdn_urls: processed_cdn_urls,
      peer_availability: processed_peer_availability,
      date_created: processed_date_created,
      date_updated: processed_date_updated,
      date_published: processed_date_published,
      last_viewed: processed_last_viewed,
      upload_device_info: processed_upload_device_info,
      processing_time_ms: processing_time_ms,
      video_embedding: processed_video_embedding,
      visual_signature: processed_visual_signature,
      sign_language_overlay_cid: processed_sign_language_overlay_cid,
      audio_description_cid: processed_audio_description_cid,
      accessibility_score: accessibility_score,
      vertical_version_cid: processed_vertical_version_cid,
      square_version_cid: processed_square_version_cid,
      story_version_cid: processed_story_version_cid,
      format_variants: processed_format_variants,
      collaboration_tree: processed_collaboration_tree,
      remix_parent_id: processed_remix_parent_id,
      remix_children_ids: processed_remix_children_ids,
      remix_permissions: processed_remix_permissions,
      collaboration_revenue_split: processed_collaboration_revenue_split,
      adaptive_bitrate_manifest: processed_adaptive_bitrate_manifest,
      buffer_health_metrics: processed_buffer_health_metrics,
      stream_protocol: processed_stream_protocol,
      drm_info: processed_drm_info,
      offline_download_cid: processed_offline_download_cid,
      filming_equipment: processed_filming_equipment,
      color_grading_lut_cid: processed_color_grading_lut_cid,
      behind_the_scenes_cid: processed_behind_the_scenes_cid,
      production_notes: processed_production_notes,
      predicted_virality_score: predicted_virality_score,
      optimal_posting_time: processed_optimal_posting_time,
      suggested_improvements: processed_suggested_improvements,
      ab_test_variants: processed_ab_test_variants,
      nft_minted: nft_minted,
      nft_contract_address: processed_nft_contract_address,
      nft_token_id: processed_nft_token_id,
      nft_metadata_cid: processed_nft_metadata_cid,
      blockchain_proof_cid: processed_blockchain_proof_cid,
      limited_edition_count: limited_edition_count,
      semantic_search_keywords: processed_semantic_search_keywords,
      visual_search_embeddings: processed_visual_search_embeddings,
      audio_fingerprint_segments: processed_audio_fingerprint_segments,
      multimodal_search_index: processed_multimodal_search_index,
      interactive_hotspots: processed_interactive_hotspots,
      branching_narrative_tree: processed_branching_narrative_tree,
      live_poll_results: processed_live_poll_results,
      gamification_elements: processed_gamification_elements,
      synchronized_viewing_rooms: processed_synchronized_viewing_rooms,
      stream_category: processed_stream_category,
      stream_tags: processed_stream_tags,
      raid_enabled: raid_enabled,
      raid_target_channel: processed_raid_target_channel,
      raid_viewer_count: raid_viewer_count,
      host_enabled: host_enabled,
      hosted_channels: processed_hosted_channels,
      subscriber_only_mode: subscriber_only_mode,
      subscriber_tiers: processed_subscriber_tiers,
      subscriber_count: subscriber_count,
      subscriber_badges: processed_subscriber_badges,
      emote_library: processed_emote_library,
      bits_enabled: bits_enabled,
      bits_received: bits_received,
      donation_alerts: processed_donation_alerts,
      donation_goals: processed_donation_goals,
      top_donors: processed_top_donors,
      moderators: processed_moderators,
      vips: processed_vips,
      chat_rules: processed_chat_rules,
      banned_words: processed_banned_words,
      slow_mode: slow_mode,
      slow_mode_duration: slow_mode_duration,
      subscriber_only_chat: subscriber_only_chat,
      follower_only_chat: follower_only_chat,
      follower_only_duration: follower_only_duration,
      emote_only_mode: emote_only_mode,
      channel_points_enabled: channel_points_enabled,
      point_rewards: processed_point_rewards,
      points_distributed: points_distributed,
      custom_rewards_redeemed: processed_custom_rewards_redeemed,
      clips_enabled: clips_enabled,
      user_generated_clips: processed_user_generated_clips,
      trending_clips: processed_trending_clips,
      clip_of_the_week: processed_clip_of_the_week,
      auto_highlight_moments: processed_auto_highlight_moments,
      simulcast_destinations: processed_simulcast_destinations,
      cross_platform_chat: cross_platform_chat,
      unified_chat_id: processed_unified_chat_id,
      is_short_form: is_short_form,
      loop_count: loop_count,
      average_loop_percentage: average_loop_percentage,
      sound_id: processed_sound_id,
      sound_usage_count: sound_usage_count,
      duet_enabled: duet_enabled,
      duet_videos: processed_duet_videos,
      stitch_enabled: stitch_enabled,
      stitch_videos: processed_stitch_videos,
      original_audio_cid: processed_original_audio_cid,
      effects_used: processed_effects_used,
      filters_applied: processed_filters_applied,
      green_screen_enabled: green_screen_enabled,
      ar_effects: processed_ar_effects,
      trending_score: trending_score,
      fyp_appearances: fyp_appearances,
      sound_trending: sound_trending,
      hashtag_trending: processed_hashtag_trending,
      challenge_id: processed_challenge_id,
      text_overlays: processed_text_overlays,
      voiceover_cid: processed_voiceover_cid,
      speed_adjustments: processed_speed_adjustments,
      transitions: processed_transitions,
      stickers: processed_stickers,
      share_to_story_count: share_to_story_count,
      profile_views_from_video: profile_views_from_video,
      follower_conversion_rate: follower_conversion_rate,
      rewatches: rewatches,
      analytics_insights: processed_analytics_insights,
      audience_similar_accounts: processed_audience_similar_accounts,
      content_inspiration: processed_content_inspiration,
      best_time_to_post: processed_best_time_to_post,
      commercial_music_license: commercial_music_license,
      music_attribution_required: music_attribution_required,
      original_content_certified: original_content_certified,
      copyright_claims: processed_copyright_claims,
      watch_pattern_signature: processed_watch_pattern_signature,
      retention_breakpoints: processed_retention_breakpoints,
      optimal_thumbnail_frame: processed_optimal_thumbnail_frame,
      hook_effectiveness_score: hook_effectiveness_score,
      data: processed_data
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

  defp process_cid({:pending, _id}), do: nil
  defp process_cid({:pending_update, _id}), do: nil
  defp process_cid({:pending_version, _id}), do: nil
  defp process_cid({:error, _}), do: nil
  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

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

  defp process_map(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, acc ->
      string_key =
        case key do
          k when is_atom(k) -> Atom.to_string(k)
          k when is_binary(k) -> k
          k when is_list(k) -> to_string(k)
          k -> inspect(k)
        end

      processed_value =
        case value do
          v when is_list(v) ->
            if Enum.all?(v, &is_integer/1) and length(v) > 0 do
              try do
                to_string(v)
              rescue
                _ -> v
              end
            else
              v
            end

          v ->
            v
        end

      Map.put(acc, string_key, processed_value)
    end)
  end

  defp process_map(:undefined), do: %{}
  defp process_map(nil), do: %{}
  defp process_map(_), do: %{}

  defp process_list(list) when is_list(list), do: list
  defp process_list(:undefined), do: []
  defp process_list(nil), do: []
  defp process_list(_), do: []

  defp process_available_qualities(qualities) when is_list(qualities) do
    Enum.map(qualities, fn quality ->
      case quality do
        {resolution, cid} -> %{resolution: to_string(resolution), cid: process_string(cid)}
        _ -> quality
      end
    end)
  end

  defp process_available_qualities(_), do: []

  defp process_chapters(chapters) when is_list(chapters) do
    Enum.map(chapters, fn
      {id, title, start, end_time} ->
        %{
          id: to_string(id),
          title: to_string(title),
          start_time: start,
          end_time: end_time
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_chapters(_), do: []

  defp process_subtitles(subtitles) when is_list(subtitles) do
    Enum.map(subtitles, fn
      {lang, cid, format} ->
        %{
          language: to_string(lang),
          cid: process_string(cid),
          format: to_string(format)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_subtitles(_), do: []

  defp process_audio_tracks(tracks) when is_list(tracks) do
    Enum.map(tracks, fn
      {lang, cid, type} ->
        %{
          language: to_string(lang),
          cid: process_string(cid),
          type: atom_to_string(type)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_audio_tracks(_), do: []

  defp process_detected_objects(objects) when is_list(objects) do
    Enum.map(objects, fn obj ->
      case obj do
        {object_name, confidence, timestamp} ->
          %{
            name: to_string(object_name),
            confidence: confidence,
            timestamp: timestamp
          }

        _ ->
          obj
      end
    end)
  end

  defp process_detected_objects(_), do: []

  defp process_scene_detection(scenes) when is_list(scenes) do
    Enum.map(scenes, fn
      {timestamp, scene_description} ->
        %{
          timestamp: timestamp,
          description: to_string(scene_description)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_scene_detection(_), do: []

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

  defp process_viewer_retention(retention) when is_list(retention) do
    Enum.map(retention, fn
      {timestamp, percentage} ->
        %{
          timestamp: timestamp,
          percentage: percentage
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_viewer_retention(_), do: []

  defp process_list_of_tuples(tuples) when is_list(tuples) do
    Enum.map(tuples, fn
      {timestamp, ad_type} ->
        %{
          timestamp: timestamp,
          ad_type: atom_to_string(ad_type)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_list_of_tuples(_), do: []

  defp process_list(list) when is_list(list), do: list
  defp process_list(_), do: []

  def changeset(video, attrs \\ %{}) do
    video
    |> cast(attrs, [
      :id,
      :ai_video_id,
      :user_id,
      :business_id,
      :file_url,
      :ipfs_cid,
      :thumbnail_cids,
      :preview_cid,
      :original_filename,
      :file_size_bytes,
      :status,
      :original_resolution,
      :available_qualities,
      :codec,
      :bitrate,
      :frame_rate,
      :aspect_ratio,
      :title,
      :description,
      :duration_seconds,
      :thumbnail_url,
      :thumbnail_timestamp,
      :preview_urls,
      :chapters,
      :subtitles,
      :audio_tracks,
      :interactive_elements,
      :auto_generated_tags,
      :detected_objects,
      :scene_detection,
      :transcript_cid,
      :content_warnings,
      :views,
      :unique_views,
      :watch_time_total,
      :avg_watch_percentage,
      :likes,
      :reactions,
      :reaction_counts,
      :comments,
      :shares,
      :saves,
      :hashtags,
      :mentions,
      :location,
      :privacy,
      :allow_comments,
      :allow_downloads,
      :allow_remixes,
      :monetized,
      :revenue_model,
      :price,
      :ad_breaks,
      :sponsor_info,
      :viewer_retention,
      :geographic_views,
      :device_breakdown,
      :referral_sources,
      :peak_concurrent_viewers,
      :is_live,
      :live_stream_url,
      :live_started_at,
      :live_ended_at,
      :live_viewers_current,
      :live_chat_id,
      :playlist_ids,
      :series_id,
      :episode_number,
      :season_number,
      :reported,
      :moderation_status,
      :age_restriction,
      :pin_info,
      :cdn_urls,
      :peer_availability,
      :date_created,
      :date_updated,
      :date_published,
      :last_viewed,
      :upload_device_info,
      :processing_time_ms,
      :video_embedding,
      :visual_signature,
      :sign_language_overlay_cid,
      :audio_description_cid,
      :accessibility_score,
      :vertical_version_cid,
      :square_version_cid,
      :story_version_cid,
      :format_variants,
      :collaboration_tree,
      :remix_parent_id,
      :remix_children_ids,
      :remix_permissions,
      :collaboration_revenue_split,
      :adaptive_bitrate_manifest,
      :buffer_health_metrics,
      :stream_protocol,
      :drm_info,
      :offline_download_cid,
      :filming_equipment,
      :color_grading_lut_cid,
      :behind_the_scenes_cid,
      :production_notes,
      :predicted_virality_score,
      :optimal_posting_time,
      :suggested_improvements,
      :ab_test_variants,
      :nft_minted,
      :nft_contract_address,
      :nft_token_id,
      :nft_metadata_cid,
      :blockchain_proof_cid,
      :limited_edition_count,
      :semantic_search_keywords,
      :visual_search_embeddings,
      :audio_fingerprint_segments,
      :multimodal_search_index,
      :interactive_hotspots,
      :branching_narrative_tree,
      :live_poll_results,
      :gamification_elements,
      :synchronized_viewing_rooms,
      :stream_category,
      :stream_tags,
      :raid_enabled,
      :raid_target_channel,
      :raid_viewer_count,
      :host_enabled,
      :hosted_channels,
      :subscriber_only_mode,
      :subscriber_tiers,
      :subscriber_count,
      :subscriber_badges,
      :emote_library,
      :bits_enabled,
      :bits_received,
      :donation_alerts,
      :donation_goals,
      :top_donors,
      :moderators,
      :vips,
      :chat_rules,
      :banned_words,
      :slow_mode,
      :slow_mode_duration,
      :subscriber_only_chat,
      :follower_only_chat,
      :follower_only_duration,
      :emote_only_mode,
      :channel_points_enabled,
      :point_rewards,
      :points_distributed,
      :custom_rewards_redeemed,
      :clips_enabled,
      :user_generated_clips,
      :trending_clips,
      :clip_of_the_week,
      :auto_highlight_moments,
      :simulcast_destinations,
      :cross_platform_chat,
      :unified_chat_id,
      :is_short_form,
      :loop_count,
      :average_loop_percentage,
      :sound_id,
      :sound_usage_count,
      :duet_enabled,
      :duet_videos,
      :stitch_enabled,
      :stitch_videos,
      :original_audio_cid,
      :effects_used,
      :filters_applied,
      :green_screen_enabled,
      :ar_effects,
      :trending_score,
      :fyp_appearances,
      :sound_trending,
      :hashtag_trending,
      :challenge_id,
      :text_overlays,
      :voiceover_cid,
      :speed_adjustments,
      :transitions,
      :stickers,
      :share_to_story_count,
      :profile_views_from_video,
      :follower_conversion_rate,
      :rewatches,
      :analytics_insights,
      :audience_similar_accounts,
      :content_inspiration,
      :best_time_to_post,
      :commercial_music_license,
      :music_attribution_required,
      :original_content_certified,
      :copyright_claims,
      :watch_pattern_signature,
      :retention_breakpoints,
      :optimal_thumbnail_frame,
      :hook_effectiveness_score,
      :data
    ])
  end

  def total_views(%__MODULE__{views: views}), do: views
  def total_views(_), do: 0

  def total_unique_views(%__MODULE__{unique_views: unique_views}), do: unique_views
  def total_unique_views(_), do: 0

  def total_reactions(%__MODULE__{reaction_counts: counts}) when is_map(counts) do
    Enum.reduce(counts, 0, fn {_type, count}, acc -> acc + count end)
  end

  def total_reactions(_), do: 0

  def is_public?(%__MODULE__{privacy: "public"}), do: true
  def is_public?(_), do: false

  def is_live?(%__MODULE__{is_live: true}), do: true
  def is_live?(_), do: false

  def has_monetization?(%__MODULE__{monetized: true}), do: true
  def has_monetization?(_), do: false

  def format_duration(nil), do: "N/A"

  def format_duration(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    "#{hours}:#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  def format_duration(_), do: "N/A"

  def format_views(views) when is_integer(views) do
    cond do
      views >= 1_000_000 -> "#{Float.round(views / 1_000_000, 1)}M"
      views >= 1_000 -> "#{Float.round(views / 1_000, 1)}K"
      true -> Integer.to_string(views)
    end
  end

  def format_views(_), do: "0"

  def format_datetime(nil), do: "N/A"

  def format_datetime(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end

  def format_datetime(_), do: "N/A"
end
