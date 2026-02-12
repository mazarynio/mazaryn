defmodule Mazaryn.Schema.Music do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  embedded_schema do
    field :id, :string
    field :ai_music_id, :string
    field :user_id, :string
    field :business_id, :string
    field :artist_id, :string
    field :file_url, :string
    field :ipfs_cid, :string
    field :original_filename, :string
    field :file_size_bytes, :integer
    field :status, :string
    field :format, :string
    field :bitrate, :integer
    field :sample_rate, :integer
    field :channels, :integer
    field :available_qualities, {:array, :map}, default: []
    field :title, :string
    field :artist, :string
    field :featured_artists, {:array, :string}, default: []
    field :album, :string
    field :album_id, :string
    field :album_art_cid, :string
    field :album_art_url, :string
    field :duration_seconds, :integer
    field :genre, {:array, :string}, default: []
    field :sub_genre, {:array, :string}, default: []
    field :mood, {:array, :string}, default: []
    field :tempo_bpm, :integer
    field :key, :string
    field :time_signature, :string
    field :composer, {:array, :string}, default: []
    field :lyricist, {:array, :string}, default: []
    field :producer, {:array, :string}, default: []
    field :label, :string
    field :isrc, :string
    field :upc, :string
    field :copyright_info, :string
    field :license_type, :string
    field :lyrics_cid, :string
    field :lyrics_synced, {:array, :map}, default: []
    field :has_explicit_content, :boolean, default: false
    field :language, :string
    field :waveform_cid, :string
    field :spectrum_analysis_cid, :string
    field :peaks_data, {:array, :map}, default: []
    field :audio_fingerprint, :string
    field :detected_instruments, {:array, :string}, default: []
    field :vocal_presence, :boolean, default: false
    field :energy_level, :float
    field :danceability, :float
    field :acousticness, :float
    field :instrumentalness, :float
    field :auto_generated_tags, {:array, :string}, default: []
    field :plays, :integer, default: 0
    field :unique_listeners, :integer, default: 0
    field :listen_time_total, :integer, default: 0
    field :completion_rate, :float, default: 0.0
    field :likes, {:array, :string}, default: []
    field :reactions, :map, default: %{}
    field :reaction_counts, :map, default: %{}
    field :comments, {:array, :string}, default: []
    field :shares, :integer, default: 0
    field :saves, :integer, default: 0
    field :added_to_playlists, :integer, default: 0
    field :hashtags, {:array, :string}, default: []
    field :mentions, {:array, :string}, default: []
    field :collaborators, {:array, :string}, default: []
    field :privacy, :string
    field :allow_comments, :boolean, default: true
    field :allow_downloads, :boolean, default: false
    field :allow_remixes, :boolean, default: false
    field :playlist_ids, {:array, :string}, default: []
    field :track_number, :integer
    field :disc_number, :integer
    field :total_tracks, :integer
    field :monetized, :boolean, default: false
    field :revenue_model, :string
    field :price_download, :float
    field :price_stream_per_play, :float
    field :streaming_royalties, :map, default: %{}
    field :skip_rate, :float, default: 0.0
    field :replay_count, :integer, default: 0
    field :geographic_plays, :map, default: %{}
    field :device_breakdown, :map, default: %{}
    field :referral_sources, :map, default: %{}
    field :peak_concurrent_listeners, :integer, default: 0
    field :listening_hours, {:array, :integer}, default: []
    field :similar_tracks, {:array, :string}, default: []
    field :radio_seed_weight, :float
    field :discovery_score, :float
    field :live_recording, :boolean, default: false
    field :concert_date, :naive_datetime
    field :venue, :string
    field :original_track_id, :string
    field :remix_type, :string
    field :remixes, {:array, :string}, default: []
    field :versions, {:array, :map}, default: []
    field :reported, {:array, :string}, default: []
    field :moderation_status, :string
    field :age_restriction, :integer
    field :content_warnings, {:array, :string}, default: []
    field :pin_info, :map
    field :cdn_urls, :map, default: %{}
    field :peer_availability, {:array, :map}, default: []
    field :release_date, :naive_datetime
    field :release_type, :string
    field :pre_release, :boolean, default: false
    field :premiere_timestamp, :naive_datetime
    field :date_created, :naive_datetime
    field :date_updated, :naive_datetime
    field :date_published, :naive_datetime
    field :last_played, :naive_datetime
    field :upload_device_info, :map, default: %{}
    field :processing_time_ms, :integer
    field :spatial_audio_cid, :string
    field :binaural_audio_cid, :string
    field :immersive_format, :string
    field :head_tracking_data, :map
    field :separated_stems_cids, :map, default: %{}
    field :stem_quality_score, :float
    field :remix_template_cid, :string
    field :midi_file_cid, :string
    field :chord_progression, {:array, :string}, default: []
    field :audio_embedding, :string
    field :musical_structure, {:array, :map}, default: []
    field :harmonic_complexity_score, :float
    field :vocal_melody_cid, :string
    field :rhythm_pattern_signature, :string
    field :collaboration_sessions, {:array, :string}, default: []
    field :multitrack_project_cid, :string
    field :version_history, {:array, :map}, default: []
    field :contributor_credits, {:array, :map}, default: []
    field :royalty_split_smart_contract, :string
    field :live_performance_recordings, {:array, :string}, default: []
    field :setlist_context, :map
    field :concert_sync_id, :string
    field :live_jam_session_id, :string
    field :virtual_concert_cid, :string
    field :sample_clearances, {:array, :map}, default: []
    field :mechanical_license_info, :map
    field :sync_licensing_available, :boolean, default: true
    field :territorial_rights, :map, default: %{}
    field :creative_commons_derivative_tree, {:array, :string}, default: []
    field :blockchain_rights_token, :string
    field :sonic_signature, :string
    field :influence_tags, {:array, :string}, default: []
    field :era_classification, :string
    field :cultural_context, {:array, :string}, default: []
    field :listening_context_scores, :map, default: %{}
    field :crossover_potential, :float
    field :adaptive_layers_cids, :map, default: %{}
    field :interactive_mixing_params, :map
    field :generative_rules, :map
    field :loop_points, {:array, :map}, default: []
    field :branching_sections, {:array, :map}, default: []
    field :lufs_integrated, :float
    field :dynamic_range, :float
    field :true_peak_db, :float
    field :mastering_chain, {:array, :string}, default: []
    field :reference_tracks, {:array, :string}, default: []
    field :mastering_engineer, :string
    field :remaster_versions, {:array, :string}, default: []
    field :fan_tier_access, :map, default: %{}
    field :early_access_until, :naive_datetime
    field :limited_edition_vinyl_link, :string
    field :fan_token_gated, :boolean, default: false
    field :community_remix_challenge_id, :string
    field :behind_the_music_cid, :string
    field :producer_commentary_cid, :string
    field :exclusive_listening_parties, {:array, :string}, default: []
    field :radio_seed_enabled, :boolean, default: true
    field :included_in_playlists, {:array, :string}, default: []
    field :playlist_reach, :integer, default: 0
    field :editorial_playlist_picks, {:array, :string}, default: []
    field :user_taste_match, :map, default: %{}
    field :blend_compatibility_score, :float
    field :vibe_tags, {:array, :string}, default: []
    field :time_of_day_affinity, :map, default: %{}
    field :friend_activity, {:array, :map}, default: []
    field :collaborative_playlists, {:array, :string}, default: []
    field :queue_additions, :integer, default: 0
    field :viral_charts_position, {:array, :map}, default: []
    field :new_music_friday_featured, :boolean, default: false
    field :release_radar_appearances, :integer, default: 0
    field :discover_weekly_appearances, :integer, default: 0
    field :monthly_listeners_trend, {:array, :map}, default: []
    field :listener_demographics, :map, default: %{}
    field :fan_concentration, :map, default: %{}
    field :top_cities, {:array, :string}, default: []
    field :playlist_conversion_rate, :float
    field :skip_30sec_rate, :float
    field :save_rate, :float
    field :playlist_add_rate, :float
    field :share_rate, :float
    field :reposts, {:array, :string}, default: []
    field :repost_count, :integer, default: 0
    field :spotlight_tracks, {:array, :string}, default: []
    field :timed_comments, {:array, :string}, default: []
    field :comment_heatmap, {:array, :map}, default: []
    field :download_enabled, :boolean, default: false
    field :download_count, :integer, default: 0
    field :downloadable_formats, {:array, :string}, default: []
    field :private_share_links, {:array, :string}, default: []
    field :scheduled_release, :naive_datetime
    field :secret_link, :string
    field :fan_powered_royalties, :boolean, default: false
    field :tip_jar_enabled, :boolean, default: false
    field :tips_received, {:array, :map}, default: []
    field :pro_unlimited_tier, :boolean, default: false
    field :waveform_comments_enabled, :boolean, default: true
    field :waveform_color, :string
    field :waveform_style, :string
    field :seekable_waveform_cid, :string
    field :replace_file_history, {:array, :map}, default: []
    field :mastered_on_soundcloud, :boolean, default: false
    field :plays_per_country, :map, default: %{}
    field :plays_per_source, :map, default: %{}
    field :embedded_plays, :integer, default: 0
    field :lossless_available, :boolean, default: false
    field :lossless_cid, :string
    field :hi_res_lossless, :boolean, default: false
    field :hi_res_cid, :string
    field :audio_quality_tier, :string
    field :dolby_atmos_available, :boolean, default: false
    field :dolby_atmos_cid, :string
    field :apple_digital_master, :boolean, default: false
    field :time_synced_lyrics, {:array, :map}, default: []
    field :lyrics_language_versions, :map, default: %{}
    field :lyrics_translation, :map, default: %{}
    field :sing_along_mode, :boolean, default: false
    field :apple_music_1_plays, :integer, default: 0
    field :station_appearances, {:array, :string}, default: []
    field :dj_mentions, {:array, :map}, default: []
    field :apple_music_editor_notes, :string
    field :curator_picks, {:array, :string}, default: []
    field :essential_album, :boolean, default: false
    field :music_video_id, :string
    field :video_available, :boolean, default: false
    field :behind_the_lyrics_video, :string
    field :composer_details, :map, default: %{}
    field :conductor, :string
    field :orchestra, :string
    field :movement_number, :integer
    field :work_id, :string
    field :catalog_number, :string
    field :shazam_count, :integer, default: 0
    field :shazam_trending, :boolean, default: false
    field :shazam_discovery_rank, :integer
    field :optimal_crossfade_point, :integer
    field :gapless_playback, :boolean, default: false
    field :crossfade_duration, :integer, default: 0
    field :ai_extended_version_cid, :string
    field :ai_radio_edit_cid, :string
    field :ai_instrumental_cid, :string
    field :ai_acapella_cid, :string
    field :ai_8d_audio_cid, :string
    field :transition_compatibility, :map, default: %{}
    field :bpm_locked, :boolean, default: false
    field :harmonic_mixing_key, :string
    field :energy_curve, {:array, :float}, default: []
    field :first_listeners_club, {:array, :string}, default: []
    field :superfan_unlocks, {:array, :map}, default: []
    field :listening_milestones, :map, default: %{}
    field :fan_challenges, {:array, :map}, default: []
    field :open_verse_enabled, :boolean, default: false
    field :stem_remix_competition_id, :string
    field :community_cover_versions, {:array, :string}, default: []
    field :listener_journey_map, {:array, :map}, default: []
    field :retention_by_segment, {:array, :map}, default: []
    field :viral_coefficient, :float
    field :playlist_velocity, :float
    field :tiktok_usage_count, :integer, default: 0
    field :instagram_story_usage, :integer, default: 0
    field :youtube_shorts_usage, :integer, default: 0
    field :viral_moment_timestamp, :integer
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:music, id, ai_music_id, user_id, business_id, artist_id, file_url, ipfs_cid,
         original_filename, file_size_bytes, status, format, bitrate, sample_rate, channels,
         available_qualities, title, artist, featured_artists, album, album_id, album_art_cid,
         album_art_url, duration_seconds, genre, sub_genre, mood, tempo_bpm, key, time_signature,
         composer, lyricist, producer, label, isrc, upc, copyright_info, license_type, lyrics_cid,
         lyrics_synced, has_explicit_content, language, waveform_cid, spectrum_analysis_cid,
         peaks_data, audio_fingerprint, detected_instruments, vocal_presence, energy_level,
         danceability, acousticness, instrumentalness, auto_generated_tags, plays,
         unique_listeners, listen_time_total, completion_rate, likes, reactions, reaction_counts,
         comments, shares, saves, added_to_playlists, hashtags, mentions, collaborators, privacy,
         allow_comments, allow_downloads, allow_remixes, playlist_ids, track_number, disc_number,
         total_tracks, monetized, revenue_model, price_download, price_stream_per_play,
         streaming_royalties, skip_rate, replay_count, geographic_plays, device_breakdown,
         referral_sources, peak_concurrent_listeners, listening_hours, similar_tracks,
         radio_seed_weight, discovery_score, live_recording, concert_date, venue,
         original_track_id, remix_type, remixes, versions, reported, moderation_status,
         age_restriction, content_warnings, pin_info, cdn_urls, peer_availability, release_date,
         release_type, pre_release, premiere_timestamp, date_created, date_updated,
         date_published, last_played, upload_device_info, processing_time_ms, spatial_audio_cid,
         binaural_audio_cid, immersive_format, head_tracking_data, separated_stems_cids,
         stem_quality_score, remix_template_cid, midi_file_cid, chord_progression,
         audio_embedding, musical_structure, harmonic_complexity_score, vocal_melody_cid,
         rhythm_pattern_signature, collaboration_sessions, multitrack_project_cid,
         version_history, contributor_credits, royalty_split_smart_contract,
         live_performance_recordings, setlist_context, concert_sync_id, live_jam_session_id,
         virtual_concert_cid, sample_clearances, mechanical_license_info,
         sync_licensing_available, territorial_rights, creative_commons_derivative_tree,
         blockchain_rights_token, sonic_signature, influence_tags, era_classification,
         cultural_context, listening_context_scores, crossover_potential, adaptive_layers_cids,
         interactive_mixing_params, generative_rules, loop_points, branching_sections,
         lufs_integrated, dynamic_range, true_peak_db, mastering_chain, reference_tracks,
         mastering_engineer, remaster_versions, fan_tier_access, early_access_until,
         limited_edition_vinyl_link, fan_token_gated, community_remix_challenge_id,
         behind_the_music_cid, producer_commentary_cid, exclusive_listening_parties,
         radio_seed_enabled, included_in_playlists, playlist_reach, editorial_playlist_picks,
         user_taste_match, blend_compatibility_score, vibe_tags, time_of_day_affinity,
         friend_activity, collaborative_playlists, queue_additions, viral_charts_position,
         new_music_friday_featured, release_radar_appearances, discover_weekly_appearances,
         monthly_listeners_trend, listener_demographics, fan_concentration, top_cities,
         playlist_conversion_rate, skip_30sec_rate, save_rate, playlist_add_rate, share_rate,
         reposts, repost_count, spotlight_tracks, timed_comments, comment_heatmap,
         download_enabled, download_count, downloadable_formats, private_share_links,
         scheduled_release, secret_link, fan_powered_royalties, tip_jar_enabled, tips_received,
         pro_unlimited_tier, waveform_comments_enabled, waveform_color, waveform_style,
         seekable_waveform_cid, replace_file_history, mastered_on_soundcloud, plays_per_country,
         plays_per_source, embedded_plays, lossless_available, lossless_cid, hi_res_lossless,
         hi_res_cid, audio_quality_tier, dolby_atmos_available, dolby_atmos_cid,
         apple_digital_master, time_synced_lyrics, lyrics_language_versions, lyrics_translation,
         sing_along_mode, apple_music_1_plays, station_appearances, dj_mentions,
         apple_music_editor_notes, curator_picks, essential_album, music_video_id,
         video_available, behind_the_lyrics_video, composer_details, conductor, orchestra,
         movement_number, work_id, catalog_number, shazam_count, shazam_trending,
         shazam_discovery_rank, optimal_crossfade_point, gapless_playback, crossfade_duration,
         ai_extended_version_cid, ai_radio_edit_cid, ai_instrumental_cid, ai_acapella_cid,
         ai_8d_audio_cid, transition_compatibility, bpm_locked, harmonic_mixing_key, energy_curve,
         first_listeners_club, superfan_unlocks, listening_milestones, fan_challenges,
         open_verse_enabled, stem_remix_competition_id, community_cover_versions,
         listener_journey_map, retention_by_segment, viral_coefficient, playlist_velocity,
         tiktok_usage_count, instagram_story_usage, youtube_shorts_usage, viral_moment_timestamp,
         data}
      ) do
    processed_ipfs_cid = process_cid(ipfs_cid)
    processed_file_url = process_string(file_url)
    processed_status = atom_to_string(status)
    processed_privacy = atom_to_string(privacy)
    processed_reactions = process_reactions(reactions)
    processed_reaction_counts = process_reaction_counts(reaction_counts)
    processed_revenue_model = atom_to_string(revenue_model)
    processed_moderation_status = atom_to_string(moderation_status)
    processed_release_type = atom_to_string(release_type)
    processed_immersive_format = atom_to_string(immersive_format)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      ai_music_id: process_string(ai_music_id),
      user_id: to_string(user_id),
      business_id: process_string(business_id),
      artist_id: process_string(artist_id),
      file_url: processed_file_url,
      ipfs_cid: processed_ipfs_cid,
      original_filename: process_string(original_filename),
      file_size_bytes: file_size_bytes,
      status: processed_status,
      format: process_string(format),
      bitrate: bitrate,
      sample_rate: sample_rate,
      channels: channels,
      available_qualities: process_list(available_qualities),
      title: to_string(title),
      artist: process_string(artist),
      featured_artists: process_string_list(featured_artists),
      album: process_string(album),
      album_id: process_string(album_id),
      album_art_cid: process_string(album_art_cid),
      album_art_url: process_string(album_art_url),
      duration_seconds: duration_seconds,
      genre: process_string_list(genre),
      sub_genre: process_string_list(sub_genre),
      mood: process_string_list(mood),
      tempo_bpm: tempo_bpm,
      key: process_string(key),
      time_signature: process_string(time_signature),
      composer: process_string_list(composer),
      lyricist: process_string_list(lyricist),
      producer: process_string_list(producer),
      label: process_string(label),
      isrc: process_string(isrc),
      upc: process_string(upc),
      copyright_info: process_string(copyright_info),
      license_type: process_string(license_type),
      lyrics_cid: process_string(lyrics_cid),
      lyrics_synced: process_list(lyrics_synced),
      has_explicit_content: has_explicit_content,
      language: process_string(language),
      waveform_cid: process_string(waveform_cid),
      spectrum_analysis_cid: process_string(spectrum_analysis_cid),
      peaks_data: process_list(peaks_data),
      audio_fingerprint: process_string(audio_fingerprint),
      detected_instruments: process_string_list(detected_instruments),
      vocal_presence: vocal_presence,
      energy_level: energy_level,
      danceability: danceability,
      acousticness: acousticness,
      instrumentalness: instrumentalness,
      auto_generated_tags: process_string_list(auto_generated_tags),
      plays: plays,
      unique_listeners: unique_listeners,
      listen_time_total: listen_time_total,
      completion_rate: completion_rate,
      likes: process_string_list(likes),
      reactions: processed_reactions,
      reaction_counts: processed_reaction_counts,
      comments: process_string_list(comments),
      shares: shares,
      saves: saves,
      added_to_playlists: added_to_playlists,
      hashtags: process_string_list(hashtags),
      mentions: process_string_list(mentions),
      collaborators: process_string_list(collaborators),
      privacy: processed_privacy,
      allow_comments: allow_comments,
      allow_downloads: allow_downloads,
      allow_remixes: allow_remixes,
      playlist_ids: process_string_list(playlist_ids),
      track_number: track_number,
      disc_number: disc_number,
      total_tracks: total_tracks,
      monetized: monetized,
      revenue_model: processed_revenue_model,
      price_download: price_download,
      price_stream_per_play: price_stream_per_play,
      streaming_royalties: process_map(streaming_royalties),
      skip_rate: skip_rate,
      replay_count: replay_count,
      geographic_plays: process_map(geographic_plays),
      device_breakdown: process_map(device_breakdown),
      referral_sources: process_map(referral_sources),
      peak_concurrent_listeners: peak_concurrent_listeners,
      listening_hours: process_list(listening_hours),
      similar_tracks: process_string_list(similar_tracks),
      radio_seed_weight: radio_seed_weight,
      discovery_score: discovery_score,
      live_recording: live_recording,
      concert_date: handle_datetime(concert_date),
      venue: process_string(venue),
      original_track_id: process_string(original_track_id),
      remix_type: process_string(remix_type),
      remixes: process_string_list(remixes),
      versions: process_list(versions),
      reported: process_string_list(reported),
      moderation_status: processed_moderation_status,
      age_restriction: age_restriction,
      content_warnings: process_string_list(content_warnings),
      pin_info: process_map(pin_info),
      cdn_urls: process_map(cdn_urls),
      peer_availability: process_list(peer_availability),
      release_date: handle_datetime(release_date),
      release_type: processed_release_type,
      pre_release: pre_release,
      premiere_timestamp: handle_datetime(premiere_timestamp),
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      date_published: handle_datetime(date_published),
      last_played: handle_datetime(last_played),
      upload_device_info: process_map(upload_device_info),
      processing_time_ms: processing_time_ms,
      spatial_audio_cid: process_string(spatial_audio_cid),
      binaural_audio_cid: process_string(binaural_audio_cid),
      immersive_format: processed_immersive_format,
      head_tracking_data: process_map(head_tracking_data),
      separated_stems_cids: process_map(separated_stems_cids),
      stem_quality_score: stem_quality_score,
      remix_template_cid: process_string(remix_template_cid),
      midi_file_cid: process_string(midi_file_cid),
      chord_progression: process_string_list(chord_progression),
      audio_embedding: process_string(audio_embedding),
      musical_structure: process_list(musical_structure),
      harmonic_complexity_score: harmonic_complexity_score,
      vocal_melody_cid: process_string(vocal_melody_cid),
      rhythm_pattern_signature: process_string(rhythm_pattern_signature),
      collaboration_sessions: process_string_list(collaboration_sessions),
      multitrack_project_cid: process_string(multitrack_project_cid),
      version_history: process_list(version_history),
      contributor_credits: process_list(contributor_credits),
      royalty_split_smart_contract: process_string(royalty_split_smart_contract),
      live_performance_recordings: process_string_list(live_performance_recordings),
      setlist_context: process_map(setlist_context),
      concert_sync_id: process_string(concert_sync_id),
      live_jam_session_id: process_string(live_jam_session_id),
      virtual_concert_cid: process_string(virtual_concert_cid),
      sample_clearances: process_list(sample_clearances),
      mechanical_license_info: process_map(mechanical_license_info),
      sync_licensing_available: sync_licensing_available,
      territorial_rights: process_map(territorial_rights),
      creative_commons_derivative_tree: process_string_list(creative_commons_derivative_tree),
      blockchain_rights_token: process_string(blockchain_rights_token),
      sonic_signature: process_string(sonic_signature),
      influence_tags: process_string_list(influence_tags),
      era_classification: process_string(era_classification),
      cultural_context: process_string_list(cultural_context),
      listening_context_scores: process_map(listening_context_scores),
      crossover_potential: crossover_potential,
      adaptive_layers_cids: process_map(adaptive_layers_cids),
      interactive_mixing_params: process_map(interactive_mixing_params),
      generative_rules: process_map(generative_rules),
      loop_points: process_list(loop_points),
      branching_sections: process_list(branching_sections),
      lufs_integrated: lufs_integrated,
      dynamic_range: dynamic_range,
      true_peak_db: true_peak_db,
      mastering_chain: process_string_list(mastering_chain),
      reference_tracks: process_string_list(reference_tracks),
      mastering_engineer: process_string(mastering_engineer),
      remaster_versions: process_string_list(remaster_versions),
      fan_tier_access: process_map(fan_tier_access),
      early_access_until: handle_datetime(early_access_until),
      limited_edition_vinyl_link: process_string(limited_edition_vinyl_link),
      fan_token_gated: fan_token_gated,
      community_remix_challenge_id: process_string(community_remix_challenge_id),
      behind_the_music_cid: process_string(behind_the_music_cid),
      producer_commentary_cid: process_string(producer_commentary_cid),
      exclusive_listening_parties: process_string_list(exclusive_listening_parties),
      radio_seed_enabled: radio_seed_enabled,
      included_in_playlists: process_string_list(included_in_playlists),
      playlist_reach: playlist_reach,
      editorial_playlist_picks: process_string_list(editorial_playlist_picks),
      user_taste_match: process_map(user_taste_match),
      blend_compatibility_score: blend_compatibility_score,
      vibe_tags: process_string_list(vibe_tags),
      time_of_day_affinity: process_map(time_of_day_affinity),
      friend_activity: process_list(friend_activity),
      collaborative_playlists: process_string_list(collaborative_playlists),
      queue_additions: queue_additions,
      viral_charts_position: process_list(viral_charts_position),
      new_music_friday_featured: new_music_friday_featured,
      release_radar_appearances: release_radar_appearances,
      discover_weekly_appearances: discover_weekly_appearances,
      monthly_listeners_trend: process_list(monthly_listeners_trend),
      listener_demographics: process_map(listener_demographics),
      fan_concentration: process_map(fan_concentration),
      top_cities: process_string_list(top_cities),
      playlist_conversion_rate: playlist_conversion_rate,
      skip_30sec_rate: skip_30sec_rate,
      save_rate: save_rate,
      playlist_add_rate: playlist_add_rate,
      share_rate: share_rate,
      reposts: process_string_list(reposts),
      repost_count: repost_count,
      spotlight_tracks: process_string_list(spotlight_tracks),
      timed_comments: process_string_list(timed_comments),
      comment_heatmap: process_list(comment_heatmap),
      download_enabled: download_enabled,
      download_count: download_count,
      downloadable_formats: process_string_list(downloadable_formats),
      private_share_links: process_string_list(private_share_links),
      scheduled_release: handle_datetime(scheduled_release),
      secret_link: process_string(secret_link),
      fan_powered_royalties: fan_powered_royalties,
      tip_jar_enabled: tip_jar_enabled,
      tips_received: process_list(tips_received),
      pro_unlimited_tier: pro_unlimited_tier,
      waveform_comments_enabled: waveform_comments_enabled,
      waveform_color: process_string(waveform_color),
      waveform_style: process_string(waveform_style),
      seekable_waveform_cid: process_string(seekable_waveform_cid),
      replace_file_history: process_list(replace_file_history),
      mastered_on_soundcloud: mastered_on_soundcloud,
      plays_per_country: process_map(plays_per_country),
      plays_per_source: process_map(plays_per_source),
      embedded_plays: embedded_plays,
      lossless_available: lossless_available,
      lossless_cid: process_string(lossless_cid),
      hi_res_lossless: hi_res_lossless,
      hi_res_cid: process_string(hi_res_cid),
      audio_quality_tier: process_string(audio_quality_tier),
      dolby_atmos_available: dolby_atmos_available,
      dolby_atmos_cid: process_string(dolby_atmos_cid),
      apple_digital_master: apple_digital_master,
      time_synced_lyrics: process_list(time_synced_lyrics),
      lyrics_language_versions: process_map(lyrics_language_versions),
      lyrics_translation: process_map(lyrics_translation),
      sing_along_mode: sing_along_mode,
      apple_music_1_plays: apple_music_1_plays,
      station_appearances: process_string_list(station_appearances),
      dj_mentions: process_list(dj_mentions),
      apple_music_editor_notes: process_string(apple_music_editor_notes),
      curator_picks: process_string_list(curator_picks),
      essential_album: essential_album,
      music_video_id: process_string(music_video_id),
      video_available: video_available,
      behind_the_lyrics_video: process_string(behind_the_lyrics_video),
      composer_details: process_map(composer_details),
      conductor: process_string(conductor),
      orchestra: process_string(orchestra),
      movement_number: movement_number,
      work_id: process_string(work_id),
      catalog_number: process_string(catalog_number),
      shazam_count: shazam_count,
      shazam_trending: shazam_trending,
      shazam_discovery_rank: shazam_discovery_rank,
      optimal_crossfade_point: optimal_crossfade_point,
      gapless_playback: gapless_playback,
      crossfade_duration: crossfade_duration,
      ai_extended_version_cid: process_string(ai_extended_version_cid),
      ai_radio_edit_cid: process_string(ai_radio_edit_cid),
      ai_instrumental_cid: process_string(ai_instrumental_cid),
      ai_acapella_cid: process_string(ai_acapella_cid),
      ai_8d_audio_cid: process_string(ai_8d_audio_cid),
      transition_compatibility: process_map(transition_compatibility),
      bpm_locked: bpm_locked,
      harmonic_mixing_key: process_string(harmonic_mixing_key),
      energy_curve: process_list(energy_curve),
      first_listeners_club: process_string_list(first_listeners_club),
      superfan_unlocks: process_list(superfan_unlocks),
      listening_milestones: process_map(listening_milestones),
      fan_challenges: process_list(fan_challenges),
      open_verse_enabled: open_verse_enabled,
      stem_remix_competition_id: process_string(stem_remix_competition_id),
      community_cover_versions: process_string_list(community_cover_versions),
      listener_journey_map: process_list(listener_journey_map),
      retention_by_segment: process_list(retention_by_segment),
      viral_coefficient: viral_coefficient,
      playlist_velocity: playlist_velocity,
      tiktok_usage_count: tiktok_usage_count,
      instagram_story_usage: instagram_story_usage,
      youtube_shorts_usage: youtube_shorts_usage,
      viral_moment_timestamp: viral_moment_timestamp,
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

  defp process_cid({:pending, _id}), do: "processing"
  defp process_cid({:pending_update, _id}), do: "processing"
  defp process_cid({:pending_version, _id}), do: "processing"
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
end
