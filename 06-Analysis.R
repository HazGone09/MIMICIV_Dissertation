# Analysis
# Times

admission_times <- admissions_clean %>%
  group_by(subject_id) %>%
  summarise(
    start_time = min(admittime, na.rm = TRUE),
    end_time = max(dischtime, na.rm = TRUE),
    death_time = if (all(is.na(deathtime))) {
      NA
    } else {
      min(deathtime, na.rm = TRUE)
    },
    .groups = "drop"
  )

diagnoses_with_time <- diagnoses_clean %>%
  left_join(
    admissions_clean %>%
      select(hadm_id, admittime),
    by = "hadm_id"
  )

OUD_times <- diagnoses_with_time %>%
  filter(
    startsWith(icd_code, "F11") | # Opioid related disorders
      startsWith(icd_code, "T40") | # Poisoning / adverse effects
      startsWith(icd_code, "Z79891") # Long-term opioid use
  ) %>%
  group_by(subject_id) %>%
  summarise(
    event_time = min(admittime),
    .groups = "drop"
  )


# Building an analysis frame

Analysis_df <- PtList_clean %>%
  distinct(subject_id, .keep_all = TRUE) %>%

  left_join(opioid_summary, by = "subject_id") %>%
  left_join(Adep_exposure_df, by = "subject_id") %>%
  left_join(MH_flag_df, by = "subject_id") %>%
  left_join(OUD_flag_df, by = "subject_id") %>%

  mutate(
    opioid_exposure = ifelse(is.na(opioid_exposure), 0, opioid_exposure),
    total_opioid_admins = ifelse(
      is.na(total_opioid_admins),
      0,
      total_opioid_admins
    ),
    Adep_exposure = ifelse(is.na(Adep_exposure), 0, Adep_exposure),
    MH_flag = ifelse(is.na(MH_flag), 0, MH_flag),
    OUD_flag = ifelse(is.na(OUD_flag), 0, OUD_flag)
  )


Analysis_df <- Analysis_df %>%
  left_join(admission_times, by = "subject_id")

Analysis_df <- Analysis_df %>%
  left_join(OUD_times, by = "subject_id")

#building survival variables

Analysis_df <- Analysis_df %>%
  mutate(
    final_end_time = pmin(event_time, death_time, end_time, na.rm = TRUE),
    final_end_time = ifelse(is.infinite(final_end_time), NA, final_end_time),

    event = ifelse(
      !is.na(event_time) &
        abs(as.numeric(difftime(event_time, final_end_time, units = "secs"))) <
          1,
      1,
      0
    ),

    time_to_event = as.numeric(
      difftime(final_end_time, start_time, units = "days")
    )
  )
