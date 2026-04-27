# 05-Feature-Clinical-Flags

MH_flag_df <- PtList_clean %>%
  filter(grepl("^F2[0-9]|^F3[0-9]|^F4[0-8]", icd_code)) %>%
  distinct(subject_id) %>%
  mutate(MH_flag = 1)

OUD_flag_df <- PtList_clean %>%
  filter(
    startsWith(icd_code, "F11") | # Opioid Related Conditions
      startsWith(icd_code, "T40") | # Narcotic Related Conditions
      startsWith(icd_code, "Z79891") # Long term use of opioid analgesia
  ) %>%
  distinct(subject_id) %>%
  mutate(OUD_flag = 1)
