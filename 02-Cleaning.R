# Cleaning

dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)

emar_clean <- emar %>%
  collect() %>%
  mutate(
    medication = toupper(trimws(medication)),
    subject_id = as.numeric(subject_id)
  )

diagnoses_clean <- diagnoses %>%
  mutate(
    icd_code = toupper(trimws(icd_code)),
    icd_code = gsub("\\.", "", icd_code),
    subject_id = as.numeric(subject_id)
  )

PtList_clean <- diagnoses_clean %>%
  left_join(patients, by = "subject_id") %>%
  mutate(
    anchor_age = as.numeric(anchor_age),
    anchor_year_group = factor(anchor_year_group, ordered = TRUE)
  )

admissions_clean <- admissions %>%
  mutate(
    subject_id = as.numeric(subject_id),
    admittime = ymd_hms(admittime),
    dischtime = ymd_hms(dischtime),
    deathtime = ymd_hms(deathtime)
  ) # needs lubridate due to deathtime being nonstandard in recording (too many NA's)


saveRDS(emar_clean, "data/cleaned/emar_clean.rds")
saveRDS(PtList_clean, "data/cleaned/PtList_clean.rds")
