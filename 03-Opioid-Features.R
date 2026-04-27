#Opioids

# Creating a look up list of the most commonly prescripbed opiated, including methadone and buprenorphine so we can scale prescription vs treatment.
# the emar has a number of spelling error and differing dosages, we don't really care about strength of drug so we'll regex the list to standardize everything.

Opi_lookup <- c(
  "MORPHINE",
  "OXYCODONE",
  "BUPRENORPHINE",
  "METHADONE",
  "HYDROMORPHONE",
  "TRAMADOL",
  "FENTANYL",
  "HYDROCODONE"
)

emar_opioids <- emar_clean %>%
  filter(grepl(paste(Opi_lookup, collapse = "|"), medication))


emar_opioids_simplified <- emar_opioids %>%
  mutate(
    primary_opioid = case_when(
      grepl("MORPHINE", toupper(medication)) ~ "Morphine",
      grepl("OXYCODONE", toupper(medication)) ~ "Oxycodone",
      grepl("HYDROMORPHONE", toupper(medication)) ~ "Hydromorphone",
      grepl("FENTANYL", toupper(medication)) ~ "Fentanyl",
      grepl("METHADONE", toupper(medication)) ~ "Methadone",
      grepl("TRAMADOL", toupper(medication)) ~ "Tramadol",
      grepl("BUPRENORPHINE", toupper(medication)) ~ "Buprenorphine",
      grepl("HYDROCODONE", toupper(medication)) ~ "Hydrocodone",
      TRUE ~ "Other opioid"
    )
  )


opioid_summary <- emar_opioids_simplified %>%
  group_by(subject_id) %>%
  summarise(
    total_opioid_admins = n(),
    opioid_exposure = 1,
    .groups = "drop"
  )

# building a plot to show administration trends

opioid_plot_df <- emar_opioids_simplified %>%
  group_by(subject_id, primary_opioid) %>%
  summarise(n_admins = n(), .groups = "drop")


opioid_plot_df <- opioid_plot_df %>%
  left_join(
    PtList_clean %>%
      select(subject_id, anchor_year_group),
    by = "subject_id"
  )

opioid_plot_df <- opioid_plot_df %>%
  group_by(anchor_year_group, primary_opioid) %>%
  summarise(n_admins = sum(n_admins), .groups = "drop")
