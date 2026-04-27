# Antidepressants

# ANTIDEPRESSANTS

# First of all lets build a list of the possible prescribed antidepressants to be able to see if our group of pts are on them.
# This list includes cyclic antidepressants such as amitriptyline and noritriptyline, but as they have multiple uses, we will categorise them as atypcial and highlight that they are
# slightly anomalous.

Adep_lookup <- c(
  "SERTRALINE",
  "FULOXETINE",
  "TRAZODONE",
  "CITALOPRAM",
  "ESCITALOPRAM",
  "PAROXETINE",
  "VENLAFAXINE",
  "DULOXETINE",
  "BUPROPION",
  "MIRAZIPINE",
  "AMITRIPTYLINE",
  "NORITRYPTYLINE",
  "IMIPRIMINE",
  "DOXEPIN",
  "DESVENLAFAXINE",
  "VORTIOXETINE",
  "VILAZODONE",
  "PHENELZINE",
  "TRANYLCYPROMINE"
)


Adep_prescriptions <- emar_clean %>%
  filter(
    grepl(
      paste(Adep_lookup, collapse = "|"),
      toupper(medication)
    )
  )

Adep_simplified <- Adep_prescriptions %>%
  mutate(
    Antidepressant_drug = case_when(
      grepl("SERTRALINE", toupper(medication)) ~ "Sertraline",
      grepl("FLUOXETINE", toupper(medication)) ~ "Fluoxetine",
      grepl("CITALOPRAM", toupper(medication)) ~ "Citalopram",
      grepl("ESCITALOPRAM", toupper(medication)) ~ "Escitalopram",
      grepl("PAROXETINE", toupper(medication)) ~ "Paroxetine",
      grepl("VENLAFAXINE", toupper(medication)) ~ "Venlafaxine",
      grepl("DULOXETINE", toupper(medication)) ~ "Duloxetine",
      grepl("DESVENLAFAXINE", toupper(medication)) ~ "Desvenlafaxine",
      grepl("BUPROPION", toupper(medication)) ~ "Bupropion",
      grepl("MIRTAZAPINE", toupper(medication)) ~ "Mirtazapine",
      grepl("TRAZODONE", toupper(medication)) ~ "Trazodone",
      grepl("AMITRIPTYLINE", toupper(medication)) ~ "Amitriptyline",
      grepl("NORTRIPTYLINE", toupper(medication)) ~ "Nortriptyline",
      grepl("IMIPRAMINE", toupper(medication)) ~ "Imipramine",
      grepl("DOXEPIN", toupper(medication)) ~ "Doxepin",
      grepl("PHENELZINE", toupper(medication)) ~ "Phenelzine",
      grepl("TRANYLCYPROMINE", toupper(medication)) ~ "Tranylcypromine",
      grepl("VORTIOXETINE", toupper(medication)) ~ "Vortioxetine",
      grepl("VILAZODONE", toupper(medication)) ~ "Vilazodone",
      TRUE ~ "Other Antidepressant"
    )
  )

#I want to group these by drug class so I can drop tricyclics from my analysis.

Adep_simplified <- Adep_simplified %>%
  mutate(
    Antidepressant_class = case_when(
      Antidepressant_drug %in%
        c(
          "Sertraline",
          "Fluoxetine",
          "Citalopram",
          "Escitalopram",
          "Paroxetine"
        ) ~ "SSRI",

      Antidepressant_drug %in%
        c(
          "Venlafaxine",
          "Desvenlafaxine",
          "Duloxetine"
        ) ~ "SNRI",

      Antidepressant_drug %in%
        c(
          "Bupropion",
          "Mirtazapine",
          "Trazodone",
          "Vilazodone",
          "Vortioxetine"
        ) ~ "Atypical antidepressant",

      Antidepressant_drug %in%
        c(
          "Amitriptyline",
          "Nortriptyline",
          "Imipramine",
          "Doxepin"
        ) ~ "Tricyclic",

      TRUE ~ "Other / unclear"
    )
  )

adep_palette <- c(
  # SSRIs
  "Sertraline" = "#1F77B4",
  "Fluoxetine" = "#AEC7E8",
  "Citalopram" = "#2CA02C",
  "Escitalopram" = "#98DF8A",
  "Paroxetine" = "#17BECF",

  # SNRIs
  "Venlafaxine" = "#FF7F0E",
  "Desvenlafaxine" = "#FFBB78",
  "Duloxetine" = "#D62728",

  # Atypical antidepressants
  "Bupropion" = "#9467BD",
  "Mirtazapine" = "#C5B0D5",
  "Trazodone" = "#8C564B",
  "Vilazodone" = "#E377C2",
  "Vortioxetine" = "#F7B6D2",

  # Tricyclics (kept muted on purpose, due to multiple usage indications)
  "Amitriptyline" = "#7F7F7F",
  "Nortriptyline" = "#BCBD22",
  "Imipramine" = "#DBDB8D",
  "Doxepin" = "#C7C7C7",

  # MAOIs (rare strong but distinct)
  "Phenelzine" = "#393B79",
  "Tranylcypromine" = "#637939"
)
# again, graphics design is not my passion...

Adep_exposure_df <- Adep_simplified %>%
  distinct(subject_id) %>%
  mutate(Adep_exposure = 1)
