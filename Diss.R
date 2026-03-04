# Project Aim: investigating links between the prescription of opiates within hospitals, and subsequent mental health deterioration(s)

# utilizing MIMIC-IV v3.1 (OCT 2024)

# installing and loading packages

install.packages("tidyverse")
install.packages("duckdb")
install.packages("duckplyr")
install.packages("purrr")

library(tidyverse)
library(duckdb)
library(duckplyr)
library(purrr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# HOSP MODULE

# I've downloaded several parts of the hosp module to use throughout this project:

# admissions.csv - dates, times, and methods of admission, also included data relating to pt deaths.
# d_icd_diagnoses.csv - a directory of the ICD codes used for diagnoses.
# diagnoses_icd.csv - list of patients with assigned icd code to show reasons for admission.
# patients.csv - the patient list, including age, gender and date of death.
# prescriptions.csv - the largest file, a record of all prescriptions issued for all patients throughout their stay.
# emar.csv - the medication administration records for patients, used to track rates of medication administration.

#~~~~~~~~~~~~~~~~~~~~~~~~~COHORT BUILDING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#first things first, there's a lot of data within MIMIC, as opposed to actually looking for prescription of opiates within hospital, i want to find some info -
#relating to admissions that were caused by opiates, I can then use this to see if any of these patients have previously been prescribed opiates by the hosp.

#By working off of opioids, i can identify whether they are also receiving some form of psychiatric treatment, primarily by looking for administrations of 
#Antidepressants like SSRI's.

ICD_diagnoses <- read.csv("d_icd_diagnoses.csv") #loading icd diagnosis data into R to test for memory constraints.
# view(ICD_diagnoses)
#filtering down to ICD codes for opiate relating admissions, ICD codes F11 & T40
#(which appear to be the main ones relating to opiates/addiction F11=ORD, T40=poisoning by narcotics)

opiate_icd_codes <- ICD_diagnoses %>%
  filter(startsWith(icd_code, "F11") | # Specific Code relating to Mental and behavioral disorders caused by use of opioids
           startsWith(icd_code, "T40") | # Poisoning by Narcotic
           startsWith(icd_code, "Z79891") # Long-term use of opioid analgesic
         )

# we now have a list of codes relating to opiates, lets try and load the admissions table and use the list to work out admissions

admissions <- read.csv("admissions.csv")
#this doesn't highlight reason for admission (because that would be simple...) lets try the patients table

#this contains the date and time of admission under 'admittime', but also useful variables that I overlooked earlier,
#such as death, race, marital status, and insurance type, which may be beneficial for future analysis.

patients <- read.csv("patients.csv")
# view(patients) # this doesn't either, I need to find a db that has PTid and RFA together in order to be able to trim my data to a manageable size.

Patient_Diagnoses <- read.csv("diagnoses_icd.csv.gz")
#view(Diagnoses_list) #ding ding ding! we have a winner, this is what we use to apply our icd diagnoses list to.


opiate_admissions <- Patient_Diagnoses %>%
  filter(
    startsWith(icd_code, "F11") | # Opioid Related Conditions
      startsWith(icd_code, "T40") | # Narcotic Related Conditions
      startsWith(icd_code, "Z79891") # Long term use of opioid analgesia
  )

#view(opiate_admissions) # this has returned a list of 12,296 admissions relating to opiates. 
#Now i want to perform a left join in order to display the 'long_title' RFA.

Opioid_related_admissions <- opiate_admissions %>%
  left_join(ICD_diagnoses %>% select(icd_code, long_title), by = "icd_code")

# nrow(opiate_admissions) # = 12,296, not to sure why sidebar displays "??"
# perfect, now i want to create a table to show my the number of admissions per reason, so i can work out the most common reasons for admission ect.

Op_admission_counts <- opiate_admissions %>%
  group_by(icd_code, long_title) %>%
  summarise(n_admissions = n(), .groups = "drop") %>%
  arrange(desc(n_admissions))

# this has provided me a really good list, but it has highlighted a slight issue, there are non-opiate RFAs being picked up (cocaine, cannabis ect).
# These all originate from the T40 icd code, but its something to work out when i've collated my full table.

# i now want to create a full patient list of information, by joining my admissions with RFA DF to the MIMIC patients table

PtList <- Opioid_related_admissions %>%
  inner_join(patients, by = "subject_id")

PtList <- PtList %>%
  left_join(
    admissions %>%
      select(
        subject_id,
        hadm_id,
        admittime,
        dischtime,
        deathtime,
        race,
        insurance
      ),
    by = c("subject_id", "hadm_id")
  )

PT_Opioid_admissions <- PtList %>%
  distinct(subject_id, hadm_id, admittime, dischtime) # 1 row per admission, in order to work out readmitted pts/exclude grouping

#this has provided me with a full patient list, lets now put together some descriptive statistics.

# first, as subject_id is pt specific, lets have a look and see if there are any 'frequent flyers'

readmitted_patients <- PtList %>%
  count(subject_id) %>%
  filter(n > 1)

#nrow(readmitted_patients) #2166.

readmitted_patients %>%
  count(n > 10) #60 patients have been admitted on >10 occasions.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Admission Rates

admissions_by_year <- PtList %>%
  count(anchor_year_group, name = "n_admissions") #Admission by anchor year grp to determine ROA

admissions_per_patient <- PtList %>%
  group_by(anchor_year_group, subject_id) %>%
  summarise(n_admissions = n(), .groups = "drop")

admission_rates <- admissions_per_patient %>%
  group_by(anchor_year_group) %>%
  summarise(
    mean_admissions = mean(n_admissions),
    median_admissions = median(n_admissions),
    n_patients = n(),
    .groups = "drop"
  )


#ggplot(admission_rates,
#       aes(x = anchor_year_group, y = median_admissions)) +
#  geom_col(fill = "#009E73") +
#  labs(x = "Anchor Year Group", y = "Median Admissions per Patient", title = "Median Admission Rates Over Time") +
#  theme_minimal(base_size = 14) #MEDIAN ROA -ALL 1

#ggplot(admission_rates, aes(x = anchor_year_group, y = mean_admissions)) +
#  geom_col(fill = "#0072B2") +
#  labs(x = "Anchor Year Group", y = "Mean Admissions per Patient", title = "Admission Rates Over Time") +
#  theme_minimal(base_size = 14) #MEAN ROA - ACTUAL DIFFERENTIATION



#PtList %>%
#  count(anchor_year_group)        # admissions

# PtList %>%
#  distinct(subject_id, anchor_year_group) %>%
#  count(anchor_year_group)        # patients

# considering the counts between adm and pt over AYG. it follows graphical trend, going from 2 per pt down to around 1.35-4 over time

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I now want to create a pie chart to show the ages of patients admitted.
PtList <- PtList %>%
  mutate(age_band = cut(
    anchor_age,
    breaks = c(18, 30, 40, 50, 60, 70, 80, Inf),
    labels = c("18–29", "30–39", "40–49", "50–59", "60–69", "70–79", "80+"),
    right = FALSE
  )) # Age Banding.


age_dist <- PtList %>%
  filter(!is.na(age_band)) %>%
  count(age_band) %>%
  mutate(prop = n / sum(n))

age_palette <- c(
  "#0072B2",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#E69F00",
  "#D55E00",
  "#CC79A7",
  "#999999",
  "#66C2A5",
  "#FC8D62",
  "#8DA0CB",
  "#E78AC3",
  "#A6D854"
)

ggplot(age_dist, aes(x = "", y = n, fill = age_band)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = age_palette) +
  labs(title = "Age distribution of patients", fill = "Age group") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# pie chart to highlight age distribution.

Age_count <- PtList %>%
  count(anchor_age)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Building a list of deceased pts

Deceasedcount <- PtList %>%
  count(dod) %>%
  filter(n > 1)

#this provides me a list of the dates that pts died, but not a filter to show patient id's specifically, ideally i want to see RFAs alongside pts so i can
# track drugs administered during their visit.
# due to the way mimic works, this also does included patients that died up to 1 year post admission,
# i would be interested in seeing pts that died during admission

PtList <- PtList %>%
  mutate(died_during_admission =
           !is.na(deathtime) &
           deathtime >= admittime &
           deathtime <= dischtime)


DDA <- PtList  %>%
  count(died_during_admission) #DDA = 289

death_summary <- PtList %>%
  group_by(anchor_year_group) %>%
  summarise(
    deaths = sum(died_during_admission, na.rm = TRUE),
    total_patients = n(),
    death_rate = deaths / total_patients,
    .groups = "drop"
  )


ggplot(death_summary,
       aes(x = anchor_year_group, y = death_rate, group = 1)) +
  geom_line(linewidth = 1, colour = "#D55E00") +
  geom_point(size = 3, colour = "#D55E00") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Anchor Year Group", y = "In-hospital Death Rate", title = "In-hospital Mortality Over Time") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PRESCRIPTIONS.CSV

# this thing is FUCKING MASSIVE...
# some serious amount of kitbashing is going to be required to even get this thing readable, let alone viewable, its the main reason why duckplyr is used.

scripts <- read_csv_duckdb('prescriptions.csv')


# still doesn't like it, lets try and force it through
Opiate_Related_Prescriptions <- scripts %>%
  semi_join(PtList %>% distinct(subject_id), by = "subject_id") %>%
  as_tibble()

#Thank you stackoverflow for that one, its now viewable- total rx's given for all pts is > 1.85mil, but we've got something workable now!!
# initial obs- this list shows the medication prescribe, but not the medication administered. That's found in the Emar

# https://ieeexplore.ieee.org/document/9474745 provides a decent idea on how to group the opiates, it's not feasible to list all of the GSN(s), and the
# self described 'formulary code' has different drugs under a similar code (Methadone/Methotrexate), so the idea is to create a list of the most commonly used
# opiates to be able to condense the scripts list. Ive also now downloaded the emar dbs as they show medication that has actually been administered to pts.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Emar.csv

emar <- read_csv_duckdb('emar.csv.gz')

emar <- emar %>%
  semi_join(PtList %>% distinct(subject_id), by = "subject_id") %>%
  as_tibble()         # 6.3 million records... but they are marked as 'administered, not given, ect. I just want administered

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Opioid Prescription Stats

emarGiven <- emar %>%
  filter(event_txt == "Administered") 
# >4.5million total items administered, not filtered to opiates though.

# grouping opioids
# initial plan is to create a lookup table, and filter the emarGiven by the lookup table.

Opi_lookup <- c(
  "MORPHINE" ,
  "OXYCODONE" ,
  "BUPRENORPHINE" ,
  "METHADONE" ,
  "HYDROMORPHONE" ,
  "TRAMADOL" ,
  "FENTANYL" ,
  "HYDROCODONE" 
)

# This is our Lookup table, using the most popular opiates, we will run searches later for antagonists such as naltrexone and naloxone.

opioid_prescriptions <- emarGiven %>%
  filter(grepl(paste(Opi_lookup, collapse = "|"), toupper(medication))) 

# this has returned exactly what i wanted it to, showing me that there are 56 different types of opioids prescribed within mimic according to the list that -
# - i defined. Now i want to simplify this down.

emar_opioids_simplified <- opioid_prescriptions %>%
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

opioid_script_summary <- emar_opioids_simplified %>%
  group_by(primary_opioid) %>%
  summarise(
    n_administrations = n(),
    n_patients = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  arrange(desc(n_administrations))


# Now we have a list of all the instances in which a pt was specifically given an opiate.
# lets get some quick stats on the figures, the full and the simplified table.

OpiCount <- opioid_prescriptions %>%
  count(medication) # all opioids prescribed

OpiCount_Simple <- opioid_simplified %>%
  count(primary_opioid) # simplified list as per

# Most commonly given opiate, as per simple list = 'OXYCODONE',  then 'HYDROMORPHONE', then 'MORPHINE', then 'METHADONE'.

# ~~~~~~~~~~~~~~~Prescribing over time~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# as we now have a list of all the prescribed opioids, as well as an anchor year group, i can create a GGplot to show the rates of prescription of opiates
# over time.

emar_opioids_simplified <- opioid_simplified %>%
  left_join(PtList %>%
              distinct(subject_id, anchor_year_group, icd_code, long_title), by = "subject_id")


opioid_plot_df <- emar_opioids_simplified %>%
  group_by(anchor_year_group, primary_opioid) %>%
  summarise(n_admins = n(), .groups = "drop")
# providing counts of opioids prescribed per anchor year grp

opioid_year_counts <- emar_opioids_simplified %>%
  distinct(subject_id, primary_opioid, anchor_year_group) %>%
  count(anchor_year_group, primary_opioid)

opioid_order <- c(
  "Morphine",
  "Oxycodone",
  "Hydromorphone",
  "Methadone",
  "Tramadol",
  "Fentanyl",
  "Buprenorphine"
)


#~~~~~~~~~~~~~~~~~~~GRAPHING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

opioid_levels <- sort(unique(opioid_plot_df$primary_opioid))

opioid_palette <- c(
  "#0072B2",# lets make it pretty
  "#D55E00",
  "#009E73",
  "#CC79A7",
  "#F0E442",
  "#56B4E9",
  "#E69F00",
  "#000000"
)

opioid_colours <- setNames(opioid_palette[seq_along(opioid_levels)], opioid_levels)

ggplot(opioid_plot_df,
       aes(x = anchor_year_group, y = n_admins, fill = primary_opioid)) +
  geom_col() +
  scale_fill_manual(values = opioid_colours) +
  labs(x = "Anchor Year Group",
       y = "Number of Administrations",
       title = "Opioid Prescription Count Over Time",
       fill = "Opioid") +
  theme_minimal(base_size = 14)

# purrr is being used to create distinct plots by type of opioid.


Opioid_plots <- opioid_plot_df %>%
  split(.$primary_opioid) %>%
  map(
    ~ ggplot(.x, aes(
      x = anchor_year_group, y = n_admins, fill = primary_opioid
    )) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = opioid_colours) +
      labs(
        title = unique(.x$primary_opioid),
        x = "Anchor Year Group",
        y = "Number of Administrations"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  )


iwalk(Opioid_plots,
      ~ ggsave(
        filename = paste0("opioid_", .y, ".tiff"),
        plot = .x,
        width = 8,
        height = 5,
        dpi = 300
      ))
# purrr being used to export graphs.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CO-PRESCRIBING

# ANTIDEPRESSANTS

# First of all lets build a list of the most commonly prescribed antidepressants to be able to see if our group of pts are on them.
# This list includes cyclic antidepressants such as amitri and noritri, but as they have multiple uses, we will categorise them and highlight that they are
# slightly anomalous.


ADEP_lookup <- c(
  "SERTRALINE" ,
  "FULOXETINE" ,
  "TRAZODONE" ,
  "CITALOPRAM" ,
  "ESCITALOPRAM" ,
  "PAROXETINE" ,
  "VENLAFAXINE" ,
  "DULOXETINE" ,
  "BUPROPION" ,
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


Adep_prescriptions <- emarGiven %>%
  filter(
    grepl(
      paste(ADEP_lookup, collapse = "|"), toupper(medication)))

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

#I want to group these by drug class so I can drop tricyclics from my graphs.

Adep_simplified <- Adep_simplified %>%
  mutate(
    Antidepressant_class = case_when(
      Antidepressant_drug %in% c(
        "Sertraline",
        "Fluoxetine",
        "Citalopram",
        "Escitalopram",
        "Paroxetine"
      ) ~ "SSRI",
      
      Antidepressant_drug %in% c(
        "Venlafaxine",
        "Desvenlafaxine", 
        "Duloxetine"
        ) ~ "SNRI",
      
      Antidepressant_drug %in% c(
        "Bupropion",
        "Mirtazapine",
        "Trazodone",
        "Vilazodone",
        "Vortioxetine"
      ) ~ "Atypical antidepressant",
      
      Antidepressant_drug %in% c(
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
  "Sertraline"      = "#1F77B4",
  "Fluoxetine"     = "#AEC7E8",
  "Citalopram"     = "#2CA02C",
  "Escitalopram"   = "#98DF8A",
  "Paroxetine"     = "#17BECF",
  
  # SNRIs
  "Venlafaxine"    = "#FF7F0E",
  "Desvenlafaxine" = "#FFBB78",
  "Duloxetine"     = "#D62728",
  
  # Atypical antidepressants
  "Bupropion"      = "#9467BD",
  "Mirtazapine"    = "#C5B0D5",
  "Trazodone"      = "#8C564B",
  "Vilazodone"     = "#E377C2",
  "Vortioxetine"   = "#F7B6D2",
  
  # Tricyclics (kept muted on purpose)
  "Amitriptyline"  = "#7F7F7F",
  "Nortriptyline"  = "#BCBD22",
  "Imipramine"     = "#DBDB8D",
  "Doxepin"        = "#C7C7C7",
  
  # MAOIs (rare → strong but distinct)
  "Phenelzine"     = "#393B79",
  "Tranylcypromine"= "#637939"
)


AdepCount <- Adep_prescriptions %>%
  count(medication) # all Adeps Administered

AdepCount_Simple <- Adep_simplified %>%
  count(Antidepressant_drug, Antidepressant_class) # simplified list

#~~~~~~~~~~~ CHART BUILDING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#`~~~~~~~~~~BY CLASS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Dropping tricyclics from chart

Adep_simplified <- Adep_simplified %>%
  left_join(PtList %>%
              select(anchor_year_group, subject_id), by = "subject_id")

Adep_no_tca <- Adep_simplified %>%
  filter(Antidepressant_class != "Tricyclic (multiple indications)")


Adep_year_counts <- Adep_no_tca %>%
  count(anchor_year_group, Antidepressant_class)

# Patient-level counts
Adep_no_tca %>%
  distinct(subject_id, anchor_year_group, Antidepressant_class) %>%
  count(anchor_year_group, Antidepressant_class)

# Administration-level counts
Adep_no_tca %>%
  count(anchor_year_group, Antidepressant_class)


ggplot(Adep_year_counts,
       aes(x = anchor_year_group, y = n, fill = Antidepressant_class)) +
  geom_col() +
  labs(title = "Antidepressant prescriptions by year",
       x = "Anchor year group",
       y = "Administered Medication Count",
       fill = "Drug class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

#~~~~~~~~~~~~~~~~~~~~BY DRUG~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Adep_Simple_count <- Adep_simplified %>%
  count(anchor_year_group, Antidepressant_drug)


Adep_drug_plots <- Adep_Simple_count %>%
  split(.$Antidepressant_drug) %>%
  map(
    ~ ggplot(.x, aes(
      x = anchor_year_group, y = n, fill = Antidepressant_drug
    )) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = adep_palette) +
      labs(
        title = unique(.x$Antidepressant_drug),
        x = "Anchor Year Group",
        y = "Number of Administrations"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  )

iwalk(Adep_drug_plots,
      ~ ggsave(
        filename = paste0("Adep_", .y, ".tiff"),
        plot = .x,
        width = 8,
        height = 5,
        dpi = 300
      ))
