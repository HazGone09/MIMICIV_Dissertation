If you've found this repository, welcome to the madness that is my final year dissertation piece
Course Focus is 'population health and medical science'
The data used is MIMIC IV v3.1

Project Aim: This project uses MIMIC-IV data to investigate the development of opioid use disorder (OUD), with the aim of identifying when and in whom OUD emerges following opioid exposure.


Initial code posted 4/3/26 includes cohort building and some graphical modelling to show initial observations.


27/4/26 - Big update time

Finally finished the full pipeline, My advice is to follow the scripts in the numbered order, the Diss.R file if the full 'notebook-style' file that i used for the majority of this, then changed over to separate scripts for ease of use.

The aim of the final 9 scripts is to develop a cox model to identify point the of an OUD being developed after the administration of opiates within hospital

The project is structured as follows:

01_boot.R  
Loads required packages and raw MIMIC-IV data sources. (obviously you will need access to MIMIC IV to be able to follow the project fully)

02_ptlist.R  
Defines the study cohort by linking patient demographics and diagnoses.

03_opioid_features.R  
Extracts and processes opioid administration data from EMAR, generating exposure variables and temporal summaries.

04_antidepressant_features.R  
Identifies antidepressant exposure and derives mental health-related covariates.

05_analysis_df.R  
Combines all derived features into a single patient-level dataset for analysis.

07_modelling.R  
Constructs survival models (mainly a Cox proportional hazards model) to evaluate time to an OUD being diagnosed within hospital.

08_visualisation.R  
Generates figures to illustrate survival patterns, exposure trends, and model outputs.

09_outputs.R  
Produces formatted tables for reporting results, including model outputs and diagnostics.
