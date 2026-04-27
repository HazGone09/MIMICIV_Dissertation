# Downloading and booting packages

install.packages("tidyverse")
install.packages("duckdb")
install.packages("duckplyr")
install.packages("purrr")
install.packages("survival")
install.packages("broom")
install.packages("car")
install.packages("gridExtra")
install.packages("grid")
install.packages("survminer")


library(broom)
library(tidyverse)
library(duckdb)
library(duckplyr)
library(purrr)
library(survival)
library(car)
library(grid)
library(gridExtra)
library(survminer)

#Reading files

ICD_diagnoses <- read.csv("d_icd_diagnoses.csv")
admissions <- read.csv("admissions.csv")
patients <- read.csv("patients.csv")
diagnoses <- read.csv("diagnoses_icd.csv.gz")
emar <- read_csv_duckdb('emar.csv.gz')
