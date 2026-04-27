# 09-Outputs

#~~~~~~~~~~~~~~~~ OUTPUT TABLES ~~~~~~~~~~~~~~~~~
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Cohort descriptors

demographics_table <- Analysis_df %>%
  summarise(
    N = n(),
    Mean_age = mean(anchor_age, na.rm = TRUE),
    SD_age = sd(anchor_age, na.rm = TRUE),
    Male_pct = mean(gender == "M", na.rm = TRUE) * 100,
    Female_pct = mean(gender == "F", na.rm = TRUE) * 100
  )

outcome_table <- Analysis_df %>%
  summarise(
    Total_patients = n(),
    OUD_cases = sum(event == 1),
    OUD_rate_pct = mean(event == 1) * 100
  )

exposure_table <- Analysis_df %>%
  summarise(
    Opioid_exposed = mean(opioid_exposure == 1) * 100,
    Adep_exposed = mean(Adep_exposure == 1) * 100,
    MH_flag_pct = mean(MH_flag == 1) * 100
  )


# Exposure/Administration tables

opioid_summary_table <- Analysis_df %>%
  summarise(
    Mean_admins = mean(total_opioid_admins),
    Median_admins = median(total_opioid_admins),
    Max_admins = max(total_opioid_admins)
  )

exposure_vs_outcome <- Analysis_df %>%
  group_by(opioid_exposure) %>%
  summarise(
    OUD_rate = mean(event) * 100,
    n = n()
  )


# Cox Model Tables

cox_table <- tidy(cox_model_final, exponentiate = TRUE, conf.int = TRUE) %>%
  select(
    Variable = term,
    HR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    HR = round(HR, 2),
    CI_lower = round(CI_lower, 2),
    CI_upper = round(CI_upper, 2),
    p_value = signif(p_value, 3)
  )

cox_table <- cox_table %>%
  mutate(
    CI = paste0(CI_lower, " – ", CI_upper)
  ) %>%
  select(Variable, HR, CI, p_value)

cox_table <- cox_table %>%
  mutate(
    p_value = ifelse(p_value < 0.001, "<0.001", as.character(p_value))
  )


model_perf <- tibble(
  Metric = c("Concordance", "Events", "Sample size"),
  Value = c(
    summary(cox_model_final)$concordance[1],
    summary(cox_model_final)$nevent,
    summary(cox_model_final)$n
  )
)

cox_table$Variable <- c(
  "Opioid dose (scaled)",
  "Mental health condition",
  "Antidepressant exposure",
  "Age (scaled)",
  "Gender"
)

# Model Diagnostics

vif_table <- vif(cox_model_final) %>%
  enframe(name = "Variable", value = "VIF") %>%
  mutate(VIF = round(VIF, 2))

schoenfeld <- cox.zph(cox_model_final)

schoenfeld_table <- as.data.frame(schoenfeld$table) %>%
  rownames_to_column(var = "Variable") %>%
  select(
    Variable,
    chisq,
    p = p
  ) %>%
  mutate(
    p = signif(p, 3)
  )

# Saving outputs as .tiffs

save_table_tiff <- function(
  table_object,
  filename,
  title = NULL,
  footnote = NULL,
  width = 9,
  height = 5.5,
  dpi = 300,
  base_size = 12
) {
  tiff(
    filename = paste0("outputs/tables/", filename, ".tiff"),
    width = width,
    height = height,
    units = "in",
    res = dpi
  )

  grid.newpage()

  # Title
  if (!is.null(title)) {
    grid.text(
      title,
      y = 0.97,
      gp = gpar(fontsize = 14, fontface = "bold")
    )
  }

  # Main table
  table_plot <- tableGrob(
    table_object,
    rows = NULL,
    theme = ttheme_minimal(base_size = base_size)
  )

  pushViewport(viewport(y = 0.48, height = 0.78))
  grid.draw(table_plot)
  popViewport()

  # Footnote
  if (!is.null(footnote)) {
    grid.text(
      footnote,
      y = 0.04,
      gp = gpar(fontsize = 9, fontface = "italic")
    )
  }

  dev.off()
}

# Saving all output tables as .TIFF files

save_table_tiff(
  demographics_table,
  "table_1_demographics",
  title = "Table 1: Demographic characteristics of the study cohort"
)

save_table_tiff(
  outcome_table,
  "table_2_outcomes",
  title = "Table 2: Outcome prevalence in the study cohort"
)

save_table_tiff(
  exposure_table,
  "table_3_exposures",
  title = "Table 3: Exposure prevalence in the study cohort"
)

save_table_tiff(
  opioid_summary_table,
  "table_4_opioid_summary",
  title = "Table 4: Summary of opioid administrations"
)

save_table_tiff(
  exposure_vs_outcome,
  "table_5_exposure_vs_outcome",
  title = "Table 5: Opioid exposure and OUD outcome"
)

save_table_tiff(
  cox_table,
  "table_6_cox_model",
  title = "Table 6: Cox proportional hazards model for time to OUD",
  footnote = "HR = hazard ratio; CI = confidence interval."
)

save_table_tiff(
  model_perf,
  "table_7_model_performance",
  title = "Table 7: Cox model performance summary"
)

save_table_tiff(
  vif_table,
  "table_8_vif",
  title = "Table 8: Variance inflation factors for model covariates",
  footnote = "VIF values < 5 indicate no problematic multicollinearity."
)

save_table_tiff(
  schoenfeld_table,
  "table_9_schoenfeld",
  title = "Table 9: Schoenfeld residual test for proportional hazards",
  footnote = "p < 0.05 suggests violation of the proportional hazards assumption."
)
