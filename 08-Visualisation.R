#08-Visualisation

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Creating Save function

save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggsave(
    filename = paste0("outputs/figures/", filename, ".tiff"),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
}

# FIGURE 1: Kaplan-Meier (Opioid Exposure)

km_fit_opioid <- survfit(
  Surv(time_to_event, event) ~ opioid_exposure,
  data = Analysis_df
)

km_opioid_plot <- ggsurvplot(
  km_fit_opioid,
  data = Analysis_df,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  title = "Figure 1: Time to Opioid Use Disorder by Opioid Exposure",
  xlab = "Time (days)",
  ylab = "Survival probability",
  legend.title = "Opioid Exposure",
  legend.labs = c("No", "Yes")
)

save_plot(km_opioid_plot$plot, "figure_1_km_opioid")

# FIGURE 2: Kaplan-Meier (Mental Health)

km_fit_mh <- survfit(
  Surv(time_to_event, event) ~ MH_flag,
  data = Analysis_df
)

km_mh_plot <- ggsurvplot(
  km_fit_mh,
  data = Analysis_df,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  title = "Figure 2: Time to Opioid Use Disorder by Mental Health Status",
  xlab = "Time (days)",
  ylab = "Survival probability",
  legend.title = "Mental Health Condition",
  legend.labs = c("No", "Yes")
)

save_plot(km_mh_plot$plot, "figure_2_km_mh")

# FIGURE 3: Opioid Trends Over Time

opioid_trend_plot <- ggplot(
  opioid_plot_df,
  aes(
    x = anchor_year_group,
    y = n_admins,
    color = primary_opioid,
    group = primary_opioid
  )
) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Figure 3: Opioid Administrations Over Time",
    x = "Anchor Year Group",
    y = "Number of Administrations",
    color = "Opioid"
  ) +
  theme_minimal(base_size = 13)

save_plot(opioid_trend_plot, "figure_3_opioid_trends", width = 10)

# FIGURE 4: Age Distribution by Gender

age_plot <- ggplot(
  Analysis_df,
  aes(x = anchor_age, fill = gender)
) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(
    title = "Figure 4: Age Distribution by Gender",
    x = "Age",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13)

save_plot(age_plot, "figure_4_age_distribution")

# FIGURE 5: Exposure vs Outcome

exposure_outcome_plot <- ggplot(
  Analysis_df,
  aes(x = factor(opioid_exposure), fill = factor(event))
) +
  geom_bar(position = "fill") +
  labs(
    title = "Figure 5: Proportion of OUD by Opioid Exposure",
    x = "Opioid Exposure",
    y = "Proportion",
    fill = "OUD Outcome"
  ) +
  theme_minimal(base_size = 13)

save_plot(exposure_outcome_plot, "figure_5_exposure_vs_outcome")

# FIGURE 6: Cox Model Forest Plot

cox_plot_df <- tidy(cox_model_final, exponentiate = TRUE, conf.int = TRUE)

cox_plot_df <- cox_plot_df %>%
  mutate(
    term = factor(
      term,
      levels = rev(term),
      labels = c(
        "Opioid Dose",
        "Mental Health Condition",
        "Antidepressant Exposure",
        "Age",
        "Gender"
      )
    )
  )

forest_plot <- ggplot(
  cox_plot_df,
  aes(x = estimate, y = term)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Figure 6: Hazard Ratios for Predictors of OUD",
    x = "Hazard Ratio (95% CI)",
    y = NULL
  ) +
  theme_minimal(base_size = 13)

save_plot(forest_plot, "figure_6_cox_forest", height = 5)
