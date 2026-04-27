# 07-Modelling

# test model
cox_model_test <- coxph(
  Surv(time_to_event, event) ~
    opioid_exposure +
    Adep_exposure +
    MH_flag,
  data = Analysis_df
)

summary(cox_model_test)

# building final model

Analysis_df <- Analysis_df %>%
  mutate(
    opioid_exposure = factor(opioid_exposure),
    Adep_exposure = factor(Adep_exposure),
    MH_flag = factor(MH_flag),
    gender = factor(gender)
  )

Analysis_df <- Analysis_df %>%
  mutate(
    age_scaled = scale(anchor_age),
    opioid_dose_scaled = scale(total_opioid_admins)
  )

cox_model_final <- coxph(
  Surv(time_to_event, event) ~
    opioid_dose_scaled +
    MH_flag +
    Adep_exposure +
    age_scaled +
    gender,
  data = Analysis_df
)

cox.zph(cox_model_final)

cox_final_VIF <- vif(cox_model_final)
VIFtab <- tibble(cox_final_VIF)

#cox_model_tv <- coxph(
#  opioid_dose_scaled +
#  tt(opioid_dose_scaled) +
# MH_flag +
#Adep_exposure +
#age_scaled +
#gender,
#data = Analysis_df,
#tt = function(x, t, ...) x * log(t)
#)
# Time variable cox model attempted, will not run on my mac (have left code in should someone with a mega pc try it :P)
