# This file replicates the analysis for Messaging interventions that increase COVID-19 vaccine willingness in Latin America - Supplementary Information
# Pablo Argote, Elena Barham, Sarah Daly, Julian E. Gerez, John Marshall, Oscar Pocasangre

## Preliminaries

library(car)
library(estimatr)
library(haven)
library(lfe)
library(plyr)
library(tidyverse)
library(texreg)
library(weights)
library(xtable)

unzip("vaccine_wide.dta.zip")
hesitancy <- read_dta("vaccine_wide.dta")

source("00_table_function.R")

# Supplementary Information Table 1 - made manually

# Supplementary Information Table 2 - vaccine information comprehension tests

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = c("correct_1",
                            "correct_2"),
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = c("Know that vaccines were approved",
                              "Know that there are minimal side effects"),
           treatment_labels = c("Any vaccine information"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table2_manip_A"
)

make_table(treatment = "factor(information_treatment)",
           interaction = NULL,
           outcome_vars = c("correct_1",
                            "correct_2"),
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_info,
           one_tailed = FALSE,
           outcome_labels = c("Know that vaccines were approved",
                              "Know that there are minimal side effects"),
           treatment_labels = c("Health",
                                "Health + herd 60%",
                                "Health + herd 70%",
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current",
                                "Health + Biden"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table2_manip_B"
)

# Supplementary Information Table 3 - effect of any vaccine information on vaccine willingness

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = "Any vaccine information",
           data = hesitant,
           table_name = "Tables and Figures/SI_table3_anyinfo_pooled"
)

## By Country

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(any_info_treatment)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$IPW_any_info_treatment,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = "Any vaccine information",
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table3_anyinfo_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Supplementary Information Table 4 - effect of different types of vaccine information on vaccine willingness.

make_table(treatment = "factor(information_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_info,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Health",
                                "Health + herd 60%",
                                "Health + herd 70%", 
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current", 
                                "Health + Biden"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table4_allinfo_pooled"
)

# Supplementary Information Table 5 - effect of being informed that the current rate of vaccination willingness in the population is above/below the rate required for herd immunity

# indicator for sample restrictions, treatment 2-7
hesitant <- hesitant %>% mutate(t27 = ifelse(information_treatment >= 2 & 
                                               information_treatment <= 7, 1, 0))

# restrict sample to treatments 2-7
hesitant27 <- hesitant %>% filter(t27 == 1)

# indicator for receiving current wtv info
hesitant27 <- hesitant27 %>% mutate(received_wtv = ifelse(information_treatment >= 5 &
                                                            information_treatment <= 7, 1, 0))


# indicator for whether herd immunity level seen is greater than wtv rate seen
hesitant27 <- hesitant27 %>% mutate(herd_seen = ifelse(information_treatment==5 | information_treatment==2, 60,
                                                       ifelse(information_treatment==6 | information_treatment==3, 70, 
                                                              ifelse(information_treatment==7 | information_treatment==4, 80, 0))))

hesitant27 <- hesitant27 %>% mutate(herd_above_wtv = ifelse(herd_seen > current_willingness, 1, 0))

# current willingness indicators
hesitant27 <- hesitant27 %>% mutate(current_56 = ifelse(current_willingness==56, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_57 = ifelse(current_willingness==57, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_58 = ifelse(current_willingness==58, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_61 = ifelse(current_willingness==61, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_64 = ifelse(current_willingness==64, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_66 = ifelse(current_willingness==66, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_67 = ifelse(current_willingness==67, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_73 = ifelse(current_willingness==73, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_75 = ifelse(current_willingness==75, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_79 = ifelse(current_willingness==79, 1, 0))

# demean the first 9 indicators by subtracting the mean of the corresponding indicator in the sample that we will use for estimation
hesitant27 <- hesitant27 %>% mutate(d_current_56 = (current_56 - mean(current_56)))
hesitant27 <- hesitant27 %>% mutate(d_current_57 = (current_57 - mean(current_57)))
hesitant27 <- hesitant27 %>% mutate(d_current_58 = (current_58 - mean(current_58)))
hesitant27 <- hesitant27 %>% mutate(d_current_61 = (current_61 - mean(current_61)))
hesitant27 <- hesitant27 %>% mutate(d_current_64 = (current_64 - mean(current_64)))
hesitant27 <- hesitant27 %>% mutate(d_current_66 = (current_66 - mean(current_66)))
hesitant27 <- hesitant27 %>% mutate(d_current_67 = (current_67 - mean(current_67)))
hesitant27 <- hesitant27 %>% mutate(d_current_73 = (current_73 - mean(current_73)))
hesitant27 <- hesitant27 %>% mutate(d_current_75 = (current_75 - mean(current_75)))

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y, " ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv + factor(current_willingness) + received_wtv*d_current_56 + received_wtv*d_current_57 + received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 + received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + received_wtv*d_current_75 + std_months_pre")),
            fixed_effects = factor(fixed_effects),
            data = hesitant27,
            se_type = "stata")
)

outcome_stats <- hesitant27 %>%
  filter(received_wtv == 0 & herd_above_wtv == 0) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table5_currentherdint", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table5_currentherdint"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Current", 
                             "Herd opinion above current rate",
                             rep("", 19),
                             "Current $\\times$ herd opinion above current rate",
                             rep("", 9)),
       custom.gof.rows = list("Outcome range" = c("[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
)

rm(models, outcome_stats, observations, rsq)

# Supplementary Information Table 6 - effect of different types of motivational message on vaccine willingness

## Pooled

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = NULL,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table6_motiv"
)

## By Country

hesitant_orig <- hesitant

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(motivation_treatment_enc)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = NULL,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = c("Altruism",
                                  "Economic recovery",
                                  "Social approval"),
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table6_motiv_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)
rm(list=setdiff(ls(), c("hesitancy", "make_table")))

# Supplementary Information Table 7 - effect of receiving any vaccination information on responding to main post-treatment outcome questions

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

## Completion variables

hesitant <- hesitant %>%
  mutate(reached_wtv = ifelse(!is.na(hesitancy_post_rec), 1, 0),
         reached_months = ifelse(!is.na(quickly_post_1_text_reversed2), 1, 0),
         reached_encourage = ifelse(!is.na(encourage_others), 1, 0))

attrition_outcomes <- c("reached_wtv", "reached_months", "reached_encourage")

attrition_outcomes_labels <- c("Reached willing to vaccinate scale", "Reached wait until vaccinate (reversed)", "Reached encourage others to vaccinate")

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = attrition_outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = attrition_outcomes_labels,
           treatment_labels = "Any vaccine information",
           data = hesitant,
           table_name = "Tables and Figures/SI_table7_attrition_anyinfo_pooled"
)

## By Country

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(any_info_treatment)",
             interaction = NULL,
             outcome_vars = attrition_outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$IPW_any_info_treatment,
             one_tailed = FALSE,
             outcome_labels = attrition_outcomes_labels,
             treatment_labels = "Any vaccine information",
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table7_attrition_anyinfo_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Supplementary Information Table 8 - effect of motivational messages on responding to main post-treatment outcome questions

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = NULL,
           outcome_vars = attrition_outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = NULL,
           one_tailed = FALSE,
           outcome_labels = attrition_outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table8_attrition_motiv_pooled"
)

## By Country

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

hesitant <- hesitant %>%
  mutate(reached_wtv = ifelse(!is.na(hesitancy_post_rec), 1, 0),
         reached_months = ifelse(!is.na(quickly_post_1_text_reversed2), 1, 0),
         reached_encourage = ifelse(!is.na(encourage_others), 1, 0))

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(motivation_treatment_enc)",
             interaction = NULL,
             outcome_vars = attrition_outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = NULL,
             one_tailed = FALSE,
             outcome_labels = attrition_outcomes_labels,
             treatment_labels = c("Altruism",
                                  "Economic recovery",
                                  "Social approval"),
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table8_attrition_motiv_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i, attrition_outcomes, attrition_outcomes_labels)

# Supplementary Information Table 9 - balance of vaccine information treatments over pre-treatment covariates

hesitant_orig <- hesitant

covariate_labels <- c("Education - None",
                      "Education - Primary",
                      "Education - Secondary",
                      "Education - Other Higher",
                      "Education - University",
                      "Gender",
                      "Running Water in Home",
                      "Sewage in Home",
                      "Electricity in Home",
                      "No Running Water, Sewage, or Electricity in Home",
                      "COVID News Consumption - TV",
                      "COVID News Consumption - Radio",
                      "COVID News Consumption - Print",
                      "COVID News Consumption - Word of Mouth",
                      "COVID News Consumption - WhatsApp",
                      "COVID News Consumption - Social Media",
                      "COVID News Consumption - News Websites",
                      "COVID Severity in Country",
                      "Herd Immunity Prior",
                      "General Vaccine Hesitancy - Protect from Disease",
                      "General Vaccine Hesitancy - Good for Community",
                      "General Vaccine Hesitancy - Trust in Government",
                      "General Vaccine Hesitancy - Follow Doctor Instructions",
                      "General Vaccine Hesitancy - Trust in International Medical Experts",
                      "General Vaccine Hesitancy - Refused Vaccine",
                      "COVID Hesitancy Reasons - Side Effects",
                      "COVID Hesitancy Reasons - Vaccine Gives COVID",
                      "COVID Hesitancy Reasons - Produced Too Quickly",
                      "COVID Hesitancy Reasons - Not Effective",
                      "COVID Hesitancy Reasons - Not At Risk of Getting COVID",
                      "COVID Hesitancy Reasons - Against Vaccines Generally",
                      "COVID Hesitancy Reasons - Prefer 'Natural' Immunity",
                      "COVID Hesitancy Reasons - Already Had COVID",
                      "COVID Hesitancy Reasons - Don't Trust Government",
                      "COVID Hesitancy Reasons - Financial Concerns",
                      "COVID Hesitancy Reasons - Other",
                      "Comorbidities - None",
                      "Comorbidities - Diabetes",
                      "Comorbidities - Cardiovascular Diseases",
                      "Comorbidities - Obesity",
                      "Comorbidities - Autoimmune Diseases",
                      "Comorbidities - Chronic Obstructive Pulmonary Disease",
                      "Comorbidities - Prefer Not To Share",
                      "Had COVID",
                      "Know Someone Seriously Ill or Passed Away COVID",
                      "COVID Economic Situation",
                      "Government Vaccine Priority",
                      "Left/Right Political Scale",
                      "Satisfied with President COVID Management",
                      "Satisfied with Mayor COVID Management",
                      "Satisfied with Health Ministry COVID Management",
                      "Would Vote for Current President",
                      "Would Vote for Current Mayor",
                      "Trust in Current President",
                      "Trust in Current Mayor",
                      "Trust in National Health Ministry",
                      "Trust in National Medical Association",
                      "Trust in Left-Wing Newspaper",
                      "Trust in Right-Wing Newspaper",
                      "Trust in Religious Leader",
                      "Trust in Local Healthcare",
                      "Trust in Armed Forces",
                      "Trust in Civil Society Organizations",
                      "Trust in Government of China",
                      "Trust in Government of U.S. Under Trump",
                      "Trust in Government of U.S. Under Biden",
                      "Trust in Government of U.K.",
                      "Trust in Government of Russia",
                      "Meeting Indoor With Non-Family Contributes to COVID",
                      "Risk Aversion 1",
                      "Risk Aversion 2",
                      "Risk Aversion 3",
                      "Risk Aversion 4",
                      "Risk Aversion 5",
                      "Discount Rate 1",
                      "Discount Rate 2",
                      "Discount Rate 3",
                      "Discount Rate 4",
                      "Donation Amount",
                      "Important to Receive Respect and Recognition",
                      "Social Influence"
)

## Recode NAs in electricity, running water sewage to 0s

hesitant <- hesitant %>%
  mutate(home_conditions_water = ifelse(is.na(home_conditions_1), 0, 1),
         home_conditions_drainage = ifelse(is.na(home_conditions_2), 0, 1),
         home_conditions_electricity = ifelse(is.na(home_conditions_3), 0, 1),
         home_conditions_none = ifelse(is.na(home_conditions_4), 0, 1))

## Recode NAs in reasons_hesitancy to 0

hesitant <- hesitant %>%
  mutate(reasons_hesitancy_side  = ifelse(is.na(reasons_hesitancy_1 ), 0, 1),
         reasons_hesitancy_covid  = ifelse(is.na(reasons_hesitancy_2 ), 0, 1),
         reasons_hesitancy_quickly  = ifelse(is.na(reasons_hesitancy_3 ), 0, 1),
         reasons_hesitancy_effective  = ifelse(is.na(reasons_hesitancy_4 ), 0, 1),
         reasons_hesitancy_risk  = ifelse(is.na(reasons_hesitancy_5 ), 0, 1),
         reasons_hesitancy_vaccine  = ifelse(is.na(reasons_hesitancy_6 ), 0, 1),
         reasons_hesitancy_naturalimm  = ifelse(is.na(reasons_hesitancy_7 ), 0, 1),
         reasons_hesitancy_hadcovid  = ifelse(is.na(reasons_hesitancy_8 ), 0, 1),
         reasons_hesitancy_govtrust  = ifelse(is.na(reasons_hesitancy_9 ), 0, 1),
         reasons_hesitancy_finance = ifelse(is.na(reasons_hesitancy_10), 0, 1),
         reasons_hesitancy_other = ifelse(is.na(reasons_hesitancy_11), 0, 1))

## Recode NAs in chronic_illness to 0

hesitant <- hesitant %>%
  mutate(cronic_illness_none           = ifelse(is.na(cronic_illness_0 ), 0, 1),
         cronic_illness_diabetes       = ifelse(is.na(cronic_illness_1 ), 0, 1),
         cronic_illness_cardiovasc     = ifelse(is.na(cronic_illness_2 ), 0, 1),
         cronic_illness_obese          = ifelse(is.na(cronic_illness_3 ), 0, 1),
         cronic_illness_autoimmune     = ifelse(is.na(cronic_illness_4 ), 0, 1),
         cronic_illness_pulmonary      = ifelse(is.na(cronic_illness_5 ), 0, 1),
         cronic_illness__rathernotsay  = ifelse(is.na(cronic_illness__8), 0, 1))

hesitant$impt_help_community_share <- as.numeric(hesitant$impt_help_community_share)

## Recode dummies for education

hesitant <- hesitant %>%
  mutate(education_none = ifelse(education_enc == 1, 1, 0),
         education_primary = ifelse(education_enc == 2, 1, 0),
         education_secondary = ifelse(education_enc == 3, 1, 0),
         education_other = ifelse(education_enc == 4, 1, 0),
         education_university = ifelse(education_enc == 5, 1, 0))

## Change some categorical variables to dummies

hesitant <- hesitant %>%
  mutate(covid_serious = ifelse(covid_serious == -8, NA, covid_serious),
         refused_vaccine = case_when(
           refused_vaccine == -10 ~ NA_real_,
           refused_vaccine == -9 ~ NA_real_,
           refused_vaccine == -8 ~ NA_real_,
           refused_vaccine == 0 ~ 0,
           refused_vaccine == 1 ~ 1
         ),
         had_covid = case_when(
           had_covid == -8 ~ NA_real_,
           had_covid == 1 ~ 0,
           had_covid == 2 ~ 1,
           had_covid == 3 ~ 1
         ),
         know_serious_covid = case_when(
           know_serious_covid == -8 ~ NA_real_,
           know_serious_covid == 0 ~ 0,
           know_serious_covid == 1 ~ 1
         ),
         covid_economic_situ = ifelse(covid_economic_situ == -8, NA, covid_economic_situ),
         gov_vaccine_priority = ifelse(gov_vaccine_priority == -8, NA, gov_vaccine_priority),
         vote_president = ifelse(vote_president == 1, 1, 0),
         vote_mayor = ifelse(vote_mayor == 1, 1, 0),
         trust_persons_insts_pres = ifelse(trust_persons_insts_1 == -8, NA, trust_persons_insts_1),
         trust_persons_insts_mayor = ifelse(trust_persons_insts_2 == -8, NA, trust_persons_insts_2),
         trust_persons_insts_health = ifelse(trust_persons_insts_3 == -8, NA, trust_persons_insts_3),
         trust_persons_insts_med = ifelse(trust_persons_insts_4 == -8, NA, trust_persons_insts_4),
         trust_persons_insts_lnews = ifelse(trust_persons_insts_5 == -8, NA, trust_persons_insts_5),
         trust_persons_insts_rnews = ifelse(trust_persons_insts_6 == -8, NA, trust_persons_insts_6),
         trust_persons_insts_relig = ifelse(trust_persons_insts_7 == -8, NA, trust_persons_insts_7),
         trust_orgs_localhealth = ifelse(trust_orgs_1 == -8, NA, trust_orgs_1),
         trust_orgs_armed = ifelse(trust_orgs_2 == -8, NA, trust_orgs_2),
         trust_orgs_civilsociety = ifelse(trust_orgs_3 == -8, NA, trust_orgs_3),
         trust_country_govs_china = ifelse(trust_country_govs_1 == -8, NA, trust_country_govs_1),
         trust_country_govs_trump = ifelse(trust_country_govs_2 == -8, NA, trust_country_govs_2),
         trust_country_govs_biden = ifelse(trust_country_govs_3 == -8, NA, trust_country_govs_3),
         trust_country_govs_uk = ifelse(trust_country_govs_4 == -8, NA, trust_country_govs_4),
         trust_country_govs_russ = ifelse(trust_country_govs_5 == -8, NA, trust_country_govs_5),
         indoor_contrib_covid = ifelse(indoor_contrib_covid == -8, NA, indoor_contrib_covid),
  )

pre_treatment_covariates <- c("education_none",
                              "education_primary",
                              "education_secondary",
                              "education_other",
                              "education_university",
                              "gender",
                              "home_conditions_water",
                              "home_conditions_drainage",
                              "home_conditions_electricity",
                              "home_conditions_none",
                              "covid_news_1",
                              "covid_news_2",
                              "covid_news_3",
                              "covid_news_4",
                              "covid_news_5",
                              "covid_news_6",
                              "covid_news_7",
                              "covid_serious",
                              "percent_for_imm_pre_1",
                              "general_hesitancy_1",
                              "general_hesitancy_2",
                              "general_hesitancy_3",
                              "general_hesitancy_4",
                              "general_hesitancy_5",
                              "refused_vaccine",
                              "reasons_hesitancy_side", 
                              "reasons_hesitancy_covid",
                              "reasons_hesitancy_quickly",
                              "reasons_hesitancy_effective",
                              "reasons_hesitancy_risk",
                              "reasons_hesitancy_vaccine",
                              "reasons_hesitancy_naturalimm",
                              "reasons_hesitancy_hadcovid",
                              "reasons_hesitancy_govtrust",
                              "reasons_hesitancy_finance",
                              "reasons_hesitancy_other",
                              "cronic_illness_none",         
                              "cronic_illness_diabetes",     
                              "cronic_illness_cardiovasc",   
                              "cronic_illness_obese",        
                              "cronic_illness_autoimmune",   
                              "cronic_illness_pulmonary",    
                              "cronic_illness__rathernotsay",
                              "had_covid",
                              "know_serious_covid",
                              "covid_economic_situ",
                              "gov_vaccine_priority",
                              "left_right_scale_1",
                              "satisfied_govt_covid_1",
                              "satisfied_govt_covid_2",
                              "satisfied_govt_covid_3",
                              "vote_president",
                              "vote_mayor",
                              "trust_persons_insts_pres",
                              "trust_persons_insts_mayor",
                              "trust_persons_insts_health",
                              "trust_persons_insts_med",
                              "trust_persons_insts_lnews",
                              "trust_persons_insts_rnews",
                              "trust_persons_insts_relig",
                              "trust_orgs_localhealth",
                              "trust_orgs_armed",
                              "trust_orgs_civilsociety",
                              "trust_country_govs_china",
                              "trust_country_govs_trump",
                              "trust_country_govs_biden",
                              "trust_country_govs_uk",
                              "trust_country_govs_russ",
                              "indoor_contrib_covid",
                              "risk_aversion_1",
                              "risk_aversion_2",
                              "risk_aversion_3",
                              "risk_aversion_4",
                              "risk_aversion_5",
                              "discount_rate_1",
                              "discount_rate_2",
                              "discount_rate_3",
                              "discount_rate_4",
                              "impt_help_community_share",
                              "impt_respect_comm",
                              "social_influence"
)

## Run Models - Information

## All respondents assigned to treatment even if we don't observe outcome variables

balance_info_all <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(information_treatment) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            weights = hesitant$IPW_info,
            se_type = "stata",
            data = hesitant)
)

## For each of the outcomes they reached

balance_info_wtv <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(information_treatment) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            weights = subset(hesitant, hesitant$reached_wtv == TRUE)$IPW_info,
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_wtv == TRUE))
)

balance_info_months <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(information_treatment) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            weights = subset(hesitant, hesitant$reached_months == TRUE)$IPW_info,
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_months == TRUE))
)

balance_info_encourage <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(information_treatment) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            weights = subset(hesitant, hesitant$reached_encourage == TRUE)$IPW_info,
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_encourage == TRUE))
)

balance_info_tests <- list(balance_info_all, balance_info_wtv, balance_info_months, balance_info_encourage)

balance_info_p_values <- lapply(1:length(balance_info_tests), function(i)
  sapply(1:length(pre_treatment_covariates), function(x)
    linearHypothesis(balance_info_tests[[i]][[x]], c("factor(information_treatment)1 = 0",
                                                     "factor(information_treatment)2 = 0",
                                                     "factor(information_treatment)3 = 0",
                                                     "factor(information_treatment)4 = 0",
                                                     "factor(information_treatment)5 = 0",
                                                     "factor(information_treatment)6 = 0",
                                                     "factor(information_treatment)7 = 0",
                                                     "factor(information_treatment)8 = 0"), test = "F")$`Pr(>F)`[2]
  ))

balance_info_p_values <- lapply(1:length(balance_info_tests), function(i)
  case_when(balance_info_p_values[[i]] > 0.1   ~ as.character(round(balance_info_p_values[[i]], 3)),
            balance_info_p_values[[i]] <= 0.1 &
              balance_info_p_values[[i]] > 0.05 ~ paste0(round(balance_info_p_values[[i]], 3), "*"),
            balance_info_p_values[[i]] <= 0.05 &
              balance_info_p_values[[i]] > 0.01 ~ paste0(round(balance_info_p_values[[i]], 3), "**"),
            balance_info_p_values[[i]] <= 0.01 ~ paste0(round(balance_info_p_values[[i]], 3), "***"))
)

balance_info_table <- cbind.data.frame(covariate_labels,
                                       balance_info_p_values[[1]],
                                       balance_info_p_values[[2]],
                                       balance_info_p_values[[3]],
                                       balance_info_p_values[[4]])

names(balance_info_table) <- c("Covariate",
                               "Information p-value - full",
                               "Information p-value - reached willing to vaccinate",
                               "Information p-value - reached wait until vaccinate",
                               "Information p-value - reached encourage others to vaccinate")

balance_info_table <- print(xtable(balance_info_table, auto = TRUE), include.rownames = FALSE)

write(balance_info_table, paste0("Tables and Figures/SI_table9_balance_info.tex"))

# Supplementary Information Table 10 - balance of motivational messages over pre-treatment covariates

## Run Models - Motivation

## All respondents assigned to treatment even if we don't observe outcome variables

balance_motiv_all <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(motivation_treatment_enc) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            se_type = "stata",
            data = hesitant)
)

## For each of the outcomes they reached

balance_motiv_wtv <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(motivation_treatment_enc) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_wtv == TRUE))
)

balance_motiv_months <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(motivation_treatment_enc) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_months == TRUE))
)

balance_motiv_encourage <- lapply(1:length(pre_treatment_covariates), function(x)
  lm_robust(as.formula(paste0(pre_treatment_covariates[x],
                              " ~ factor(motivation_treatment_enc) +
                       std_months_pre")),
            fixed_effects = factor(fixed_effects),
            se_type = "stata",
            data = subset(hesitant, hesitant$reached_encourage == TRUE))
)

balance_motiv_tests <- list(balance_motiv_all, balance_motiv_wtv, balance_motiv_months, balance_motiv_encourage)

balance_motiv_p_values <- lapply(1:length(balance_info_tests), function(i)
  sapply(1:length(pre_treatment_covariates), function(x)
    linearHypothesis(balance_motiv_tests[[i]][[x]], c("factor(motivation_treatment_enc)1 = 0",
                                                      "factor(motivation_treatment_enc)2 = 0",
                                                      "factor(motivation_treatment_enc)3 = 0"), test = "F")$`Pr(>F)`[2]
  ))

balance_motiv_p_values <- lapply(1:length(balance_motiv_tests), function(i)
  case_when(balance_motiv_p_values[[i]] > 0.1   ~ as.character(round(balance_motiv_p_values[[i]], 3)),
            balance_motiv_p_values[[i]] <= 0.1 &
              balance_motiv_p_values[[i]] > 0.05 ~ paste0(round(balance_motiv_p_values[[i]], 3), "*"),
            balance_motiv_p_values[[i]] <= 0.05 &
              balance_motiv_p_values[[i]] > 0.01 ~ paste0(round(balance_motiv_p_values[[i]], 3), "**"),
            balance_motiv_p_values[[i]] <= 0.01 ~ paste0(round(balance_motiv_p_values[[i]], 3), "***"))
)


balance_motiv_table <- cbind.data.frame(covariate_labels,
                                        balance_motiv_p_values[[1]],
                                        balance_motiv_p_values[[2]],
                                        balance_motiv_p_values[[3]],
                                        balance_motiv_p_values[[4]])

names(balance_motiv_table) <- c("Covariate",
                                "Motivation p-value - full",
                                "Motivation p-value - reached willing to vaccinate",
                                "Motivation p-value - reached wait until vaccinate",
                                "Motivation p-value - reached encourage others to vaccinate")

balance_info_table <- print(xtable(balance_motiv_table, auto = TRUE), include.rownames = FALSE)

write(balance_info_table, "Tables and Figures/SI_table10_balance_motiv.tex")

hesitant <- hesitant_orig

rm(hesitant_orig, covariate_labels, pre_treatment_covariates)
rm(list=ls(pattern="^balance"))

# Supplementary Information Table 11 - Lee bounds on the effect of any vaccine information on vaccine willingness

# Supplementary Information Table 12 - Lee bounds on the effect of different types of motivational message on vaccine willingness

# Supplementary Information Table 13 - effect of social approval versus altruistic motivational messages on vaccine willingness

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

hesitant$approval_vs_altruism <- ifelse(hesitant$motivation_treatment == "social", 1, 
                                        ifelse(hesitant$motivation_treatment == "altruism", 0, NA))

make_table(treatment = "approval_vs_altruism",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = NULL,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table13_approval_v_altruism"
)

# Supplementary Information Table 14 - effect of different types of vaccine information on reasons for becoming less hesitant, among treated respondents

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

df <- hesitancy %>% filter(sample_descriptive==1) %>% 
  dplyr::select(country, treat_change_reason_1, treat_change_reason_2, treat_change_reason_3, 
                treat_change_reason_4, treat_change_reason_5, treat_change_reason_6, 
                treat_change_reason_7, treat_change_reason_8, treat_change_reason_9, 
                information_treatment, pool_treatment1, pool_treatment2, 
                pool_treatment3, IPW_info, IPW_pool_treatment1, IPW_pool_treatment2, IPW_pool_treatment3, weight_joint, weight_rake,
                std_months_pre, fixed_effects)

df[, 2:10] <- as.data.frame(lapply(df[,2:10], function(x) replace(x, is.na(x), 0)))

df <- df %>% filter(information_treatment != 0)

dv_reasons <-  c("treat_change_reason_1", "treat_change_reason_2", "treat_change_reason_3", 
                 "treat_change_reason_4", "treat_change_reason_5", "treat_change_reason_6", "treat_change_reason_7", "treat_change_reason_8", "treat_change_reason_9")

df$info_treatment_anybase <- ifelse(df$information_treatment == 1, 0, (df$information_treatment-1))

make_table(treatment = "factor(info_treatment_anybase)",
           interaction = NULL,
           outcome_vars = dv_reasons,
           fixed_effects = "factor(fixed_effects)",
           weights = df$IPW_info,
           one_tailed = FALSE,
           outcome_labels = c("Less worried about side effects", "Less worried about getting Covid-19 from vaccine", "Less worried about speed of development", "Less worried about vaccine ineffectiveness", "Now getting vaccinated even if low risk", "No longer wants immunity from infection", "Now getting vaccinated even if already had Covid−19", "Now trusting of government", "Less worried about cost"),
           treatment_labels = c("Health + herd 60%",
                                "Health + herd 70%", 
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current", 
                                "Health + Biden"),
           data = df,
           table_name = "Tables and Figures/SI_table14_reasonschange"
)

rm(df, dv_reasons)

# Supplementary Information Table 15 - effect of any vaccine information on vaccine willingness, by pre-treatment covariate

hesitant <- hesitant %>% mutate(vote_president_rec = ifelse(vote_president != 1, 0, 1))

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

make_table(treatment = "factor(any_info_treatment)",
           interaction = "factor(any_info_treatment)*sex + factor(any_info_treatment)*factor(age_bin) + factor(any_info_treatment)*nse + factor(any_info_treatment)*vote_president_rec + factor(any_info_treatment)*factor(education_enc)",
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Any vaccine information", "Woman", "Age, 25-34", "Age, 35-44", "Age, 45-54", "Age, 55-64", "Age, 65+", "Socioeconomic Class", "Would vote for president", "Primary education", "Secondary education", "Other higher education", "University education", "Any vaccine information $\\times$ woman", "Any vaccine information $\\times$ age, 25-34", "Any vaccine information $\\times$ age, 35-44", "Any vaccine information $\\times$ age, 45-54", "Any vaccine information $\\times$ age, 55-64", "Any vaccine information $\\times$ age, 65+", "Any vaccine information $\\times$ socioeconomic class", "Any vaccine information $\\times$ would vote for president", "Any vaccine information $\\times$ primary education", "Any vaccine information $\\times$ secondary education", "Any vaccine information $\\times$ other higher education", "Any vaccine information $\\times$ university"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table15_allhet_anyinfo"
)

# Supplementary Information Table 16 - effect of different types of different expert opinion herd immunity opinion on vaccine willingness, by how the information relates to individual prior beliefs

outcomes <- c("percent_for_imm_post_1", "hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Posterior beliefs about herd immunity", "Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

hesitant27 <- hesitant %>% filter(information_treatment >= 2 & information_treatment <= 7)
hesitant27 <- hesitant27 %>% mutate(herd_rate_seen = ifelse(information_treatment == 2 | information_treatment==5, 60, 
                                                            ifelse(information_treatment==3 | information_treatment==6, 70, 
                                                                   ifelse(information_treatment==4 | information_treatment==7, 80, 0))))

hesitant27 <- hesitant27 %>% mutate(above = ifelse(herd_rate_seen > percent_for_imm_pre_1, 1, 0))

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y,
                              " ~ ",
                              "above",
                              " + ",
                              "std_months_pre")),
            data = hesitant27,
            weights = hesitant27$IPW_pool_treatment2,
            fixed_effects = factor(fixed_effects) + percent_for_imm_pre_1,
            se_type = 'stata')
)

outcome_stats <- hesitant27 %>%
  filter(above == 0) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table16_hi_het_A", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table16_hi_het_A"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Priors above reported herd rate"),
       custom.gof.rows = list("Outcome range" = c("0-100", "[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
       omit.coef = "std_months_pre"
)

hesitant27 <- hesitant27 %>% mutate(diff_priors_info = percent_for_imm_pre_1 - herd_rate_seen)
hesitant27 <- hesitant27 %>% mutate(priors_bins = ifelse(abs(diff_priors_info) <=5, "[x-5,x+5]", 
                                                         ifelse(diff_priors_info < -5 & diff_priors_info >= -15, "[x-15,x-5)", 
                                                                ifelse(diff_priors_info < -15, "p<x-15", 
                                                                       ifelse(diff_priors_info > 5 & diff_priors_info <= 15, "(x+5,x+15]", 
                                                                              ifelse(diff_priors_info > 15, "p>x+15", 0))))))

hesitant27$priors_bins <- factor(hesitant27$priors_bins)
hesitant27$priors_bins <- relevel(hesitant27$priors_bins, "[x-5,x+5]")

hesitant27$prior_bins_for_table <- factor(as.numeric(hesitant27$priors_bins)-1)

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y,
                              " ~ ",
                              "factor(prior_bins_for_table)",
                              " + ",
                              "std_months_pre")),
            data = hesitant27,
            weights = hesitant27$IPW_pool_treatment2,
            fixed_effects = factor(fixed_effects) + percent_for_imm_pre_1,
            se_type = 'stata')
)

outcome_stats <- hesitant27 %>%
  filter(prior_bins_for_table == 0) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table16_hi_het_B", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table16_hi_het_B"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Priors 5-15pp above reported herd rate",
                             "Priors 5-15pp below reported herd rate", 
                             "Priors 15pp below reported herd rate",
                             "Priors 15pp above reported herd rate"),
       custom.gof.rows = list("Outcome range" = c("0-100", "[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
       omit.coef = "std_months_pre"
)

rm(hesitant27)
rm(models, outcome_stats, observations, rsq)

# Supplementary Information Table 17 - effect of vaccine information on vaccine willingness, by how cur-rent willingness relates to individual prior beliefs

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

outcomes <- c("mun_hesitancy_post_1", "hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Posterior beliefs about rate municipal willingness", "Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

hesitant <- hesitant %>% mutate(current_info = as.numeric(information_treatment>=5 & information_treatment<=7),
                                weight_current_info = case_when(current_info == 1 ~ 1/(3/14),
                                                                current_info == 0 ~ 1/(11/14)))

hesitant <- hesitant %>% mutate(diff_wtv_prior = current_willingness - mun_hesitancy_pre_1)

# Now bin p in [x-5,x+5], p in [x-15,x+15), p<x-15, 
# p in [x-5,x+5], p in [x-15,x-5), p<x-15, p in (x+5,x+5], p>x+15, use p in [x=5,x+5] as the omitted baseline group.

hesitant <- hesitant %>% mutate(wtvpriorbins = ifelse(abs(diff_wtv_prior) <=5, "[x-5,x+5]", 
                                                      ifelse(diff_wtv_prior < -5 & diff_wtv_prior >= -15, "[x-15,x-5)", 
                                                             ifelse(diff_wtv_prior < -15, "p<x-15", 
                                                                    ifelse(diff_wtv_prior > 5 & diff_wtv_prior <= 15, "(x+5,x+15]", 
                                                                           ifelse(diff_wtv_prior > 15, "p>x+15", 0))))))

hesitant$wtvpriorbins <- as.factor(hesitant$wtvpriorbins)
hesitant$wtvpriorbins <- relevel(hesitant$wtvpriorbins, "[x-5,x+5]")
hesitant$prior_bins_for_table <- factor(as.numeric(hesitant$wtvpriorbins)-1)

hesitant <- hesitant %>% mutate(prior_below_current = ifelse(current_willingness > mun_hesitancy_pre_1, 1, 0))

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y,
                              " ~ ",
                              "current_info*prior_below_current",
                              " + ",
                              "std_months_pre")),
            data = hesitant,
            weights = hesitant$weight_current_info,
            fixed_effects = factor(fixed_effects),
            se_type = 'stata')
)

outcome_stats <- hesitant %>%
  filter(factor(pool_treatment1) == 0) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table17_cw_het_A", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table17_cw_het_A"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Received current willingness treatment",
                             "Prior below current willingness",
                             "Received current willingness treatment $\\times$ priors below current willingness"),
       custom.gof.rows = list("Outcome range" = c("0-100", "[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
       omit.coef = "std_months_pre"
)

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y,
                              " ~ ",
                              "current_info*factor(prior_bins_for_table)",
                              " + ",
                              "std_months_pre")),
            data = hesitant,
            weights = hesitant$weight_current_info,
            fixed_effects = factor(fixed_effects),
            se_type = 'stata')
)

outcome_stats <- hesitant %>%
  filter(factor(pool_treatment1) == 0) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table17_cw_het_B", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table17_cw_het_B"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Received current willingness treatment",
                             "Current willingness priors 5-15pp above rate",
                             "Current willingness priors 5-15pp below rate",
                             "Current willingness priors 15pp below rate",
                             "Current willingness priors 15pp above rate", 
                             "Received current willingness treatment $\\times$ Current willingness priors 5-15pp above rate",
                             "Received current willingness treatment $\\times$ Current willingness priors 5-15pp below rate",
                             "Received current willingness treatment $\\times$ Current willingness priors 15pp below rate",
                             "Received current willingness treatment $\\times$ Current willingness priors 15pp above rate"),
       custom.gof.rows = list("Outcome range" = c("0-100", "[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
       omit.coef = "std_months_pre"
)

rm(list=setdiff(ls(), c("hesitancy", "make_table")))

# Supplementary Information Table 18 - correlation between prior beliefs and prior vaccine willingness

hesitancy <- hesitancy %>%
  filter(speeder != 1)

hesitancy$quickly_pre_1_text_reversed <- abs(hesitancy$quickly_pre_1_text-12)

outcomes <- c("hesitancy_pre_rec", "hesitancy_dummy_pre", "quickly_pre_1_text_reversed")

outcomes_labels <- c("Willing to vaccinate scale (pre-treatment)", "Willing to vaccinate (pre-treatment)", "Months to vaccinate (pre-treatment)")

outcome_range <- c("{1-5}", "{0,1}", "[0-12]")

models_pre <- lapply(1:length(outcomes), function(y)
  lm_robust(as.formula(
    paste0(outcomes[y], " ~ ",
           paste(c("mun_hesitancy_pre_1", "percent_for_imm_pre_1",
                   "mun_hesitancy_pre_1*percent_for_imm_pre_1"), collapse = " + "))),
    data = hesitancy,
    se_type = "stata")
)

ivs <- c("Constant", "Pre-treatment uptake rate", "Pre-treatment herd immunity",
         "Pre-treatment uptake $\\times$ herd immunity")

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models_pre[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models_pre[[y]]$r.squared, 3)
)

texreg(models_pre,
       include.ci = FALSE,
       file = "Tables and Figures/SI_table18_pretreat_outcomes_on_rates.tex",
       caption = NULL,
       label = paste0("table:", "SI_table18_pretreat_outcomes_on_rates"),
       stars = c(0.01, 0.05, 0.1),
       digits = 6,
       custom.model.names = outcomes_labels,
       custom.coef.names = ivs,
       custom.gof.rows = list("Outcome range" = outcome_range,
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
)

# Supplementary Information Table 19 - effect of any motivational messages on vaccine willingness, by pre-treatment covariate

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

hesitant <- hesitant %>% mutate(vote_president_rec = ifelse(vote_president != 1, 0, 1))

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = "factor(motivation_treatment_enc)*sex + factor(motivation_treatment_enc)*age_bin + factor(motivation_treatment_enc)*nse + factor(motivation_treatment_enc)*vote_president_rec + factor(motivation_treatment_enc)*factor(education_enc)",
           outcome_vars = outcomes,
           weights = NULL,
           fixed_effects = "factor(fixed_effects)",
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism", "Economic recovery", "Social Approval",
                                "Woman",
                                "Age, 25-34", "Age, 35-44", "Age, 45-54", "Age, 55-64", "Age, 65+",
                                "Socioeconomic Class",
                                "Would vote for president",
                                "Primary education",
                                "Secondary education",
                                "Other higher education",
                                "University education",
                                "Altruism $\\times$ woman", "Economic recovery $\\times$ woman", "Social status $\\times$ woman",
                                "Altruism $\\times$ age, 25-34", "Economic recovery $\\times$ age, 25-34", "Social status $\\times$ age, 25-34",
                                "Altruism $\\times$ age, 35-44", "Economic recovery $\\times$ age, 35-44", "Social status $\\times$ age, 35-44",
                                "Altruism $\\times$ age, 45-54", "Economic recovery $\\times$ age, 45-54", "Social status $\\times$ age, 45-54",
                                "Altruism $\\times$ age, 55-64", "Economic recovery $\\times$ age, 55-64", "Social status $\\times$ age, 55-64",
                                "Altruism $\\times$ age, 65+", "Economic recovery $\\times$ age, 65+", "Social status $\\times$ age, 65+",
                                "Altruism $\\times$ socioeconomic class", "Economic recovery $\\times$ socioeconomic class", "Social status $\\times$ socioeconomic class",
                                "Altruism $\\times$ would vote for President", "Economic recovery $\\times$ would vote for President", "Social status $\\times$ would vote for President",
                                "Altruism $\\times$ primary education", "Economic recovery $\\times$ primary education", "Social status $\\times$ primary education",
                                "Altruism $\\times$ secondary education", "Economic recovery $\\times$ secondary education", "Social status $\\times$ secondary education",
                                "Altruism $\\times$ other higher education", "Economic recovery $\\times$ other higher education", "Social status $\\times$ other higher education",
                                "Altruism $\\times$ university", "Economic recovery $\\times$ university", "Social status $\\times$ university"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table19_allhet_motiv"
)

# Supplementary Information Table 20 - effect of any vaccine information on vaccine willingness, by motivational message

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = "factor(motivation_treatment_enc)*factor(any_info_treatment)",
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social status",
                                "Any vaccine information",
                                "Altruism $\\times$ Any vaccine information",
                                "Economic recovery $\\times$ Any vaccine information",
                                "Social status $\\times$ Any vaccine information"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table20_anyinfo_x_motiv"
)

# Supplementary Information Table 21 - effect of any vaccine information on demand for further vaccine information

outcomes <- c("want_more_info", "clicked")

outcomes_labels <- c("Request more information", "Visited PAHO website")

hesitancy <- read_dta("vaccine_wide.dta")

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1) %>%
  mutate(clicked = ifelse(is.na(want_more_info), NA, clicked))

## Pooled

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = "Any vaccine information",
           data = hesitant,
           table_name = "Tables and Figures/SI_table21_anyinfo_pooled_behav"
)

## By Country

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ),
  clicked = ifelse(is.na(want_more_info), NA, clicked))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(any_info_treatment)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$IPW_any_info_treatment,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = "Any vaccine information",
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table21_anyinfo_", unique(hesitant_orig$country)[i], "_behav")
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Supplementary Information Table 22 - effect of different types of vaccine information treatment on demandfor further vaccine information

make_table(treatment = "factor(information_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_info,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Health",
                                "Health + herd 60%",
                                "Health + herd 70%", 
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current", 
                                "Health + Biden"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table22_allinfo_pooled_behav"
)

# Supplementary Information Table 23 - effect of different types of motivational message on demand for further vaccine information

## Pooled

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = NULL,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table23_motiv_behav_pooled"
)

## By Country

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1) %>%
  mutate(clicked = ifelse(is.na(want_more_info), NA, clicked))

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ),
  clicked = ifelse(is.na(want_more_info), NA, clicked))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(motivation_treatment_enc)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = NULL,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = c("Altruism",
                                  "Economic recovery",
                                  "Social approval"),
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table23_motiv_", unique(hesitant_orig$country)[i], "_behav")
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Weighted Tables - Joint Weights

rm(list=setdiff(ls(), "make_table"))

hesitancy <- read_dta("vaccine_wide.dta")

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

# Table 24

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment*hesitant$weight_joint,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = "Any vaccine information",
           data = hesitant,
           table_name = "Tables and Figures/SI_table24_anyinfo_joint_pooled"
)

## By Country

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(any_info_treatment)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$IPW_any_info_treatment*hesitant$weight_joint,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = "Any vaccine information",
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table24_anyinfo_joint_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Table 25

make_table(treatment = "factor(information_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_info*hesitant$weight_joint,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Health",
                                "Health + herd 60%",
                                "Health + herd 70%", 
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current", 
                                "Health + Biden"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table25_allinfo_pooled_joint"
)

# Table 26

# indicator for sample restrictions, treatment 2-7
hesitant <- hesitant %>% mutate(t27 = ifelse(information_treatment >= 2 & 
                                               information_treatment <= 7, 1, 0))

# restrict sample to treatments 2-7
hesitant27 <- hesitant %>% filter(t27 == 1)

# indicator for receiving current wtv info
hesitant27 <- hesitant27 %>% mutate(received_wtv = ifelse(information_treatment >= 5 &
                                                            information_treatment <= 7, 1, 0))


# indicator for whether herd immunity level seen is greater than wtv rate seen
hesitant27 <- hesitant27 %>% mutate(herd_seen = ifelse(information_treatment==5 | information_treatment==2, 60,
                                                       ifelse(information_treatment==6 | information_treatment==3, 70, 
                                                              ifelse(information_treatment==7 | information_treatment==4, 80, 0))))

hesitant27 <- hesitant27 %>% mutate(herd_above_wtv = ifelse(herd_seen > current_willingness, 1, 0))

# current willingness indicators
hesitant27 <- hesitant27 %>% mutate(current_56 = ifelse(current_willingness==56, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_57 = ifelse(current_willingness==57, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_58 = ifelse(current_willingness==58, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_61 = ifelse(current_willingness==61, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_64 = ifelse(current_willingness==64, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_66 = ifelse(current_willingness==66, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_67 = ifelse(current_willingness==67, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_73 = ifelse(current_willingness==73, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_75 = ifelse(current_willingness==75, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_79 = ifelse(current_willingness==79, 1, 0))

# demean the first 9 indicators by subtracting the mean of the corresponding indicator in the sample that we will use for estimation
hesitant27 <- hesitant27 %>% mutate(d_current_56 = (current_56 - mean(current_56)))
hesitant27 <- hesitant27 %>% mutate(d_current_57 = (current_57 - mean(current_57)))
hesitant27 <- hesitant27 %>% mutate(d_current_58 = (current_58 - mean(current_58)))
hesitant27 <- hesitant27 %>% mutate(d_current_61 = (current_61 - mean(current_61)))
hesitant27 <- hesitant27 %>% mutate(d_current_64 = (current_64 - mean(current_64)))
hesitant27 <- hesitant27 %>% mutate(d_current_66 = (current_66 - mean(current_66)))
hesitant27 <- hesitant27 %>% mutate(d_current_67 = (current_67 - mean(current_67)))
hesitant27 <- hesitant27 %>% mutate(d_current_73 = (current_73 - mean(current_73)))
hesitant27 <- hesitant27 %>% mutate(d_current_75 = (current_75 - mean(current_75)))

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y, " ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv + factor(current_willingness) + received_wtv*d_current_56 + received_wtv*d_current_57 + received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 + received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + received_wtv*d_current_75 + std_months_pre")),
            fixed_effects = factor(fixed_effects),
            data = hesitant27,
            weights = hesitant27$weight_joint,
            se_type = "stata")
)

outcome_stats <- hesitant27 %>%
  filter(received_wtv == 0 & herd_above_wtv == 0) %>%
  filter(!is.na(weight_joint)) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~weighted.mean(., w = weight_joint, na.rm = TRUE),
                SD = ~sqrt(wtd.var(., w = weight_joint, na.rm = TRUE)))))

observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table26_currentherdint_joint", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table26_currentherdint_joint"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Current", 
                             "Herd opinion above current rate",
                             rep("", 19),
                             "Current $\\times$ herd opinion above current rate",
                             rep("", 9)),
       custom.gof.rows = list("Outcome range" = c("[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
)

rm(models, outcome_stats, observations, rsq)

# Table 27

## Pooled

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$weight_joint,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table27_motiv_joint"
)

## By Country

hesitant_orig <- hesitant

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(motivation_treatment_enc)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$weight_joint,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = c("Altruism",
                                  "Economic recovery",
                                  "Social approval"),
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table27_motiv_joint_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)
rm(list=setdiff(ls(), c("hesitancy", "make_table")))

# Weighted Tables - Rake Weights

rm(list=setdiff(ls(), "make_table"))

hesitancy <- read_dta("vaccine_wide.dta")

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

outcomes_labels <- c("Willing to vaccinate scale", "Willing to vaccinate", "Wait until vaccinate (reversed)", "Encourage others to vaccinate")

# Table 28

make_table(treatment = "factor(any_info_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_any_info_treatment*hesitant$weight_rake_ses,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = "Any vaccine information",
           data = hesitant,
           table_name = "Tables and Figures/SI_table28_anyinfo_rake_pooled"
)

## By Country

hesitant_orig <- hesitant

hesitant_orig <- hesitant_orig %>%
  mutate(country = case_when(
    country == "Perú" ~ "Peru",
    country == "México" ~ "Mexico",
    country == "Chile" ~ "Chile",
    country == "Colombia" ~ "Colombia",
    country == "Argentina" ~ "Argentina",
    country == "Brasil" ~ "Brazil"
  ))

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(any_info_treatment)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$IPW_any_info_treatment*hesitant$weight_rake_ses,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = "Any vaccine information",
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table28_anyinfo_rake_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)

# Table 29

make_table(treatment = "factor(information_treatment)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$IPW_info*hesitant$weight_rake_ses,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Health",
                                "Health + herd 60%",
                                "Health + herd 70%", 
                                "Health + herd 80%",
                                "Health + herd 60% + current",
                                "Health + herd 70% + current",
                                "Health + herd 80% + current", 
                                "Health + Biden"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table29_allinfo_pooled_rake"
)

# Table 30

# indicator for sample restrictions, treatment 2-7
hesitant <- hesitant %>% mutate(t27 = ifelse(information_treatment >= 2 & 
                                               information_treatment <= 7, 1, 0))

# restrict sample to treatments 2-7
hesitant27 <- hesitant %>% filter(t27 == 1)

# indicator for receiving current wtv info
hesitant27 <- hesitant27 %>% mutate(received_wtv = ifelse(information_treatment >= 5 &
                                                            information_treatment <= 7, 1, 0))


# indicator for whether herd immunity level seen is greater than wtv rate seen
hesitant27 <- hesitant27 %>% mutate(herd_seen = ifelse(information_treatment==5 | information_treatment==2, 60,
                                                       ifelse(information_treatment==6 | information_treatment==3, 70, 
                                                              ifelse(information_treatment==7 | information_treatment==4, 80, 0))))

hesitant27 <- hesitant27 %>% mutate(herd_above_wtv = ifelse(herd_seen > current_willingness, 1, 0))

# current willingness indicators
hesitant27 <- hesitant27 %>% mutate(current_56 = ifelse(current_willingness==56, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_57 = ifelse(current_willingness==57, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_58 = ifelse(current_willingness==58, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_61 = ifelse(current_willingness==61, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_64 = ifelse(current_willingness==64, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_66 = ifelse(current_willingness==66, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_67 = ifelse(current_willingness==67, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_73 = ifelse(current_willingness==73, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_75 = ifelse(current_willingness==75, 1, 0))
hesitant27 <- hesitant27 %>% mutate(current_79 = ifelse(current_willingness==79, 1, 0))

# demean the first 9 indicators by subtracting the mean of the corresponding indicator in the sample that we will use for estimation
hesitant27 <- hesitant27 %>% mutate(d_current_56 = (current_56 - mean(current_56)))
hesitant27 <- hesitant27 %>% mutate(d_current_57 = (current_57 - mean(current_57)))
hesitant27 <- hesitant27 %>% mutate(d_current_58 = (current_58 - mean(current_58)))
hesitant27 <- hesitant27 %>% mutate(d_current_61 = (current_61 - mean(current_61)))
hesitant27 <- hesitant27 %>% mutate(d_current_64 = (current_64 - mean(current_64)))
hesitant27 <- hesitant27 %>% mutate(d_current_66 = (current_66 - mean(current_66)))
hesitant27 <- hesitant27 %>% mutate(d_current_67 = (current_67 - mean(current_67)))
hesitant27 <- hesitant27 %>% mutate(d_current_73 = (current_73 - mean(current_73)))
hesitant27 <- hesitant27 %>% mutate(d_current_75 = (current_75 - mean(current_75)))

models <- lapply(outcomes, function(y)
  lm_robust(as.formula(paste0(y, " ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv + factor(current_willingness) + received_wtv*d_current_56 + received_wtv*d_current_57 + received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 + received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + received_wtv*d_current_75 + std_months_pre")),
            fixed_effects = factor(fixed_effects),
            data = hesitant27,
            weights = hesitant27$weight_rake_ses,
            se_type = "stata")
)

outcome_stats <- hesitant27 %>%
  filter(received_wtv == 0 & herd_above_wtv == 0) %>%
  filter(!is.na(weight_rake_ses)) %>%
  dplyr::summarize(across(
    all_of(outcomes),
    .fns = list(Mean = ~weighted.mean(., w = weight_rake_ses, na.rm = TRUE),
                SD = ~sqrt(wtd.var(., w = weight_rake_ses, na.rm = TRUE)))))
observations <- sapply(1:length(outcomes), function(y)
  format(nobs(models[[y]]), big.mark = ","))

rsq <- sapply(1:length(outcomes), function(y)
  round(models[[y]]$r.squared, 3)
)

texreg(models,
       include.ci = FALSE,
       file = paste0("Tables and Figures/SI_table30_currentherdint_rake", ".tex"),
       caption = NULL,
       label = paste0("table:", "SI_table30_currentherdint_rake"),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = outcomes_labels,
       custom.coef.names = c("Current", 
                             "Herd opinion above current rate",
                             rep("", 19),
                             "Current $\\times$ herd opinion above current rate",
                             rep("", 9)),
       custom.gof.rows = list("Outcome range" = c("[1,5]", "{0,1}", "[0,12]", "{0,1}"),
                              "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                              "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                              "Observations" = observations,
                              "R$^{2}$" = rsq
       ),
)

rm(models, outcome_stats, observations, rsq)

# Table 31

## Pooled

make_table(treatment = "factor(motivation_treatment_enc)",
           interaction = NULL,
           outcome_vars = outcomes,
           fixed_effects = "factor(fixed_effects)",
           weights = hesitant$weight_rake_ses,
           one_tailed = FALSE,
           outcome_labels = outcomes_labels,
           treatment_labels = c("Altruism",
                                "Economic recovery",
                                "Social approval"),
           data = hesitant,
           table_name = "Tables and Figures/SI_table31_motiv_rake"
)

## By Country

hesitant_orig <- hesitant

for (i in 1:length(unique(hesitant_orig$country))) {
  hesitant <- subset(hesitant_orig, hesitant_orig$country == unique(hesitant_orig$country)[i])
  
  make_table(treatment = "factor(motivation_treatment_enc)",
             interaction = NULL,
             outcome_vars = outcomes,
             fixed_effects = "factor(fixed_effects)",
             weights = hesitant$weight_rake_ses,
             one_tailed = FALSE,
             outcome_labels = outcomes_labels,
             treatment_labels = c("Altruism",
                                  "Economic recovery",
                                  "Social approval"),
             data = hesitant,
             table_name = paste0("Tables and Figures/SI_table31_motiv_rake_", unique(hesitant_orig$country)[i])
  )
}

hesitant <- hesitant_orig

rm(hesitant_orig, i)
rm(list=setdiff(ls(), c("hesitancy", "make_table")))

## Fix all tables

tables_list <- list.files(path = "Tables and Figures/", pattern = "^SI")

SI_tables <- lapply(tables_list, function(t) read.delim(paste0("Tables and Figures/", t), header = FALSE))

SI_tables <- lapply(1:length(tables_list), function(t)
  SI_tables[[t]][!grepl("^R\\$\\^2", SI_tables[[t]]$V1) &
                   !grepl("^Adj.", SI_tables[[t]]$V1) &
                   !grepl("^DF", SI_tables[[t]]$V1) &
                   !grepl("^N *", SI_tables[[t]]$V1),]
)

for (t in 1:length(tables_list)) {
  write(SI_tables[[t]], file = paste0("Tables and Figures/", tables_list[[t]]))
}

## Note: edit control group mean and std. dev. for Table 5
## Note: manually edit control group mean and std dev. for Table 20