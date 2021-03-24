library(dplyr)
library(estimatr)
library(Hmisc)
library(texreg)

make_table <- function(treatment,
                       interaction,
                       outcome_vars,
                       fixed_effects,
                       weights,
                       one_tailed,
                       outcome_labels,
                       treatment_labels,
                       data,
                       table_name) {
  
  if (length(interaction) > 0) {
    models <- lapply(outcome_vars, function(y)
        lm_robust(as.formula(paste0(y,
                               " ~ ",
                               treatment, "+", 
                               interaction,
                               " + ",
                               "std_months_pre")),
             data = data,
             weights = weights,
             fixed_effects = fixed_effects,
             se_type = 'stata')
      )
  } else {
    models <- lapply(outcome_vars, function(y)
      lm_robust(as.formula(paste0(y,
                                    " ~ ",
                                    treatment,
                                    " + ",
                                    "std_months_pre")),
                  data = data,
                  weights = weights,
                  fixed_effects = fixed_effects,
                  se_type = 'stata')
      )
  }
  
  estimates <- lapply(1:length(outcome_vars), function(y)
      as.numeric(summary(models[[y]])$coefficients[,'Estimate'])
  )

  ses <- lapply(1:length(outcome_vars), function(y)
    as.numeric(summary(models[[y]])$coefficients[,'Std. Error'])
  )
  
  if (one_tailed == TRUE) {
  
    pvals <- lapply(1:length(outcome_vars), function(y)
      sapply(1:length(estimates[[t]][[y]]), function(p)
        if (estimates[[y]][p] > 0) {
          (as.numeric(summary(models[[y]])$coefficients[,'Pr(>|t|)'])[p])/2
          } else if (estimates[[y]][p] <= 0) {
            as.numeric(summary(models[[y]])$coefficients[,'Pr(>|t|)'])[p]
          }
        )
      )
  
  } else if (one_tailed == FALSE) {
    
    pvals <- lapply(1:length(outcome_vars), function(y)
      sapply(1:length(estimates[[y]]), function(p)
        as.numeric(summary(models[[y]])$coefficients[,'Pr(>|t|)'])[p]
        )
      )
    
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(hesitant[,y], na.rm = TRUE),3), "-", round(max(hesitant[,y], na.rm = TRUE),3))
    ))
  
  treatment_vars_non_factor <- gsub("factor", "", treatment)
  treatment_vars_non_factor <- gsub("\\(", "", treatment_vars_non_factor)
  treatment_vars_non_factor <- gsub("\\)", "", treatment_vars_non_factor)
  
  if (length(weights) > 0) {
    data$use_weights <- weights
    
    outcome_stats <- data %>%
      filter(get(treatment_vars_non_factor) == 0) %>%
      filter(!is.na(use_weights)) %>%
      dplyr::summarize(across(
        all_of(outcome_vars),
        .fns = list(Mean = ~weighted.mean(., w = use_weights, na.rm = TRUE),
                    SD = ~sqrt(wtd.var(., w = use_weights, na.rm = TRUE)))))
    } else {
    outcome_stats <- data %>%
      filter(get(treatment_vars_non_factor) == 0) %>%
      dplyr::summarize(across(
        all_of(outcome_vars),
        .fns = list(Mean = ~mean(., na.rm = TRUE),
                    SD = ~sd(., na.rm = TRUE))))
  }
    
    observations <- sapply(1:length(outcome_vars), function(y)
      format(nobs(models[[y]]), big.mark = ","))
    
    rsq <- sapply(1:length(outcome_vars), function(y)
    round(models[[y]]$r.squared, 3)
    )
  
  texreg(models,
         include.ci = FALSE,
         file = paste0(table_name, ".tex"),
         caption = NULL,
         label = paste0("table:", table_name),
         stars = c(0.01, 0.05, 0.1),
         digits = 3,
         custom.model.names = outcome_labels,
         custom.coef.names = treatment_labels,
         custom.gof.rows = list("Outcome range" = outcome_range,
                                "Control outcome mean" = round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),2),
                                "Control outcome std. dev" = round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),2),
                                "Observations" = observations,
                                "R$^{2}$" = rsq
         ),
         override.pvalues = pvals,
         omit.coef = "std_months_pre"
  )
  
}
