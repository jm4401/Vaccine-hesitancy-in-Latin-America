# This file replicates the analysis for Messaging interventions that increase COVID-19 vaccine willingness in Latin America - Main Text
# Pablo Argote, Elena Barham, Sarah Daly, Julian E. Gerez, John Marshall, Oscar Pocasangre

## Preliminaries

library(haven)
library(lfe)
library(plyr)
library(tidyverse)
library(weights)

unzip("vaccine_wide.dta.zip")
hesitancy <- read_dta("vaccine_wide.dta")

# Figure 1 - flow chart created in TikZ

# Figure 2 - distribution of vaccine willingness across countries

## Panel A

df <- hesitancy %>% select(country, hesitancy_pre, hesitancy_pre_rec, weight_joint)
df$country <- recode(df$country, "Perú"= 'Peru', "México"='Mexico', 'Chile'='Chile', 'Colombia'='Colombia', 
                     'Argentina'='Argentina', 'Brasil'='Brazil')

df$country <- as.factor(df$country)
df$country <- factor(df$country, levels= c("Peru", "Mexico", "Colombia", "Chile", "Brazil", "Argentina"))
df <- df %>% filter(!is.na(hesitancy_pre))

df <- df %>% mutate(dummy = ifelse(hesitancy_pre==5 | hesitancy_pre==4, 1, 0))
wpct(df$dummy, df$weight_joint)
wpct(df$dummy[df$country=='Peru'], df$weight_joint[df$country=='Peru'])
wpct(df$dummy[df$country=='Mexico'], df$weight_joint[df$country=='Mexico'])
wpct(df$dummy[df$country=='Colombia'], df$weight_joint[df$country=='Colombia'])
wpct(df$dummy[df$country=='Chile'], df$weight_joint[df$country=='Chile'])
wpct(df$dummy[df$country=='Brazil'], df$weight_joint[df$country=='Brazil'])
wpct(df$dummy[df$country=='Argentina'], df$weight_joint[df$country=='Argentina'])

df_w <- df %>% group_by(country) %>% summarise(weightedprop = wpct(hesitancy_pre, weight_joint))
df_w <- df_w %>% mutate(hesitancy_cat = NA)
hesitancy_categories <- c(-8, 1, 2, 3, 4, 5)
df_w$hesitancy_cat <- rep(hesitancy_categories, 6)

df_w <- df_w %>% mutate(hesitancy_cat_labels = case_when(
    hesitancy_cat == 1 ~ 'Strongly Disagree',
    hesitancy_cat == 2 ~ 'Disagree',
    hesitancy_cat == 3 ~ 'Neither Agree nor Disagree',
    hesitancy_cat == 4 ~ 'Agree', 
    hesitancy_cat == 5 ~ 'Strongly Agree',
    hesitancy_cat == -8 ~ "Don't Know"
  )
)

df_w$hesitancy_cat_labels <- factor(df_w$hesitancy_cat_labels, levels = c("Don't Know",
                                                                          "Strongly Disagree",
                                                                          "Disagree",
                                                                          "Neither Agree nor Disagree", 
                                                                          "Agree",
                                                                          "Strongly Agree"))

df_w$country <- recode(df_w$country, "Perú"= 'Peru', "México"='Mexico', 'Chile'='Chile', 'Colombia'='Colombia', 
                       'Argentina'='Argentina', 'Brasil'='Brazil')
df_w$country <- as.factor(df_w$country)
df_w$country <- factor(df_w$country, levels= c("Peru", "Mexico", "Colombia", "Chile", "Brazil", "Argentina"))

hesitancy5p_dk <- ggplot() +
  geom_bar(data = df_w, aes(x = country, y=weightedprop, fill=hesitancy_cat_labels), position="stack", stat="identity") +
  coord_flip() + 
  #ggtitle("If a vaccine were available to me, I would get it") +
  ylab(" ") +
  xlab("") +
  scale_fill_brewer(palette="Blues")+
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text= element_text(size=12), 
        legend.text = element_text(size = 12))

pdf("Tables and Figures/Figure2_A.pdf")
print(hesitancy5p_dk)
dev.off()

## Panel B

df <- hesitancy %>% select(country, quickly_pre_1_text, weight_joint, weight_joint)

df$country <- recode(df$country, "Perú"= 'Peru', "México"='Mexico', 'Chile'='Chile', 'Colombia'='Colombia', 
                       'Argentina'='Argentina', 'Brasil'='Brazil')

df$country <- as.factor(df$country)

avg_time2vax <- weighted.mean(na.omit(df$quickly_pre_1_text, df$weight_joint))

avg_perc <- df %>% group_by(country) %>% summarise(Months = weighted.mean(na.omit(quickly_pre_1_text, weight_joint)))

names(avg_perc)[1] <- "Country"
avg_perc$Months <- round(avg_perc$Months, 2)

df <- hesitancy %>% select(country, quickly_pre, quickly_pre_1_text, weight_joint, weight_joint)
df$country <- recode(df$country, "Perú"= 'Peru', "México"='Mexico', 'Chile'='Chile', 'Colombia'='Colombia', 
                     'Argentina'='Argentina', 'Brasil'='Brazil')

df$country <- as.factor(df$country)

df$quickly_pre_1_text[df$quickly_pre==0] <- 'never'

df$time_cat <- recode(df$quickly_pre_1_text, 'never'='Never', '0'='0-1 Months',
                      '0.1'='0-1 Months', '0.01'='0-1 Months',
                      '0.2'='0-1 Months', '0.5'='0-1 Months', 
                      '0.6'= '0-1 Months', '1'='0-1 Months', 
                      '1.2'='0-1 Months', 
                      '2'='2-3 Months', '3'= '2-3 Months',
                      '4'= '4-5 Months', '5'= '4-5 Months', 
                      '6'= '6-7 Months', '7'='6-7 Months',
                      '8'='8-9 Months', '9'='8-9 Months', 
                      '10'='10-11 Months', '11'='10-11 Months', 
                      '12'='12 or More Months')

dfw <- df %>% group_by(country) %>% summarise(weightedprop = wpct(time_cat, weight_joint))
dfw <- dfw %>% mutate(time_cat = NA)

time_categories <- c("0-1 Months", "2-3 Months","4-5 Months",
                     "6-7 Months", "8-9 Months", "10-11 Months", 
                     "12 or More Months", "Never")

dfw$time_cat <- rep(time_categories, 6)
dfw$time_cat <- factor(dfw$time_cat, levels = c("Never", "12 or More Months", "10-11 Months", 
                                                "8-9 Months", "6-7 Months", "4-5 Months", 
                                                "2-3 Months", "0-1 Months"))

dfw$country <- factor(dfw$country, levels = c('Peru', 'Mexico', 'Colombia', 'Chile', 
                                              'Brazil', "Argentina"))

dfw$country <- as.factor(dfw$country)

timetovaxplot <- ggplot()+
  geom_bar(data=dfw, aes(x = country, y= weightedprop, fill=time_cat), position="stack",     stat="identity")+
  coord_flip() + 
  #ggtitle("If a vaccine were available to you, how long would you wait to get it? ")+
  xlab("")+
  ylab("")+
  scale_fill_brewer(palette="Blues")+
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text= element_text(size=12), 
        legend.text = element_text(size = 12))

pdf("Tables and Figures/Figure2_B.pdf")
print(timetovaxplot)
dev.off()

# Figure 3 - reasons for initial vaccine hesitancy and response to vaccine information treatments

## Panel A

df <- hesitancy %>% select(finished, weight_joint, country, reasons_hesitancy_1, reasons_hesitancy_2, reasons_hesitancy_3, 
                           reasons_hesitancy_4, reasons_hesitancy_5, reasons_hesitancy_6, 
                           reasons_hesitancy_7, reasons_hesitancy_8, reasons_hesitancy_9, 
                           reasons_hesitancy_10, treat_change_reason_1, treat_change_reason_2, 
                           treat_change_reason_3, treat_change_reason_4, treat_change_reason_5, 
                           treat_change_reason_6, treat_change_reason_7, treat_change_reason_8, 
                           treat_change_reason_9, treat_change_re_time_page_submit)

df <- df %>% filter(!is.na(treat_change_re_time_page_submit))
df <- as.data.frame(lapply(df, function(x) replace(x, is.na(x), 0)))


df_pre <- df %>% 
  select(finished, weight_joint, country, reasons_hesitancy_1, reasons_hesitancy_2, reasons_hesitancy_3, 
         reasons_hesitancy_4, reasons_hesitancy_5, reasons_hesitancy_6, 
         reasons_hesitancy_7, reasons_hesitancy_8, reasons_hesitancy_9, 
         reasons_hesitancy_10)


colnames(df_pre) <- c('finished', 'weight_joint', 'country', "Side Effects", "Fear of Infection from Vax", "Vax Developed Too Fast", 
                      "Vax Not Effective", "Low Infection Risk", "Anti Vax", 
                      "Prefer Immunity from Infection", "Already Had COVID", "Mistrusts Gov", 
                      "Cost")


df_long <- df_pre %>% gather(key = reason, value = value, 4:13)
df_long <- df_long %>% group_by(reason) %>% summarise(mean_wr = weighted.mean(value, weight_joint))

df_long <- df_long %>% filter(reason != 'Anti Vax')

plot_reasons <- df_long %>% arrange(mean_wr) %>% 
  mutate(reason=factor(reason, levels=reason)) %>% 
  ggplot(aes(x=reason, y=mean_wr)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) + 
  xlab("") + ylab("") +
  scale_x_discrete(labels=c("Low infection risk",
                            "Prefers immunity \nfrom infection",
                            "Already had Covid-19",
                            "Fears getting \nCovid-19 from \nvaccine",
                            "Cost", "Vaccine effectiveness",
                            "Mistrusts \ngovernment", "Vaccine developed \ntoo fast",
                            "Side effects")) + 
  theme(axis.text.y = element_text(size = 13))

pdf(file="Tables and Figures/Figure3_A.pdf")
print(plot_reasons)
dev.off()

## Panel B

df_post <- df %>% select(finished, weight_joint, country, treat_change_reason_1, treat_change_reason_2, 
                         treat_change_reason_3, treat_change_reason_4, treat_change_reason_5, 
                         treat_change_reason_6, treat_change_reason_7, treat_change_reason_8, 
                         treat_change_reason_9)

colnames(df_post) <- c('finished', 'weight_joint', 'country', "Side Effects", "Fear of Infection from Vax", "Vax Developed Too Fast", 
                       "Vax Not Effective", "Low Infection Risk", 
                       "Prefer Immunity from Infection", "Already Had COVID", "Mistrusts Gov", 
                       "Cost")

df_long <- df_post %>% gather(key = reason, value = value, 4:12)
df_long <- df_long %>% group_by(reason) %>% summarise(mean_wr = weighted.mean(value, weight_joint))

reasons_post <- df_long %>% arrange(mean_wr) %>% 
  mutate(reason=factor(reason, levels=reason)) %>% 
  ggplot( aes(x=reason, y=mean_wr)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) + 
  xlab("") + ylab("") +
  scale_x_discrete(labels=c("Now trusting \nof government",
                            "No longer wants \nimmunity from infection",
                            "Now getting vaccinated \neven if already \nhad Covid-19",
                            "No longer worried \nabout cost",
                            "Now getting vaccinated \neven if low risk",
                            "Less worried \nabout side effects",
                            "Less worried \nabout getting \nCovid-19 from vaccine",
                            "Less worried \nabout speed of \ndevelopment",
                            "Less worried \nabout vaccine \nineffectiveness")) +
  theme(axis.text.y = element_text(size = 13)) 

print(reasons_post)

pdf(file="Tables and Figures/Figure3_B.pdf")
print(reasons_post)
dev.off()

rm(list=setdiff(ls(), "hesitancy"))

# Figure 4 - average effects of basic vaccine information on vaccine willingness, by country

## Panel A

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

hesitant$country <-  recode(hesitant$country, 'Argentina'='Argentina', 'Brasil'='Brazil', 'Chile'='Chile', 'Colombia'='Colombia', 'México'='Mexico', 'Perú'='Peru')

hesitant <- hesitant %>% filter(!is.na(any_info_treatment))

outcomes <- c("hesitancy_post_rec", "hesitancy_dummy_post", "quickly_post_1_text_reversed2", "encourage2")

fig_all <- lapply(outcomes, function(y)
  felm(
    as.formula(
      paste0(y, "~ factor(any_info_treatment) + std_months_pre | factor(fixed_effects)")
      ),
    data = hesitant,
    weights = hesitant$IPW_any_info_treatment,
    cmethod = 'reghdfe'
    )
)

names(fig_all) <- outcomes

fig_countries <- lapply(outcomes, function(y)
  lapply(unique(hesitant$country), function(i)
       felm(
         as.formula(
           paste0(y, "~ factor(any_info_treatment) + std_months_pre | factor(fixed_effects)")
           ),
         data = hesitant,
         weights = hesitant$IPW_any_info_treatment[hesitant$country==i],
         cmethod = 'reghdfe',
         subset = (country==i)
         )
  )
)

names(fig_countries) <- outcomes

fig <- lapply(outcomes, function(y)
  list(fig_all[[y]],
       fig_countries[[y]][[1]],
       fig_countries[[y]][[2]],
       fig_countries[[y]][[3]],
       fig_countries[[y]][[4]],
       fig_countries[[y]][[5]],
       fig_countries[[y]][[6]]
       )
  )

names(fig) <- outcomes

treatment <- lapply(outcomes, function(y) as.data.frame(matrix(NA, nrow = length(fig[[y]]), ncol = 10)))

names(treatment) <- outcomes

for (y in outcomes) {
  names(treatment[[y]]) <- c("coefs", y, "rse", "lower_ci", "higher_ci", "pval", "pvaltext", "coefplus", "any_info_treatment", "country")
  treatment[[y]]$coefs <- rep("factor(any_info_treatment)1", length(fig[[y]]))
  treatment[[y]][,y] <- sapply(fig[[y]], function(i) coef(i))['factor(any_info_treatment)1',]
  treatment[[y]]$rse <- sapply(1:length(fig[[y]]), function(i) fig[[y]][[i]]$rse['factor(any_info_treatment)1'])
  treatment[[y]]$lower_ci <- sapply(1:length(fig[[y]]), function(i) confint(fig[[y]][[i]], level = 0.95)['factor(any_info_treatment)1',]['2.5 %'])
  treatment[[y]]$higher_ci <- sapply(1:length(fig[[y]]), function(i) confint(fig[[y]][[i]], level = 0.95)['factor(any_info_treatment)1',]['97.5 %'])
  treatment[[y]]$pval <- sapply(1:length(fig[[y]]), function(i) fig[[y]][[i]]$rpval['factor(any_info_treatment)1'])
  treatment[[y]]$pvaltext <- ifelse(treatment[[y]]$pval < 0.01, 'p < 0.01', paste('p = ', round(treatment[[y]]$pval, 2)))
  treatment[[y]]$any_info_treatment <- rep(1, length(fig[[y]]))
  treatment[[y]]$country <- c("All", unique(hesitant$country))
}

control_all <- lapply(outcomes, function(y)
  colMeans(hesitant[which(hesitant$any_info_treatment == 0),y], na.rm = TRUE)
)

names(control_all) <- outcomes

for (y in outcomes) {
  names(control_all[[y]]) <- "All"
}

control_countries <- lapply(outcomes, function(y)
  sapply(unique(hesitant$country), function(i)
    colMeans(hesitant[which(hesitant$any_info_treatment == 0 & hesitant$country == i),y],
             na.rm = TRUE)
  )
)

names(control_countries) <- outcomes

control <- lapply(outcomes, function(y) data.frame(c(control_all[[y]], control_countries[[y]])))

names(control) <- outcomes

for (y in outcomes) {
  names(control[[y]]) <- y
  rownames(control[[y]]) <- NULL
  control[[y]]$country <- c("All", "Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Peru")
  control[[y]]$any_info_treatment <- rep(0, length(fig[[y]]))
  control[[y]]$coefs <- rep("control", length(fig[[y]]))
  treatment[[y]]$coefplus <- control[[y]][,y] + treatment[[y]][,y]
  control[[y]]$coefplus <- control[[y]][,y]
}

regout <- lapply(outcomes, function(y) rbind.fill(treatment[[y]], control[[y]]))

names(regout) <- outcomes

n_all <- hesitant %>% group_by(any_info_treatment) %>% summarise(n = n()) %>%
  mutate(coefs = c("control",
                   "factor(any_info_treatment)1")) %>%
  mutate(ntext = paste0('n=', n)) %>%
  mutate(country = "All") %>%
  mutate(any_info_treatment = as.numeric(any_info_treatment))

n_countries <- lapply(unique(hesitant$country), function(i)
  hesitant %>% 
    filter(country == i) %>%
    group_by(any_info_treatment) %>% 
    summarise(n = n()) %>%
    mutate(coefs = c("control",
                     "factor(any_info_treatment)1")) %>% 
    mutate(ntext = paste0('n=', n)) %>%
    mutate(country = i) %>%
    mutate(any_info_treatment = as.numeric(any_info_treatment))
)

n_combined <- bind_rows(list(n_all, n_countries)) %>%
  group_by(country) %>%
  mutate(n_total = sum(n)) %>%
  mutate(n_total_text = paste0("n=", n_total))

regout <- lapply(outcomes, function(y) left_join(regout[[y]], n_combined, by = c('coefs', 'country', 'any_info_treatment')))

names(regout) <- outcomes

atebp <- ggplot(data = regout[['hesitancy_post_rec']], aes(x = country, y=coefplus, fill=coefs)) +
  geom_bar(width = 0.8, position="dodge", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, position = position_nudge(x = 0.2), size=0.7) +
  coord_cartesian(ylim=c(2.75, 3.7)) +
  ylab("Vaccine willingness (strongly disagree - strongly agree)") +
  xlab(" ") +
  scale_x_discrete(labels = paste(regout[['hesitancy_post_rec']]$country, "\n", regout[['hesitancy_post_rec']]$n_total_text)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Any vaccine information")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.95)), vjust=-1, position = position_dodge(0.9)) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text = element_text(size=13))

print(atebp)

pdf(file="Tables and Figures/Figure4_A.pdf")
print(atebp)
dev.off()

## Panel B

atebp <- ggplot(data = regout[['hesitancy_dummy_post']], aes(x = country, y=coefplus, fill=coefs)) +
  geom_bar(width = 0.8, position="dodge", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, position = position_nudge(x = 0.2)) +
  coord_cartesian(ylim=c(0.3, 0.65)) +
  ylab("Willing to vaccinate") +
  xlab(" ") +
  scale_x_discrete(labels = paste(regout[['hesitancy_post_rec']]$country, "\n", regout[['hesitancy_post_rec']]$n_total_text)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Any vaccine information")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.95)), vjust=-1, position = position_dodge(0.9)) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text = element_text(size=13))

print(atebp)

pdf(file="Tables and Figures/Figure4_B.pdf")
print(atebp)
dev.off()

## Panel C

atebp <- ggplot(data = regout[['quickly_post_1_text_reversed2']], aes(x = country, y=coefplus, fill=coefs)) +
  geom_bar(width = 0.8, position="dodge", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, position = position_nudge(x = 0.2)) +
  coord_cartesian(ylim=c(4.5, 8)) +
  ylab("Months would wait to get vaccinated once eligible") +
  xlab(" ") +
  scale_x_discrete(labels = paste(regout[['hesitancy_post_rec']]$country, "\n", regout[['hesitancy_post_rec']]$n_total_text)) +
  scale_y_continuous(breaks = c(5,  6,  7),
                     labels = c("6", "5",  "4")) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Any vaccine information")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1, position = position_dodge(1.17)) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text = element_text(size=13))

print(atebp)

pdf(file="Tables and Figures/Figure4_C.pdf")
print(atebp)
dev.off()

## Panel D

atebp <-  ggplot(data = regout[['encourage2']], aes(x = country, y=coefplus, fill=coefs)) +
  geom_bar(width = 0.8, position="dodge", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, position = position_nudge(x = 0.2)) +
  coord_cartesian(ylim=c(0.3, 0.8)) +
  ylab("Likely to encourage others to get vaccinated") +
  xlab("") +
  scale_x_discrete(labels = paste(regout[['hesitancy_post_rec']]$country, "\n", regout[['hesitancy_post_rec']]$n_total_text)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Any vaccine information")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.95)), vjust=-1, position = position_dodge(0.9)) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text = element_text(size=13))

print(atebp)

pdf(file="Tables and Figures/Figure4_D.pdf")
print(atebp)
dev.off()

rm(list=setdiff(ls(), c("hesitancy", "hesitant")))

# Figure 5 - average effects of vaccine information variants on vaccine willingness

## Panel A

hesitant <- hesitant %>% filter(!is.na(IPW_info))

hesitant$information_treatment <- factor(hesitant$information_treatment, 
                                         levels = c(0, 1, 8, 2, 3, 4, 5, 6, 7))

fig_dummy_m <- felm(hesitancy_post_rec ~
                      factor(information_treatment) +
                      std_months_pre | factor(fixed_effects),
                    data = hesitant,
                    weights = hesitant$IPW_info,
                    cmethod = 'reghdfe')

coefficients = c("A",
                 "B",
                 "C",
                 "D",
                 "E",
                 "F",
                 "G",
                 "H",
                 "std_months_pre")

coefs <- as.data.frame(fig_dummy_m$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_dummy_m$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_dummy_m, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)

regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')

pval <- as.data.frame(round(fig_dummy_m$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_dummy_m$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_dummy_m$rpval, 2)`)))

regout <- merge(regout, pval, by=c('coefs'))

regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$information_treatment==0]))
                                        + hesitancy_post_rec))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "hesitancy_post_rec", 'coefplus'))
control$coefs <- c('0_control')
control$hesitancy_post_rec <- (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$information_treatment==0])))
control$coefplus <- (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$information_treatment==0])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(information_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("0_control",
                            "A",
                            "B",
                            "C",
                            "D",
                            "E",
                            "F",
                            "G",
                            "H"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=information_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.3, size=0.71) +
  ylab("Vaccine willingness (strongly disagree - strongly agree)") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(3, 3.5)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Vaccine", "+Biden", "+Herd 60%", "+Herd 70%", "+Herd 80%", 
                                                "+Herd 60% +Current", "+Herd 70% +Current", "+Herd 80% +Current")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=14), 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size=13.5))
print(atebp)

pdf("Tables and Figures/Figure5_A.pdf", width = 8.5, height = 7.2)
print(atebp)
dev.off()

## Panel B

fig_dummy_m <- felm(hesitancy_dummy_post ~
                      factor(information_treatment) +
                      std_months_pre | factor(fixed_effects),
                    data = hesitant,
                    weights = hesitant$IPW_info,
                    cmethod = 'reghdfe')

coefs <- as.data.frame(fig_dummy_m$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_dummy_m$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_dummy_m, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)

regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')

pval <- as.data.frame(round(fig_dummy_m$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_dummy_m$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_dummy_m$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))

regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$information_treatment==0]))
                                        + hesitancy_dummy_post))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "hesitancy_dummy_post", 'coefplus'))
control$coefs <- c('0_control')
control$hesitancy_dummy_post <- (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$information_treatment==0])))
control$coefplus <- (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$information_treatment==0])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(information_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("0_control",
                            "A",
                            "B",
                            "C",
                            "D",
                            "E",
                            "F",
                            "G",
                            "H"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))


atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=information_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Willing to vaccinate") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(0.3, .55)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Vaccine", "+Biden", "+Herd 60%", "+Herd 70%", "+Herd 80%", 
                                                "+Herd 60%, +Current", "+Herd 70%, +Current", "+Herd 80%, +Current" 
  )) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=14), 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size=13.5))

print(atebp)

pdf("Tables and Figures/Figure5_B.pdf", width = 8.5, height = 7.2)
print(atebp)
dev.off()

## Panel C

fig_dummy_m <- felm(quickly_post_1_text_reversed2 ~
                      factor(information_treatment) +
                      std_months_pre | factor(fixed_effects),
                    data = hesitant,
                    weights = hesitant$IPW_info,
                    cmethod = 'reghdfe')

coefs <- as.data.frame(fig_dummy_m$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_dummy_m$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_dummy_m, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)

regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')

pval <- as.data.frame(round(fig_dummy_m$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_dummy_m$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_dummy_m$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))

regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$information_treatment==0]))
                                        + quickly_post_1_text_reversed2))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "quickly_post_1_text_reversed2", 'coefplus'))
control$coefs <- c('0_control')
control$quickly_post_1_text_reversed2 <- (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$information_treatment==0])))
control$coefplus <- (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$information_treatment==0])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(information_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("0_control",
                            "A",
                            "B",
                            "C",
                            "D",
                            "E",
                            "F",
                            "G",
                            "H"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill= as.factor(information_treatment))) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Months would wait to get vaccinated once eligible") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(5, 6.75)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_y_continuous(labels=abs ) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Vaccine", "+Biden", "+Herd 60%", "+Herd 70%", "+Herd 80%", 
                                                "+Herd 60%, +Current", "+Herd 70%, +Current", "+Herd 80%, +Current")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=14), 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size=13.5))

print(atebp)

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill= as.factor(information_treatment))) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Months would wait to get vaccinated once eligible") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(5, 6.75)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_y_continuous(breaks = c(5.0, 5.5, 6.0, 6.5),
                     labels = c("6.5", "6.0", "5.5", "5.0")) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Vaccine", "+Biden", "+Herd 60%", "+Herd 70%", "+Herd 80%", 
                                                "+Herd 60%, +Current", "+Herd 70%, +Current", "+Herd 80%, +Current")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=14), 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size=13.5))

print(atebp)

pdf("Tables and Figures/Figure5_C.pdf", width = 8.5, height = 7.2)
print(atebp)
dev.off()

## Panel D

fig_dummy_m <- felm(encourage2 ~
                      factor(information_treatment) +
                      std_months_pre | factor(fixed_effects),
                    data = hesitant,
                    weights = hesitant$IPW_info,
                    cmethod = 'reghdfe')


coefs <- as.data.frame(fig_dummy_m$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_dummy_m$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_dummy_m, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)

regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')
pval <- as.data.frame(round(fig_dummy_m$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_dummy_m$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_dummy_m$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))


regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$encourage2[hesitant$information_treatment==0]))
                                        + encourage2))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "encourage2", 'coefplus'))
control$coefs <- c('0_control')
control$encourage2 <- (mean(na.omit(hesitant$encourage2[hesitant$information_treatment==0])))
control$coefplus <- (mean(na.omit(hesitant$encourage2[hesitant$information_treatment==0])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(information_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("0_control",
                            "A",
                            "B",
                            "C",
                            "D",
                            "E",
                            "F",
                            "G",
                            "H"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=information_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Likely to encourage others to get vaccinated") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(0.5, .67)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Vaccine", "+Biden", "+Herd 60%", "+Herd 70%", "+Herd 80%", 
                                                "+Herd 60%, +Current", "+Herd 70%, +Current", "+Herd 80%, +Current")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=14), 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size=13.5))

print(atebp)

pdf("Tables and Figures/Figure5_D.pdf", width = 8.5, height = 7.2)
print(atebp)
dev.off()

rm(list=setdiff(ls(), c("hesitancy")))

# Figure 6 - effects of current willingness information on vaccine willingness, by whether current willingness is above or below the expert herd immunity opinion a respondent was exposed to

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)

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

# Analysis

m1_wtv <- felm(hesitancy_post_rec ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv +
                 factor(current_willingness) + 
                 received_wtv*d_current_56 + received_wtv*d_current_57 +
                 received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 +
                 received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + 
                 received_wtv*d_current_75 + std_months_pre | factor(fixed_effects), 
               data = hesitant27, 
               cmethod='reghdfe')

m1_dummy <- felm(hesitancy_dummy_post ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv +
                   factor(current_willingness) + 
                   received_wtv*d_current_56 + received_wtv*d_current_57 +
                   received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 +
                   received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + 
                   received_wtv*d_current_75 + std_months_pre | factor(fixed_effects), 
                 data = hesitant27, 
                 cmethod='reghdfe')

m1_months <- felm(quickly_post_1_text_reversed2 ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv +
                    factor(current_willingness) + 
                    received_wtv*d_current_56 + received_wtv*d_current_57 +
                    received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 +
                    received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + 
                    received_wtv*d_current_75 + std_months_pre | factor(fixed_effects), 
                  data = hesitant27, 
                  cmethod='reghdfe')

m1_encourage <- felm(encourage2 ~ received_wtv + herd_above_wtv + received_wtv*herd_above_wtv +
                       factor(current_willingness) + 
                       received_wtv*d_current_56 + received_wtv*d_current_57 +
                       received_wtv*d_current_58 + received_wtv*d_current_61 + received_wtv*d_current_64 +
                       received_wtv*d_current_66 + received_wtv*d_current_67 + received_wtv*d_current_73 + 
                       received_wtv*d_current_75 + std_months_pre | factor(fixed_effects), 
                     data = hesitant27, 
                     cmethod='reghdfe')

regout <- setNames(data.frame(matrix(ncol = 3, nrow = 8)), c("outcome", "coef",  
                                                             "se"))

coefs <- c('wtv_scale', 'wtv_share', 'months', 'encourage')
regout$outcome <- rep(coefs, 2)

interaction_indicator <- c(0, 1)
regout <- regout %>% group_by(outcome) %>% mutate(interaction = interaction_indicator)

wtv_coefs <- c(m1_wtv$coefficients[1], m1_dummy$coefficients[1], 
               m1_months$coefficients[1], m1_encourage$coefficients[1])
regout$coef[regout$interaction==0] <- wtv_coefs

wtv_coefsinteract <- c(m1_wtv$coefficients[1] + m1_wtv$coefficients[22], 
                       m1_dummy$coefficients[1] + m1_dummy$coefficients[22], 
                       m1_months$coefficients[1] + m1_months$coefficients[22], 
                       m1_encourage$coefficients[1] + m1_encourage$coefficients[22])
regout$coef[regout$interaction==1] <- wtv_coefsinteract

coefs_se <- c(m1_wtv$se[1], m1_dummy$se[1], m1_months$se[1], m1_encourage$se[1])
regout$se[regout$interaction==0] <- coefs_se

coefs_intrxn_se <- c(m1_wtv$se[1] + ((m1_wtv$coefficients[22])*m1_wtv$se[22]) + (2*m1_wtv$coefficients[22]*vcov(m1_wtv)[1,22]),
                     m1_dummy$se[1] + ((m1_dummy$coefficients[22])*m1_dummy$se[22]) + (2*m1_dummy$coefficients[22]*vcov(m1_dummy)[1,22]), 
                     m1_months$se[1] + ((m1_months$coefficients[22])*m1_months$se[22]) + (2*m1_months$coefficients[22]*vcov(m1_months)[1,22]),
                     m1_encourage$se[1] + ((m1_encourage$coefficients[22])*m1_encourage$se[22]) + (2*m1_encourage$coefficients[22]*vcov(m1_encourage)[1,22]))

regout$se[regout$interaction==1] <- coefs_intrxn_se

regout <- regout %>% mutate(coef_low = (coef-(1.96*se)))
regout <- regout %>% mutate(coef_high = (coef+(1.96*se)))

pval_main <- c(m1_wtv$pval[1], 
               m1_dummy$pval[1], 
               m1_months$pval[1], 
               m1_encourage$pval[1]) %>% round(2)

tval <- abs(regout$coef[regout$interaction==1]/regout$se[regout$interaction==1])
pval_int <- (1-pnorm(tval))*2
pval_int <- round(pval_int, 2)

regout <- regout %>% mutate(pval = NA)
regout$pval[regout$interaction==0] <- pval_main
regout$pval[regout$interaction==1] <- pval_int
regout <- regout %>% mutate(pvaltext = ifelse(pval < 0.01, "p<0.01", 
                                              paste0("p=", pval)))

regout$interaction[regout$interaction==0] <- c("Current > Herd")
regout$interaction[regout$interaction==1] <- c("Current <= Herd")
regout$interaction <- factor(regout$interaction, levels = c("Current > Herd", "Current <= Herd"))

regout <- regout %>% mutate(ord_outcome = ifelse(outcome=='wtv_scale', 'A', 
                                                 ifelse(outcome=='wtv_share', 'B', 
                                                        ifelse(outcome=='months', 'C', 
                                                               ifelse(outcome=='encourage', 'D', 0)))))

herd_current <- ggplot(regout, aes(ord_outcome, coef)) +
  geom_errorbar(
    aes(ymin = coef_low, ymax = coef_high, color = interaction),
    position = position_dodge(0.4), width = 0.15, size=1.55
  ) +
  geom_point(aes(color = interaction), position = position_dodge(0.4), size=2.25) +
  scale_color_manual(values = c("#47ABF9", "#D32B23"), 
                     labels=c("Current > Herd", "Current <= Herd")) +
  xlab('') +
  ylab('Conditional effect of current treatment') +
  geom_hline(yintercept=0, linetype='dashed') +
  coord_cartesian(ylim = c(-0.4, 0.5), expand = TRUE, clip = "off") +
  annotate(geom = 'text', x = c(0.89, 1.11, 1.89, 2.11, 2.89, 3.11, 3.89, 4.11), 
           y=c(-0.02, -0.17, -0.015, -0.095, -.265, -.345, -0.025, -0.085), 
           label=c('n=1947', 'n=953', 'n=1947', 'n=953', "n=1922", "n=944", "n=1862", "n=909"), 
           size=4) +
  scale_x_discrete(labels = c("Vaccine \nwillingness \n(scale) ", "Willing \nto vaccinate", 
                              "Months would \nwait to get vaccinated \nonce eligible", "Likely to \nencourage others \nto get vaccinated")) +
  theme(legend.position = "right", legend.title = element_blank()) +
  geom_text(aes(label=pvaltext, x=c(0.92, 1.92, 2.92, 3.92, 1.13, 2.13, 3.13, 4.11), 
                y=c(0.26, 0.13, 0.44, 0.125, 0.05, 0.02,  0.29, 0.04)), vjust=-1) +
  theme(axis.text = element_text(size=14), 
        legend.text = element_text(size = 14), 
        axis.title.y = element_text(size=14), 
        axis.ticks = element_blank(), 
        legend.position = "bottom")

print(herd_current)

pdf("Tables and Figures/Figure6.pdf")
print(herd_current)
dev.off()

rm(list=setdiff(ls(), c("hesitancy", "hesitant")))

# Figure 7 - Average effects of motivational messages on vaccine willingness

## Panel A

hesitant <- hesitancy %>%
  filter(sample_causal == 1) %>%
  filter(speeder != 1)


hesitant <- hesitant %>% filter(motivation_treatment != "")


hesitant$motivation_treatment <- factor(hesitant$motivation_treatment, 
                                        levels=c('none', 'altruism', 'economic', 'social'))


fig_motiv <- felm(hesitancy_post_rec ~
                    factor(motivation_treatment) +
                    std_months_pre | factor(fixed_effects),
                  data = hesitant,
                  cmethod = 'reghdfe')

coefficients = c("1",
                 "2",
                 "3",
                 "std_months_pre")


coefs <- as.data.frame(fig_motiv$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_motiv$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_motiv, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)


regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')
pval <- as.data.frame(round(fig_motiv$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_motiv$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_motiv$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))



regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$motivation_treatment=='none']))
                                        + hesitancy_post_rec))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "hesitancy_post_rec", 'coefplus'))
control$coefs <- c('none')
control$hesitancy_post_rec <- (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$motivation_treatment=='none'])))
control$coefplus <- (mean(na.omit(hesitant$hesitancy_post_rec[hesitant$motivation_treatment=='none'])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(motivation_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("none",
                            "1",
                            "2",
                            "3"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

regout$coefs <- factor(regout$coefs, 
                       levels = c('none', '1', '2', '3'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=motivation_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Vaccine willingness (strongly disagree - strongly agree)") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(3.15, 3.45)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Altruism", "Economic recovery", "Social approval")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text=element_text(size=13))

print(atebp)
pdf("Tables and Figures/Figure7_A.pdf")
print(atebp)
dev.off()

## Panel B

fig_motiv <- felm(hesitancy_dummy_post ~
                    factor(motivation_treatment) +
                    std_months_pre | factor(fixed_effects),
                  data = hesitant,
                  cmethod = 'reghdfe')


coefs <- as.data.frame(fig_motiv$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_motiv$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_motiv, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)


regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')
pval <- as.data.frame(round(fig_motiv$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_motiv$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_motiv$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))


regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$motivation_treatment=='none']))
                                        + hesitancy_dummy_post))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "hesitancy_dummy_post", 'coefplus'))
control$coefs <- c('none')
control$hesitancy_post_rec <- (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$motivation_treatment=='none'])))
control$coefplus <- (mean(na.omit(hesitant$hesitancy_dummy_post[hesitant$motivation_treatment=='none'])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(motivation_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("none",
                            "1",
                            "2",
                            "3"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

regout$coefs <- factor(regout$coefs, 
                       levels = c('none', '1', '2', '3'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=motivation_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Share willing to vaccinate") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(0.35, 0.5)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Altruism", "Economic recovery", "Social approval")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text=element_text(size=13))

print(atebp)

pdf("Tables and Figures/Figure7_B.pdf")
print(atebp)
dev.off()

## Panel C

fig_motiv <- felm(quickly_post_1_text_reversed2 ~
                    factor(motivation_treatment) +
                    std_months_pre | factor(fixed_effects),
                  data = hesitant,
                  cmethod = 'reghdfe')


coefs <- as.data.frame(fig_motiv$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_motiv$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_motiv, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)


regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')
pval <- as.data.frame(round(fig_motiv$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_motiv$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_motiv$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))


regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$motivation_treatment=='none']))
                                        + quickly_post_1_text_reversed2))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "quickly_post_1_text_reversed2", 'coefplus'))
control$coefs <- c('none')
control$quickly_post_1_text_reversed2 <- (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$motivation_treatment=='none'])))
control$coefplus <- (mean(na.omit(hesitant$quickly_post_1_text_reversed2[hesitant$motivation_treatment=='none'])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(motivation_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("none",
                            "1",
                            "2",
                            "3"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

regout$coefs <- factor(regout$coefs, 
                       levels = c('none', '1', '2', '3'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=motivation_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Months would wait to get vaccinated once eligible") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(5.5, 6.5)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_y_continuous(breaks = c(5.5, 5.75,  6, 6.25, 6.5),
                     labels = c("6.5", "6.25", "6", "6.25", "5.5")) +
  scale_fill_brewer(palette="Blues", labels = c("Control", "Altruism", "Economic recovery", "Social approval")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text=element_text(size=13))


print(atebp)

pdf("Tables and Figures/Figure7_C.pdf")
print(atebp)
dev.off()

## Panel D

fig_motiv <- felm(encourage2 ~
                    factor(motivation_treatment) +
                    std_months_pre | factor(fixed_effects),
                  data = hesitant,
                  cmethod = 'reghdfe')

coefs <- as.data.frame(fig_motiv$coefficients)
coefs <- coefs %>% mutate(coefs = coefficients)
rse <- as.data.frame(fig_motiv$rse)
rse <- rse %>% mutate(coefs = coefficients)
ci <- as.data.frame(confint(fig_motiv, level=0.95))
ci <- ci %>% mutate(coefs = coefficients)


regout <- merge(coefs, rse, by=c('coefs'))
colnames(regout)[3] <- c('rse')
regout <- merge(regout, ci, by=c('coefs'))
colnames(regout)[4:5] <- c('lower_ci', 'higher_ci')
regout <- regout %>% filter(coefs != 'std_months_pre')
pval <- as.data.frame(round(fig_motiv$rpval, 2))
pval <- pval %>% mutate(coefs = coefficients)
pval <- pval %>% mutate(pvaltext = ifelse(pval$`round(fig_motiv$rpval, 2)`<0.01, 'p<0.01', paste('p=', pval$`round(fig_motiv$rpval, 2)`)))
regout <- merge(regout, pval, by=c('coefs'))


regout <- regout %>% mutate(coefplus = (mean(na.omit(hesitant$encourage2[hesitant$motivation_treatment=='none']))
                                        + encourage2))

control <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("coefs", "encourage2", 'coefplus'))
control$coefs <- c('none')
control$encourage2 <- (mean(na.omit(hesitant$encourage2[hesitant$motivation_treatment=='none'])))
control$coefplus <- (mean(na.omit(hesitant$encourage2[hesitant$motivation_treatment=='none'])))

regout <- rbind.fill(regout, control)

n <- hesitant %>% group_by(motivation_treatment) %>% summarise(n = n())
n <- n %>% mutate(coefs = c("none",
                            "1",
                            "2",
                            "3"))

n <- n %>% mutate(ntext = paste0('n=', n))
regout <- merge(regout, n, by=c('coefs'))

regout$coefs <- factor(regout$coefs, 
                       levels = c('none', '1', '2', '3'))

atebp <- ggplot(data = regout, aes(x = coefs, y=coefplus, fill=motivation_treatment)) +
  geom_bar(position="stack", color = 'black', stat="identity") +
  geom_errorbar(aes(ymin = coefplus-abs(1.96*rse), ymax = coefplus+abs(1.96*rse)), width=0.2, size=0.71) +
  ylab("Share likely to encourage others to get vaccinated") +
  xlab("") +
  theme(legend.position="bottom", legend.title = element_blank()) +
  coord_cartesian(ylim=c(.5, .65)) +
  scale_x_discrete(labels=regout$ntext) + 
  scale_fill_brewer(palette="Blues", labels = c("Control", "Altruism", "Economic recovery", "Social approval")) +
  geom_text(aes(label=pvaltext, y=coefplus + (rse*1.96)), vjust=-1) +
  theme(axis.title = element_text(size=15), 
        legend.text = element_text(size = 15), 
        axis.text=element_text(size=13))

print(atebp)

pdf("Tables and Figures/Figure7_D.pdf")
print(atebp)
dev.off()

rm(list=setdiff(ls(), "hesitancy"))