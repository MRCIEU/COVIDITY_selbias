###First code by AFS, 20/10-02/11/2020 



#########################################################################################################
## 1. Create R environment
#########################################################################################################

rm(list=ls())

setwd("/path/to/folder")

library(tidyverse)
library(haven)
library(ResourceSelection)
library(pROC)
library(car)
library(mice)
library(data.table)
library(compareGroups)
library(readr)
library(VennDiagram)
library(RColorBrewer)
library(ggforestplot)
library(ggpubr)



#########################################################################################################
## 2. Read in the dataset with all variables and make a copy 
#########################################################################################################

COVID1_YP_raw <- read_delim("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/081/working/data/COVIDITY/yp_outc_pred.csv", ",")
head(COVID1_YP_raw)
COVID1_YP <- COVID1_YP_raw



#########################################################################################################
## 3. Data preparation 
#########################################################################################################

COVID1_YP <- COVID1_YP %>%  
  mutate(yp_sex = as.factor(yp_sex)) %>%
  mutate(yp_edu = as.factor(yp_edu)) %>%
  mutate(yp_everSmk = as.factor(yp_everSmk)) %>%
  mutate(yp_recent_Smk = as.factor(yp_recent_Smk)) %>%
  mutate(had_covid1 = as.factor(had_covid1)) %>%
  mutate(had_covid1_controls2 = as.factor(had_covid1_controls2)) %>%
  mutate(had_covid_suspNo1 = as.factor(had_covid_suspNo1)) %>%
  mutate(had_covid_suspNo1_controls2 = as.factor(had_covid_suspNo1_controls2)) %>%
  mutate(menni_pred_q1 = as.factor(menni_pred_q1)) %>%
  mutate(menni_pred_q1_controls2 = as.factor(menni_pred_q1_controls2)) %>%
  mutate(had_covid2 = as.factor(had_covid2)) %>%
  mutate(had_covid2_controls2 = as.factor(had_covid2_controls2)) %>%
  mutate(had_covid_suspNo2 = as.factor(had_covid_suspNo2)) %>%
  mutate(had_covid_suspNo2_controls2 = as.factor(had_covid_suspNo2_controls2)) %>%
  mutate(menni_pred_q2 = as.factor(menni_pred_q2)) %>%
  mutate(menni_pred_q2_controls2 = as.factor(menni_pred_q2_controls2)) %>%
  mutate(covid_nonq1 = as.factor(covid_nonq1)) %>%
  mutate(noncovid_nonq1 = as.factor(noncovid_nonq1))  %>%
  mutate(sarscov2_data1 = as.factor(sarscov2_data1))    %>%
  mutate(covid_nonq2 = as.factor(covid_nonq2)) %>%
  mutate(noncovid_nonq2 = as.factor(noncovid_nonq2))  %>%
  mutate(sarscov2_data2 = as.factor(sarscov2_data2))  

COVID1_YP <- COVID1_YP %>%  
  mutate(yp_age1_yrs = as.numeric(yp_age_yrs_COVID1)) %>%
  mutate(yp_age1_months = as.numeric(yp_age_months_COVID1)) %>%
  mutate(yp_age2_yrs = as.numeric(yp_age_yrs_COVID2)) %>%
  mutate(yp_age2_months = as.numeric(yp_age_months_COVID2)) %>%
  mutate(yp_IMD = as.numeric(yp_IMD)) %>%
  mutate(yp_bmi = as.numeric(yp_bmi))  

# Convert age and BMI to Z-scores for the multivariable analyses
hist(COVID1_YP$yp_age_months_COVID1) #will use age in months from q1
COVID1_YP <- COVID1_YP %>%
  mutate(age_z = (yp_age_months_COVID1 - mean(yp_age_months_COVID1, na.rm = TRUE)) / sd(yp_age_months_COVID1, na.rm = TRUE)) %>%
  mutate(bmi_z = (yp_bmi - mean(yp_bmi, na.rm = TRUE)) / sd(yp_bmi, na.rm = TRUE))
hist(COVID1_YP$age_z)

# For regression analyses tables:
var_list <- list("had_covid1",  "had_covid2",  "had_covid1_controls2",  "had_covid2_controls2")
table_OR_names <- c("Outcome", "Variable", "coef", "N", "Beta", "SE", "Coefficient_OR", "Lower_CI", "Upper_CI", "pvalue")



#########################################################################################################
## 4. Palettes
#########################################################################################################

#palette for 2 samples
color <- c("darkcyan", "darkslateblue")

#palette for 2 samples and 2 models
color2 <- c("darkcyan", "darkslateblue", "darkcyan", "darkslateblue")



#########################################################################################################
## 5. All cohort
#########################################################################################################

# Model adjusted for age, sex, smoking, education and IMD

table_OR_bmi_yp <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z + yp_sex + age_z + yp_everSmk + yp_edu + yp_IMD"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, n and p-values to the table
  outcome <- i[[1]]
  varName <- "BMI (z-score)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],    #coefficient
                     summary(model)$coefficients[, 2],    #SE
                     round(exp(model$coefficients), 2),   #OR
                     round(exp(confint(model)[, 1]), 2),  #Lower CI
                     round(exp(confint(model)[, 2]), 2),  #Upper CI
                     round(coef(summary(model))[, 4], 3)) #pval
  
  colnames(temp) <- table_OR_names
  table_OR_bmi_yp <- rbind(table_OR_bmi_yp, temp)
  table_OR_bmi_yp <- table_OR_bmi_yp %>%
    filter(coef != "(Intercept)")
}

table_OR_bmi_yp

write.table(table_OR_bmi_yp, file = "yp/bmi/Model3_OR_bmi_alloutcomes.csv", row.names = FALSE, sep=";")



#########################################################################################################
## 6. Stratified analyses - women
#########################################################################################################

women <- filter(COVID1_YP, yp_sex==2)

# Model adjusted for age, smoking, BMI, education and IMD

table_OR_bmi_women <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z + age_z + yp_everSmk + yp_edu + yp_IMD"), data = women, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, n and p-values to the table
  outcome <- i[[1]]
  varName <- "BMI (z-score)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],    #coefficient
                     summary(model)$coefficients[, 2],    #SE
                     round(exp(model$coefficients), 2),   #OR
                     round(exp(confint(model)[, 1]), 2),  #Lower CI
                     round(exp(confint(model)[, 2]), 2),  #Upper CI
                     round(coef(summary(model))[, 4], 3)) #pval
  
  colnames(temp) <- table_OR_names
  table_OR_bmi_women <- rbind(table_OR_bmi_women, temp)
  table_OR_bmi_women <- table_OR_bmi_women %>%
    filter(coef != "(Intercept)")
}

table_OR_bmi_women

write.table(table_OR_bmi_women, file = "yp/bmi/Model3_OR_bmi_alloutcomes_women.csv", row.names = FALSE, sep=";")



#########################################################################################################
## 7. Stratified analyses - men
#########################################################################################################

men <- filter(COVID1_YP, yp_sex==1)

# Model adjusted for age, smoking, BMI, education and IMD
#########################################################################################################

table_OR_bmi_men <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z + age_z + yp_everSmk + yp_edu + yp_IMD"), data = men, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, n and p-values to the table
  outcome <- i[[1]]
  varName <- "BMI (z-score)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],    #coefficient
                     summary(model)$coefficients[, 2],    #SE
                     round(exp(model$coefficients), 2),   #OR
                     round(exp(confint(model)[, 1]), 2),  #Lower CI
                     round(exp(confint(model)[, 2]), 2),  #Upper CI
                     round(coef(summary(model))[, 4], 3)) #pval
  
  colnames(temp) <- table_OR_names
  table_OR_bmi_men <- rbind(table_OR_bmi_men, temp)
  table_OR_bmi_men <- table_OR_bmi_men %>%
    filter(coef != "(Intercept)")
}

table_OR_bmi_men

write.table(table_OR_bmi_men, file = "yp/bmi/Model3_OR_bmi_alloutcomes_men.csv", row.names = FALSE, sep=";")



#########################################################################################################
## 8. Forest plots
#########################################################################################################

#Create one dataset with the results of the non-stratified analysis and the sex-stratified analyses
table_OR_bmi_yp$cohort <- "G1 cohort"
table_OR_bmi_women$cohort <- "G1 females"
table_OR_bmi_men$cohort <- "G1 males"
model <- rbind(
  table_OR_bmi_yp,
  table_OR_bmi_women,
  table_OR_bmi_men)

#rename predictor and outcomes for the forest plot
model <- model[which(model$coef=="bmi_z"),]
model$coef <- dplyr::recode(model$coef, 
                            "bmi_z"="BMI (z-score)")



# Q1
model_q1 <- model[which(model$Outcome=="had_covid1_controls2" |
                          model$Outcome=="had_covid1" ),]

model_q1$Outcome <- dplyr::recode(model_q1$Outcome,
                                  "had_covid1"="SARS-CoV-2 (+) VS SARS-CoV-2 (-)",
                                  "had_covid1_controls2"="SARS-CoV-2 (+) VS everyone else") 

q1 <- ggforestplot::forestplot(df = model_q1,
                               name = cohort,
                               estimate = Beta,
                               se = SE,
                               pvalue = pvalue,
                               psignif = 0.05,
                               logodds = TRUE,
                               colour = Outcome,
                               shape = Outcome,
                               title = "Association between BMI and SARS-CoV-2 infection\nin ALSPAC G1 Cohort, Q1",
                               xlab = "OR for SARS-CoV-2 infection\n(95% CI) per 1-SD increment in BMI"
) +
  scale_color_manual(values=color2) + 
  annotation_logticks(sides="b") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14, face="italic"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.box.background = element_rect()
  )

# Q2
model_q2 <- model[which(model$Outcome=="had_covid2_controls2" |
                          model$Outcome=="had_covid2" ),]

model_q2$Outcome <- dplyr::recode(model_q2$Outcome,
                                  "had_covid2"="SARS-CoV-2 (+) VS SARS-CoV-2 (-)",
                                  "had_covid2_controls2"="SARS-CoV-2 (+) VS everyone else") 

q2 <- ggforestplot::forestplot(df = model_q2,
                               name = cohort,
                               estimate = Beta,
                               se = SE,
                               pvalue = pvalue,
                               psignif = 0.05,
                               logodds = TRUE,
                               colour = Outcome,
                               shape = Outcome,
                               title = "Association between BMI and SARS-CoV-2 infection\nin ALSPAC G1 Cohort, Q2",
                               xlab = "OR for SARS-CoV-2 infection\n(95% CI) per 1-SD increment in BMI"
) +
  scale_color_manual(values=color2) + 
  annotation_logticks(sides="b") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14, face="italic"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.box.background = element_rect()
  )

# Combine in one figure
q1 <- q1 + 
  theme(plot.title = element_blank())
q2 <- q2 + 
  theme(plot.title = element_blank())
(figure <- ggarrange(q1,
                     q2,
                     ncol = 2, 
                     align = "h",
                     hjust = -0.1,
                     vjust = 0.5,
                     widths = c(1, 1),
                     labels = c("A. Questionnaire 1","B. Questionnaire 2"),
                     font.label = list(size = 12, face = "bold.italic"),
                     common.legend = T, 
                     legend = "bottom")) 
(figure <- annotate_figure(figure,
                    top = text_grob("Association between BMI and SARS-CoV-2 infection\nALSPAC G1 Cohort\n\n", 
                                    face = "bold", 
                                    size = 14) )  %>%
    ggexport(.,
             filename = "yp/bmi/q1q2.png", 
             width = 1300, 
             height = 500))


