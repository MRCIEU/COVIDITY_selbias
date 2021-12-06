###First version of code by AFS 02/12/2020

#########################################################################################################
##### 1. Create R environment
#########################################################################################################

rm(list=ls())

setwd("path/to/folder")

library(readr)
library(haven)
library(data.table)
library(tidyverse)
library(ResourceSelection)
library(pROC)
library(car)
library(mice)
library(compareGroups)
library(RColorBrewer)



#########################################################################################################
##### 2. Read in the datasets with candidate predictors and outcomes and make a copy
#########################################################################################################

COVID1_mum_raw <- read_delim("mum_outc_pred.csv", ",")
head(COVID1_mum_raw)
COVID1_mum <- COVID1_mum_raw



#########################################################################################################
##### 3. Data preparation
#########################################################################################################

COVID1_mum <- COVID1_mum %>%  
  mutate(g0mum_ethnicity = as.factor(g0mum_ethnicity)) %>%
  mutate(g0mum_edu = as.factor(g0mum_edu)) %>%
  mutate(g0mum_urbanRural = as.factor(g0mum_urbanRural)) %>%
  mutate(g0mum_IMD = as.factor(g0mum_IMD)) %>%
  mutate(g0mum_everSmk = as.factor(g0mum_everSmk)) %>%
  mutate(g0mum_recent_Smk = as.factor(g0mum_recent_Smk)) %>%
  mutate(g0mum_alc_audit_grp = as.factor(g0mum_alc_audit_grp)) %>%
  mutate(g0mum_asthma = as.factor(g0mum_asthma)) %>%
  mutate(g0mum_respD_prepand= as.factor(g0mum_respD_prepand)) %>%
  mutate(g0mum_cvascmetD_prepand= as.factor(g0mum_cvascmetD_prepand)) %>%
  mutate(g0mum_cancer_prepand= as.factor(g0mum_cancer_prepand)) %>%
  mutate(g0mum_mentalD_prepand= as.factor(g0mum_mentalD_prepand)) %>%
  mutate(had_covid1 = as.factor(had_covid1)) %>%
  mutate(had_covid2 = as.factor(had_covid2)) %>%
  mutate(covid_nonq1 = as.factor(covid_nonq1)) %>%
  mutate(noncovid_nonq1 = as.factor(noncovid_nonq1)) %>%
  mutate(covid_nonq2 = as.factor(covid_nonq2)) %>%
  mutate(noncovid_nonq2 = as.factor(noncovid_nonq2)) %>%
  mutate(sarscov2_data1 = as.factor(sarscov2_data1)) %>%
  mutate(sarscov2_data2 = as.factor(sarscov2_data2)) 

COVID1_mum <- COVID1_mum %>%  
  mutate(g0mum_age_COVID1 = as.numeric(g0mum_age_COVID1)) %>%
  mutate(g0mum_age_COVID2 = as.numeric(g0mum_age_COVID2)) %>%
  mutate(g0mum_bmi = as.numeric(g0mum_bmi)) %>%
  mutate(g0mum_sys_bp = as.numeric(g0mum_sys_bp)) %>%         
  mutate(g0mum_dia_bp = as.numeric(g0mum_dia_bp)) 

COVID1_mum <- COVID1_mum %>%  
  mutate(aln = as.character(aln)) 

# For univariable analyses tables
var_list <- list("had_covid1", "had_covid2", "covid_nonq1", "covid_nonq2", "noncovid_nonq1", "noncovid_nonq2", "sarscov2_data1", "sarscov2_data2")
table_OR_names <- c("Outcome", "Variable", "coef", "N", "Beta", "SE", "Coefficient_OR", "Lower_CI", "Upper_CI", "pvalue")



#########################################################################################################
##### 4. Potential predictors of selection - distributions and logistic regression
#########################################################################################################

#########################################################################################################
#### Age 

#Standardise to Z-scores
hist(COVID1_mum$g0mum_age_COVID1) 
hist(COVID1_mum$g0mum_age_COVID2) 
COVID1_mum <- COVID1_mum %>%
  mutate(age1_z = (g0mum_age_COVID1 - mean(g0mum_age_COVID1, na.rm = TRUE)) / sd(g0mum_age_COVID1, na.rm = TRUE)) %>%
  mutate(age2_z = (g0mum_age_COVID2 - mean(g0mum_age_COVID2, na.rm = TRUE)) / sd(g0mum_age_COVID2, na.rm = TRUE))
hist(COVID1_mum$age1_z)
hist(COVID1_mum$age2_z)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_age_COVID1) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_age <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(g0mum_age_COVID1, na.rm = TRUE), 
              SD_Yes = sd(g0mum_age_COVID1, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_age, file = "mum/Descriptives_age.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_age <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "age", n = n(), nMiss = sum(is.na(g0mum_age_COVID1)), 
              perMiss = round((sum(is.na(g0mum_age_COVID1)) / n()) * 100, 2)))

write.table(table_missingness_age, file = "mum/Missingness_age.csv", row.names = FALSE, sep=";")

# Univariable logistic model 

## Age at Q1 completion
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ age1_z"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "age (z-score: years)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_age <- rbind(table_OR_age, temp)
  table_OR_age <- table_OR_age %>%
    filter(coef != "(Intercept)")
}

table_OR_age

write.table(table_OR_age, file = "mum/Univar_OR_age1.csv", row.names = FALSE, sep=";")

## Age at Q2 completion
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ age2_z"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "age (z-score: years)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_age <- rbind(table_OR_age, temp)
  table_OR_age <- table_OR_age %>%
    filter(coef != "(Intercept)")
}

table_OR_age

write.table(table_OR_age, file = "mum/Univar_OR_age2.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Ethnicity

# Descriptive stats 
table(COVID1_mum$g0mum_ethnicity)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_ethnicity) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
   mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_ethnic <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_ethnicity)) %>%
    group_by(outcomeName, g0mum_ethnicity) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100) %>%
    mutate(g0mum_ethnicity = ifelse(g0mum_ethnicity == 1, "White (1)", "Non-White (2)")))

write.table(table_desc_ethnic, file = "mum/Descriptives_ethnicity.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_ethnic <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "ethnicity", n = n(), nMiss = sum(is.na(g0mum_ethnicity)), 
              perMiss = round((sum(is.na(g0mum_ethnicity)) / n()) * 100, 2)))

write.table(table_missingness_ethnic, file = "mum/Missingness_ethnicity.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_ethnic <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_ethnicity"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Ethnicity (ref = White)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_ethnic <- rbind(table_OR_ethnic, temp)
  table_OR_ethnic <- table_OR_ethnic %>%
    filter(coef != "(Intercept)")
}

table_OR_ethnic

write.table(table_OR_ethnic, file = "mum/Univar_OR_ethnic.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Education (1 'CSE/none' 2 'Vocational' 3 'O level' 4 'A level' 5 'Degree': recoded as 1-4 being 3 within 1

# Descriptive stats 
table(COVID1_mum$g0mum_edu)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_edu) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
 mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_edu <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_edu)) %>%
    group_by(outcomeName, g0mum_edu) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2), 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2)) %>%
    mutate(g0mum_edu = as.factor(g0mum_edu)) %>%
    mutate(g0mum_edu = fct_recode(g0mum_edu, "CSE/None" = "1", "Vocational" = "2", "O level" = "3",
                                  "A level" = "4", "Degree" = "5")))

write.csv(table_desc_edu, file = "./yp/DescriptivesByOutcome_edu.csv", row.names = FALSE)

# Missingness
(table_missingness_edu <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "education", n = n(), nMiss = sum(is.na(g0mum_edu)), 
              perMiss = round((sum(is.na(g0mum_edu)) / n()) * 100, 2),
              n_CSE = sum(g0mum_edu == 1, na.rm = TRUE), per_CSE = round((n_CSE / (n - nMiss)) * 100, 2),
              n_Voc = sum(g0mum_edu == 2, na.rm = TRUE), per_Voc = round((n_Voc / (n - nMiss)) * 100, 2),
              n_Olevel = sum(g0mum_edu == 3, na.rm = TRUE), per_Olevel = round((n_Olevel / (n - nMiss)) * 100, 2),
              n_Alevel = sum(g0mum_edu == 4, na.rm = TRUE), per_Alevel = round((n_Alevel / (n - nMiss)) * 100, 2),
              n_Deg = sum(g0mum_edu == 5, na.rm = TRUE), per_Deg = round((n_Deg / (n - nMiss)) * 100, 2)))

write.csv(table_missingness_edu, file = "./yp/MissAndDesc_edu.csv", row.names = FALSE)

# Univariable logistic regressions   
## Recode as YPs (1 'GCSE or lower/none' 2 'Vocational' 3 'AS/A level' 4 'Degree or higher') - O level (3) in the lower level
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_edu = fct_recode(g0mum_edu, "1" = "3", "3" = "4", "4" = "5"))
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_edu = as.factor(g0mum_edu)) %>%
  mutate(g0mum_edu = fct_recode(g0mum_edu, "GCSE/Lower" = "1", "Vocational" = "2", 
                                "AS/A level" = "3", "Degree" = "4"))
levels(COVID1_mum$g0mum_edu)

table_OR_edu <- data.frame(matrix(ncol=10, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_edu"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Education (ref = CSE/Lower)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient                      
                     summary(model)$coefficients[, 2],  #SE                      
                     round(exp(model$coefficients), 2),   #OR 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_edu <- rbind(table_OR_edu, temp)
  table_OR_edu <- table_OR_edu %>%
    filter(coef != "(Intercept)")
}

table_OR_edu

write.table(table_OR_edu, file = "mum/Univar_OR_edu.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### BMI

#Standardise to Z-scores
hist(COVID1_mum$g0mum_bmi)
COVID1_mum <- COVID1_mum %>%
  mutate(bmi_z = (g0mum_bmi - mean(g0mum_bmi, na.rm = TRUE)) / sd(g0mum_bmi, na.rm = TRUE))
hist(COVID1_mum$bmi_z)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_bmi) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_bmi <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_bmi)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(g0mum_bmi, na.rm = TRUE), 
              SD_Yes = sd(g0mum_bmi, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_bmi, file = "mum/Descriptives_bmi.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_bmi <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "BMI", n = n(), nMiss = sum(is.na(g0mum_bmi)), 
              perMiss = round((sum(is.na(g0mum_bmi)) / n()) * 100, 2)))

write.table(table_missingness_bmi, file = "mum/Missingness_bmi.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_bmi <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "BMI (z-score)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_bmi <- rbind(table_OR_bmi, temp)
  table_OR_bmi <- table_OR_bmi %>%
    filter(coef != "(Intercept)")
}

table_OR_bmi

write.table(table_OR_bmi, file = "mum/Univar_OR_bmi.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Systolic BP

#Standardise to Z-scores
hist(COVID1_mum$g0mum_sys_bp)
COVID1_mum <- COVID1_mum %>%
  mutate(sys_BP_z = (g0mum_sys_bp - mean(g0mum_sys_bp, na.rm = TRUE)) / sd(g0mum_sys_bp, na.rm = TRUE))
hist(COVID1_mum$sys_BP_z)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_sys_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_sysBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_sys_bp)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(g0mum_sys_bp, na.rm = TRUE), 
              SD_Yes = sd(g0mum_sys_bp, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_sysBP, file = "mum/Descriptives_sysBP.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_sysBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Systolic BP", n = n(), nMiss = sum(is.na(g0mum_sys_bp)), 
              perMiss = round((sum(is.na(g0mum_sys_bp)) / n()) * 100, 2)))

write.table(table_missingness_sysBP, file = "mum/Missingness_sysBP.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_sysBP <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ sys_BP_z"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Systolic BP (z-score: mmHg)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_sysBP <- rbind(table_OR_sysBP, temp)
  table_OR_sysBP <- table_OR_sysBP %>%
    filter(coef != "(Intercept)")
}

table_OR_sysBP

write.table(table_OR_sysBP, file = "mum/Univar_OR_sysBP.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Diastolic BP

#Standardise to Z-scores
hist(COVID1_mum$g0mum_dia_bp)
COVID1_mum <- COVID1_mum %>%
  mutate(dia_BP_z = (g0mum_dia_bp - mean(g0mum_dia_bp, na.rm = TRUE)) / sd(g0mum_dia_bp, na.rm = TRUE))
hist(COVID1_mum$dia_BP_z)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_dia_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
   mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_diaBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_dia_bp)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(g0mum_dia_bp, na.rm = TRUE), 
              SD_Yes = sd(g0mum_dia_bp, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_diaBP, file = "mum/Descriptives_diaBP.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_diaBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Diastolic BP", n = n(), nMiss = sum(is.na(g0mum_dia_bp)), 
              perMiss = round((sum(is.na(g0mum_dia_bp)) / n()) * 100, 2)))

write.table(table_missingness_diaBP, file = "mum/Missingness_diaBP.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_diaBP <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ dia_BP_z"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Diastolic BP (z-score: mmHg)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_diaBP <- rbind(table_OR_diaBP, temp)
  table_OR_diaBP <- table_OR_diaBP %>%
    filter(coef != "(Intercept)")
}

table_OR_diaBP

write.table(table_OR_diaBP, file = "mum/Univar_OR_diaBP.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Urban/rural index

# Combine into binary urban (1) vs other (2/3/4)
table(COVID1_mum$g0mum_urbanRural)
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_urbanRural = fct_recode(g0mum_urbanRural, "2" = "3", "2" = "4"))
table(COVID1_mum$g0mum_urbanRural)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_urbanRural) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
   mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_urbanRural <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_urbanRural)) %>%
    group_by(outcomeName, g0mum_urbanRural) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100) %>%
    mutate(g0mum_urbanRural = ifelse(g0mum_urbanRural == 1, "Urban (1)", "Rural (2)")))

write.table(table_desc_urbanRural, file = "mum/Descriptives_urbanRural.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_urbanRural <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Urban/Rural", n = n(), nMiss = sum(is.na(g0mum_urbanRural)), 
              perMiss = round((sum(is.na(g0mum_urbanRural)) / n()) * 100, 2)))

write.table(table_missingness_urbanRural, file = "mum/Missingness_urbanRural.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_urbanRural <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_urbanRural"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Urban/Rural (ref = Urban)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_urbanRural <- rbind(table_OR_urbanRural, temp)
  table_OR_urbanRural <- table_OR_urbanRural %>%
    filter(coef != "(Intercept)")
}

table_OR_urbanRural

write.table(table_OR_urbanRural, file = "mum/Univar_OR_urbanRural.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### IMD

# Descriptive stats 
table(COVID1_mum$g0mum_IMD)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_IMD) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_IMD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_IMD)) %>%
    group_by(outcomeName, g0mum_IMD) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) %>%
  mutate(g0mum_IMD = fct_recode(g0mum_IMD, "1/Least dep." = "1", "5/Most dep." = "5"))

write.table(table_desc_IMD, file = "mum/Descriptives_IMD.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_IMD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "IMD", n = n(), nMiss = sum(is.na(g0mum_IMD)), 
              perMiss = round((sum(is.na(g0mum_IMD)) / n()) * 100, 2)))

write.table(table_missingness_IMD, file = "mum/Missingness_IMD.csv", row.names = FALSE, sep=";")

# Univariable logistic model - final results with a continuous IMD 
COVID1_mum$g0mum_IMD = as.numeric(COVID1_mum$g0mum_IMD) 
summary(COVID1_mum$g0mum_IMD)

table_OR_IMD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_IMD"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "IMD (ref = 1/Least dep.)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_IMD <- rbind(table_OR_IMD, temp)
  table_OR_IMD <- table_OR_IMD %>%
    filter(coef != "(Intercept)")
}

table_OR_IMD

write.table(table_OR_IMD, file = "mum/Univar_OR_IMD.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Smoking (ever/never)

# Descriptive stats 
table(COVID1_mum$g0mum_everSmk)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
        g0mum_everSmk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%

  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_everSmk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_everSmk)) %>%
    group_by(outcomeName, g0mum_everSmk) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) %>%
  mutate(g0mum_everSmk = fct_recode(g0mum_everSmk, "No" = "0", "Yes" = "1"))

write.table(table_desc_everSmk, file = "mum/Descriptives_everSmk.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_everSmk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Ever smoked", n = n(), nMiss = sum(is.na(g0mum_everSmk)), 
              perMiss = round((sum(is.na(g0mum_everSmk)) / n()) * 100, 2)))

write.table(table_missingness_everSmk, file = "mum/Missingness_everSmk.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
levels(COVID1_mum$g0mum_everSmk)
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_everSmk = fct_recode(g0mum_everSmk, "No" = "0", "Yes" = "1"))
levels(COVID1_mum$g0mum_everSmk)

table_OR_everSmk <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_everSmk"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Ever smoked (ref = No)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_everSmk <- rbind(table_OR_everSmk, temp)
  table_OR_everSmk <- table_OR_everSmk %>%
    filter(coef != "(Intercept)")
}

table_OR_everSmk

write.table(table_OR_everSmk, file = "mum/Univar_OR_everSmk.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Smoking - current, former, never 

# Descriptive stats 
table(COVID1_mum$g0mum_recent_Smk)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_recent_Smk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
 mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_recent_Smk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_recent_Smk)) %>%
    group_by(outcomeName, g0mum_recent_Smk) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) 
write.table(table_desc_recent_Smk, file = "mum/Descriptives_recent_Smk.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_recent_Smk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Recent smoking", n = n(), nMiss = sum(is.na(g0mum_recent_Smk)), 
              perMiss = round((sum(is.na(g0mum_recent_Smk)) / n()) * 100, 2)))

write.table(table_missingness_recent_Smk, file = "mum/Missingness_recent_Smk.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_recent_Smk <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_recent_Smk"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Recent smoking (ref = No)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_recent_Smk <- rbind(table_OR_recent_Smk, temp)
  table_OR_recent_Smk <- table_OR_recent_Smk %>%
    filter(coef != "(Intercept)")
}

table_OR_recent_Smk

write.table(table_OR_recent_Smk, file = "mum/Univar_OR_recent_Smk.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Alcohol abuse 
# AUDIT (Alcohol Use Disorders Identification Test) scores from 0 to 40, derived variables categorized individuals into low risk (1: AUDIT score of 0 to 7), hazardous (2: 8 to 15), harmful (3: 16 to 19) and high risk (4: 20 to 40)

# Combine hazardous (2), harmful (3) and high risk (4)
table(COVID1_mum$g0mum_alc_audit_grp)
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_alc_audit_grp = fct_recode(g0mum_alc_audit_grp, "2" = "3", "2" = "4"))
table(COVID1_mum$g0mum_alc_audit_grp)

# Descriptive stats 
COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_alc_audit_grp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_alc <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_alc_audit_grp)) %>%
    group_by(outcomeName, g0mum_alc_audit_grp) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) 

write.table(table_desc_alc, file = "mum/Descriptives_alc.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_alc <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Alcohol abuse", n = n(), nMiss = sum(is.na(g0mum_alc_audit_grp)), 
              perMiss = round((sum(is.na(g0mum_alc_audit_grp)) / n()) * 100, 2)))

write.table(table_missingness_alc, file = "mum/Missingness_alc.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
levels(COVID1_mum$g0mum_alc_audit_grp)
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_alc_audit_grp = fct_recode(g0mum_alc_audit_grp, "Low risk" = "1", "Hazardous/harmful/high risk" = "2"))
levels(COVID1_mum$g0mum_alc_audit_grp)

table_OR_alc <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_alc_audit_grp"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Alcohol abuse (ref = No)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_alc <- rbind(table_OR_alc, temp)
  table_OR_alc <- table_OR_alc %>%
    filter(coef != "(Intercept)")
}

table_OR_alc

write.table(table_OR_alc, file = "mum/Univar_OR_alc.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Respiratory comorbidities 

# Descriptive stats 
table(COVID1_mum$g0mum_respD_prepand)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_respD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_respD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_respD_prepand)) %>%
    group_by(outcomeName, g0mum_respD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(g0mum_respD_prepand = fct_recode(g0mum_respD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_respD, file = "mum/Descriptives_respD_prepand.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_respD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_respD_prepand", n = n(), nMiss = sum(is.na(g0mum_respD_prepand)),
              perMiss = round((sum(is.na(g0mum_respD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_respD, file = "mum/Missingness_respD_prepand.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_respD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_respD_prepand"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Pre-pandemic respiratory comorb (ref = none)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2),
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_respD <- rbind(table_OR_respD, temp)
  table_OR_respD <- table_OR_respD %>%
    filter(coef != "(Intercept)")
}

table_OR_respD

write.table(table_OR_respD, file = "mum/Univar_OR_respD_prepand.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Cardiometabolic comorbidities

# Descriptive stats 
table(COVID1_mum$g0mum_cvascmetD_prepand)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_cvascmetD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_cvascmetD_prepand <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_cvascmetD_prepand)) %>%
    group_by(outcomeName, g0mum_cvascmetD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(g0mum_cvascmetD_prepand = fct_recode(g0mum_cvascmetD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_cvascmetD_prepand, file = "mum/Descriptives_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Missingness category 
(table_missingness_cvascmetD_prepand <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_cvascmetD_prepand", n = n(), nMiss = sum(is.na(g0mum_cvascmetD_prepand)),
              perMiss = round((sum(is.na(g0mum_cvascmetD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cvascmetD_prepand, file = "mum/Missingness_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_cvascmetD_prepand <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_cvascmetD_prepand"), data = COVID1_mum, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Pre-pandemic cardiometabolic comorb (ref = none)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2),
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_cvascmetD_prepand <- rbind(table_OR_cvascmetD_prepand, temp)
  table_OR_cvascmetD_prepand <- table_OR_cvascmetD_prepand %>%
    filter(coef != "(Intercept)")
}

table_OR_cvascmetD_prepand

write.table(table_OR_cvascmetD_prepand, file = "mum/Univar_OR_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Mental comorbidities

# Descriptive stats 
table(COVID1_mum$g0mum_mentalD_prepand)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_mentalD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_mentalD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_mentalD_prepand)) %>%
    group_by(outcomeName, g0mum_mentalD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(g0mum_mentalD_prepand = fct_recode(g0mum_mentalD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_mentalD, file = "mum/Descriptives_mentalD.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_mentalD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_mentalD_prepand", n = n(), nMiss = sum(is.na(g0mum_mentalD_prepand)),
              perMiss = round((sum(is.na(g0mum_mentalD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_mentalD, file = "mum/Missingness_mentalD.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_mentalD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)

  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_mentalD_prepand"), data = COVID1_mum, family = "binomial")
  print(summary(model))

  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Pre-pandemic mental comorb (ref = none)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2),
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))

  colnames(temp) <- table_OR_names
  table_OR_mentalD <- rbind(table_OR_mentalD, temp)
  table_OR_mentalD <- table_OR_mentalD %>%
    filter(coef != "(Intercept)")
}

table_OR_mentalD

write.table(table_OR_mentalD, file = "mum/Univar_OR_mentalD.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Cancer

# Descriptive stats 
table(COVID1_mum$g0mum_cancer_prepand)

COVID1_mum_long <- COVID1_mum %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         g0mum_cancer_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
   mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_mum_long)
summary(COVID1_mum_long)

(table_desc_cancer <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue) & !is.na(g0mum_cancer_prepand)) %>%
    group_by(outcomeName, g0mum_cancer_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(g0mum_cancer_prepand = fct_recode(g0mum_cancer_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_cancer, file = "mum/Descriptives_cancer.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_cancer <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_cancer_prepand", n = n(), nMiss = sum(is.na(g0mum_cancer_prepand)),
              perMiss = round((sum(is.na(g0mum_cancer_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cancer, file = "mum/Missingness_cancer.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_cancer <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)

  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ g0mum_cancer_prepand"), data = COVID1_mum, family = "binomial")
  print(summary(model))

  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Cancer (ref = none)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2),
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))

  colnames(temp) <- table_OR_names
  table_OR_cancer <- rbind(table_OR_cancer, temp)
  table_OR_cancer <- table_OR_cancer %>%
    filter(coef != "(Intercept)")
}

table_OR_cancer

write.table(table_OR_cancer, file = "mum/Univar_OR_cancer.csv", row.names = FALSE, sep=";")
