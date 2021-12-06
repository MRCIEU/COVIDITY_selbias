###First version of code by AFS, 02/12/2020

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

COVID1_YP_raw <- read_delim("yp_outc_pred.csv", ",")
head(COVID1_YP_raw)
COVID1_YP <- COVID1_YP_raw



#########################################################################################################
##### 3. Data preparation
#########################################################################################################

COVID1_YP <- COVID1_YP %>%  
  mutate(yp_sex = as.factor(yp_sex)) %>%
  mutate(yp_ethnicity = as.factor(yp_ethnicity)) %>%
  mutate(yp_edu = as.factor(yp_edu)) %>%
  mutate(yp_urbanRural = as.factor(yp_urbanRural)) %>%
  mutate(yp_IMD = as.factor(yp_IMD)) %>%
  mutate(yp_everSmk = as.factor(yp_everSmk)) %>%
  mutate(yp_recent_Smk = as.factor(yp_recent_Smk)) %>%
  mutate(yp_alc_Abuse = as.factor(yp_alc_Abuse)) %>%
  mutate(yp_asthma = as.factor(yp_asthma)) %>%
  mutate(yp_cvascmetD_prepand= as.factor(yp_cvascmetD_prepand)) %>%
  mutate(yp_immuneD_prepand= as.factor(yp_immuneD_prepand)) %>%
  mutate(yp_mentalD_prepand= as.factor(yp_mentalD_prepand)) %>%
  mutate(had_covid1 = as.factor(had_covid1)) %>%
  mutate(had_covid2 = as.factor(had_covid2)) %>%
  mutate(covid_nonq1 = as.factor(covid_nonq1)) %>%
  mutate(noncovid_nonq1 = as.factor(noncovid_nonq1)) %>%
  mutate(covid_nonq2 = as.factor(covid_nonq2)) %>%
  mutate(noncovid_nonq2 = as.factor(noncovid_nonq2)) %>%
  mutate(sarscov2_data1 = as.factor(sarscov2_data1)) %>%
  mutate(sarscov2_data2 = as.factor(sarscov2_data2)) 

COVID1_YP <- COVID1_YP %>%  
  mutate(yp_age1_yrs = as.numeric(yp_age_yrs_COVID1)) %>%
  mutate(yp_age1_months = as.numeric(yp_age_months_COVID1)) %>%
  mutate(yp_age2_yrs = as.numeric(yp_age_yrs_COVID2)) %>%
  mutate(yp_age2_months = as.numeric(yp_age_months_COVID2)) %>%
  mutate(yp_bmi = as.numeric(yp_bmi)) %>%
  mutate(yp_sys_bp = as.numeric(yp_sys_bp)) %>%         
  mutate(yp_dia_bp = as.numeric(yp_dia_bp)) 

COVID1_YP <- COVID1_YP %>%  
  mutate(aln = as.character(aln)) 

# For univariable analyses tables:
var_list <- list("had_covid1", "had_covid2", "covid_nonq1", "covid_nonq2", "noncovid_nonq1", "noncovid_nonq2", "sarscov2_data1", "sarscov2_data2")
table_OR_names <- c("Outcome", "Variable", "coef", "N", "Beta", "SE", "Coefficient_OR", "Lower_CI", "Upper_CI", "pvalue")



#########################################################################################################
##### 4. Potential predictors of selection - distributions and logistic regression
#########################################################################################################

#########################################################################################################
#### Sex 

# Descriptive stats
table(COVID1_YP$yp_sex)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_sex) %>%
  gather(outcomeName, outcomeValue, had_covid1:noncovid_nonq2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_sex <- COVID1_YP_long %>%
  filter(!is.na(outcomeValue)) %>%
  group_by(outcomeName, yp_sex) %>%
  summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
            per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
            n_No = sum(outcomeValue == 0, na.rm = TRUE),
            per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
  mutate(n_total = sum(n), per_total = (n / n_total) * 100) %>%
  mutate(yp_sex = ifelse(yp_sex == 1, "Male (1)", "Female (2)")))

write.table(table_desc_sex, file = "yp/Descriptives_sex.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_sex <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_Sex", n = n(), nMiss = sum(is.na(yp_sex)), 
              perMiss = round((sum(is.na(yp_sex)) / n()) * 100, 2)))

write.table(table_missingness_sex, file = "yp/Missingness_sex.csv", row.names = FALSE, sep=";")

# Univariable logistic model
table_OR_sex <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_sex"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, n and p-values to the table
  outcome <- i[[1]]
  varName <- "Sex (ref = Male)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), #OR
                     round(exp(confint(model)[, 1]), 2), #CI
                     round(exp(confint(model)[, 2]), 2), #CI
                     round(coef(summary(model))[, 4], 3)) #pval
  
  colnames(temp) <- table_OR_names
  table_OR_sex <- rbind(table_OR_sex, temp)
  table_OR_sex <- table_OR_sex %>%
    filter(coef != "(Intercept)")
}

table_OR_sex

write.table(table_OR_sex, file = "yp/Univar_OR_sex.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Age (in months)

#Standardise to Z-scores
hist(COVID1_YP$yp_age_months_COVID1) 
hist(COVID1_YP$yp_age_months_COVID2)

COVID1_YP <- COVID1_YP %>%
  mutate(age1_z = (yp_age_months_COVID1 - mean(yp_age_months_COVID1, na.rm = TRUE)) / sd(yp_age_months_COVID1, na.rm = TRUE)) %>%
  mutate(age2_z = (yp_age_months_COVID2 - mean(yp_age_months_COVID2, na.rm = TRUE)) / sd(yp_age_months_COVID2, na.rm = TRUE))

hist(COVID1_YP$age1_z)
hist(COVID1_YP$age2_z)

# Descriptive stats
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_age_months_COVID1) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_age <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(yp_age_months_COVID1, na.rm = TRUE), 
              SD_Yes = sd(yp_age_months_COVID1, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_age, file = "yp/Descriptives_age.csv", row.names = FALSE, sep=";")

# Missingness
(table_missingness_age <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "age", n = n(), nMiss = sum(is.na(yp_age_months_COVID1)), 
              perMiss = round((sum(is.na(yp_age_months_COVID1)) / n()) * 100, 2)))

write.table(table_missingness_age, file = "yp/Missingness_age.csv", row.names = FALSE, sep=";")

# Univariable logistic model 

##Age at Q1 completion
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ age1_z"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "age (z-score: months)"
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

write.table(table_OR_age, file = "yp/Univar_OR_age1.csv", row.names = FALSE, sep=";")

##Age at Q2 completion
table_OR_age2 <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ age2_z"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "age (z-score: months)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_age2 <- rbind(table_OR_age2, temp)
  table_OR_age2 <- table_OR_age2 %>%
    filter(coef != "(Intercept)")
}

table_OR_age2

write.table(table_OR_age2, file = "yp/Univar_OR_age2.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Ethnicity

# Descriptive stats 
table(COVID1_YP$yp_ethnicity)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_ethnicity) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_ethnic <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_ethnicity)) %>%
    group_by(outcomeName, yp_ethnicity) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100) %>%
    mutate(yp_ethnicity = ifelse(yp_ethnicity == 1, "White (1)", "Non-White (2)")))

write.table(table_desc_ethnic, file = "yp/Descriptives_ethnicity.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_ethnic <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "ethnicity", n = n(), nMiss = sum(is.na(yp_ethnicity)), 
              perMiss = round((sum(is.na(yp_ethnicity)) / n()) * 100, 2)))

write.table(table_missingness_ethnic, file = "yp/Missingness_ethnicity.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_ethnic <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_ethnicity"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_ethnic, file = "yp/Univar_OR_ethnic.csv", row.names = FALSE, sep=";")


#########################################################################################################
#### Education (1 'GCSE or lower/none' 2 'Vocational' 3 'AS/A level' 4 'Degree or higher')

# Descriptive stats
table(COVID1_YP$yp_edu)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_edu) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_edu <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_edu)) %>%
    group_by(outcomeName, yp_edu) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
  mutate(n_total = sum(n), per_total = (n / n_total) * 100))
  
write.table(table_desc_edu, file = "yp/Descriptives_edu.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_edu <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "education", n = n(), nMiss = sum(is.na(yp_edu)), 
              perMiss = round((sum(is.na(yp_edu)) / n()) * 100, 2)))

write.table(table_missingness_edu, file = "yp/Missingness_edu.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_edu <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_edu"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Education (ref = GCSE/Lower)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_edu <- rbind(table_OR_edu, temp)
  table_OR_edu <- table_OR_edu %>%
    filter(coef != "(Intercept)")
}

table_OR_edu

write.table(table_OR_edu, file = "yp/Univar_OR_edu.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### BMI

#Standardise to Z-scores
hist(COVID1_YP$yp_bmi)
COVID1_YP <- COVID1_YP %>%
  mutate(bmi_z = (yp_bmi - mean(yp_bmi, na.rm = TRUE)) / sd(yp_bmi, na.rm = TRUE))
hist(COVID1_YP$bmi_z)

# Descriptive stats
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
        yp_bmi) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_bmi <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_bmi)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(yp_bmi, na.rm = TRUE), 
              SD_Yes = sd(yp_bmi, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_bmi, file = "yp/Descriptives_bmi.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_bmi <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "BMI", n = n(), nMiss = sum(is.na(yp_bmi)), 
              perMiss = round((sum(is.na(yp_bmi)) / n()) * 100, 2)))

write.table(table_missingness_bmi, file = "yp/Missingness_bmi.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_bmi <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_bmi, file = "yp/Univar_OR_bmi.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Systolic BP

#Standardise to Z-scores
hist(COVID1_YP$yp_sys_bp)
COVID1_YP <- COVID1_YP %>%
  mutate(sys_BP_z = (yp_sys_bp - mean(yp_sys_bp, na.rm = TRUE)) / sd(yp_sys_bp, na.rm = TRUE))
hist(COVID1_YP$sys_BP_z)

# Descriptive stats 
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_sys_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_sysBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_sys_bp)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(yp_sys_bp, na.rm = TRUE), 
              SD_Yes = sd(yp_sys_bp, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_sysBP, file = "yp/Descriptives_sysBP.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_sysBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Systolic BP", n = n(), nMiss = sum(is.na(yp_sys_bp)), 
              perMiss = round((sum(is.na(yp_sys_bp)) / n()) * 100, 2)))

write.table(table_missingness_sysBP, file = "yp/Missingness_sysBP.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_sysBP <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ sys_BP_z"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_sysBP, file = "yp/Univar_OR_sysBP.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Diastolic BP

#Standardise to Z-scores
hist(COVID1_YP$yp_dia_bp)
COVID1_YP <- COVID1_YP %>%
  mutate(dia_BP_z = (yp_dia_bp - mean(yp_dia_bp, na.rm = TRUE)) / sd(yp_dia_bp, na.rm = TRUE))
hist(COVID1_YP$dia_BP_z)

# Descriptive stats 
COVID1_YP_long <- COVID1_YP %>%
   select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_dia_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_diaBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_dia_bp)) %>%
    group_by(outcomeName, outcomeValue) %>%
    summarise(n = n(), mean = mean(yp_dia_bp, na.rm = TRUE), 
              SD_Yes = sd(yp_dia_bp, na.rm = TRUE)) %>%
    mutate(outcomeValue = ifelse(outcomeValue == 0, "No (0)", "Yes (1)")))

write.table(table_desc_diaBP, file = "yp/Descriptives_diaBP.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_diaBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Diastolic BP", n = n(), nMiss = sum(is.na(yp_dia_bp)), 
              perMiss = round((sum(is.na(yp_dia_bp)) / n()) * 100, 2)))

write.table(table_missingness_diaBP, file = "yp/Missingness_diaBP.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_diaBP <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ dia_BP_z"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_diaBP, file = "yp/Univar_OR_diaBP.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Urban/rural index 

# Combine into binary urban (1) vs other (2/3/4)
table(COVID1_YP$yp_urbanRural)
COVID1_YP <- COVID1_YP %>%
  mutate(yp_urbanRural = fct_recode(yp_urbanRural, "2" = "3", "2" = "4"))
table(COVID1_YP$yp_urbanRural)

# Descriptive stats 
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_urbanRural) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_urbanRural <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_urbanRural)) %>%
    group_by(outcomeName, yp_urbanRural) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100) %>%
    mutate(yp_urbanRural = ifelse(yp_urbanRural == 1, "Urban (1)", "Rural (2)")))

write.table(table_desc_urbanRural, file = "yp/Descriptives_urbanRural.csv", row.names = FALSE, sep=";")

# Missingness
(table_missingness_urbanRural <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Urban/Rural", n = n(), nMiss = sum(is.na(yp_urbanRural)), 
              perMiss = round((sum(is.na(yp_urbanRural)) / n()) * 100, 2)))

write.table(table_missingness_urbanRural, file = "yp/Missingness_urbanRural.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_urbanRural <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_urbanRural"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_urbanRural, file = "yp/Univar_OR_urbanRural.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### IMD

# Descriptive stats 
table(COVID1_YP$yp_IMD)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_IMD) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_IMD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_IMD)) %>%
    group_by(outcomeName, yp_IMD) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) %>%
    mutate(yp_IMD = fct_recode(yp_IMD, "1/Least dep." = "1", "5/Most dep." = "5"))

write.table(table_desc_IMD, file = "yp/Descriptives_IMD.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_IMD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "IMD", n = n(), nMiss = sum(is.na(yp_IMD)), 
              perMiss = round((sum(is.na(yp_IMD)) / n()) * 100, 2)))

write.table(table_missingness_IMD, file = "yp/Missingness_IMD.csv", row.names = FALSE, sep=";")

# Univariable logistic model - final results with a continuous IMD
COVID1_YP$yp_IMD = as.numeric(COVID1_YP$yp_IMD) 
summary(COVID1_YP$yp_IMD)

table_OR_IMD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_IMD"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_IMD, file = "yp/Univar_OR_IMD.csv", row.names = FALSE, sep=";")

#########################################################################################################
# Smoking (ever/never)

# Descriptive stats 
table(COVID1_YP$yp_everSmk)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_everSmk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_everSmk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_everSmk)) %>%
    group_by(outcomeName, yp_everSmk) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) %>%
  mutate(yp_everSmk = fct_recode(yp_everSmk, "No" = "0", "Yes" = "1"))

write.table(table_desc_everSmk, file = "yp/Descriptives_everSmk.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_everSmk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Ever smoked", n = n(), nMiss = sum(is.na(yp_everSmk)), 
              perMiss = round((sum(is.na(yp_everSmk)) / n()) * 100, 2)))

write.table(table_missingness_everSmk, file = "yp/Missingness_everSmk.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
levels(COVID1_YP$yp_everSmk)
COVID1_YP <- COVID1_YP %>%
  mutate(yp_everSmk = fct_recode(yp_everSmk, "No" = "0", "Yes" = "1"))
levels(COVID1_YP$yp_everSmk)

table_OR_everSmk <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_everSmk"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_everSmk, file = "yp/Univar_OR_everSmk.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Smoking - current, former, never

# Descriptive stats 
table(COVID1_YP$yp_recent_Smk)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_recent_Smk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
 mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_recent_Smk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_recent_Smk)) %>%
    group_by(outcomeName, yp_recent_Smk) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) 

write.table(table_desc_recent_Smk, file = "yp/Descriptives_recent_Smk.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_recent_Smk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Recent smoking", n = n(), nMiss = sum(is.na(yp_recent_Smk)), 
              perMiss = round((sum(is.na(yp_recent_Smk)) / n()) * 100, 2)))

write.table(table_missingness_recent_Smk, file = "yp/Missingness_recent_Smk.csv", row.names = FALSE, sep=";")

# Univariable logistic model
table_OR_recent_Smk <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_recent_Smk"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_recent_Smk, file = "yp/Univar_OR_recent_Smk.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Alcohol abuse 

# Combine 'mild' (1), 'moderate' (2) and 'severe' (3)
table(COVID1_YP$yp_alc_Abuse)
COVID1_YP <- COVID1_YP %>%
  mutate(yp_alc_Abuse = fct_recode(yp_alc_Abuse, "1" = "2", "1" = "3"))
table(COVID1_YP$yp_alc_Abuse)

# Descriptive stats 
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_alc_Abuse) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_alc <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_alc_Abuse)) %>%
    group_by(outcomeName, yp_alc_Abuse) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE), 
              per_Yes = mean(outcomeValue, na.rm = TRUE) * 100, 
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = 100 - mean(outcomeValue, na.rm = TRUE) * 100) %>%
    mutate(n_total = sum(n), per_total = (n / n_total) * 100)) %>%
  mutate(yp_alc_Abuse = fct_recode(yp_alc_Abuse, "No" = "0", "Mild/Mod/Severe" = "1"))

write.table(table_desc_alc, file = "yp/Descriptives_alc.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_alc <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Alcohol abuse", n = n(), nMiss = sum(is.na(yp_alc_Abuse)), 
              perMiss = round((sum(is.na(yp_alc_Abuse)) / n()) * 100, 2)))

write.table(table_missingness_alc, file = "yp/Missingness_alc.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
levels(COVID1_YP$yp_alc_Abuse)
COVID1_YP <- COVID1_YP %>%
  mutate(yp_alc_Abuse = fct_recode(yp_alc_Abuse, "No" = "0", "Mild/Mod/Severe" = "1"))
levels(COVID1_YP$yp_alc_Abuse)

table_OR_alc <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_alc_Abuse"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_alc, file = "yp/Univar_OR_alc.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Asthma 

# Descriptive stats 
table(COVID1_YP$yp_asthma)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_asthma) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_respD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_asthma)) %>%
    group_by(outcomeName, yp_asthma) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(yp_asthma = fct_recode(yp_asthma, "No" = "0", "Yes" = "1"))

write.table(table_desc_respD, file = "yp/Descriptives_asthma_prepand.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_respD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_asthma", n = n(), nMiss = sum(is.na(yp_asthma)),
              perMiss = round((sum(is.na(yp_asthma)) / n()) * 100, 2)))

write.table(table_missingness_respD, file = "yp/Missingness_asthma_prepand.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_respD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_asthma"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Pre-pandemic asthma (ref = none)"
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

write.table(table_OR_respD, file = "yp/Univar_OR_asthma_prepand.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Cardiometabolic comorbidities

# Descriptive stats 
table(COVID1_YP$yp_cvascmetD_prepand)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_cvascmetD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_cvascmetD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_cvascmetD_prepand)) %>%
    group_by(outcomeName, yp_cvascmetD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(yp_cvascmetD_prepand = fct_recode(yp_cvascmetD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_cvascmetD, file = "yp/Descriptives_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Missingness
(table_missingness_cvascmetD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_cvascmetD_prepand", n = n(), nMiss = sum(is.na(yp_cvascmetD_prepand)),
              perMiss = round((sum(is.na(yp_cvascmetD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cvascmetD, file = "yp/Missingness_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Univariable logistic model
table_OR_cvascmetD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_cvascmetD_prepand"), data = COVID1_YP, family = "binomial")
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
  table_OR_cvascmetD <- rbind(table_OR_cvascmetD, temp)
  table_OR_cvascmetD <- table_OR_cvascmetD %>%
    filter(coef != "(Intercept)")
}

table_OR_cvascmetD

write.table(table_OR_cvascmetD, file = "yp/Univar_OR_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Mental comorbidities

# Descriptive stats 
table(COVID1_YP$yp_mentalD_prepand)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_mentalD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_mentalD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_mentalD_prepand)) %>%
    group_by(outcomeName, yp_mentalD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(yp_mentalD_prepand = fct_recode(yp_mentalD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_mentalD, file = "yp/Descriptives_mentalD.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_mentalD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_mentalD_prepand", n = n(), nMiss = sum(is.na(yp_mentalD_prepand)),
              perMiss = round((sum(is.na(yp_mentalD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_mentalD, file = "yp/Missingness_mentalD.csv", row.names = FALSE, sep=";")

# Univariable logistic model 
table_OR_mentalD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)

  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_mentalD_prepand"), data = COVID1_YP, family = "binomial")
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

write.table(table_OR_mentalD, file = "yp/Univar_OR_mentalD.csv", row.names = FALSE, sep=";")

#########################################################################################################
#### Immune comorbidities

# Descriptive stats 
table(COVID1_YP$yp_immuneD_prepand)

COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1, had_covid2, sarscov2_data1, sarscov2_data2, covid_nonq1, noncovid_nonq1, covid_nonq2, noncovid_nonq2,
         yp_immuneD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid2", "sarscov2_data1", "sarscov2_data2", "covid_nonq1", "noncovid_nonq1", "covid_nonq2", "noncovid_nonq2")) %>%
  mutate(outcomeValue = as.numeric(outcomeValue))

head(COVID1_YP_long)
summary(COVID1_YP_long)

(table_desc_immuneD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue) & !is.na(yp_immuneD_prepand)) %>%
    group_by(outcomeName, yp_immuneD_prepand) %>%
    summarise(n = n(), n_Yes = sum(outcomeValue == 1, na.rm = TRUE),
              per_Yes = round(mean(outcomeValue, na.rm = TRUE) * 100, 2),
              n_No = sum(outcomeValue == 0, na.rm = TRUE),
              per_No = round(100 - mean(outcomeValue, na.rm = TRUE) * 100, 2)) %>%
    mutate(n_total = sum(n), per_total = round((n / n_total) * 100, 2))) %>%
  mutate(yp_immuneD_prepand = fct_recode(yp_immuneD_prepand, "No" = "0", "Yes" = "1"))

write.table(table_desc_immuneD, file = "yp/Descriptives_immuneD_prepand.csv", row.names = FALSE, sep=";")

# Missingness 
(table_missingness_immuneD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_immuneD_prepand", n = n(), nMiss = sum(is.na(yp_immuneD_prepand)),
              perMiss = round((sum(is.na(yp_immuneD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_immuneD, file = "yp/Missingness_immuneD_prepand.csv", row.names = FALSE, sep=";")

# Univariable logistic model
table_OR_immuneD <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ yp_immuneD_prepand"), data = COVID1_YP, family = "binomial")
  print(summary(model))
  
  # Add these coefficients, CIs, N and p-values to the table
  outcome <- i[[1]]
  varName <- "Pre-pandemic immune comorb (ref = none)"
  coef <- names(model$coefficients)
  temp <- data.frame(outcome, varName, coef, nobs(model),
                     summary(model)$coefficients[, 1],  #coefficient
                     summary(model)$coefficients[, 2],  #SE
                     round(exp(model$coefficients), 2),
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_immuneD <- rbind(table_OR_immuneD, temp)
  table_OR_immuneD <- table_OR_immuneD %>%
    filter(coef != "(Intercept)")
}

table_OR_immuneD

write.table(table_OR_immuneD, file = "yp/Univar_OR_immuneD_prepand.csv", row.names = FALSE, sep=";")

