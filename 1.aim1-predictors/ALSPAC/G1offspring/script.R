###First version of code by AFS 02/12/2020

#########################################################################################################
##### 1. Create R environment
#########################################################################################################

rm(list=ls())

setwd("path/to/folder")

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

#########################################################################################################
##### 2. Read in the datasets with candidate predictors and outcomes and make a copy
#########################################################################################################

COVID1_YP_raw <- read_delim("yp_outc_pred.csv", ",")
head(COVID1_YP_raw)
COVID1_YP <- COVID1_YP_raw

COVID1_mum_raw <- read_delim("mum_outc_pred.csv", ",")
head(COVID1_mum_raw)
COVID1_mum <- COVID1_mum_raw

#########################################################################################################
##### 3. Prepare the variables 
#########################################################################################################

#########################################################################################################
#Categorical variables as factors and all variables with interpretable names

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
  mutate(yp_diabetes_prepand= as.factor(yp_diabetes_prepand)) %>%
  mutate(yp_htn_prepand= as.factor(yp_htn_prepand)) %>%
  mutate(yp_cvd_prepand= as.factor(yp_cvd_prepand)) %>%
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
  mutate(g0mum_diabetes_prepand= as.factor(g0mum_diabetes_prepand)) %>%
  mutate(g0mum_htn_prepand= as.factor(g0mum_htn_prepand)) %>%
  mutate(g0mum_cvd_prepand= as.factor(g0mum_cvd_prepand)) %>%
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

#########################################################################################################
# For univariate analyses tables:
var_list <- list("had_covid1", "had_covid2", 
                 "covid_nonq1","covid_nonq2",
                 "noncovid_nonq1","noncovid_nonq2",
                 "sarscov2_data1", "sarscov2_data2")
table_OR_names <- c("Outcome", "Variable", "coef", "N", "Beta", "SE", "Coefficient_OR", "Lower_CI", "Upper_CI", "pvalue")

#########################################################################################################
##### 4. Descriptive data 
#########################################################################################################

#########################################################################################################
#### G1 cohort


res_yp <- compareGroups( ~ sarscov2_data1+
                           sarscov2_data2 +
                           had_covid1 +
                           had_covid2 +
                           yp_age_yrs_COVID1 +
                           yp_sex +
                                    yp_ethnicity +
                                    yp_edu +
                                    yp_IMD +
                                    yp_urbanRural + 
                                    yp_alc_Abuse + 
                                    yp_everSmk +
                                    yp_recent_Smk + 
                                    yp_bmi + 
                                    yp_sys_bp + 
                                    yp_dia_bp +
                           yp_cvascmetD_prepand +
                           yp_asthma +
                           yp_immuneD_prepand +
                           yp_mentalD_prepand,
                                  data = COVID1_YP)
res_yp <- createTable(res_yp)
export2csv(res_yp, file="yp/whole_sample.csv", sep=";")

yp1 <- filter(COVID1_YP, sarscov2_data1==1)
res_yp1 <- compareGroups( ~                            had_covid1 +
                           had_covid2 +
                           yp_age_yrs_COVID1 +
                           yp_sex +
                           yp_ethnicity +
                           yp_edu +
                           yp_IMD +
                           yp_urbanRural + 
                           yp_alc_Abuse + 
                           yp_everSmk +
                           yp_recent_Smk + 
                           yp_bmi + 
                           yp_sys_bp + 
                           yp_dia_bp +
                           yp_cvascmetD_prepand +
                           yp_asthma +
                           yp_immuneD_prepand +
                           yp_mentalD_prepand,
                         data = yp1)
res_yp1 <- createTable(res_yp1)
export2csv(res_yp1, file="yp/subsample1.csv", sep=";")

yp2 <- filter(COVID1_YP, sarscov2_data2==1)
res_yp2 <- compareGroups( ~                            had_covid1 +
                           had_covid2 +
                           yp_age_yrs_COVID2 +
                           yp_sex +
                           yp_ethnicity +
                           yp_edu +
                           yp_IMD +
                           yp_urbanRural + 
                           yp_alc_Abuse + 
                           yp_everSmk +
                           yp_recent_Smk + 
                           yp_bmi + 
                           yp_sys_bp + 
                           yp_dia_bp +
                           yp_cvascmetD_prepand +
                           yp_asthma +
                           yp_immuneD_prepand +
                           yp_mentalD_prepand,
                         data = yp2)
res_yp2 <- createTable(res_yp2)
export2csv(res_yp2, file="yp/subsample2.csv", sep=";")


#Having COVID 
#########################################################################################################

yp_hadcovid <- filter(COVID1_YP, had_covid==1)
res_yp_hadcovid <- compareGroups( ~ yp_sex +
                                    yp_ethnicity +
                                    yp_edu +
                                    yp_IMD +
                                    yp_urbanRural + 
                                    yp_alc_Abuse + 
                                    yp_everSmk +
                                    yp_recent_Smk + 
                                    yp_age_yrs_COVID1 +
                                    yp_bmi + 
                                    yp_sys_bp + 
                                    yp_dia_bp ,
                                  data = yp_hadcovid)
res_yp_hadcovid <- createTable(res_yp_hadcovid)
export2csv(res_yp_hadcovid, file="yp/had_covid.csv", sep=";")

yp_hadcovid_no <- filter(COVID1_YP, had_covid==0)
res_yp_hadcovid_no <- compareGroups( ~ yp_sex +
                                    yp_ethnicity +
                                    yp_edu +
                                    yp_IMD +
                                    yp_urbanRural + 
                                    yp_alc_Abuse + 
                                    yp_everSmk +
                                    yp_recent_Smk + 
                                    yp_age_yrs_COVID1 +
                                    yp_bmi + 
                                    yp_sys_bp + 
                                    yp_dia_bp ,
                                  data = yp_hadcovid_no)
res_yp_hadcovid_no <- createTable(res_yp_hadcovid_no)
export2csv(res_yp_hadcovid_no, file="yp/had_covid_no.csv", sep=";")

yp_hadcovid_conf <- filter(COVID1_YP, had_covid_suspNo==1)
res_yp_hadcovid_conf <- compareGroups( ~ yp_sex +
                                         yp_ethnicity +
                                         yp_edu +
                                         yp_IMD +
                                         yp_urbanRural + 
                                         yp_alc_Abuse + 
                                         yp_everSmk +
                                         yp_recent_Smk + 
                                         yp_age_yrs_COVID1 +
                                         yp_bmi + 
                                         yp_sys_bp + 
                                         yp_dia_bp ,
                                       data = yp_hadcovid_conf)
res_yp_hadcovid_conf <- createTable(res_yp_hadcovid_conf)
export2csv(res_yp_hadcovid_conf, file="yp/had_covid_conf.csv", sep=";")

yp_hadcovid_conf_no <- filter(COVID1_YP, had_covid_suspNo==0)
res_yp_hadcovid_conf_no <- compareGroups( ~ yp_sex +
                                       yp_ethnicity +
                                       yp_edu +
                                       yp_IMD +
                                       yp_urbanRural + 
                                       yp_alc_Abuse + 
                                       yp_everSmk +
                                       yp_recent_Smk + 
                                       yp_age_yrs_COVID1 +
                                       yp_bmi + 
                                       yp_sys_bp + 
                                       yp_dia_bp ,
                                     data = yp_hadcovid_conf_no)
res_yp_hadcovid_conf_no <- createTable(res_yp_hadcovid_conf_no)
export2csv(res_yp_hadcovid_conf_no, file="yp/had_covid_conf_no.csv", sep=";")

yp_menni_pred <- filter(COVID1_YP, menni_pred==1)
res_yp_menni_pred <- compareGroups( ~ yp_sex +
                                      yp_ethnicity +
                                      yp_edu +
                                      yp_IMD +
                                      yp_urbanRural + 
                                      yp_alc_Abuse + 
                                      yp_everSmk +
                                      yp_recent_Smk + 
                                      yp_age_yrs_COVID1 +
                                      yp_bmi + 
                                      yp_sys_bp + 
                                      yp_dia_bp ,
                                    data = yp_menni_pred)
res_yp_menni_pred <- createTable(res_yp_menni_pred)
export2csv(res_yp_menni_pred, file="yp/had_covid_pred.csv", sep=";")

yp_menni_pred_no <- filter(COVID1_YP, menni_pred==0)
res_yp_menni_pred_no <- compareGroups( ~ yp_sex +
                                         yp_ethnicity +
                                         yp_edu +
                                         yp_IMD +
                                         yp_urbanRural + 
                                         yp_alc_Abuse + 
                                         yp_everSmk +
                                         yp_recent_Smk + 
                                         yp_age_yrs_COVID1 +
                                         yp_bmi + 
                                         yp_sys_bp + 
                                         yp_dia_bp ,
                                       data = yp_menni_pred_no)
res_yp_menni_pred_no <- createTable(res_yp_menni_pred_no)
export2csv(res_yp_menni_pred_no, file="yp/had_covid_pred_no.csv", sep=";")


#Having data on COVID 
#########################################################################################################

yp_data <- filter(COVID1_YP, sarscov2_data==1)
res_yp_data <- compareGroups( ~ yp_sex +
                                yp_ethnicity +
                                yp_edu +
                                yp_IMD +
                                yp_urbanRural + 
                                yp_alc_Abuse + 
                                yp_everSmk +
                                yp_recent_Smk + 
                                yp_age_yrs_COVID1 +
                                yp_bmi + 
                                yp_sys_bp + 
                                yp_dia_bp ,
                              data = yp_data)
res_yp_data <- createTable(res_yp_data)
export2csv(res_yp_data, file="yp/data.csv", sep=";")

yp_data_no <- filter(COVID1_YP, sarscov2_data==0)
res_yp_data_no <- compareGroups( ~ yp_sex +
                                   yp_ethnicity +
                                   yp_edu +
                                   yp_IMD +
                                   yp_urbanRural + 
                                   yp_alc_Abuse + 
                                   yp_everSmk +
                                   yp_recent_Smk + 
                                   yp_age_yrs_COVID1 +
                                   yp_bmi + 
                                   yp_sys_bp + 
                                   yp_dia_bp ,
                                 data = yp_data_no)
res_yp_data_no <- createTable(res_yp_data_no)
export2csv(res_yp_data_no, file="yp/data_no.csv", sep=";")

yp_data_conf <- filter(COVID1_YP, sarscov2_data_no_ownsusp==1)
res_yp_data_conf <- compareGroups( ~ yp_sex +
                                     yp_ethnicity +
                                     yp_edu +
                                     yp_IMD +
                                     yp_urbanRural + 
                                     yp_alc_Abuse + 
                                     yp_everSmk +
                                     yp_recent_Smk + 
                                     yp_age_yrs_COVID1 +
                                     yp_bmi + 
                                     yp_sys_bp + 
                                     yp_dia_bp ,
                                   data = yp_data_conf)
res_yp_data_conf <- createTable(res_yp_data_conf)
export2csv(res_yp_data_conf, file="yp/data_conf.csv", sep=";")

yp_data_conf_no <- filter(COVID1_YP, sarscov2_data_no_ownsusp==0)
res_yp_data_conf_no <- compareGroups( ~ yp_sex +
                                        yp_ethnicity +
                                        yp_edu +
                                        yp_IMD +
                                        yp_urbanRural + 
                                        yp_alc_Abuse + 
                                        yp_everSmk +
                                        yp_recent_Smk + 
                                        yp_age_yrs_COVID1 +
                                        yp_bmi + 
                                        yp_sys_bp + 
                                        yp_dia_bp ,
                                      data = yp_data_conf_no)
res_yp_data_conf_no <- createTable(res_yp_data_conf_no)
export2csv(res_yp_data_conf_no, file="yp/data_conf_no.csv", sep=";")

yp_data_pred <- filter(COVID1_YP, sarscov2_data_menni==1)
res_yp_menni_pred <- compareGroups( ~ yp_sex +
                                      yp_ethnicity +
                                      yp_edu +
                                      yp_IMD +
                                      yp_urbanRural + 
                                      yp_alc_Abuse + 
                                      yp_everSmk +
                                      yp_recent_Smk + 
                                      yp_age_yrs_COVID1 +
                                      yp_bmi + 
                                      yp_sys_bp + 
                                      yp_dia_bp ,
                                    data = yp_data_pred)
res_yp_menni_pred <- createTable(res_yp_menni_pred)
export2csv(res_yp_menni_pred, file="yp/data_pred.csv", sep=";")

yp_data_pred_no <- filter(COVID1_YP, sarscov2_data_menni==0)
res_yp_data_pred_no <- compareGroups( ~ yp_sex +
                                        yp_ethnicity +
                                        yp_edu +
                                        yp_IMD +
                                        yp_urbanRural + 
                                        yp_alc_Abuse + 
                                        yp_everSmk +
                                        yp_recent_Smk + 
                                        yp_age_yrs_COVID1 +
                                        yp_bmi + 
                                        yp_sys_bp + 
                                        yp_dia_bp ,
                                      data = yp_data_pred_no)
res_yp_data_pred_no <- createTable(res_yp_data_pred_no)
export2csv(res_yp_data_pred_no, file="yp/data_pred_no.csv", sep=";")


#Cases being positive / negative, controls non-assessed
#########################################################################################################

yp_yesnonq <- filter(COVID1_YP, covid_nonq==1)
res_yesnonq <- compareGroups( ~ yp_sex +
                                yp_ethnicity +
                                yp_edu +
                                yp_IMD +
                                yp_urbanRural + 
                                yp_alc_Abuse + 
                                yp_everSmk +
                                yp_recent_Smk + 
                                yp_age_yrs_COVID1 +
                                yp_bmi + 
                                yp_sys_bp + 
                                yp_dia_bp ,
                              data = yp_yesnonq)
res_yesnonq <- createTable(res_yesnonq)
export2csv(res_yesnonq, file="yp/positive_cases.csv", sep=";")

yp_yesnonq_no <- filter(COVID1_YP, covid_nonq==0)
res_yesnonq_no <- compareGroups( ~ yp_sex +
                                   yp_ethnicity +
                                   yp_edu +
                                   yp_IMD +
                                   yp_urbanRural + 
                                   yp_alc_Abuse + 
                                   yp_everSmk +
                                   yp_recent_Smk + 
                                   yp_age_yrs_COVID1 +
                                   yp_bmi + 
                                   yp_sys_bp + 
                                   yp_dia_bp ,
                                 data = yp_yesnonq_no)
res_yesnonq_no <- createTable(res_yesnonq_no)
export2csv(res_yesnonq_no, file="yp/positive_controls.csv", sep=";")


yp_nononq <- filter(COVID1_YP, noncovid_nonq==1)
res_nononq <- compareGroups( ~ yp_sex +
                               yp_ethnicity +
                               yp_edu +
                               yp_IMD +
                               yp_urbanRural + 
                               yp_alc_Abuse + 
                               yp_everSmk +
                               yp_recent_Smk + 
                               yp_age_yrs_COVID1 +
                               yp_bmi + 
                               yp_sys_bp + 
                               yp_dia_bp ,
                             data = yp_nononq)
res_nononq <- createTable(res_nononq)
export2csv(res_nononq, file="yp/negative_cases.csv", sep=";")

yp_nononq_no <- filter(COVID1_YP, noncovid_nonq==0)
res_nononq_no <- compareGroups( ~ yp_sex +
                                  yp_ethnicity +
                                  yp_edu +
                                  yp_IMD +
                                  yp_urbanRural + 
                                  yp_alc_Abuse + 
                                  yp_everSmk +
                                  yp_recent_Smk + 
                                  yp_age_yrs_COVID1 +
                                  yp_bmi + 
                                  yp_sys_bp + 
                                  yp_dia_bp ,
                                data = yp_nononq_no)
res_nononq_no <- createTable(res_nononq_no)
export2csv(res_nononq_no, file="yp/negative_controls.csv", sep=";")



#########################################################################################################
#### mum

res_mum <- compareGroups( ~ sarscov2_data1+
                           sarscov2_data2 +
                           had_covid1 +
                           had_covid2 +
                            g0mum_age_COVID1 +
                            g0mum_ethnicity +
                            g0mum_edu +
                            g0mum_IMD +
                            g0mum_urbanRural + 
                            g0mum_alc_audit_grp + 
                            g0mum_everSmk +
                            g0mum_recent_Smk + 
                            g0mum_bmi + 
                            g0mum_sys_bp + 
                            g0mum_dia_bp +
                            g0mum_cvascmetD_prepand +
                            g0mum_respD_prepand +
                            g0mum_cancer_prepand +
                            g0mum_mentalD_prepand,
                          data = COVID1_mum)
res_mum <- createTable(res_mum)
export2csv(res_mum, file="mum/whole_sample.csv", sep=";")

mum1 <- filter(COVID1_mum, sarscov2_data1==1)
res_mum1 <- compareGroups( ~                            had_covid1 +
                            had_covid2 +
                             g0mum_age_COVID1 +
                             g0mum_ethnicity +
                             g0mum_edu +
                             g0mum_IMD +
                             g0mum_urbanRural + 
                             g0mum_alc_audit_grp + 
                             g0mum_everSmk +
                             g0mum_recent_Smk + 
                             g0mum_bmi + 
                             g0mum_sys_bp + 
                             g0mum_dia_bp +
                             g0mum_cvascmetD_prepand +
                             g0mum_respD_prepand +
                             g0mum_cancer_prepand +
                             g0mum_mentalD_prepand,
                           data = mum1)
res_mum1 <- createTable(res_mum1)
export2csv(res_mum1, file="mum/subsample1.csv", sep=";")

mum2 <- filter(COVID1_mum, sarscov2_data2==1)
res_mum2 <- compareGroups( ~                            had_covid1 +
                            had_covid2 +
                             g0mum_age_COVID2 +
                             g0mum_ethnicity +
                             g0mum_edu +
                             g0mum_IMD +
                             g0mum_urbanRural + 
                             g0mum_alc_audit_grp + 
                             g0mum_everSmk +
                             g0mum_recent_Smk + 
                             g0mum_bmi + 
                             g0mum_sys_bp + 
                             g0mum_dia_bp +
                             g0mum_cvascmetD_prepand +
                             g0mum_respD_prepand +
                             g0mum_cancer_prepand +
                             g0mum_mentalD_prepand,
                          data = mum2)
res_mum2 <- createTable(res_mum2)
export2csv(res_mum2, file="mum/subsample2.csv", sep=";")



#Having COVID 
#########################################################################################################

mum_hadcovid <- filter(COVID1_mum, had_covid==1)
res_mum_hadcovid <- compareGroups( ~ g0mum_sex +
                                     g0mum_ethnicity +
                                     g0mum_edu +
                                     g0mum_IMD +
                                     g0mum_urbanRural + 
                                     g0mum_alc_audit_grp + 
                                     g0mum_everSmk +
                                     g0mum_recent_Smk + 
                                     g0mum_age_COVID1 +
                                     g0mum_bmi + 
                                     g0mum_sys_bp + 
                                     g0mum_dia_bp ,
                                   data = mum_hadcovid)
res_mum_hadcovid <- createTable(res_mum_hadcovid)
export2csv(res_mum_hadcovid, file="mum/had_covid.csv", sep=";")

mum_hadcovid_no <- filter(COVID1_mum, had_covid==0)
res_mum_hadcovid_no <- compareGroups( ~ g0mum_sex +
                                        g0mum_ethnicity +
                                        g0mum_edu +
                                        g0mum_IMD +
                                        g0mum_urbanRural + 
                                        g0mum_alc_audit_grp + 
                                        g0mum_everSmk +
                                        g0mum_recent_Smk + 
                                        g0mum_age_COVID1 +
                                        g0mum_bmi + 
                                        g0mum_sys_bp + 
                                        g0mum_dia_bp ,
                                      data = mum_hadcovid_no)
res_mum_hadcovid_no <- createTable(res_mum_hadcovid_no)
export2csv(res_mum_hadcovid_no, file="mum/had_covid_no.csv", sep=";")

mum_hadcovid_conf <- filter(COVID1_mum, had_covid_suspNo==1)
res_mum_hadcovid_conf <- compareGroups( ~ g0mum_sex +
                                          g0mum_ethnicity +
                                          g0mum_edu +
                                          g0mum_IMD +
                                          g0mum_urbanRural + 
                                          g0mum_alc_audit_grp + 
                                          g0mum_everSmk +
                                          g0mum_recent_Smk + 
                                          g0mum_age_COVID1 +
                                          g0mum_bmi + 
                                          g0mum_sys_bp + 
                                          g0mum_dia_bp ,
                                        data = mum_hadcovid_conf)
res_mum_hadcovid_conf <- createTable(res_mum_hadcovid_conf)
export2csv(res_mum_hadcovid_conf, file="mum/had_covid_conf.csv", sep=";")

mum_hadcovid_conf_no <- filter(COVID1_mum, had_covid_suspNo==0)
res_mum_hadcovid_conf_no <- compareGroups( ~ g0mum_sex +
                                             g0mum_ethnicity +
                                             g0mum_edu +
                                             g0mum_IMD +
                                             g0mum_urbanRural + 
                                             g0mum_alc_audit_grp + 
                                             g0mum_everSmk +
                                             g0mum_recent_Smk + 
                                             g0mum_age_COVID1 +
                                             g0mum_bmi + 
                                             g0mum_sys_bp + 
                                             g0mum_dia_bp ,
                                           data = mum_hadcovid_conf_no)
res_mum_hadcovid_conf_no <- createTable(res_mum_hadcovid_conf_no)
export2csv(res_mum_hadcovid_conf_no, file="mum/had_covid_conf_no.csv", sep=";")

mum_menni_pred <- filter(COVID1_mum, menni_pred==1)
res_mum_menni_pred <- compareGroups( ~ g0mum_sex +
                                       g0mum_ethnicity +
                                       g0mum_edu +
                                       g0mum_IMD +
                                       g0mum_urbanRural + 
                                       g0mum_alc_audit_grp + 
                                       g0mum_everSmk +
                                       g0mum_recent_Smk + 
                                       g0mum_age_COVID1 +
                                       g0mum_bmi + 
                                       g0mum_sys_bp + 
                                       g0mum_dia_bp ,
                                     data = mum_menni_pred)
res_mum_menni_pred <- createTable(res_mum_menni_pred)
export2csv(res_mum_menni_pred, file="mum/had_covid_pred.csv", sep=";")

mum_menni_pred_no <- filter(COVID1_mum, menni_pred==0)
res_mum_menni_pred_no <- compareGroups( ~ g0mum_sex +
                                          g0mum_ethnicity +
                                          g0mum_edu +
                                          g0mum_IMD +
                                          g0mum_urbanRural + 
                                          g0mum_alc_audit_grp + 
                                          g0mum_everSmk +
                                          g0mum_recent_Smk + 
                                          g0mum_age_COVID1 +
                                          g0mum_bmi + 
                                          g0mum_sys_bp + 
                                          g0mum_dia_bp ,
                                        data = mum_menni_pred_no)
res_mum_menni_pred_no <- createTable(res_mum_menni_pred_no)
export2csv(res_mum_menni_pred_no, file="mum/had_covid_pred_no.csv", sep=";")


#Having data on COVID 
#########################################################################################################

mum_data <- filter(COVID1_mum, sarscov2_data==1)
res_mum_data <- compareGroups( ~ g0mum_sex +
                                 g0mum_ethnicity +
                                 g0mum_edu +
                                 g0mum_IMD +
                                 g0mum_urbanRural + 
                                 g0mum_alc_audit_grp + 
                                 g0mum_everSmk +
                                 g0mum_recent_Smk + 
                                 g0mum_age_COVID1 +
                                 g0mum_bmi + 
                                 g0mum_sys_bp + 
                                 g0mum_dia_bp ,
                               data = mum_data)
res_mum_data <- createTable(res_mum_data)
export2csv(res_mum_data, file="mum/data.csv", sep=";")

mum_data_no <- filter(COVID1_mum, sarscov2_data==0)
res_mum_data_no <- compareGroups( ~ g0mum_sex +
                                    g0mum_ethnicity +
                                    g0mum_edu +
                                    g0mum_IMD +
                                    g0mum_urbanRural + 
                                    g0mum_alc_audit_grp + 
                                    g0mum_everSmk +
                                    g0mum_recent_Smk + 
                                    g0mum_age_COVID1 +
                                    g0mum_bmi + 
                                    g0mum_sys_bp + 
                                    g0mum_dia_bp ,
                                  data = mum_data_no)
res_mum_data_no <- createTable(res_mum_data_no)
export2csv(res_mum_data_no, file="mum/data_no.csv", sep=";")

mum_data_conf <- filter(COVID1_mum, sarscov2_data_no_ownsusp==1)
res_mum_data_conf <- compareGroups( ~ g0mum_sex +
                                      g0mum_ethnicity +
                                      g0mum_edu +
                                      g0mum_IMD +
                                      g0mum_urbanRural + 
                                      g0mum_alc_audit_grp + 
                                      g0mum_everSmk +
                                      g0mum_recent_Smk + 
                                      g0mum_age_COVID1 +
                                      g0mum_bmi + 
                                      g0mum_sys_bp + 
                                      g0mum_dia_bp ,
                                    data = mum_data_conf)
res_mum_data_conf <- createTable(res_mum_data_conf)
export2csv(res_mum_data_conf, file="mum/data_conf.csv", sep=";")

mum_data_conf_no <- filter(COVID1_mum, sarscov2_data_no_ownsusp==0)
res_mum_data_conf_no <- compareGroups( ~ g0mum_sex +
                                         g0mum_ethnicity +
                                         g0mum_edu +
                                         g0mum_IMD +
                                         g0mum_urbanRural + 
                                         g0mum_alc_audit_grp + 
                                         g0mum_everSmk +
                                         g0mum_recent_Smk + 
                                         g0mum_age_COVID1 +
                                         g0mum_bmi + 
                                         g0mum_sys_bp + 
                                         g0mum_dia_bp ,
                                       data = mum_data_conf_no)
res_mum_data_conf_no <- createTable(res_mum_data_conf_no)
export2csv(res_mum_data_conf_no, file="mum/data_conf_no.csv", sep=";")

mum_data_pred <- filter(COVID1_mum, sarscov2_data_menni==1)
res_mum_menni_pred <- compareGroups( ~ g0mum_sex +
                                       g0mum_ethnicity +
                                       g0mum_edu +
                                       g0mum_IMD +
                                       g0mum_urbanRural + 
                                       g0mum_alc_audit_grp + 
                                       g0mum_everSmk +
                                       g0mum_recent_Smk + 
                                       g0mum_age_COVID1 +
                                       g0mum_bmi + 
                                       g0mum_sys_bp + 
                                       g0mum_dia_bp ,
                                     data = mum_data_pred)
res_mum_menni_pred <- createTable(res_mum_menni_pred)
export2csv(res_mum_menni_pred, file="mum/data_pred.csv", sep=";")

mum_data_pred_no <- filter(COVID1_mum, sarscov2_data_menni==0)
res_mum_data_pred_no <- compareGroups( ~ g0mum_sex +
                                         g0mum_ethnicity +
                                         g0mum_edu +
                                         g0mum_IMD +
                                         g0mum_urbanRural + 
                                         g0mum_alc_audit_grp + 
                                         g0mum_everSmk +
                                         g0mum_recent_Smk + 
                                         g0mum_age_COVID1 +
                                         g0mum_bmi + 
                                         g0mum_sys_bp + 
                                         g0mum_dia_bp ,
                                       data = mum_data_pred_no)
res_mum_data_pred_no <- createTable(res_mum_data_pred_no)
export2csv(res_mum_data_pred_no, file="mum/data_pred_no.csv", sep=";")


#Cases being positive / negative, controls non-assessed
#########################################################################################################

mum_yesnonq <- filter(COVID1_mum, covid_nonq==1)
res_yesnonq <- compareGroups( ~ g0mum_sex +
                                g0mum_ethnicity +
                                g0mum_edu +
                                g0mum_IMD +
                                g0mum_urbanRural + 
                                g0mum_alc_audit_grp + 
                                g0mum_everSmk +
                                g0mum_recent_Smk + 
                                g0mum_age_COVID1 +
                                g0mum_bmi + 
                                g0mum_sys_bp + 
                                g0mum_dia_bp ,
                              data = mum_yesnonq)
res_yesnonq <- createTable(res_yesnonq)
export2csv(res_yesnonq, file="mum/positive_cases.csv", sep=";")

mum_yesnonq_no <- filter(COVID1_mum, covid_nonq==0)
res_yesnonq_no <- compareGroups( ~ g0mum_sex +
                                   g0mum_ethnicity +
                                   g0mum_edu +
                                   g0mum_IMD +
                                   g0mum_urbanRural + 
                                   g0mum_alc_audit_grp + 
                                   g0mum_everSmk +
                                   g0mum_recent_Smk + 
                                   g0mum_age_COVID1 +
                                   g0mum_bmi + 
                                   g0mum_sys_bp + 
                                   g0mum_dia_bp ,
                                 data = mum_yesnonq_no)
res_yesnonq_no <- createTable(res_yesnonq_no)
export2csv(res_yesnonq_no, file="mum/positive_controls.csv", sep=";")


mum_nononq <- filter(COVID1_mum, noncovid_nonq==1)
res_nononq <- compareGroups( ~ g0mum_sex +
                               g0mum_ethnicity +
                               g0mum_edu +
                               g0mum_IMD +
                               g0mum_urbanRural + 
                               g0mum_alc_audit_grp + 
                               g0mum_everSmk +
                               g0mum_recent_Smk + 
                               g0mum_age_COVID1 +
                               g0mum_bmi + 
                               g0mum_sys_bp + 
                               g0mum_dia_bp ,
                             data = mum_nononq)
res_nononq <- createTable(res_nononq)
export2csv(res_nononq, file="mum/negative_cases.csv", sep=";")

mum_nononq_no <- filter(COVID1_mum, noncovid_nonq==0)
res_nononq_no <- compareGroups( ~ g0mum_sex +
                                  g0mum_ethnicity +
                                  g0mum_edu +
                                  g0mum_IMD +
                                  g0mum_urbanRural + 
                                  g0mum_alc_audit_grp + 
                                  g0mum_everSmk +
                                  g0mum_recent_Smk + 
                                  g0mum_age_COVID1 +
                                  g0mum_bmi + 
                                  g0mum_sys_bp + 
                                  g0mum_dia_bp ,
                                data = mum_nononq_no)
res_nononq_no <- createTable(res_nononq_no)
export2csv(res_nononq_no, file="mum/negative_controls.csv", sep=";")





#########################################################################################################
####### 6b. Looking at descriptive data by all the predictors
#########################################################################################################

#########################################################################################################
#### Sex 

#Yp
#########################################################################################################
table(COVID1_YP$yp_sex)
# 1    2 
# 7579 7270

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(had_covid1,had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         covid_nonq1, noncovid_nonq1,
         covid_nonq2, noncovid_nonq2,
         
         yp_sex) %>%
  gather(outcomeName, outcomeValue, had_covid1:noncovid_nonq2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1", 
       "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
        
       "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
       "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
       
       "covid_nonq1", "noncovid_nonq1", 
       "covid_nonq2", "noncovid_nonq2"
       )) %>%
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


# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_sex <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_Sex", n = n(), nMiss = sum(is.na(yp_sex)), 
              perMiss = round((sum(is.na(yp_sex)) / n()) * 100, 2)))

write.table(table_missingness_sex, file = "yp/Missingness_sex.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_sex <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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
                     round(exp(model$coefficients), 2), 
                     round(exp(confint(model)[, 1]), 2),
                     round(exp(confint(model)[, 2]), 2),
                     round(coef(summary(model))[, 4], 3))
  
  colnames(temp) <- table_OR_names
  table_OR_sex <- rbind(table_OR_sex, temp)
  table_OR_sex <- table_OR_sex %>%
    filter(coef != "(Intercept)")
}

table_OR_sex

write.table(table_OR_sex, file = "yp/Univar_OR_sex.csv", row.names = FALSE, sep=";")





#########################################################################################################
#### Age - As continuous, will standardise to Z-scores to make more interpretable and comparable between measures

#YP
#########################################################################################################
hist(COVID1_YP$yp_age_months_COVID1) #will use age in months from q1
hist(COVID1_YP$yp_age_months_COVID2) #will use age in months from q2

COVID1_YP <- COVID1_YP %>%
  mutate(age1_z = (yp_age_months_COVID1 - mean(yp_age_months_COVID1, na.rm = TRUE)) / sd(yp_age_months_COVID1, na.rm = TRUE))
COVID1_YP <- COVID1_YP %>%
  mutate(age2_z = (yp_age_months_COVID2 - mean(yp_age_months_COVID2, na.rm = TRUE)) / sd(yp_age_months_COVID2, na.rm = TRUE))

hist(COVID1_YP$age1_z)
hist(COVID1_YP$age2_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,
         covid_nonq2, noncovid_nonq2,
         had_covid1,had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         yp_age_months_COVID1) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of age for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_age <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "age", n = n(), nMiss = sum(is.na(yp_age_months_COVID1)), 
              perMiss = round((sum(is.na(yp_age_months_COVID1)) / n()) * 100, 2)))

write.table(table_missingness_age, file = "yp/Missingness_age.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_age2 <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
hist(COVID1_mum$g0mum_age_COVID1) #will use age in years from q1
hist(COVID1_mum$g0mum_age_COVID2) #will use age in years from q2

#Mum's age is in years - Stantardized as all continuous vars
COVID1_mum <- COVID1_mum %>%
  mutate(age1_z = (g0mum_age_COVID1 - mean(g0mum_age_COVID1, na.rm = TRUE)) / sd(g0mum_age_COVID1, na.rm = TRUE))
COVID1_mum <- COVID1_mum %>%
  mutate(age2_z = (g0mum_age_COVID2 - mean(g0mum_age_COVID2, na.rm = TRUE)) / sd(g0mum_age_COVID2, na.rm = TRUE))

hist(COVID1_mum$age1_z)
hist(COVID1_mum$age2_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_age_COVID1) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of age for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_age <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "age", n = n(), nMiss = sum(is.na(g0mum_age_COVID1)), 
              perMiss = round((sum(is.na(g0mum_age_COVID1)) / n()) * 100, 2)))

write.table(table_missingness_age, file = "mum/Missingness_age.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_age <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
table(COVID1_YP$yp_ethnicity)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_ethnicity) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_ethnic <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "ethnicity", n = n(), nMiss = sum(is.na(yp_ethnicity)), 
              perMiss = round((sum(is.na(yp_ethnicity)) / n()) * 100, 2)))

write.table(table_missingness_ethnic, file = "yp/Missingness_ethnicity.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_ethnic <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################

table(COVID1_mum$g0mum_ethnicity)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_ethnicity) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_ethnic <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "ethnicity", n = n(), nMiss = sum(is.na(g0mum_ethnicity)), 
              perMiss = round((sum(is.na(g0mum_ethnicity)) / n()) * 100, 2)))

write.table(table_missingness_ethnic, file = "mum/Missingness_ethnicity.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_ethnic <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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
#### Education
#YPs have 1 'GCSE or lower/none' 2 'Vocational' 3 'AS/A level' 4 'Degree or higher'
#G0s have 1 'CSE/none' 2 'Vocational' 3 'O level' 4 'A level' 5 'Degree': recoded as 1-4 being 3 within 1

# YP
#########################################################################################################
table(COVID1_YP$yp_edu)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_edu) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_edu <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "education", n = n(), nMiss = sum(is.na(yp_edu)), 
              perMiss = round((sum(is.na(yp_edu)) / n()) * 100, 2)))

write.table(table_missingness_edu, file = "yp/Missingness_edu.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_edu <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



# mum 
#########################################################################################################

table(COVID1_mum$g0mum_edu)

COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_edu) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2")) %>%
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



# Also want to make a summary of education for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q) - Plus some combined descriptive stats for this outcome
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



######################################################################################################### 
#### Univariate logistic regressions   

# Update factor levels
table(COVID1_mum$g0mum_edu)

#### Education - DS from pre-pandemic data, but recode as YPs 
#1 'CSE/none' 2 'Vocational' 3 'O level' 4 'A level' 5 'Degree'
#recode as YPs: 1 'GCSE or lower/none' 2 'Vocational' 3 'AS/A level' 4 'Degree or higher'
COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_edu = fct_recode(g0mum_edu, "1" = "3", "3" = "4", "4" = "5"))

COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_edu = as.factor(g0mum_edu)) %>%
  mutate(g0mum_edu = fct_recode(g0mum_edu, "GCSE/Lower" = "1", "Vocational" = "2", 
                                "AS/A level" = "3", "Degree" = "4"))
levels(COVID1_mum$g0mum_edu)


# Make a table to append the OR results to
table_OR_edu <- data.frame(matrix(ncol=10, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
hist(COVID1_YP$yp_bmi)

# Convert to Z-scores
COVID1_YP <- COVID1_YP %>%
  mutate(bmi_z = (yp_bmi - mean(yp_bmi, na.rm = TRUE)) / sd(yp_bmi, na.rm = TRUE))

hist(COVID1_YP$bmi_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_bmi) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_bmi <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "BMI", n = n(), nMiss = sum(is.na(yp_bmi)), 
              perMiss = round((sum(is.na(yp_bmi)) / n()) * 100, 2)))

write.table(table_missingness_bmi, file = "yp/Missingness_bmi.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_bmi <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
hist(COVID1_mum$g0mum_bmi)

# Convert to Z-scores
COVID1_mum <- COVID1_mum %>%
  mutate(bmi_z = (g0mum_bmi - mean(g0mum_bmi, na.rm = TRUE)) / sd(g0mum_bmi, na.rm = TRUE))

hist(COVID1_mum$bmi_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_bmi) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_bmi <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "BMI", n = n(), nMiss = sum(is.na(g0mum_bmi)), 
              perMiss = round((sum(is.na(g0mum_bmi)) / n()) * 100, 2)))

write.table(table_missingness_bmi, file = "mum/Missingness_bmi.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_bmi <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
hist(COVID1_YP$yp_sys_bp)

# Convert to Z-scores
COVID1_YP <- COVID1_YP %>%
  mutate(sys_BP_z = (yp_sys_bp - mean(yp_sys_bp, na.rm = TRUE)) / sd(yp_sys_bp, na.rm = TRUE))

hist(COVID1_YP$sys_BP_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_sys_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_sysBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Systolic BP", n = n(), nMiss = sum(is.na(yp_sys_bp)), 
              perMiss = round((sum(is.na(yp_sys_bp)) / n()) * 100, 2)))

write.table(table_missingness_sysBP, file = "yp/Missingness_sysBP.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_sysBP <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
hist(COVID1_mum$g0mum_sys_bp)

# Convert to Z-scores
COVID1_mum <- COVID1_mum %>%
  mutate(sys_BP_z = (g0mum_sys_bp - mean(g0mum_sys_bp, na.rm = TRUE)) / sd(g0mum_sys_bp, na.rm = TRUE))

hist(COVID1_mum$sys_BP_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_sys_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_sysBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Systolic BP", n = n(), nMiss = sum(is.na(g0mum_sys_bp)), 
              perMiss = round((sum(is.na(g0mum_sys_bp)) / n()) * 100, 2)))

write.table(table_missingness_sysBP, file = "mum/Missingness_sysBP.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_sysBP <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
hist(COVID1_YP$yp_dia_bp)

# Convert to Z-scores
COVID1_YP <- COVID1_YP %>%
  mutate(dia_BP_z = (yp_dia_bp - mean(yp_dia_bp, na.rm = TRUE)) / sd(yp_dia_bp, na.rm = TRUE))

hist(COVID1_YP$dia_BP_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_dia_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_diaBP <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Diastolic BP", n = n(), nMiss = sum(is.na(yp_dia_bp)), 
              perMiss = round((sum(is.na(yp_dia_bp)) / n()) * 100, 2)))

write.table(table_missingness_diaBP, file = "yp/Missingness_diaBP.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_diaBP <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
hist(COVID1_mum$g0mum_dia_bp)

# Convert to Z-scores
COVID1_mum <- COVID1_mum %>%
  mutate(dia_BP_z = (g0mum_dia_bp - mean(g0mum_dia_bp, na.rm = TRUE)) / sd(g0mum_dia_bp, na.rm = TRUE))

hist(COVID1_mum$dia_BP_z)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_dia_bp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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


# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_diaBP <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Diastolic BP", n = n(), nMiss = sum(is.na(g0mum_dia_bp)), 
              perMiss = round((sum(is.na(g0mum_dia_bp)) / n()) * 100, 2)))

write.table(table_missingness_diaBP, file = "mum/Missingness_diaBP.csv", row.names = FALSE, sep=";")


# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_diaBP <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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
#### Urban/rural status - combine into binary urban vs other

#YP
#########################################################################################################
table(COVID1_YP$yp_urbanRural)

COVID1_YP <- COVID1_YP %>%
  mutate(yp_urbanRural = fct_recode(yp_urbanRural, "2" = "3", "2" = "4"))

table(COVID1_YP$yp_urbanRural)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_urbanRural) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_urbanRural <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Urban/Rural", n = n(), nMiss = sum(is.na(yp_urbanRural)), 
              perMiss = round((sum(is.na(yp_urbanRural)) / n()) * 100, 2)))

write.table(table_missingness_urbanRural, file = "yp/Missingness_urbanRural.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_urbanRural <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
table(COVID1_mum$g0mum_urbanRural)

COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_urbanRural = fct_recode(g0mum_urbanRural, "2" = "3", "2" = "4"))

table(COVID1_mum$g0mum_urbanRural)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         
         covid_nonq2, noncovid_nonq2,   
         had_covid1, had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         g0mum_urbanRural) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2")) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_urbanRural <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Urban/Rural", n = n(), nMiss = sum(is.na(g0mum_urbanRural)), 
              perMiss = round((sum(is.na(g0mum_urbanRural)) / n()) * 100, 2)))

write.table(table_missingness_urbanRural, file = "mum/Missingness_urbanRural.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_urbanRural <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
table(COVID1_YP$yp_IMD)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_IMD) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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


# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_IMD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "IMD", n = n(), nMiss = sum(is.na(yp_IMD)), 
              perMiss = round((sum(is.na(yp_IMD)) / n()) * 100, 2)))

write.table(table_missingness_IMD, file = "yp/Missingness_IMD.csv", row.names = FALSE, sep=";")


# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_IMD <- data.frame(matrix(ncol=8, nrow=0))

COVID1_YP$yp_IMD = as.numeric(COVID1_YP$yp_IMD) 
summary(COVID1_YP$yp_IMD)

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################
table(COVID1_mum$g0mum_IMD)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_IMD) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_IMD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "IMD", n = n(), nMiss = sum(is.na(g0mum_IMD)), 
              perMiss = round((sum(is.na(g0mum_IMD)) / n()) * 100, 2)))

write.table(table_missingness_IMD, file = "mum/Missingness_IMD.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_IMD <- data.frame(matrix(ncol=8, nrow=0))

COVID1_mum$g0mum_IMD = as.numeric(COVID1_mum$g0mum_IMD) 
summary(COVID1_mum$g0mum_IMD)


# Now loop through all the variables add the results to the tables
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
# Smoking

#### YP has ever smoked
#########################################################################################################
table(COVID1_YP$yp_everSmk)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_everSmk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_everSmk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Ever smoked", n = n(), nMiss = sum(is.na(yp_everSmk)), 
              perMiss = round((sum(is.na(yp_everSmk)) / n()) * 100, 2)))

write.table(table_missingness_everSmk, file = "yp/Missingness_everSmk.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Update factor names
levels(COVID1_YP$yp_everSmk)

COVID1_YP <- COVID1_YP %>%
  mutate(yp_everSmk = fct_recode(yp_everSmk, "No" = "0", "Yes" = "1"))
levels(COVID1_YP$yp_everSmk)

# Make a table to append the OR results to
table_OR_everSmk <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#### mum has ever smoked 
#########################################################################################################
table(COVID1_mum$g0mum_everSmk)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_everSmk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_everSmk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Ever smoked", n = n(), nMiss = sum(is.na(g0mum_everSmk)), 
              perMiss = round((sum(is.na(g0mum_everSmk)) / n()) * 100, 2)))

write.table(table_missingness_everSmk, file = "mum/Missingness_everSmk.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Update factor names
levels(COVID1_mum$g0mum_everSmk)

COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_everSmk = fct_recode(g0mum_everSmk, "No" = "0", "Yes" = "1"))
levels(COVID1_mum$g0mum_everSmk)

# Make a table to append the OR results to
table_OR_everSmk <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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





#### YP recent smoking - current, former, never
#########################################################################################################
table(COVID1_YP$yp_recent_Smk)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_recent_Smk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_recent_Smk <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Recent smoking", n = n(), nMiss = sum(is.na(yp_recent_Smk)), 
              perMiss = round((sum(is.na(yp_recent_Smk)) / n()) * 100, 2)))

write.table(table_missingness_recent_Smk, file = "yp/Missingness_recent_Smk.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
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



#### mum recent smoking - current, former, never 
#########################################################################################################
table(COVID1_mum$g0mum_recent_Smk)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_recent_Smk) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_recent_Smk <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Recent smoking", n = n(), nMiss = sum(is.na(g0mum_recent_Smk)), 
              perMiss = round((sum(is.na(g0mum_recent_Smk)) / n()) * 100, 2)))

write.table(table_missingness_recent_Smk, file = "mum/Missingness_recent_Smk.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_recent_Smk <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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
#### alcohol abuse 

# AUDIT (Alcohol Use Disorders Identification Test) scores from 0 to 40, 
# derived variables categorized individuals into low risk (1: AUDIT score of 0 to 7), hazardous (2: 8 to 15), harmful (3: 16 to 19) and high risk (4: 20 to 40)

#### YP has alcohol abuse diagnosis - Combine 'mild', 'moderate' and 'severe'
#########################################################################################################
# would use AUDIT score, but could not be calculated at F@24 clinic, so will use 'DSM5 alcohol use disorder symptoms' instead ***
# YPB4389f FKAL1450
table(COVID1_YP$yp_alc_Abuse)

COVID1_YP <- COVID1_YP %>%
  mutate(yp_alc_Abuse = fct_recode(yp_alc_Abuse, "1" = "2", "1" = "3"))

table(COVID1_YP$yp_alc_Abuse)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         yp_alc_Abuse) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_alc <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Alcohol abuse", n = n(), nMiss = sum(is.na(yp_alc_Abuse)), 
              perMiss = round((sum(is.na(yp_alc_Abuse)) / n()) * 100, 2)))

write.table(table_missingness_alc, file = "yp/Missingness_alc.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Update factor names
levels(COVID1_YP$yp_alc_Abuse)

COVID1_YP <- COVID1_YP %>%
  mutate(yp_alc_Abuse = fct_recode(yp_alc_Abuse, "No" = "0", "Mild/Mod/Severe" = "1"))
levels(COVID1_YP$yp_alc_Abuse)

# Make a table to append the OR results to
table_OR_alc <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#### mum - Combine hazardous, harmful, high risk
#########################################################################################################
table(COVID1_mum$g0mum_alc_audit_grp)

COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_alc_audit_grp = fct_recode(g0mum_alc_audit_grp, "2" = "3", "2" = "4"))

table(COVID1_mum$g0mum_alc_audit_grp)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
         
         g0mum_alc_audit_grp) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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


# Also want to make a summary for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_alc <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "Alcohol abuse", n = n(), nMiss = sum(is.na(g0mum_alc_audit_grp)), 
              perMiss = round((sum(is.na(g0mum_alc_audit_grp)) / n()) * 100, 2)))

write.table(table_missingness_alc, file = "mum/Missingness_alc.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Update factor names
levels(COVID1_mum$g0mum_alc_audit_grp)

COVID1_mum <- COVID1_mum %>%
  mutate(g0mum_alc_audit_grp = fct_recode(g0mum_alc_audit_grp, "Low risk" = "1", "Hazardous/harmful/high risk" = "2"))
levels(COVID1_mum$g0mum_alc_audit_grp)

# Make a table to append the OR results to
table_OR_alc <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

# - prepandemic

#YP
#########################################################################################################
table(COVID1_YP$yp_asthma)
# 0    1 
# 6948 3140

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         yp_asthma) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName,  "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_respD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_asthma", n = n(), nMiss = sum(is.na(yp_asthma)),
              perMiss = round((sum(is.na(yp_asthma)) / n()) * 100, 2)))

write.table(table_missingness_respD, file = "yp/Missingness_asthma_prepand.csv", row.names = FALSE, sep=";")


# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_respD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#Mums
#########################################################################################################
table(COVID1_mum$g0mum_respD_prepand)
# 0     1 
# 1537  387

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         g0mum_respD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_respD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_respD_prepand", n = n(), nMiss = sum(is.na(g0mum_respD_prepand)),
              perMiss = round((sum(is.na(g0mum_respD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_respD, file = "mum/Missingness_respD_prepand.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_respD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

# - Cmetab prepandemic 

#YP
#########################################################################################################
table(COVID1_YP$yp_cvascmetD_prepand)

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         yp_cvascmetD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_cvascmetD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_cvascmetD_prepand", n = n(), nMiss = sum(is.na(yp_cvascmetD_prepand)),
              perMiss = round((sum(is.na(yp_cvascmetD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cvascmetD, file = "yp/Missingness_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_cvascmetD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#Mums
#########################################################################################################
table(COVID1_mum$g0mum_cvascmetD_prepand)
# 0    1 
# 1441  518 

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         g0mum_cvascmetD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_cvascmetD_prepand <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_cvascmetD_prepand", n = n(), nMiss = sum(is.na(g0mum_cvascmetD_prepand)),
              perMiss = round((sum(is.na(g0mum_cvascmetD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cvascmetD_prepand, file = "mum/Missingness_cvascmetD_prepand.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_cvascmetD_prepand <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP
#########################################################################################################
table(COVID1_YP$yp_mentalD_prepand)
# 0    1 
#4150 3344

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         yp_mentalD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_mentalD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_mentalD_prepand", n = n(), nMiss = sum(is.na(yp_mentalD_prepand)),
              perMiss = round((sum(is.na(yp_mentalD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_mentalD, file = "yp/Missingness_mentalD.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_mentalD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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



#mum
#########################################################################################################

table(COVID1_mum$g0mum_mentalD_prepand)
# 0    1 
# 748 616 

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         g0mum_mentalD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_mentalD <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_mentalD_prepand", n = n(), nMiss = sum(is.na(g0mum_mentalD_prepand)),
              perMiss = round((sum(is.na(g0mum_mentalD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_mentalD, file = "mum/Missingness_mentalD.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_mentalD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

#YP #none
#########################################################################################################

#mum
#########################################################################################################

table(COVID1_mum$g0mum_cancer_prepand)
# 0    1 
# 1408   66

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_mum_long <- COVID1_mum %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         g0mum_cancer_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_cancer <- COVID1_mum_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "g0mum_cancer_prepand", n = n(), nMiss = sum(is.na(g0mum_cancer_prepand)),
              perMiss = round((sum(is.na(g0mum_cancer_prepand)) / n()) * 100, 2)))

write.table(table_missingness_cancer, file = "mum/Missingness_cancer.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_cancer <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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





#########################################################################################################
#### Immune comorbidities

# - prepandemic
#YP
#########################################################################################################
table(COVID1_YP$yp_immuneD_prepand)
# 0    1 
# 3914   80

## Descriptive stats - Make all of the outcomes in one variable (long format), then use 'group_by'
COVID1_YP_long <- COVID1_YP %>%
  select(covid_nonq1, noncovid_nonq1,         covid_nonq2, noncovid_nonq2,        had_covid1,         had_covid_suspNo1, menni_pred_q1,
         had_covid2, had_covid_suspNo2, menni_pred_q2,
         
         sarscov2_data1, sarscov2_data_no_ownsusp1, sarscov2_data_menni1,
         sarscov2_data2, sarscov2_data_no_ownsusp2, sarscov2_data_menni2,
        
         yp_immuneD_prepand) %>%
  gather(outcomeName, outcomeValue, covid_nonq1:sarscov2_data_menni2) %>%
  mutate(outcomeName = as.factor(outcomeName)) %>%
  mutate(outcomeName = fct_relevel(outcomeName, "had_covid1", "had_covid_suspNo1", "menni_pred_q1",
                                   "had_covid2", "had_covid_suspNo2", "menni_pred_q2",
                                   
                                   "sarscov2_data1", "sarscov2_data_no_ownsusp1", "sarscov2_data_menni1",
                                   "sarscov2_data2", "sarscov2_data_no_ownsusp2", "sarscov2_data_menni2",
                                   
                                   "covid_nonq1", "noncovid_nonq1", 
                                   "covid_nonq2", "noncovid_nonq2"
                                   )) %>%
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

# Also want to make a summary of sex for each missingness category (e.g., whole ALSPAC sample, only those sent a Q,
# only those completed a Q, those with self-reported COVID data, etc.)
(table_missingness_immuneD <- COVID1_YP_long %>%
    filter(!is.na(outcomeValue)) %>%
    group_by(outcomeName) %>%
    summarise(var = "yp_immuneD_prepand", n = n(), nMiss = sum(is.na(yp_immuneD_prepand)),
              perMiss = round((sum(is.na(yp_immuneD_prepand)) / n()) * 100, 2)))

write.table(table_missingness_immuneD, file = "yp/Missingness_immuneD_prepand.csv", row.names = FALSE, sep=";")

# Loop through all COVID outcomes and quest responseness, running the univariable logistic model and storing the estimates
# Make a table to append the OR results to
table_OR_immuneD <- data.frame(matrix(ncol=8, nrow=0))

# Now loop through all the variables add the results to the tables
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

