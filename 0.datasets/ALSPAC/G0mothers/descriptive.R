###First version of code by AFS 02/12/2020



#########################################################################################################
##### 1. Create R environment
#########################################################################################################

rm(list=ls())

setwd("path/to/folder")

library(readr)
library(compareGroups)



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


#########################################################################################################
##### 4. Descriptive data 
#########################################################################################################

# Whole sample
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

#assessed subsample
mum1 <- filter(COVID1_mum, sarscov2_data1==1)
res_mum1 <- compareGroups( ~ had_covid1 +
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
res_mum2 <- compareGroups( ~ had_covid1 +
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

#those with no data
yp_data_no <- filter(COVID1_YP, sarscov2_data1==0)
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
export2csv(res_yp_data_no, file="yp/data_no1.csv", sep=";")

yp_data_no2 <- filter(COVID1_YP, sarscov2_data2==0)
res_yp_data_no2 <- compareGroups( ~ yp_sex +
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
                                 data = yp_data_no2)
res_yp_data_no2 <- createTable(res_yp_data_no2)
export2csv(res_yp_data_no2, file="yp/data_no2.csv", sep=";")

# those with COVID 
mum_hadcovid <- filter(COVID1_mum, had_covid1==1)
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

mum_hadcovid2 <- filter(COVID1_mum, had_covid2==1)
res_mum_hadcovid2 <- compareGroups( ~ g0mum_sex +
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
                                   data = mum_hadcovid2)
res_mum_hadcovid2 <- createTable(res_mum_hadcovid2)
export2csv(res_mum_hadcovid2, file="mum/had_covid2.csv", sep=";")

# those with no COVID 
mum_hadcovid_no <- filter(COVID1_mum, had_covid1==0)
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

mum_hadcovid_no2 <- filter(COVID1_mum, had_covid2==0)
res_mum_hadcovid_no2 <- compareGroups( ~ g0mum_sex +
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
                                      data = mum_hadcovid_no2)
res_mum_hadcovid_no2 <- createTable(res_mum_hadcovid_no2)
export2csv(res_mum_hadcovid_no2, file="mum/had_covid_no2.csv", sep=";")
