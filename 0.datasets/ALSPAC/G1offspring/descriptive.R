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



#########################################################################################################
##### 4. Descriptive data 
#########################################################################################################

# Whole sample
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

#assessed subsample
yp1 <- filter(COVID1_YP, sarscov2_data1==1)
res_yp1 <- compareGroups( ~ had_covid1 +
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
res_yp2 <- compareGroups( ~ had_covid1 +
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
yp_hadcovid <- filter(COVID1_YP, had_covid1==1)
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

yp_hadcovid2 <- filter(COVID1_YP, had_covid2==1)
res_yp_hadcovid2 <- compareGroups( ~ yp_sex +
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
                                  data = yp_hadcovid2)
res_yp_hadcovid2 <- createTable(res_yp_hadcovid2)
export2csv(res_yp_hadcovid2, file="yp/had_covid2.csv", sep=";")

# those with no COVID 
yp_hadcovid_no <- filter(COVID1_YP, had_covid1==0)
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

yp_hadcovid_no2 <- filter(COVID1_YP, had_covid2==0)
res_yp_hadcovid_no2 <- compareGroups( ~ yp_sex +
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
                                  data = yp_hadcovid_no2)
res_yp_hadcovid_no2 <- createTable(res_yp_hadcovid_no2)
export2csv(res_yp_hadcovid_no2, file="yp/had_covid_no2.csv", sep=";")
