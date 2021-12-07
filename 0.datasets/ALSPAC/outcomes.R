###First version of code by AFS 02-27/10/2020 



#########################################################################################################
# 1. Create R environment
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
# 2. Read in the datasets created from the modification of the datasets developed by DS by adding new variables
#########################################################################################################

COVID1_YP_raw <- read_delim("outcomes_yp_20210526.csv", ",")
head(COVID1_YP_raw)
COVID1_YP <- COVID1_YP_raw


COVID1_mum_raw <- read_delim("outcomes_mum_20210526.csv", ",")
head(COVID1_mum_raw)
COVID1_mum <- COVID1_mum_raw



#########################################################################################################
# 3. Derive outcomes
#########################################################################################################

#Had COVID, Q1, section A - "a9: Participant thinks they have or have had COVID-19: COVID1" - covid1yp_2580, covid1m_2580
#Had COVID, Q2, section A, a3 (grouped): Participant thinks they have or have had COVID-19: COVID2 - covid2yp_1061, covid2m_1061


#########################################################################################################
# Assessed vs non-assessed = having data on COVID-19: cases those COVID+ or COVID-, controls those NA 

# Q1
COVID1_YP$sarscov2_data1 <- ifelse(is.na(COVID1_YP$covid1yp_2580),
                                   0,
                                   1)
COVID1_mum$sarscov2_data1 <- ifelse(is.na(COVID1_mum$covid1m_2580),
                                    0,
                                    1)
                                    
# Q2
COVID1_YP$sarscov2_data2 <- ifelse(is.na(COVID1_YP$covid2yp_1061),
                                   0,
                                   1)

COVID1_mum$sarscov2_data2 <- ifelse(is.na(COVID1_mum$covid2m_1061),
                                    0,
                                    1)

#########################################################################################################
# Assessed positive vs assessed negative = had_covid

# Q1 
# had COVID-19 including own suspicion 
COVID1_YP$had_covid1 <- ifelse(COVID1_YP$covid1yp_2580==1 | 
                                 COVID1_YP$covid1yp_2580==2,
                               1, 
                               ifelse(COVID1_YP$covid1yp_2580==0,
                                      0,
                                      NA))

COVID1_mum$had_covid1 <- ifelse(COVID1_mum$covid1m_2580==1 | 
                                  COVID1_mum$covid1m_2580==2,
                                1, 
                                ifelse(COVID1_mum$covid1m_2580==0,
                                       0,
                                       NA))
                                       
# Q2 
# had COVID-19 including own suspicion 
COVID1_YP$had_covid2 <- ifelse(COVID1_YP$covid2yp_1061==1 | 
                                 COVID1_YP$covid2yp_1061==2,
                               1, 
                               0)
COVID1_mum$had_covid2 <- ifelse(COVID1_mum$covid2m_1061==1 | 
                                  COVID1_mum$covid2m_1061==2,
                                1, 
                                0)

#########################################################################################################
# Having COVID-19 vs non-assessed

#Q1
COVID1_YP$covid_nonq1 <-  ifelse(is.na(COVID1_YP$covid1yp_2580),
                                      0,
                                 ifelse(COVID1_YP$covid1yp_2580==1 | 
                                 COVID1_YP$covid1yp_2580==2,
                                 1, 
                                 NA))
COVID1_mum$covid_nonq1 <- ifelse(is.na(COVID1_mum$covid1m_2580),
                                 0,
                                 ifelse(COVID1_mum$covid1m_2580==1 | 
                                          COVID1_mum$covid1m_2580==2,
                                        1, 
                                        NA))

#Q2
COVID1_YP$covid_nonq2 <- ifelse(is.na(COVID1_YP$covid2yp_1061),
                                0,
                                ifelse(COVID1_YP$covid2yp_1061==1 | 
                                         COVID1_YP$covid2yp_1061==2,
                                       1, 
                                       NA))
COVID1_mum$covid_nonq2 <- ifelse(is.na(COVID1_mum$covid2m_1061),
                                 0,
                                 ifelse(COVID1_mum$covid2m_1061==1 | 
                                          COVID1_mum$covid2m_1061==2,
                                        1, 
                                        NA))

#########################################################################################################
# Not having COVID-19 vs non-assessed

#Q1
COVID1_YP$noncovid_nonq1 <- ifelse(is.na(COVID1_YP$covid1yp_2580),
                                       0,
                                   ifelse(COVID1_YP$covid1yp_2580==0,
                                          1, 
                                          NA))

COVID1_mum$noncovid_nonq1 <- ifelse(is.na(COVID1_mum$covid1m_2580),
                                        0,
                                    ifelse(COVID1_mum$covid1m_2580==0,
                                           1, 
                                           NA))

#Q2
COVID1_YP$noncovid_nonq2 <- ifelse(is.na(COVID1_YP$covid2yp_1061),
                                       0,
                                       ifelse(COVID1_YP$covid2yp_1061==0,
                                1, 
                                NA))

COVID1_mum$noncovid_nonq2 <- ifelse(is.na(COVID1_mum$covid2m_1061),
                                        0,
                                        ifelse(COVID1_mum$covid2m_1061==0,
                                 1, 
                                 NA))
                                 
#########################################################################################################
# Having COVID-19 VS everyone else

#Q1
COVID1_YP$had_covid1_controls2 <- ifelse(is.na(COVID1_YP$had_covid1) |
                                           COVID1_YP$had_covid1==0,
                                         0, 
                                         1)
COVID1_mum$had_covid1_controls2 <- ifelse(is.na(COVID1_mum$had_covid1)|
                                            COVID1_mum$had_covid1==0,
                                          0, 
                                          1)


#Q2
COVID1_YP$had_covid2_controls2 <- ifelse(is.na(COVID1_YP$had_covid2|
                                                 COVID1_YP$had_covid2==0),
                                         0, 
                                         1)
COVID1_mum$had_covid2_controls2 <- ifelse(is.na(COVID1_mum$had_covid2|
                                                  COVID1_mum$had_covid2==0),
                                          0, 
                                          1)



#########################################################################################################
# 4. Save datasets
#########################################################################################################

write.csv(COVID1_YP, file = "yp_outc_pred.csv", row.names = FALSE)
write.csv(COVID1_mum, file = "mum_outc_pred.csv", row.names = FALSE)

