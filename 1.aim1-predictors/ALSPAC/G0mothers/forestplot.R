### First code by AFS 08-10/12/2020 



#########################################################################################################
## 1. Create R environment
#########################################################################################################

rm(list=ls())

setwd("/path/to/folder")

library(readr)
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggforestplot)
library(ggpubr)
library(gridExtra)



#########################################################################################################
# 2. Read in tables with results
#########################################################################################################

# Identify list of files
list_of_files <- list.files(path = "mum", recursive = TRUE,
                            pattern = "Univar_OR_*",
                            full.names = TRUE)
list_of_files

# Combine list of files
pred_comp <- rbindlist(lapply(list_of_files, fread))                          
head(pred_comp)
  
#Select only predictors of selection 
pred_comp <- pred_comp[which(pred_comp$coef=="age1_z" | 
                                     pred_comp$coef=="age2_z" | 
                                     pred_comp$coef=="bmi_z" | 
                                     pred_comp$coef=="sys_BP_z" | 
                                     pred_comp$coef=="dia_BP_z" | 
                                     pred_comp$Variable=="Ethnicity (ref = White)" | 
                                     pred_comp$Variable=="Recent smoking (ref = No)"|
                                     pred_comp$Variable=="Alcohol abuse (ref = No)"|
                                     pred_comp$Variable=="Education (ref = CSE/Lower)"|
                                     pred_comp$Variable=="IMD (ref = 1/Least dep.)"|
                                     pred_comp$Variable=="Urban/Rural (ref = Urban)"|
                                     pred_comp$Variable=="Pre-pandemic cardiometabolic comorb (ref = none)" |
                                     pred_comp$Variable=="Pre-pandemic respiratory comorb (ref = none)" |
                                     pred_comp$Variable=="Pre-pandemic mental comorb (ref = none)" |
                                     pred_comp$Variable=="Cancer (ref = none)"),]

#Add variable group for the classification of the predictors of selection
pred_comp$group <- NA
pred_comp$group <- ifelse(pred_comp$coef=="age1_z" | 
                            pred_comp$coef=="age2_z" | 
                            pred_comp$Variable=="Ethnicity (ref = White)",
                          "Demographic factors",
                          pred_comp$group)
pred_comp$group <- ifelse(pred_comp$Variable=="Recent smoking (ref = No)"|
                            pred_comp$Variable=="Alcohol abuse (ref = No)",
                          "Behavioral factors",
                          pred_comp$group)
pred_comp$group <- ifelse(pred_comp$Variable=="Education (ref = GCSE/Lower)"|
                            pred_comp$Variable=="IMD (ref = 1/Least dep.)"|
                            pred_comp$Variable=="Urban/Rural (ref = Urban)",
                          "Social factors",
                          pred_comp$group)
pred_comp$group <- ifelse(pred_comp$Variable=="Pre-pandemic cardiometabolic comorb (ref = none)" |
                            pred_comp$Variable=="Pre-pandemic respiratory comorb (ref = none)" |
                            pred_comp$Variable=="Pre-pandemic mental comorb (ref = none)" |
                            pred_comp$Variable=="Cancer (ref = none)",
                          "Comorbidities",
                          pred_comp$group)
pred_comp$group <- ifelse(pred_comp$coef=="bmi_z" | 
                            pred_comp$coef=="sys_BP_z" | 
                            pred_comp$coef=="dia_BP_z",
                          "Anthropometric factors",
                          pred_comp$group)



#########################################################################################################
# 3. Create datasets for categorical and continuous variables and rename them
#########################################################################################################

univar_cat <- pred_comp[-which(pred_comp$coef=="age1_z" | 
                               pred_comp$coef=="age2_z" |
                               pred_comp$coef=="g0mum_IMD"|
                               pred_comp$coef=="bmi_z" |
                               pred_comp$coef=="sys_BP_z" | 
                               pred_comp$coef=="dia_BP_z"), ]
univar_cat$coef <- dplyr::recode(univar_cat$coef,
                           "g0mum_ethnicity2"="Ethnicity - non-white (ref = white)",
                           "g0mum_recent_Smk1"="Smoking - former (ref = never)",
                           "g0mum_recent_Smk2"="Smoking - current (ref = never)",
                           "g0mum_alc_audit_grpHazardous/harmful/high risk"="Alcohol abuse (ref = no)",
                           "g0mum_eduVocational"="Education level - Vocational (ref = GCSE/lower)",
                           "g0mum_eduAS/A level"="Education level - AS/A level (ref = GCSE/lower)",
                           "g0mum_eduDegree"="Education level - Degree or higher (ref = GCSE/lower)",
                           "g0mum_urbanRural2"="Indexes of non-urban area (ref = urban)",
                           "g0mum_respD_prepand1"= "Respiratory comorbidities (ref = no)",
                           "g0mum_cvascmetD_prepand1" ="Cardiometabolic comorbidities (ref = no)",
                           "g0mum_mentalD_prepand1"="Adverse mental health outcomes (ref = no)",
                           "g0mum_cancer_prepand1"="Cancer (ref = no)")

univar_cont <- pred_comp[which(pred_comp$coef=="age1_z" | 
                               pred_comp$coef=="age2_z" |
                               pred_comp$coef=="g0mum_IMD"|
                               pred_comp$coef=="bmi_z" | 
                               pred_comp$coef=="sys_BP_z" | 
                               pred_comp$coef=="dia_BP_z"), ]
univar_cont$coef <- dplyr::recode(univar_cont$coef, 
                                  "age1_z"="Standardized Age Q1", 
                                  "age2_z"="Standardized Age Q2", 
                                  "bmi_z"="Standardized BMI",
                                  "g0mum_IMD"="Index of multiple deprivation",
                                  "sys_BP_z"="Standardized SBP", 
                                  "dia_BP_z"="Standardized DBP")



#########################################################################################################
# 4. Forest plots for Q1
#########################################################################################################

#########################################################################################################
# Categorical vars

cat <- univar_cat[which(univar_cat$Outcome=="sarscov2_data1"|
                          univar_cat$Outcome=="had_covid1"|
                          univar_cat$Outcome=="covid_nonq1"|
                          univar_cat$Outcome=="noncovid_nonq1"),]

cat$beta <- log(cat$Coefficient_OR)
cat$se <- (log(cat$Upper_CI)-log(cat$Lower_CI))/(1.96*2)

cat$Outcome <- as.factor(cat$Outcome)
levels(cat$Outcome) 
levels(cat$Outcome) <- c("SARS-CoV-2 (+) vs non-assessed", 
                         "SARS-CoV-2 (+) vs SARS-CoV-2 (-)",
                         "SARS-CoV-2 (-) vs non-assessed", 
                         "Assessed vs non-assessed")
cat <- cat %>%
  mutate(coef = as.factor(coef)) %>%
        mutate(coef = fct_relevel(coef,
                                  "Ethnicity - non-white (ref = white)",
                                  "Education level - Degree or higher (ref = GCSE/lower)",
                                  "Education level - AS/A level (ref = GCSE/lower)",
                                  "Education level - Vocational (ref = GCSE/lower)",
                                  "Indexes of non-urban area (ref = urban)",
                                  "Smoking - current (ref = never)",
                                  "Smoking - former (ref = never)",
                                  "Alcohol abuse (ref = no)",
                                  "Respiratory comorbidities (ref = no)",
                                  "Adverse mental health outcomes (ref = no)",
                                  "Cardiometabolic comorbidities (ref = no)",
                                  "Cancer (ref = no)"))
cat <- cat %>%
  mutate(Outcome = fct_relevel(Outcome, 
                               "Assessed vs non-assessed",
                               "SARS-CoV-2 (+) vs non-assessed",
                               "SARS-CoV-2 (-) vs non-assessed", 
                               "SARS-CoV-2 (+) vs SARS-CoV-2 (-)"))
cat <- cat %>%
  mutate(group = as.factor(group)) %>%
  mutate(group = fct_relevel(group, 
                             "Demographic factors",
                             "Social factors",
                             "Behavioral factors",
                             "Comorbidities"))

cat1 <- ggforestplot::forestplot(df = cat,
                                  name = coef,
                                  estimate = beta,
                                  se = se,
                                  logodds = TRUE,
                                  pvalue = pvalue,
                                  psignif = 0.05,
                                  colour = Outcome,
                                  shape = Outcome,
                                  xlab = "OR (95% CI)"
) +
  ggforce::facet_col(
    facets = ~group,
    scales = "free_y",
    space = "free"
  ) +
  scale_x_continuous(breaks = scales::extended_breaks(n=10)) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(size = 12, face="italic"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.box.background = element_rect(),
    legend.position	= "bottom",
    legend.direction = "vertical"
  )

#########################################################################################################
# Continuous vars

cont <- univar_cont[which(univar_cont$Outcome=="sarscov2_data1"|
                            univar_cont$Outcome=="had_covid1"|
                            univar_cont$Outcome=="covid_nonq1"|
                            univar_cont$Outcome=="noncovid_nonq1"),]

cont$beta <- log(cont$Coefficient_OR)
cont$se <- (log(cont$Upper_CI)-log(cont$Lower_CI))/(1.96*2)

cont$Outcome <- as.factor(cont$Outcome)
levels(cont$Outcome)
levels(cont$Outcome) <- c("SARS-CoV-2 (+) vs non-assessed", 
                          "SARS-CoV-2 (+) vs SARS-CoV-2 (-)",
                          "SARS-CoV-2 (-) vs non-assessed", 
                          "Assessed vs non-assessed")
cont <- cont %>%
  mutate(Outcome = fct_relevel(Outcome, 
                               "Assessed vs non-assessed",
                               "SARS-CoV-2 (+) vs non-assessed",
                               "SARS-CoV-2 (-) vs non-assessed", 
                               "SARS-CoV-2 (+) vs SARS-CoV-2 (-)"))

cont <- cont[-which(cont$coef=="Standardized Age Q2"),]
cont <- cont %>%
  mutate(coef = as.factor(coef)) %>%
  mutate(coef = fct_relevel(coef, 
                            "Standardized Age Q1",
                            "Index of multiple deprivation",
                            "Standardized BMI",
                            "Standardized DBP",
                            "Standardized SBP"))
cont$coef <- dplyr::recode(cont$coef, 
                                  "Standardized Age Q1"="Age", 
                                  "Standardized BMI"="BMI",
                           "Standardized SBP"="Systolic BP", 
                           "Standardized DBP"="Diastolic BP")
cont <- cont %>%
  mutate(group = as.factor(group)) %>%
  mutate(group = fct_relevel(group, 
                             "Demographic factors",
                             "Social factors",
                             "Anthropometric factors"))

cont1 <- ggforestplot::forestplot(df = cont,
                                  name = coef,
                                  estimate = beta,
                                  se = se,
                                  logodds = TRUE,
                                  pvalue = pvalue,
                                  psignif = 0.05,
                                  colour = Outcome,
                                  shape = Outcome,
                                  xlab = "OR (95% CI) per 1-SD increment\nin candidate predictors of selection"
) +
  ggforce::facet_col(
    facets = ~group,
    scales = "free_y",
    space = "free"
  ) +
  scale_x_continuous(breaks = scales::extended_breaks(n=10)) +
  theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, face="bold.italic", hjust=0.5),
      axis.title.x = element_text(size = 12, face="italic"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 12),
    legend.box.background = element_rect(),
    legend.position	= "bottom",
    legend.direction = "vertical"
  )



#########################################################################################################
# 5. Forest plots for Q2
#########################################################################################################

#########################################################################################################
# Categorical vars

cat <- univar_cat[which(univar_cat$Outcome=="sarscov2_data2"|
                          univar_cat$Outcome=="had_covid2"|
                          univar_cat$Outcome=="covid_nonq2"|
                          univar_cat$Outcome=="noncovid_nonq2"),]

cat$beta <- log(cat$Coefficient_OR)
cat$se <- (log(cat$Upper_CI)-log(cat$Lower_CI))/(1.96*2)

cat$Outcome <- as.factor(cat$Outcome)
levels(cat$Outcome) 
levels(cat$Outcome) <- c("SARS-CoV-2 (+) vs non-assessed", 
                         "SARS-CoV-2 (+) vs COVID-19 (-)",
                         "SARS-CoV-2 (-) vs non-assessed", 
                         "Assessed vs non-assessed")
cat <- cat %>%
  mutate(Outcome = fct_relevel(Outcome, 
                               "Assessed vs non-assessed",
                               "SARS-CoV-2 (+) vs non-assessed",
                               "SARS-CoV-2 (-) vs non-assessed", 
                               "SARS-CoV-2 (+) vs COVID-19 (-)"))
cat <- cat %>%
  mutate(coef = as.factor(coef)) %>%
  mutate(coef = fct_relevel(coef,
                            "Ethnicity - non-white (ref = white)",
                            "Sex - female",
                            "Education level - Degree or higher (ref = GCSE/lower)",
                            "Education level - AS/A level (ref = GCSE/lower)",
                            "Education level - Vocational (ref = GCSE/lower)",
                              "Indexes of non-urban area (ref = urban)",
                            "Smoking - current (ref = never)",
                            "Smoking - former (ref = never)",
                            "Alcohol abuse (ref = no)",
                            "Asthma (ref = no)",
                            "Adverse mental health outcomes (ref = no)",
                            "Cardiometabolic comorbidities (ref = no)",
                            "Autoimmune comorbidities (ref = no)"))
cat <- cat %>%
  mutate(group = as.factor(group)) %>%
  mutate(group = fct_relevel(group, 
                             "Demographic factors",
                             "Social factors",
                             "Behavioral factors",
                             "Comorbidities"))

cat2 <- ggforestplot::forestplot(df = cat,
                                  name = coef,
                                  estimate = beta,
                                  se = se,
                                  logodds = TRUE,
                                  pvalue = pvalue,
                                  psignif = 0.05,
                                  colour = Outcome,
                                  shape = Outcome,
                                  xlab = "OR (95% CI)"
) +
  ggforce::facet_col(
    facets = ~group,
    scales = "free_y",
    space = "free"
  ) +
  scale_x_continuous(breaks = scales::extended_breaks(n=10)) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(size = 12, face="italic"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.box.background = element_rect(),
    legend.position	= "bottom",
    legend.direction = "vertical"
  )

#########################################################################################################
# Continuous vars

cont <- univar_cont[which(univar_cont$Outcome=="sarscov2_data2"|
                            univar_cont$Outcome=="had_covid2"|
                            univar_cont$Outcome=="covid_nonq2"|
                            univar_cont$Outcome=="noncovid_nonq2"),]

cont$beta <- log(cont$Coefficient_OR)
cont$se <- (log(cont$Upper_CI)-log(cont$Lower_CI))/(1.96*2)

cont$Outcome <- as.factor(cont$Outcome)
levels(cont$Outcome)
levels(cont$Outcome) <- c("SARS-CoV-2 (+) vs non-assessed", 
                          "SARS-CoV-2 (+) vs COVID-19 (-)",
                          "SARS-CoV-2 (-) vs non-assessed", 
                          "Assessed vs non-assessed")
cont <- cont %>%
  mutate(Outcome = fct_relevel(Outcome, 
                               "Assessed vs non-assessed",
                               "SARS-CoV-2 (+) vs non-assessed",
                               "SARS-CoV-2 (-) vs non-assessed", 
                               "SARS-CoV-2 (+) vs COVID-19 (-)"))

cont <- cont %>%
  mutate(group = as.factor(group)) %>%
  mutate(group = fct_relevel(group, 
                             "Demographic factors",
                             "Social factors",
                             "Anthropometric factors"))
cont <- cont[-which(cont$coef=="Standardized Age Q1"),]
cont <- cont %>%
  mutate(coef = as.factor(coef)) %>%
  mutate(coef = fct_relevel(coef, 
                            "Standardized Age Q2",
                            "Index of multiple deprivation",
                            "Standardized BMI",
                            "Standardized DBP",
                            "Standardized SBP"))

cont$coef <- dplyr::recode(cont$coef, 
                           "Standardized Age Q2"="Age", 
                           "Standardized BMI"="BMI",
                           "Standardized SBP"="Systolic BP", 
                           "Standardized DBP"="Diastolic BP")

cont2 <- ggforestplot::forestplot(df = cont,
                                  name = coef,
                                  estimate = beta,
                                  se = se,
                                  logodds = TRUE,
                                  pvalue = pvalue,
                                  psignif = 0.05,
                                  colour = Outcome,
                                  shape = Outcome,
                                  xlab = "OR (95% CI) per 1-SD increment\nin candidate predictors of selection"
) +
  ggforce::facet_col(
    facets = ~group,
    scales = "free_y",
    space = "free"
  ) +
  scale_x_continuous(breaks = scales::extended_breaks(n=10)) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(size = 12, face="italic"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.box.background = element_rect(),
    legend.position	= "bottom",
    legend.direction = "vertical"
  )



#########################################################################################################
# 6. Forest plots for q1/q2
#########################################################################################################

#########################################################################################################
# Categorical vars
figure1 <- ggarrange(cat1,                                               
                     cat2, 
                     ncol = 2, 
                     align = "h",
                     labels = c("A. Questionnaire 1\n","B. Questionnaire 2\n"),
                     hjust = -0.1,
                     vjust = 0.5,
                     font.label = list(size = 12, face = "bold.italic"),
                     common.legend = T, 
                     legend = "bottom",
                     widths = c(1, 1))  
 
(figure <- annotate_figure(figure1,
                   top = text_grob("Association between candidate predictors of selection and outcomes describing selection\nALSPAC G0 mothers Cohort - Categorical variables\n\n", 
                                   face = "bold", 
                                   size = 14))  %>%
   ggexport(.,
            filename = "mum/forestplots/q1q2cat.png", 
            width = 1000, 
            height = 800))

#########################################################################################################
# Continuous vars
(figure2 <- ggarrange(cont1,                                               
                     cont2, 
                     ncol = 2, 
                     align = "h",
                     labels = c("A. Questionnaire 1\n","B. Questionnaire 2\n"),
                     hjust = -0.1,
                     vjust = 0.5,
                     font.label = list(size = 12, face = "bold.italic"),
                     common.legend = T, 
                     legend = "bottom",
                     widths = c(1, 1)))  
(figure <- annotate_figure(figure2,
                   top = text_grob("Association between candidate predictors of selection and outcomes describing selection\nALSPAC G0 mothers Cohort - Continuous variables\n\n", 
                                   face = "bold", 
                                   size = 14))  %>%
   ggexport(.,
            filename = "mum/forestplots/q1q2cont.png", 
            width = 1000, 
            height = 500))
