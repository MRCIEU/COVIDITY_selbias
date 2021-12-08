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

COVID1_mum <- read_delim("mum_outc_pred.csv", ",")
head(COVID1_mum_raw)
COVID1_mum <- COVID1_mum_raw



#########################################################################################################
## 3. Data preparation 
#########################################################################################################

COVID1_mum <- COVID1_mum %>%  
  mutate(g0mum_edu = as.factor(g0mum_edu)) %>%
  mutate(g0mum_everSmk = as.factor(g0mum_everSmk)) %>%
  mutate(g0mum_recent_Smk = as.factor(g0mum_recent_Smk)) %>%
  mutate(had_covid1 = as.factor(had_covid1)) %>%
  mutate(had_covid1_controls2 = as.factor(had_covid1_controls2)) %>%
  mutate(had_covid2 = as.factor(had_covid2)) %>%
  mutate(had_covid2_controls2 = as.factor(had_covid2_controls2)) %>% 

COVID1_mum <- COVID1_mum %>%  
  mutate(g0mum_age_COVID1 = as.numeric(g0mum_age_COVID1)) %>%
  mutate(g0mum_age_COVID2 = as.numeric(g0mum_age_COVID2)) %>%
  mutate(g0mum_IMD = as.numeric(g0mum_IMD)) %>%
  mutate(g0mum_bmi = as.numeric(g0mum_bmi))  

# Convert age and BMI to Z-scores for the multivariable analyses
hist(COVID1_mum$g0mum_age_COVID1) 
hist(COVID1_mum$g0mum_bmi) 
COVID1_mum <- COVID1_mum %>%
  mutate(age_z = (g0mum_age_COVID1 - mean(g0mum_age_COVID1, na.rm = TRUE)) / sd(g0mum_age_COVID1, na.rm = TRUE)) %>%
  mutate(bmi_z = (g0mum_bmi - mean(g0mum_bmi, na.rm = TRUE)) / sd(g0mum_bmi, na.rm = TRUE))
hist(COVID1_mum$age_z)
hist(COVID1_mum$bmi_z) 

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
## 5. Logistic regression
#########################################################################################################

# Model adjusted for age, smoking, education and IMD

table_OR_bmi <- data.frame(matrix(ncol=8, nrow=0))

for (i in var_list) {
  print(i)
  
  ## Odds ratio results
  model <- glm(paste(i[[1]], "~ bmi_z + age_z + g0mum_everSmk + g0mum_edu + g0mum_IMD"), data = COVID1_mum, family = "binomial")
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
  table_OR_bmi <- rbind(table_OR_bmi, temp)
  table_OR_bmi <- table_OR_bmi %>%
    filter(coef != "(Intercept)")
}

table_OR_bmi

write.table(table_OR_bmi, file = "mum/bmi/Model3_OR_bmi_alloutcomes.csv", row.names = FALSE, sep=";")



#########################################################################################################
## 6. Forest plots
#########################################################################################################

#rename predictor and outcomes for the forest plot
model <- table_OR_bmi[which(table_OR_bmi$coef=="bmi_z"),]
model$coef <- dplyr::recode(model$coef, 
                            "bmi_z"="BMI (z-score)")

# Q1
model_q1 <- model[which(model$Outcome=="had_covid1_controls2" |
                          model$Outcome=="had_covid1" ),]

model_q1$Outcome <- dplyr::recode(model_q1$Outcome,
                                  "had_covid1"="SARS-CoV-2 (+) VS SARS-CoV-2 (-)",
                                  "had_covid1_controls2"="SARS-CoV-2 (+) VS everyone else") 
model_q1$Outcome <- as.factor(model_q1$Outcome)
levels(model_q1$Outcome)
model_q1 <- model_q1 %>%
  mutate(Outcome = fct_rev(Outcome))

model_q1$comp <- "G0 mothers cohort"

q1 <- ggforestplot::forestplot(df = model_q1,
                               name = comp,
                               estimate = Beta,
                               se = SE,
                               pvalue = pvalue,
                               psignif = 0.05,
                               logodds = TRUE,
                               colour = Outcome,
                               shape = Outcome,
                               title = "Association between BMI and SARS-CoV-2 infection\nin ALSPAC G0 Mothers Cohort, Q1",
                               xlab = "OR for SARS-CoV-2 infection\n(95% CI) per 1-SD increment in BMI"
) +
  scale_color_manual(values=color) + 
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

model_q2$Outcome <- as.factor(model_q2$Outcome)
levels(model_q2$Outcome)
model_q2 <- model_q2 %>%
  mutate(Outcome = fct_rev(Outcome))

q2 <- ggforestplot::forestplot(df = model_q2,
                               name = comp,
                               estimate = Beta,
                               se = SE,
                               pvalue = pvalue,
                               psignif = 0.05,
                               logodds = TRUE,
                               colour = Outcome,
                               shape = Outcome,
                               title = "Association between BMI and SARS-CoV-2 infection\nin ALSPAC G0 Mothers Cohort, Q2",
                               xlab = "OR for SARS-CoV-2 infection\n(95% CI) per 1-SD increment in BMI"
) +
  scale_color_manual(values=color) + 
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
                    top = text_grob("Association between BMI and SARS-CoV-2 infection\nALSPAC G0 Mothers Cohort\n\n", 
                                    face = "bold", 
                                    size = 14) )  %>%
    ggexport(.,
             filename = "mum/bmi/q1q2.png", 
             width = 1300, 
             height = 500))
