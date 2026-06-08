## CCM R Workshop Session 1: Regressions

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")

## 00. Load libraries ----------------------------------------------------

# install.packages("tidyverse", "corrplot")
library(tidyverse) # we will use this library, which comes with its own syntax
library(stats)
library(sjPlot) # plot regression output

#

############################### REGRESSIONS ############################### 


# A) Main effects --------------------------------------------------

## Continuous variables
# Do age and the blood pressure contribute to blood glucose levels?

main_effects_model1 <- lm(y ~ x1 + x2, data = df)
summary(model) # use `summary()` to see coefficients, standard errors, and p-values

# plot the significant main effects 
# main effect of predictor (sysBP)
plot_model(main_effects_model1, 
           type = "pred", # provides estimated marginal effects predicted by the model (after accounting for effects of the other variables in the model)
           terms = predictor, 
           colors = "black",
           axis.title = c("Systolic Blood Pressure", "Glucose"),
           title = "Main effect of systolic BP on glucose") + theme_apa()

## Categorical variables
# Are smoking status and use of BP medications related to cholesterol levels?

main_effects_model2 <- lm(y ~ x1 + x2, data = df)
summary(model) ## interpretation of the output 

# plot the significant main effects
# 1. main effect of predictor1
plot_model(main_effects_model2, 
           type = "pred",
           terms = "currentSmoker",
           color = "black",
           axis.title = c("Smoking Status", "Cholesterol"),
           title = "Main effect of smoking status on cholesterol levels") + theme_classic()

# 2. main effect of predictor 2
plot_model(main_effects_model2, 
           type = "pred",
           terms = "BPMeds",
           color = "black",
           axis.title = c("Use of BP Meds", "Cholesterol"),
           title = "Main effect of BP Meds on cholesterol levels") + theme_classic()

## Categorical and continuous variables 
# CHALLENGE: Are sex and cholesterol levels related to blood pressure?

# main_effects_model3 <- lm(sysBP ~ sex + totChol, data = prepped_df)
# summary(main_effects_model3)

#

# B) Interactions ---------------------------

## Between categorical and continuous variables
# How are sex and cholesterol levels related to blood pressure?

interactions_model3 <- lm(y ~ x1 * x2, data = prepped_df)
summary(interactions_model3)

plot_model(interactions_model3,
           type = "pred",
           colors = c("darkorange", "purple4"),
           terms = c("totChol", "sex")) + theme_classic() # include both predictors here to see their interaction
#

## CHALLENGE: how are age, hypertension, and history of stroke related to cholesterol levels? 

interactions_model4 <- lm(y ~ x1 * x2 * x3, data = prepped_df)
summary(interactions_model4)

plot_model(interactions_model4, 
           type = "pred",
           terms = c(predictors)) + theme_classic()
