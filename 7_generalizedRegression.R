## CCM R Workshop Session 2 Generalized Regression

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")


# Load libraries ----------------------------------------------------

# install.packages("tidyverse", "corrplot")
library(tidyverse) # we will use this library, which comes with its own syntax
library(corrplot) # visualize correlations
library(stats)
library(sjPlot) # plot regression output
library(effects) # plot marginal effects
library(emmeans) # estimate marginal means
library(ggeffects)
library(pscl) # zero-inflated poisson regression
library(marginaleffects) # plot ZIP model output
#

# Logistic Regression -----------------------------------------------------

# What is the relationship between cigarette smoking, sex, and heart disease?

# first, let us set up contrasts - we will use simple effect coding for our binary predictors

levels(prepped_df$sex)
contrasts(prepped_df$sex)
contrasts(prepped_df$sex) <- c(-1, 1) # females are the reference group here
contrasts(prepped_df$sex)

# now, we can specify the model

log_model_1 <- glm(TenYearCHD ~ currentSmoker * sex, 
                   data = prepped_df, family = binomial(link="logit")) 
summary(log_model_1)

# examine odds ratios
exp(coef(log_model_1))

emmeans(log_model_1, ~ currentSmoker | sex, type = "response")

# plot the significant effects

plot_model(log_model_1, 
           type = "pred", # provides estimated marginal effects predicted by the model (after accounting for effects of the other variables in the model)
           terms = c("currentSmoker", "sex"),
           colors = c("darkorange", "purple3"),
           axis.title = c("Cigarettes Smoked per Day", "Predicted Probability of Heart Disease"),
           legend.title = "Smoking Status", 
           title = " ") + theme_apa() 

# you can also plot the results using ggplot2

log_model1_means <- as.data.frame(emmeans(log_model_1, ~ currentSmoker | sex ,type="response"))
log_model1_means

ggplot(log_model1_means, aes(x=factor(currentSmoker, levels = c(0, 1), labels = c("Non-Smoker", "Smoker")),
                             y=prob, fill = sex)) +
  geom_bar(stat="identity",position="dodge",color="grey",show.legend=T) +
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),
                position=position_dodge(0.9),width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=4,
               position=position_dodge(width=0.9),vjust=-.6) +
  scale_fill_manual(values=c("darkorange", "purple3")) +
  ylab("Predicted Probability of Heart Disease") + 
  xlab("Smoking Status") + 
  theme_apa() 

# does a history of stroke and hypertension predict heart disease? does this vary between males and females?

levels(prepped_df$prevalentStroke)
contrasts(prepped_df$prevalentStroke)
contrasts(prepped_df$prevalentStroke) <- c(-1, 1) # no history of stroke is the reference group here
contrasts(prepped_df$prevalentStroke)

levels(prepped_df$prevalentHyp)
contrasts(prepped_df$prevalentHyp) <- c(-1, 1) # no hypertension is the reference group here
contrasts(prepped_df$prevalentHyp)

log_model_2 <- glm(TenYearCHD ~ prevalentStroke * sex + prevalentHyp * sex,
                   data = prepped_df, family = binomial(link="logit"))
summary(log_model_2)

exp(coef(log_model_2))

plot_model(log_model_2, 
           type = "pred", # provides estimated marginal effects predicted by the model (after accounting for effects of the other variables in the model)
           terms = "prevalentStroke",
           colors = c("darkorange", "purple3"),
           axis.title = c("History of Stroke", "Predicted Probability of Heart Disease"),
           legend.title = "Sex", line.size = 1.2,
           title = " ") + theme_apa() 

plot_model(log_model_2, 
           type = "pred", # provides estimated marginal effects predicted by the model (after accounting for effects of the other variables in the model)
           terms = c("prevalentHyp", "sex"),
           colors = c("darkorange", "purple3"),
           axis.title = c("History of Hypertension", "Predicted Probability of Heart Disease"),
           legend.title = "Sex", line.size = 1.2,
           title = " ") + theme_apa() 

#


# Poisson regression ------------------------------------------------------

# we only really have one count variable in our dataframe - cigsPerDay - so let's
# first create some new count variables using our data

prepped_df <- prepped_df %>% 
  mutate(high_bp_count = (sysBP > 140) + (diaBP > 90)) %>% 
  mutate(high_bmi = BMI > 30,
         high_glucose = glucose >= 100,
         high_chol = totChol >= 240) %>% 
  rowwise() %>% 
  mutate(lifestyle_risk = sum(c_across(c(currentSmoker, BPMeds, diabetes)) == 1),
         biometric_flags = sum(c_across(c(high_bp_count, high_bmi, high_glucose, high_chol))))  # as a measure of cumulative cardio risk

# check the dispersion of the newly added variables
# dispersion = variance/mean
# for Poisson, we want this to be close to 1

# dispersion_bp <- var(prepped_df$high_bp_count, na.rm = T)/mean(prepped_df$high_bp_count, na.rm = T)
# dispersion_bp # 1.17, should be OK to proceed with Poisson regression

dispersion_lifestyle <- var(prepped_df$lifestyle_risk, na.rm = T)/mean(prepped_df$lifestyle_risk, na.rm = T)
dispersion_lifestyle # 0.53, underdispersed

dispersion_biometric <- var(prepped_df$biometric_flags, na.rm = T)/mean(prepped_df$biometric_flags, na.rm = T)
dispersion_biometric # 1.12, should be OK to proceed with Poisson regression 

# let's see if there are any socioeconomic, demographic, or lifestyle factors that are related to the number of biometric flags

poisson_model_1 <- glm(biometric_flags ~ age * sex + education_class + cigsPerDay + diabetes, 
                       data = prepped_df,
                       family = poisson(link="log"))
summary(poisson_model_1)

plot_model(poisson_model_1, 
           type = "pred", # provides estimated marginal effects predicted by the model (after accounting for effects of the other variables in the model)
           terms = c("age", "sex"),
           colors = c("darkorange", "purple3"),
           axis.title = c("Age", "Number of Biometric Flags"),
           legend.title = "Sex", line.size = 1.2, 
           title = " ") + theme_apa() 
#


# Zero-inflated Poisson regression -----------------------------------------

# how can you tell if there are more 0's than expected?
# use a Vuong test to compare a Poisson model with a ZIP model -
# this test compares two non-nested models by evaluating the difference in predicted log-likelihoods for each observation
# if the outcome is significant and the z-stat is negative, it is better to use the ZIP model

hist(prepped_df$cigsPerDay)

# let's see if we can predict how many cigarettes people smoke per day from their age and sex

poisson_model_2 <- glm(cigsPerDay ~ age * sex, 
                       data = prepped_df,
                       family = "poisson")

zip_model_1 <- zeroinfl(cigsPerDay ~ age * sex | # models the Poisson process
                        age * sex,               # models the Bernoulli process
                        data = prepped_df,
                        dist = "poisson")

vuong(poisson_model_2, zip_model_1) # strongly supports the ZIP model over the Poisson model

# let's take a look at the ZIP output
summary(zip_model_1)

# looks like age and sex independently predict the number of cigarettes smoked,
# but there is an interaction between age and sex in predicting the zero-counts
# let's break this down a little bit:

# plot the predicted count values from the combined ZIP model (counts + zero-inflation)
# 'type = "response"' returns predictions on the original count scale
plot_predictions(zip_model_1, condition = c("age", "sex"), type = "response", colors = c("darkorange", "purple3")) + theme_classic()

# plot the zero-inflation component of the model only
# interpretation = likelihood/probability of not smoking any cigarettes as a function of age and sex
plot_predictions(zip_model_1, condition = c("age", "sex"), type = "zero") + theme_classic()

# plot the predicted count only (minus the zero-inflation)
# shows expected cigarette counts assuming the person is in the count process
plot(ggpredict(zip_model_1, terms = c("age", "sex"), type = "count")) + theme_classic()



