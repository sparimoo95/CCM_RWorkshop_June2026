## CCM R Workshop Session 2 Multi-level Models

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")

#
# Load libraries ----------------------------------------------------

# install.packages("tidyverse", "corrplot")
library(tidyverse) # we will use this library, which comes with its own syntax
library(stats)
library(sjPlot) # plot regression output
library(effects) # plot marginal effects
library(emmeans) # estimate marginal means
library(lmerTest) # model random effects
library(marginaleffects) # plot ZIP model output
#


# Import data -------------------------------------------------------------

job_sat_raw <- read.csv("msmr_nssjobsat.csv", header = TRUE, sep = ",")

#

# Data preparation --------------------------------------------------------

str(job_sat_raw)

job_sat_df <- job_sat_raw %>% 
  mutate(dept = as.factor(dept),
         payscale = as.factor(payscale),
         jobsat_binary = as.factor(jobsat_binary)) %>% 
  # add a column for employee ID, assumes each row = 1 employee 
  mutate(id = row_number()) %>% 
  # create a new column for income and populate it with values depending on payscale
  mutate(income = case_when(payscale == 5 ~ runif(n(), 30000, 40000),
                            payscale == 6 ~ runif(n(), 40000, 50000),
                            payscale == 7 ~ runif(n(), 50000, 60000),
                            payscale == 8 ~ runif(n(), 60000, 70000),
                            payscale == 9 ~ runif(n(), 70000, 80000),
                            payscale == 10 ~ runif(n(), 80000, 90000)))

str(job_sat_df)

#

# Linear regression -------------------------------------------------------

# let's look at the relationship between income and job satisfaction
# controlling for the National Student Satisfaction Rating (NSSRating)

fixed_effects_model <- lm(jobsat ~ income + NSSrating, data = job_sat_df)
summary(fixed_effects_model)

plot_model(fixed_effects_model, type = "pred", terms = "income") + theme_apa() 

# the significant effect means that the employees at a higher payscale report
# higher job satisfaction than the average job satisfaction of employees at lower payscales
###

# Random intercepts model -------------------------

# but each individual is nested within a department
# it is possible that employees in one department are systematically more satisfied
# with their job than employees in another department
# we can model this between-department variability using a random intercept for the department

random_int_model = lmer(jobsat ~ scale(income) + NSSrating + (1 | dept), data = job_sat_df)
summary(random_int_model)

# you can obtain the coefficients of your fixed effects and random effects together
coef(random_int_model)

# you can get the random and fixed effects coefficients separately
fixef(random_int_model) # coefficients for your fixed effects
ranef(random_int_model) # random intercept for each department

# plot fixed effects i.e., the average employee satisfaction within each payscale 
plot_model(random_int_model, type = "pred", terms = "income") + theme_apa() 

# plot random effects i.e., the average employee satisfaction within each department
plot_model(random_int_model, type = "re") + theme_apa()

# you can see that the job satisfaction rating is generally lower in some departments (red)
# than in other departments (blue)

# you can plot each random intercept too
random_intercept_model_pred_data <- expand.grid(income = seq(min(job_sat_df$income), max(job_sat_df$income), length.out = 100),
                                            dept = unique(job_sat_df$dept),
                                            NSSrating = mean(job_sat_df$NSSrating, na.rm = TRUE))

# Predict using the model (including random effects)
random_intercept_model_pred_data$predicted <- predict(random_int_model, newdata = random_intercept_model_pred_data, re.form = NULL)

# Plot lines for each department
ggplot(random_intercept_model_pred_data, aes(x = income, y = predicted, color = dept)) +
  geom_line(size = 1.1, show.legend = T) +
  labs(x = "Income", y = "Predicted Job Satisfaction", color = "Department") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10, legend.use.title = "Department")

#
# Random slopes model -----------------------------------------------------

# it is possible for the relationship between payscale and job satisfaction to differ between departments
# e.g., what if, in one department, higher payscale is related to lower job satisfaction?
# we can model this using a random slope

random_slope_model = lmer(jobsat ~ scale(income) + NSSrating + (scale(income) | dept), data = job_sat_df)
summary(random_slope_model) 

# you can obtain the coefficients of your fixed effects and random effects together
coef(random_slope_model)

# you can get the random and fixed effects coefficients separately
fixef(random_slope_model) # coefficients for your fixed effects
ranef(random_slope_model) # random slope for each level of the fixed effect, per department


# plot fixed effects i.e., the average employee satisfaction within each payscale 
plot_model(random_slope_model, type = "pred", terms = "income") + theme_apa() 

# plot random effects i.e., the average employee satisfaction within each department
plot_model(random_slope_model, type = "re", ci.lvl = 0.95, se = TRUE) + theme_apa()

# plot each random slope
random_slope_model_pred_data <- expand.grid(income = seq(min(job_sat_df$income), max(job_sat_df$income), length.out = 100),
                         dept = unique(job_sat_df$dept),
                         NSSrating = mean(job_sat_df$NSSrating, na.rm = TRUE))

# Predict using the model (including random effects)
random_slope_model_pred_data$predicted <- predict(random_slope_model, newdata = random_slope_model_pred_data, re.form = NULL)

# Plot lines for each department
ggplot(random_slope_model_pred_data, aes(x = income, y = predicted, color = dept)) +
  geom_line(size = 1.1, show.legend = T) +
  labs(x = "Income", y = "Predicted Job Satisfaction", color = "Department") +
  theme_apa(legend.pos = "bottom", legend.font.size = 8, legend.use.title = "Department")




