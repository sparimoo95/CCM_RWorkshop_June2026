## CCM R Workshop Session 1: Correlations

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")

## 00. Load libraries ----------------------------------------------------

# install.packages("tidyverse", "corrplot")
library(tidyverse) # we will use this library, which comes with its own syntax
library(stats)
library(corrplot)
library(ggpubr)
#

############################### CORRELATIONS ###########################

# A) Calculate correlations --------------------------------------

# just get the r to see whether age and sysBP are correlated
cor(x1, x2, use = "na.or.complete", method = "pearson")

# perform a correlation test to test for statistical significance
cor.test(x1, x2, use = "na.or.complete", method = "pearson")

# formula syntax: (we will re-visit this for other statistical tests)
# cor.test(~ df$x + df$y, use = "na.or.complete")
cor.test(~ x1 + x2, data = df, use = "na.or.complete")

# by default, `cor.test()` performs a pearson correlation
# you can also specify the type of correlation using the `method` argument
# other useful arguments include `conf.level` to specify the confidence level for the confidence intervals[

#
# B) Visualize correlations between pairs of variables -----------------------------------------------

ggplot(df, aes(x = x1, y = x2)) +
  # add a black correlation line with SE intervals around it using `geom_smooth()`
  geom_smooth(method = "lm", color = "grey3") +
  # `method` allows you to specify the smoothing method; we will use 'lm'
  # `se = TRUE` adds 95% confidence intervals around the line
  # you can also add individual datapoints to the figure using `geom_point()`
  geom_point(alpha = 0.5, color = "purple4", position = "jitter", size = 1) +
  theme_apa(x.font.size = 20, y.font.size = 20) + 
  xlab("Age") + ylab("Systolic BP") 

#

## CHALLENGE: 1. perform a correlation test to see if the number of cigarettes smoked per day is related to blood glucose levels
# 2. plot the correlation and show individual data points

#
# C) Create correlation plots -----------------------------------------------

# 1. Create a new data frame with just the numeric variables
# there are multiple ways to do this
# (i) using `select()` with tidyverse
numeric_vars_df1 <- prepped_df %>% 
  select(where(is.numeric)) # you can also input the actual column names here

# or base R
numeric_vars_df2 <- select(prepped_df, where(is.numeric))

# check that the two dataframes are the same
all.equal(numeric_vars_df1, numeric_vars_df2) # TRUE

# (ii) using `subset()` 
numeric_vars_df3 <- subset(prepped_df, select = c(age, cigsPerDay, totChol, sysBP, diaBP, BMI, heartRate, glucose))

all.equal(numeric_vars_df1, numeric_vars_df3) # TRUE

# (iii) by indexing columns 
numeric_vars_df4 <- prepped_df[ , c(2,5,10:15)]

all.equal(numeric_vars_df1, numeric_vars_df4) # TRUE 

# 2. Create a correlation matrix
corr_matrix <- cor(numeric_vars_df1, use = "na.or.complete") # `use =` is important so that observations containing NA are not included
corr_matrix

# 3. Create a correlation plot 
corrplot(corr_matrix) # default output; size and color of the circles indicates size of the correlation
corrplot(corr_matrix, method = "number") # show correlation values; number AND color both indicate size of the correlation
corrplot(corr_matrix, method = "color") # fills in the squares; color indicates size of the correlation

# more options are available when you type `?corrplot` into the console (or in the script)

