## CCM R Workshop Session 1: T-Tests

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")

## 00. Load libraries ----------------------------------------------------

library(tidyverse) # we will use this library, which comes with its own syntax
library(stats)

#

############################### T-TESTS ###########################
# A) One-way t-tests ------------------------------------------------------

# Are the number of cigarettes smoked by our sample different from 0? 
t.test(df$var)

# what about 10?
t.test(df$var, mu = mu)

# what if you want to test a directional hypothesis? 
# i.e., do people smoke more than 5 cigarettes per day, on average?
t.test(df$var, mu = mu, alternative = "greater")

#

# B) Two-way t-tests ------------------------------------------------------

# Do people with diabetes have higher blood glucose levels than those without diabetes?
my_ttest <- t.test(y ~ x, data = df)
my_ttest$p.value

#

# C) Visualize t-test results ---------------------------------------------

# one-way t-test
ggplot(prepped_df, aes(x = "", y = cigsPerDay)) +
  geom_bar(stat = "summary", fun = mean, fill = "skyblue", color = "black", width = 0.2) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.05) +
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), 
            vjust = -0.75, size = 5) +
  labs(x = "", y = "Cigarettes Smoked Per Day") +
  theme_apa(x.font.size = 22, y.font.size = 18)

# two-way t-test
# essentially plotting bar charts 
ggplot(prepped_df, aes(x = diabetes, y = glucose, fill = diabetes)) +
  geom_bar(stat = "summary", color = "black", show.legend = F) + # changing `color` will change the color of the bar outline
  theme_apa(x.font.size = 22, y.font.size = 18, legend.pos = "bottom", legend.font.size = 10) +
  labs(x = " ", y = "Blood Glucose Levels") +
  # add error bars 
  geom_errorbar(stat = "summary", width = 0.2) + # setting `stat = "summary"` will plot the standard error
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), 
            vjust = -1.6, size = 5) +
  # either (1) manually change the colors
  scale_fill_manual(values = c("purple2", "darkorange2")) + 
  # or (2) select from existing color palettes
  # scale_fill_brewer(palette = "Set2") + # you can explore more by typing `?scale_fill_brewer`
  # change the labels on the x-axis to be more descriptive 
  scale_x_discrete(labels = c("No Diabetes", "Diabetes"))

#