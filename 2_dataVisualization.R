## CCM R Workshop Session 1: Data visualization

# setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - June 2026/data/")

## 00. Load libraries ----------------------------------------------------

library(tidyverse)
library(patchwork) # combine plots into one figure

#

############################### VISUALIZATIONS ####################

# A) Visualize the data using base R -------------------------------------------------

# let's look at the distribution of the numeric variables in the sample
# use `hist(df$variable)`; it takes in a numeric input variable and produces a basic histogram
hist(prepped_df$age)  # age 
hist(prepped_df$cigsPerDay)  # cigarettes smoked per day
hist(prepped_df$heartRate)  # and so on

# what about our factors, like sex?
hist(prepped_df$sex) # this would throw an error because you can't plot the distribution of categorical variables

# use the `plot()` function to plot a basic bar chart displaying frequencies/counts
# the syntax is similar to before: `plot(df$variable)`
plot(prepped_df$sex)  # sex
plot(prepped_df$education_class)  # education

# `plot()` isn't great for visualizing continuous variables on their own
# try plotting the cigarettes smoked per day
plot(prepped_df$cigsPerDay) # looks like a mess

# but you can also look at two variables at once using `plot()`
# let's see the number of cigarettes smoked by males vs females
# the syntax here will include both of our variables: `plot(x = df$variable1, y = df$variable2)`
plot(x = prepped_df$sex, y = prepped_df$cigsPerDay) # basic boxplot 
plot(x = prepped_df$age, y = prepped_df$sysBP) # basic scatterplot 

#
# B) Distributions ------------------------------------

# let's plot the histogram for age again, but this time using ggplot; the basic syntax is below:
# ggplot(df, aes(x = "x variable", y = "y variable", group = "grouping variable")) +
#   geom_<chart_type>(stat = "statistic", na.rm = TRUE) + 
#   additional layers 

## age histogram
ggplot(prepped_df, aes(x = heartRate)) + # use `+` to add more layers to your plot 
  # we want the bars to be lightblue and the outline to be black
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +  
  # `binwidth` will change the width of each bar (e.g., 1 = bars will go up in 1 year increments)
  # `fill` will fill in the bars with the color indicated
  # `color` will outline the bars with the color indicated
  # you can also change the background, font, etc. of the plot using `theme_ABC`
  # type in `theme_` and press Tab to see your options; we will use theme_apa for now
  # set the font sizes to 20
  theme_apa(x.font.size = 20, y.font.size = 20) + 
  # you can change the axis labels as well 
  labs(x = "Heart Rate",
       y = "Count") # alternative syntax for changing axis labels: xlab() + ylab()

# you can plot a histogram and overlay it with a density plot as well, with some minor tweaks to the code above
ggplot(prepped_df, aes(x = heartRate)) +
  # change the y-axis to density instead of counts
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black") +
  # by default, `geom_density()` will just plot a density curve
  # let's fill the density curve and make it orange and 30% transparent so that we can see the bars underneath
  geom_density(fill = "orange", alpha = 0.3) +
  theme_apa(x.font.size = 20, y.font.size = 20) +
  labs(x = "Heart Rate",
       y = "Density") 

## Boxplot showing the distribution of BMI by sex 
ggplot(prepped_df, aes(x = sex, y = BMI, fill = sex)) +
  geom_boxplot(fill = "blue", alpha = .7, width = 0.36, notch = FALSE, # `notch = TRUE` can be used to compare the median between groups based on overlap
               outliers = TRUE, outlier.color = "black", outlier.shape = 1) + # you can choose to display and customize outliers
  labs(x = "Sex", y = "BMI") +
  theme_apa(x.font.size = 20, y.font.size = 20)

## Violin plot showing the same data as above
ggplot(prepped_df, aes(x = sex, y = BMI, fill = sex)) +
  geom_violin(fill = "orange", alpha = .7, width = 0.36, draw_quantiles = 0.5) + # just plotting the median line here 
  labs(x = "Sex", y = "BMI") +
  theme_apa(x.font.size = 20, y.font.size = 20)

# you can add points to show individual datapoints on top of the violin or box plot
ggplot(prepped_df, aes(x = sex, y = BMI, fill = sex)) +
  geom_violin(fill = "orange", alpha = .7, width = 0.36, draw_quantiles = 0.5) +
  geom_point(alpha = 0.1, color = "purple4", position = "jitter", show.legend = F) +
  labs(x = "Sex", y = "BMI") +
  theme_apa(x.font.size = 20, y.font.size = 20)

#

# C) Bar charts -----------------------------------------------------------

# if you want to see the average number of cigarettes smoked per day by those who did or did not develop CHD
ggplot(prepped_df, aes(x = TenYearCHD, y = cigsPerDay, fill = TenYearCHD)) +
  # `fill = variable` tells ggplot which variables should be colored differently 
  # use `geom_bar()` to create a barplot, since the x-variable is categorical
  geom_bar(stat = "summary", fun.data = mean_se, show.legend = F, color = "black") +
  # `stat = "summary"` provides a mean value of the y-variable for each x-variable group 
  # `fun.data = mean_se` tells it to calculate the group average
  # `show.legend` can be set to TRUE/T or FALSE/F  
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  # you can also add an error bar to the figure based on the standard error
  scale_fill_manual(values = c("purple2", "darkorange2")) +
  # you can change the colors of the bars as well by supplying them manually as above or by using a preset palette:
  # scale_fill_brewer(palette = "Dark2") +
  # you can explore color palettes offered by ggplot2 here: 
  # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), 
            vjust = -1.6, size = 5) +
  # you can add text labels to your figures using the same stat as the bars and errorbars 
  # you can adjust the position of the text using `vjust` (vertical) or `hjust` (horizontal)
  # and adjust the size of the text using `size`
  theme_apa(x.font.size = 20, y.font.size = 20) +
  labs(x = "CHD Diagnosis",
       y = "Mean Cigarettes Smoked per Day") 

# what if you want to see the same information as above, but separated by sex
# use `facet_wrap()` to subset the data by sex
ggplot(prepped_df, aes(x = TenYearCHD, y = cigsPerDay, fill = TenYearCHD)) +
  geom_bar(stat = "summary", fun.data = mean_se, show.legend = F, color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  scale_fill_manual(values = c("purple2", "darkorange2")) +
  theme_apa(x.font.size = 20, y.font.size = 20) +
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), 
            vjust = -1.6, size = 5) +
  labs(x = "CHD Diagnosis",
       y = "Mean Cigarettes Smoked per Day") +
  facet_wrap(~ sex)


#
# D) Scatter-plots ---------------------------------------------------------

# let's look at the relationship between numeric variables
# is age related to blood cholesterol levels?
ggplot(prepped_df, aes(x = age, y = totChol)) +
  # use `geom_point()` to create a scatterplot
  # `alpha = 0.5` sets transparency to 50%
  # `position = "jitter"` makes it so that the points are scattered a little around their true value
  geom_point(alpha = 0.2, color = "purple4", position = "jitter") +
  # add a line of best fit using `geom_smooth()`
  # `method = "lm"` fits a linear regression line
  geom_smooth(method = "lm", stat = "smooth", se = TRUE, level = 0.95, color = "darkolivegreen3") + 
  theme_apa(x.font.size = 20, y.font.size = 20) +
  labs(x = "Age",
       y = "Total Cholesterol") 

# you can show grouping within a single plot
ggplot(prepped_df, aes(x = age, y = totChol, color = sex)) +
  geom_point(alpha = 0.5, position = "jitter") +
  # scale_fill_manual(values = c("darkorange", "purple3")) +
  scale_color_brewer(palette = 'Dark2') +
  # geom_smooth(method = "lm", stat = "smooth", se = TRUE, level = 0.95, color = "darkolivegreen3") + 
  theme_apa(x.font.size = 20, y.font.size = 20) +
  labs(x = "Age",
       y = "Total Cholesterol") 

## you can also show the relationship between age and cholesterol levels separately for males and females
## and for those with and without CHD 
ggplot(prepped_df, aes(x = age, y = totChol)) +
  geom_point(alpha = 0.5, color = "purple4", position = "jitter") +
  geom_smooth(method = "lm", stat = "smooth", se = TRUE, level = 0.95, color = "darkolivegreen3") + 
  theme_apa(x.font.size = 20, y.font.size = 20) +
  labs(x = "Age",
       y = "Total Cholesterol") +
  # you can subset the data by more than one variable
  # for factors, you can assign labels of your own if the levels aren't very descriptive themselves
  facet_wrap(~ factor(sex, levels = c("F", "M"), labels = c("Female", "Male")) + 
               factor(TenYearCHD, levels = c(0, 1), labels = c("No CHD", "CHD")))

## check the levels of a variable using the `levels()` function
## e.g., levels(prepped_df$sex) or levels(prepped_df$TenYearCHD)

# E) Combining plots into a single figure ---------------------------------

## let's make a few plots and put them into the same figure 
# 1. education levels of males and females

plot_1 <- ggplot(prepped_df[!is.na(prepped_df$education_class),], ## this removes the NAs from the `education_class` variable
                 aes(x = education_class, fill = education_class)) + 
  geom_histogram(stat = "count", color = "black", na.rm = TRUE, show.legend = F) +  
  theme_apa(x.font.size = 14, y.font.size = 14) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Education",
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~sex)
plot_1

# 2. age and total cholesterol levels
plot_2 <- ggplot(prepped_df, aes(x = age, y = totChol)) +
  geom_point(alpha = 0.5, color = "purple4", position = "jitter") +
  geom_smooth(method = "lm", stat = "smooth", se = TRUE, level = 0.95, color = "darkolivegreen3") + 
  theme_apa(x.font.size = 14, y.font.size = 14) +
  labs(x = "Age",
       y = "Total Cholesterol")
plot_2  

# 3. blood pressure based on history of stroke and hypertension
plot_3 <- ggplot(prepped_df, aes(x = factor(prevalentStroke, levels = c(0, 1), labels = c("No Prior Stroke", "Prior Stroke")), 
                                 y = sysBP, fill = prevalentStroke)) +
  geom_bar(stat = "summary", fun.data = mean_se, show.legend = F, color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  scale_fill_manual(values = c("purple2", "darkorange2")) +
  theme_apa(x.font.size = 14, y.font.size = 14) +
  geom_text(stat = "summary", fun = mean, aes(label = round(..y.., 2)), 
            vjust = -0.5, size = 2.5) +
  labs(x = "History of Stroke",
       y = "CHD Diagnosis") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ factor(prevalentHyp, levels = c(0, 1), labels = c("No Hypertension", "Hypertension")))
plot_3


# combine the three plots
plot_4 <- plot_1 + plot_2 + plot_3
plot_4 ## this is the simplest way to combine the three plots - they will just sit next to each other

# use `/` to stack plots on top of each other and use `|` to place plots next to each other horizontally
plot_5 <- plot_1 / (plot_2 | plot_3) + 
  # add some annotations to your figure
  plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ")") # & theme(axis.title.y = element_blank()) 
  ## you can use the commented line above to remove the y-axis labels (or x-axis labels by changing to `axis.title.x`)
plot_5

