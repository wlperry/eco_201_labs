# install packages-----
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("readxl")
# install.packages("janitor")

# load libraries from packages -----
library(skimr)
library(readxl)
library(tidyverse)

# load the data file from the data subdirectory
reg.df <- read_excel("data/lab_2_paperweights.xlsx")

# now to look at the plot of the data
regression.plot <- reg.df |>  # the data to use called a dataframe
  ggplot(aes(x=mass_g , y=area_cm2)) + # aes is aesthetics and it is the x and y variables and x= y= does not need to be defined
  geom_point() + # adding a geometry of a point 
  geom_smooth(method="lm", se=FALSE) + # adding a geometry of a smoothed line as a linear model
  labs(title="Paperweight Mass vs Area", # add plot title
       x=" Mass (g)", # add x title label
       y="Area (cm^2)") # add y title label
regression.plot # call the plot to show

# save the regression plot ---
ggsave(regression.plot, file = "figures/regression_plot.pdf", 
       width = 4, height = 4, units = "in", dpi = 300)

# The linear model or linear regression ------
# now to get the formula of the line the is in blue
# Assuming reg.df is your data frame and linear.model is your lm object
linear.model <- lm(area_cm2 ~ mass_g, data = reg.df)  # Refit the model if necessary

# Extracting coefficients -0 the slope and intercept
coefficients <- coef(linear.model)
intercept <- coefficients[1]
slope <- coefficients[2]

# Create the line equation as a string
line_equation <- sprintf("y = %.4fx + %.4f", slope, intercept)

# Print the line equation
print(line_equation)

# now we have the linear model which allows us to transform mass into area
# y = 130.9774 (slope - how much area changes with a change in 1 unit of mass) * x + -0.0983 (y intercept)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Read in leaf trace masses ------
trace.df <- read_excel("data/trace_masses.xlsx")

# now we need to use the lineaer equation above to predict the 
# unknown area from the mass of the trace

# Mutate or create a new column of leaf area -----
trace.df <- trace.df |> 
  mutate(leaf_area_cm2 = slope * trace_mass_g + intercept)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Graph Leaf Areas -----
leaf_area.plot <- trace.df |> 
  ggplot(aes(x= leaf_type , y = leaf_area_cm2))+
  stat_summary(
    fun = mean, 
    geom = "point", 
    size = 3,    
    color = "red"
  ) +
  stat_summary( # Calculate and plot error bars for mean and SE
    fun.data = mean_se, 
    geom = "errorbar",  
    width = 0.2, 
    color = "blue"
  ) +
  labs(
    title = "Leaf Area by Type",
    x = "Leaf Type",
    y = "Area (cm^2)"
  )
leaf_area.plot

# save the regression plot ---
ggsave(leaf_area.plot, file = "figures/leaf_area_plot.pdf", 
       width = 4, height = 4, units = "in", dpi = 300)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Determine the mean and standard errors -----
mean_se.df <- trace.df |>
  group_by(leaf_type) |> 
  summarize(
    mean_area_cm2 = round(mean(leaf_area_cm2, na.rm = TRUE),2),
   std_err_area =  round(sd(leaf_area_cm2, na.rm = TRUE)/sqrt(n()),2)
  )

# write the file out to a tab delimited file
write_tsv(mean_se.df, file=("output/leaf_area_mean_se.tsv"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Test if means are significantly different with a T-Test ----
# Performing the t-test
t_test.model <- t.test(leaf_area_cm2 ~ leaf_type, data = trace.df)

# Viewing the results
print(t_test.model)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# If we wanted to visualize how our T value falls on a T Distribution we can -------
# Extract t-value and degrees of freedom
t_value <- t_test.model$statistic
df <- t_test.model$parameter

# Plotting the t-distribution
t_dist_df <- data.frame(x = seq(-4, 4, length.out = 100))
t_dist_df$y <- dt(t_dist_df$x, df)

# Assuming t_dist_df and df are already defined
# Calculate critical values for annotation
critical_left <- qt(0.025, df, lower.tail = TRUE)
critical_right <- qt(0.975, df, lower.tail = TRUE)
max_y <- max(t_dist_df$y) * 0.9  # Calculate the y-position for the annotations

ggplot(t_dist_df, aes(x = x, y = y)) +
  geom_line() +  # Plot t-distribution
  geom_vline(xintercept = critical_left, linetype = "dashed", color = "red") +
  geom_vline(xintercept = critical_right, linetype = "dashed", color = "red") +
  geom_vline(xintercept = t_value, color = "blue", linetype = "dashed") +
  annotate("text", x = critical_right + 0.2, y = max_y, 
           label = "Observation \nunlikely", hjust = 0, color = "red") +
  annotate("text", x = critical_right - 0.2, y = max_y, 
           label = "Might be \nexpected", hjust = 1, color = "blue") +
  annotate("text", x = t_value, y = max(t_dist_df$y), label = sprintf("t = %.2f", t_value), vjust = -1, color = "blue") +
  labs(title = "Visualization of t-value on t-Distribution",
       x = "t-value",
       y = "Density")

