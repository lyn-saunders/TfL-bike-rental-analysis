# install required libraries

library(car)
library(lmtest)

# manually written Levene's test 

levene_test <- function(aov_model){
  # extract the response variable and the grouping factor from the model
  response_variable <- model.response(model.frame(aov_model))
  grouping_factor <- model.frame(aov_model)[, 2]
  #calculate deviations
  deviations <- abs(response_variable - ave(response_variable, grouping_factor, FUN = mean))
  # create a new data frame for the ANOVA
  deviation_data <- data.frame(deviations = deviations, group = grouping_factor)
  # perform the ANOVA on the deviations
  levene_result <- aov(deviations ~ group, data = deviation_data)
  return(summary(levene_result))
  }

# upload the dataset
bike_sharing_2022 <- read.csv("/cs/home/iw72/Documents/R/Bike_sharing_2022.csv")

# split the dataset into seasons
season_data <- split(bike_sharing_2022, bike_sharing_2022$season)

# initialise an empty data frame to store the results
summary_stats <- data.frame(
  season = character(),
  mean_temperature = numeric(),
  sd_temperature = numeric(),
  mean_humidity = numeric(),
  sd_humidity = numeric(),
  mean_windspeed = numeric(),
  sd_windspeed = numeric(),
  mean_rentals = numeric(),
  sd_rentals = numeric(),
  stringsAsFactors = FALSE
)

## TASK 1 ##

# Conduct an appropriate exploratory analysis of the data that is informed by the questions posed in tasks 2 to 4 below.

# Calculate summary statistics for each season
for (season in names(season_data)) {
  data <- season_data[[season]]
  summary_stats <- rbind(summary_stats, data.frame(
    season = season,
    mean_temperature = mean(data$temperature, na.rm = TRUE),
    sd_temperature = sd(data$temperature, na.rm = TRUE),
    mean_humidity = mean(data$humidity, na.rm = TRUE),
    sd_humidity = sd(data$humidity, na.rm = TRUE),
    mean_windspeed = mean(data$windspeed, na.rm = TRUE),
    sd_windspeed = sd(data$windspeed, na.rm = TRUE),
    mean_rentals = mean(data$count, na.rm = TRUE),
    sd_rentals = sd(data$count, na.rm = TRUE)
  ))
}
print(summary_stats)

# Figure 1

# Scatter plot of temperature vs bike rentals
plot(bike_sharing_2022$temperature, bike_sharing_2022$count, 
     xlab = "Temperature (°C)", ylab = "Number of Rentals",
     main = "Bike Rentals vs Temperature", pch = 19, col = "pink")

# With a linear regression line
abline(lm(count ~ temperature, data = bike_sharing_2022), col = "black")

# Figure 2

# Boxplot of bike rentals by season
boxplot(count ~ season, data = bike_sharing_2022, 
        main = "Bike Rentals by Season", 
        xlab = "Season", ylab = "Number of Rentals", col = "pink")

# Average rental count on weekends and holidays
mean_weekend <- tapply(bike_sharing_2022$count, bike_sharing_2022$weekend, mean)
mean_holiday <- tapply(bike_sharing_2022$count, bike_sharing_2022$holiday, mean)
cat("Average Rentals on Weekends:", mean_weekend[2], "\n")
cat("Average Rentals on Weekdays:", mean_weekend[1], "\n")
cat("Average Rentals on Holidays:", mean_holiday[2], "\n")
cat("Average Rentals on Non-holidays:", mean_holiday[1], "\n")

# Figure 3

# Histogram of bike rentals 
hist(bike_sharing_2022$count, breaks = 30, 
     main = "Distribution of Bike Rentals", 
     xlab = "Number of Rentals", col = "pink", border = "black")

### TASK 2 ###

## Days with fewer than 35,000 rented bikes are not profitable for TfL. Therefore, the managing director of TfL asks you to do the following tasks a) - d); make sure to include interpretations of your analyses above that are useful for the target
audience.

# a) Calculate the expected proportion of days with fewer than 35,000 rented bikes
total_days <- nrow(bike_sharing_2022) 

# Count days with fewer than 35,000 rentals
days_with_fewer <- sum(bike_sharing_2022$count < 35000)
expected_proportion <- days_with_fewer / total_days
cat("Expected Proportion:", expected_proportion, "\n")

# b) Estimate a 90% confidence interval for the proportion found in part a)

# Calculate standard error of sample proportion
se_sample_proportion <- sqrt((expected_proportion*(1 - expected_proportion)) / total_days) 
z_95 <- qnorm(0.95)

# Calculate the limits for the confidence interval
upper_lim <- expected_proportion + (z_95 * se_sample_proportion)
lower_lim <- expected_proportion - (z_95 * se_sample_proportion)
confidence_int <- c(lower_lim, upper_lim)
cat("90% Confidence Interval:", confidence_int, "\n")

# c) Calculate the expected proportions of days with fewer than 35,000 rented bikes for each season
spring_days_with_fewer <- sum(bike_sharing_2022$count[bike_sharing_2022$season == "spring"] < 35000)
summer_days_with_fewer <- sum(bike_sharing_2022$count[bike_sharing_2022$season == "summer"] < 35000)
fall_days_with_fewer <- sum(bike_sharing_2022$count[bike_sharing_2022$season == "fall"] < 35000)
winter_days_with_fewer <- sum(bike_sharing_2022$count[bike_sharing_2022$season == "winter"] < 35000)

total_spring_days <- sum(bike_sharing_2022$season == "spring")
total_summer_days <- sum(bike_sharing_2022$season == "summer")
total_fall_days <- sum(bike_sharing_2022$season == "fall")
total_winter_days <- sum(bike_sharing_2022$season == "winter")

# Calculate the proportions and put in a vector
expected_proportions <- c(
  spring_days_with_fewer / total_spring_days,
  summer_days_with_fewer / total_summer_days,
  fall_days_with_fewer / total_fall_days,
  winter_days_with_fewer / total_winter_days
)

cat("Expected proportions for each season:", expected_proportions, "\n")

# d) Calculate whether there is a difference between the proportions in winter and spring?
observed_difference <- expected_proportions[4] - expected_proportions[1]
se_difference <- sqrt(((expected_proportions[4] * (1 - expected_proportions[4]) / total_winter_days)) + ((expected_proportions[1] * (1 - expected_proportions[1])) / total_spring_days))
print(se_difference)
t_stat <- observed_difference / se_difference

# Cwo proportions, two-tailed test chi^2 test
prop.test(x = c(25, 16), n = c(26, 21), alternative = "two.sided", correct = TRUE)

### TASK 3 ###

# Test whether the expected number of rented bikes varies across seasons. Interpret and explain your results. Furthermore, test whether there is a difference between the expected number of bikes rented on working days and weekends. Interpret and explain your results. In addition,
compute the power of the above test, assuming that the true difference is the one observed. For the observed sample size, what effect size (i.e., difference between the expected values) would be required to obtain a power of 80%? For the given effect size, what sample size would
be required to obtain a power of 80%? Explain the implications of your results for the target audience.

# ANOVA test to see if the number of rented bikes varies across seasons
anova_result <- aov(count ~ season, data = bike_sharing_2022)
anova_summary <- summary(anova_result)
print(anova_summary)

#Llevene test for assumption violation (equality of variances)
levene_test_result <- levene_test(anova_result)
print(levene_test_result)

#Tto test for violation of the normality of residuals assumption
residuals_anova <- residuals(anova_result)
shapiro_test <- shapiro.test(residuals_anova)
print(shapiro_test)

#Eextract the F-statistic from the ANOVA summary
f_statistic <- anova_summary[[1]]$"F value"[1]  # Correctly access the F value as a numeric value
print(f_statistic)

# Calculate p-value using the F-statistic
group_means <- tapply(bike_sharing_2022$count, bike_sharing_2022$season, mean)  # Get group means
p_value <- pf(f_statistic, df1 = length(group_means) - 1, df2 = nrow(bike_sharing_2022) - length(group_means), lower.tail = FALSE)
print(paste("P-value from F-test:", p_value))

# t-testing if the number of rented bikes differs between working days and weekends
t_test_result <- t.test(count ~ weekend, data = bike_sharing_2022)
print(t_test_result)

# Power analysis for t-test (compare working days vs weekends)
observed_difference <- mean(bike_sharing_2022$count[bike_sharing_2022$weekend == 1]) - mean(bike_sharing_2022$count[bike_sharing_2022$weekend == 0])
std_dev <- sd(bike_sharing_2022$count)

# Approximate power calculation 
n1 <- sum(bike_sharing_2022$weekend == 1)
n2 <- sum(bike_sharing_2022$weekend == 0)
pooled_sd <- sqrt(((n1 - 1) * var(bike_sharing_2022$count[bike_sharing_2022$weekend == 1]) + (n2 - 1) * var(bike_sharing_2022$count[bike_sharing_2022$weekend == 0])) / (n1 + n2 - 2))
t_value <- abs(observed_difference / (pooled_sd * sqrt(1/n1 + 1/n2)))

# Calculate the effect size for the t-test (observed_difference / pooled_sd)
effect_size <- observed_difference / pooled_sd

# Power calculation
power <- pt(t_value, df = n1 + n2 - 2, lower.tail = FALSE) * 2
print(paste("Power of the test:", power))

# Estimate the sample size required to obtain 80% power for the observed effect size
sample_size_for_80_power <- power.t.test(delta = observed_difference, 
                                         sd = pooled_sd, 
                                         power = 0.8, 
                                         sig.level = 0.05, 
                                         type = "two.sample")$n

print(paste("Sample size required for 80% power:", sample_size_for_80_power))

### TASK 4 ###

# Determine how the variables temperature, humidity, windspeed, season, weekend, and holiday affect the number of rented bikes. Interpret the estimated parameters of your model. Estimate the expected number that the TfL can expect to be rented on any given day, together
with 95% bounds, for parts a) - d):

# Treat the categorical variables as factors in the original dataset
bike_sharing_2022$season <- factor(bike_sharing_2022$season)
bike_sharing_2022$weekend <- factor(bike_sharing_2022$weekend, levels = c(0, 1))  
bike_sharing_2022$holiday <- factor(bike_sharing_2022$holiday, levels = c(0, 1))  

# Fit the multiple regression model (no longer used - keep as a comparison)
model <- lm(count ~ temperature + humidity + windspeed + season + weekend + holiday, data = bike_sharing_2022)
summary(model)

# Refit the model with interaction terms 
interaction_model <- lm(count ~ temperature * windspeed + humidity * windspeed + season + weekend + holiday, data = bike_sharing_2022)
summary(interaction_model)

# Perform stepwise selection to improve model
stepwise_interaction_model <- step(interaction_model, direction = "both")
summary(stepwise_interaction_model)

# Check if the assumptions of the new multiple linear regression model have been violated

# Breusch-Pagan test (for homoscedasticity assumption)
bp_test <- bptest(stepwise_interaction_model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)

# VIF test (forr multicolinearity)
vif_values <- vif(stepwise_interaction_model)
print("Variance Inflation Factor (VIF) values:")
print(vif_values)

# Durbin-Watson test (for autocorrelation)
dw_test <- dwtest(stepwise_interaction_model)
print("Durbin-Watson Test for Autocorrelation:")
print(dw_test)

# Perform Shapiro-Wilk test (for normality of residuals)
shapiro_test <- shapiro.test(residuals(stepwise_interaction_model))
print(shapiro_test)

# Predictions for the following scenarios

# a) A working day in spring with temperature 16°C, 8% humidity, 12 km/h windspeed
new_data_spring <- data.frame(
  temperature = 16,
  humidity = 8,
  windspeed = 12,
  season = factor("spring", levels = levels(bike_sharing_2022$season)),
  weekend = factor(0, levels = c(0, 1)),  # Consistent factor levels
  holiday = factor(0, levels = c(0, 1))   # Consistent factor levels
)

# Predict using the interaction model
pred_spring <- predict(stepwise_interaction_model, new_data_spring, interval = "confidence")
print("Spring working day prediction with interaction model:")
print(pred_spring)

# b) A holiday on a summer weekend with temperature 26°C, 30% humidity, 7 km/h windspeed
new_data_summer <- data.frame(
  temperature = 26,
  humidity = 30,
  windspeed = 7,
  season = factor("summer", levels = levels(bike_sharing_2022$season)),
  weekend = factor(1, levels = c(0, 1)),  # Consistent factor levels
  holiday = factor(1, levels = c(0, 1))   # Consistent factor levels
)

# Predict using the interaction model
pred_summer <- predict(stepwise_interaction_model, new_data_summer, interval = "confidence")
print("Summer holiday weekend prediction with interaction model:")
print(pred_summer)

# c) A working day in autumn with temperature 10°C, 85% humidity, 30 km/h windspeed
new_data_autumn <- data.frame(
  temperature = 10,
  humidity = 85,
  windspeed = 30,
  season = factor("fall", levels = levels(bike_sharing_2022$season)),
  weekend = factor(0, levels = c(0, 1)),  # Consistent factor levels
  holiday = factor(0, levels = c(0, 1))   # Consistent factor levels
)

# Predict using the interaction model
pred_autumn <- predict(stepwise_interaction_model, new_data_autumn, interval = "confidence")
print("Autumn working day prediction with interaction model:")
print(pred_autumn)

# d) A winter weekend (not a holiday) with temperature -1°C, 70% humidity, 16 km/h windspeed
new_data_winter <- data.frame(
  temperature = -1,
  humidity = 70,
  windspeed = 16,
  season = factor("winter", levels = levels(bike_sharing_2022$season)),
  weekend = factor(1, levels = c(0, 1)),  # Consistent factor levels
  holiday = factor(0, levels = c(0, 1))   # Consistent factor levels
)

# Predict using the interaction model
pred_winter <- predict(stepwise_interaction_model, new_data_winter, interval = "confidence")
print("Winter weekend prediction with interaction model:")
print(pred_winter)

#end of code#
