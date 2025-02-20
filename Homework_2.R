#  Boxplot of Lung Cancer Deaths Distribution

library(ggplot2)

df <- read.csv('lung_cancer_prediction_dataset.csv')

country_deaths <- aggregate(Annual_Lung_Cancer_Deaths ~ Country, data = df, FUN = sum)

ggplot(country_deaths, aes(y = Annual_Lung_Cancer_Deaths)) +
  geom_boxplot() +
  labs(title = "Boxplot of Annual Lung Cancer Deaths by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_minimal()

#Histogram of PM2.5 AQI Values

library(ggplot2)

df <- read.csv('global_air_pollution_dataset.csv')

ggplot(df, aes(x=PM2.5_AQI_Value)) +
  geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) +
  ggtitle("Histogram of PM2.5 AQI Values") +
  xlab("PM2.5 AQI Value") +
  ylab("Frequency") +
  theme_minimal()

# Density Plot of the Lung Cancer Mortality Rate

mortality_rates <- subset(df, Mortality_Rate > 0)$Mortality_Rate

ggplot(data.frame(Mortality_Rate = mortality_rates), aes(x = Mortality_Rate)) +
  geom_density(fill = "skyblue2", alpha = 0.5) +
  labs(title = "Density Plot of Lung Cancer Mortality Rate")

#  Scatter Plot
library(ggthemes)
set.seed(123)
normal_data <- rnorm(100)
logistic_data <- rlogis(100)

df_random <- data.frame(Normal = normal_data, Logistic = logistic_data)

ggplot(df_random, aes(x = Normal, y = Logistic)) +
  geom_point(color = "brown") +
  theme_solarized(light = FALSE) +
  labs(title = "Scatter Plot of Normal vs. Logistic Distributions")
