library(ggplot2)
cars <- read.csv("cars.csv")
cars_subset <- data.frame(
  driveline = cars$Engine.Information.Driveline,
  city_mpg = cars$Fuel.Information.City.mpg
)
#b
# Plot violin plots
ggplot(cars_subset, aes(x = driveline, y = city_mpg, fill = driveline)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "City MPG by Driveline",
       x = "Driveline",
       y = "City MPG")
#c
anova_result <- aov(city_mpg ~ driveline, data = cars_subset)
summary(anova_result)