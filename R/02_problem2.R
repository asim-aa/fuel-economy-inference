library(ggplot2)
cars <- read.csv("cars.csv", check.names = TRUE)
df <- subset(cars,
             Engine.Information.Driveline %in% c("Front-wheel drive",
                                                 "Rear-wheel drive") &
               grepl("Manual|Automatic", Engine.Information.Transmission,
                     ignore.case = TRUE),
             select = c(Fuel.Information.City.mpg,
                        Engine.Information.Driveline,
                        Engine.Information.Transmission))
names(df) <- c("city_mpg","driveline","transmission")
df$trans_type <- ifelse(grepl("Manual", df$transmission, ignore.case = TRUE),
                        "Manual", "Automatic")
df$driveline <- factor(df$driveline,
                       levels = c("Front-wheel drive","Rear-wheel drive"))
df$trans_type <- factor(df$trans_type)
df <- na.omit(df)
ggplot(df, aes(driveline, city_mpg, fill = trans_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "City MPG by Driveline (blocked by Transmission)",
       x = "Driveline", y = "City MPG", fill = "Transmission") +
  theme_minimal()
fit <- lm(city_mpg ~ driveline + trans_type, data = df)
anova(fit)
summary(fit)