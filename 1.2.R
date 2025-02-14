library(ggplot2)

# Read the data
setwd("~/Library/CloudStorage/OneDrive-UvA/CLS/Experimental Design and Data Analysis/Assignment 1/EDDA_assignment1")
data <- read.table("crops.txt", header=TRUE)
data$County <- factor(data$County)  
data$Related <- factor(data$Related, levels = c("no", "yes"))

# a

## Plot the data
ggplot(data, aes(x = County, y = Crops, fill = Related)) +
  geom_boxplot() +
  labs(title = "Crops by County and Related Factor",
       x = "County",
       y = "Crops") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Run ANOVA
model = lm(Crops ~ County + Related, data=data); anova(model); summary(model)
model_int = lm(Crops ~ County * Related, data=data); anova(model_int); summary(model_int)

## Check distribution of residuals
par(mfrow=c(1,2)); qqnorm(residuals(model))
plot(fitted(model), residuals(model))

qqnorm(residuals(model_int))
plot(fitted(model_int), residuals(model_int))

## Estimate the crops for a typical farm in County 3 for which landlord and tenant are not related

farm <- data.frame(County = factor(3, levels = levels(data$County)),
                   Related = "no")
predicted_crops <- predict(model, newdata = farm)
print(predicted_crops)