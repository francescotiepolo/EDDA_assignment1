library(ggplot2)
library(patchwork)

# Read the data
setwd("~/Library/CloudStorage/OneDrive-UvA/CLS/Experimental Design and Data Analysis/Assignment 1/EDDA_assignment1/Part 2")
data <- read.table("crops.txt", header=TRUE)
data$County <- factor(data$County)  
data$Related <- factor(data$Related, levels = c("no", "yes"))


# a

## Plot the data to observe interaction
interaction.plot(data$County, data$Related, data$Crops)

## Run ANOVA
model = lm(Crops ~ County + Related, data=data); anova(model); summary(model)
model_int = lm(Crops ~ County * Related, data=data); anova(model_int); summary(model_int)
anova(model_int, model)

## Check distribution of residuals
par(mfrow=c(1,2)); qqnorm(residuals(model))
plot(fitted(model), residuals(model))

## Estimate the crops for a typical farm in County 3 for which landlord and tenant are not related

farm1 <- data.frame(County = factor(3, levels = levels(data$County)),
                   Related = "no")
predicted_crops1 <- predict(model, newdata = farm1)
predicted_crops1


#b

## Check interaction county/related and size
p1 <- ggplot(data, aes(x=Size, y=Crops, color=County)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("Effect of Size on Crops by County")
p2 <- ggplot(data, aes(x=Size, y=Crops, color=Related)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("Effect of Size on Crops by Relatedness")
p1 + p2

## Model with size without any interaction
model_base_size <- lm(Crops ~ County + Related + Size, data=data)
anova(model_base_size); anova(model_base_size, model)

## Model with interaction between County and Size - BEST!!!
model_interact_county <- lm(Crops ~ Related + County * Size, data = data)
anova(model_interact_county); summary(model_interact_county)
anova(model_interact_county, model_base_size)

## Model with interaction between Related and Size
model_interact_related <- lm(Crops ~ County + Related * Size, data = data)
anova(model_interact_related); summary(model_interact_related)
anova(model_interact_related, model_base_size)


# c

## Explain these observations:
anova(model_interact_county); summary(model_interact_county)


# d
farm2 <- data.frame(County = factor(2, levels = levels(data$County)),
                      Size = 165,
                      Related = "yes")
predicted_crops2 <- predict(model_interact_county, farm2, interval="prediction")
predicted_crops2
