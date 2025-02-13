# Read the data
setwd("~/Library/CloudStorage/OneDrive-UvA/CLS/Experimental Design and Data Analysis/Assignment 1/EDDA_assignment1")
data <- read.table("crops.txt", header=TRUE)

# a
model = lm(Crops ~ County + Related, data=data); anova(model)
model_int = lm(Crops ~ County * Related, data=data); anova(model_int)