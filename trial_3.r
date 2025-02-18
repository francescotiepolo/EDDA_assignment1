library(MASS)
data <- npk

# 3a
randomized_data <- do.call(rbind, lapply(unique(data$block), function(b) {
  # Randomized treatments applied to 2 out of 4 plots
  N_treatments <- sample(rep(c(0,1), 2))  
  P_treatments <- sample(rep(c(0,1), 2))  
  K_treatments <- sample(rep(c(0,1), 2))  

  # Create a data frame for this block
  data.frame(Block = b, Plot = 1:4, N = N_treatments, P = P_treatments, K = K_treatments)
}))

randomized_data$yield <- npk$yield
print(randomized_data)

# 3b

boxplot(randomized_data$yield ~ randomized_data$N, col = c("red", "blue"),
        names = c("No Nitrogen", "Nitrogen"),
        main = "Yield Comparison: With & Without Nitrogen",
        xlab = "Nitrogen Treatment", ylab = "Yield")

# 3c
randomized_data$Block <- as.factor(randomized_data$Block)
randomized_data$N <- as.factor(randomized_data$N)
npkaov <- lm(yield ~ Block*N, data = randomized_data)
two_way_anova <- anova(npkaov)
print(two_way_anova)
# Friedman can be done because it does not assume normality, but since Block seems to be not significant, it would be an irrelevant test.