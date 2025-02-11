library(boot)

data <- read.table("cholesterol.txt", header = TRUE, sep = " ")

print(data)

# 1a

pdf("plots_1a.pdf")  

hist(data$Before, main = "Histogram of Before", col = "#191958")

hist(data$After8weeks, main = "Histogram of After 8 Weeks", col = "red")

qqnorm(data$Before, main = "Q-Q Plot of Before")
qqline(data$Before, col = "blue")

qqnorm(data$After8weeks, main = "Q-Q Plot of After 8 Weeks")
qqline(data$After8weeks, col = "red")

dev.off()

# 1b

t_paired <- t.test(data$Before, data$After8weeks, paired = TRUE)
t_unpaired <- t.test(data$Before, data$After8weeks, paired = FALSE)
#do we need this one? 
#var_test <- var.test(data$Before, data$After8weeks)
#print(var_test)

print(t_paired)
print(t_unpaired)

# The data is paired since the "Before" value and the "After 8 Weeks" value represent the same subject measured twice.
# Mann-Whitney only if the data is independent.
# Permutations seems yes since does not assume normality

# 1c

differences <- data$Before - data$After8weeks

# 97% CI based on normality
CI_n <- t.test(differences, conf.level = 0.97)$conf.int
print(CI_n)

# mean difference
boot_mean <- function(data, indices) {
  d <- data[indices]  # Resample with replacement
  return(mean(d))
}

# bootstrap with 10,000 resamples
boot_results <- boot(differences, boot_mean, R = 10000)

# 97% bootstrap CI
CI_b <- boot.ci(boot_results, type = "perc", conf = 0.97)
print(CI_b)