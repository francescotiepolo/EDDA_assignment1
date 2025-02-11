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

# Mann-Whitney only if the data is independent.
# Permutations seems yes since does not assume normality