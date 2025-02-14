library(boot)

data <- read.table("cholesterol.txt", header = TRUE, sep = " ")

print(data)

# 1a

pdf("plots_1a.pdf")  

par(mfrow=c(1,2))
hist(data$Before, main = "Histogram of Before", col = "#191958")

hist(data$After8weeks, main = "Histogram of After 8 Weeks", col = "red")

qqnorm(data$Before, main = "Q-Q Plot of Before")
qqline(data$Before, col = "blue")

qqnorm(data$After8weeks, main = "Q-Q Plot of After 8 Weeks")
qqline(data$After8weeks, col = "red")

print(cor(data$Before,data$After8weeks))
cor.test(data$Before,data$After8weeks)
par(mfrow=c(1,1))
plot(data$Before~data$After8weeks); abline(lm(data$Before~data$After8weeks))

# 1b

t_paired <- t.test(data$Before, data$After8weeks, paired = TRUE)
wil_paired <- wilcox.test(data$Before, data$After8weeks, paired = TRUE)
#do we need this one? 
#var_test <- var.test(data$Before, data$After8weeks)
#print(var_test)

print(t_paired)
print(wil_paired)

boxplot(data$Before, data$After8weeks, 
        names = c("Before", "After 8 Weeks"), 
        main = "Boxplot of Cholesterol Levels", 
        col = c("blue", "red"))

# The data is paired since the "Before" value and the "After 8 Weeks" value represent the same subject measured twice.
# Mann-Whitney only if the data is independent, here it is paired, so not applicable.

# Permutations seems yes since does not assume normality
mystat=function(x,y) {mean(x-y)}
B=10000; tstar=numeric(B)
for(i in 1:B){
  datastar=t(apply(cbind(data$Before,data$After8weeks),1,sample))
  tstar[i]=mystat(datastar[,1],datastar[,2]) }
myt=mystat(data$Before,data$After8weeks)
print(myt)

hist(tstar, main="Permutation Test Distribution", col="lightblue", border="black")

lines(rep(myt, 2), c(0, max(hist(tstar, plot=FALSE)$counts)), col="red", lwd=2)
dev.off()

pl <- sum(tstar < myt) / B
pr <- sum(tstar > myt) / B
p <- 2 * min(pl, pr)

print(p)


# 1c
# 97% CI based on normality
CI_n <- t.test(data$After8weeks, conf.level = 0.97)$conf.int
print(CI_n)

# Bootstrap

B <- 1000
Tstar <- numeric(B)

# Bootstrap resampling
for (i in 1:B) {
  Xstar <- sample(data$After8weeks, replace = TRUE) 
  Tstar[i] <- mean(Xstar)
}

# Calculate the quantiles for the confidence interval
Tstar15 <- quantile(Tstar, 0.015)
Tstar985 <- quantile(Tstar, 0.985)

# Print the confidence interval
cat("Bootstrap 97% Confidence Interval:", Tstar15, "to", Tstar985, "\n")
