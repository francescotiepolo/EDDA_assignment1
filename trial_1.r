data <- read.table("cholesterol.txt", header = TRUE, sep = " ")

print(data)

pdf("plots_1a.pdf")  # Saves all following plots to a PDF file

hist(data$Before, main = "Histogram of Before", col = "#191958")

hist(data$After8weeks, main = "Histogram of After 8 Weeks", col = "red")

qqnorm(data$Before, main = "Q-Q Plot of Before")
qqline(data$Before, col = "blue")

qqnorm(data$After8weeks, main = "Q-Q Plot of After 8 Weeks")
qqline(data$After8weeks, col = "red")

dev.off()

