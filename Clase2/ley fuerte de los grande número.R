
dados<- 1:6
roll <- function(n) {
  mean(sample(dados, size = n, replace = TRUE))
}

plot(sapply(1:1000, roll), type = "l", xlab = "# of dice", ylab = "average")
abline(h = 3.5, col = "red")
sapply(1:1000, roll)
mean(1:6)
