library(data.table)
library(ggplot2)

l <- 0.15  # The learning Rate lambda
w <- 1.00  # Initial assumption for weight
b <- 2.00  # Initial assumption for beta
i <- 1.00  # Input learning data
y <- 0.00  # Output Learning data

chart <- data.table(epoch = numeric(), cost = numeric())
epoch <- 1

# Define our Cost Function
costf <- function (a, y)
{ (a - y) ^ 2 / 2 }

while (epoch %% 30 != 0) {
  z     <- w * i + b
  a     <- z
  c     <- costf(a, y)
  chart <- rbind(chart, data.table(epoch = epoch, cost = c))
  
  dw    <- (a - y) * i
  db    <- a - y
  w     <- w - dw * l
  b     <- b - db * l
  epoch <- epoch + 1
}

ggplot(data = chart, aes(x = epoch, y = cost)) +
  geom_line() + theme_classic()

cat(sprintf("Cost:\t%+f\nWeight:\t%+f\nBias:\t%+f\n", c, w, b))
