library(data.table)
library(ggplot2)

l <- 0.15  # The learning Rate lambda
i <- 1.00  # Input learning data
y <- 0.00  # Output Learning data

chart1 <- data.table(epoch = numeric(), cost1 = numeric())
chart2 <- data.table(epoch = numeric(), cost2 = numeric())
chart3 <- data.table(epoch = numeric(), cost3 = numeric())
chart4 <- data.table(epoch = numeric(), cost4 = numeric())

# Define our Cost Function
costf <- function (a, y)
{ (a - y) ^ 2 / 2 }

# Define our activation function
logistic <- function (z){
  return(1/(1+exp(-z)))
}

# Define the derivative of the activation function
logistic_gradient <- function (z){
  logistic(z) * (1 - logistic(z))
}


# Run the iteration with the first set of weight and bias assumtions
w <- 0.0  # Initial assumption for weight
b <- 0.0  # Initial assumption for beta
epoch <- 1
while (epoch %% 200 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart1 <- rbind(chart1, data.table(epoch = epoch, cost1 = c))
  
  dw     <- (a - y) * i * logistic_gradient(z)
  db     <- (a - y) *     logistic_gradient(z)
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the next set of weight and bias assumtions
w <- 1.00  # Initial assumption for weight
b <- 1.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 200 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart2 <- rbind(chart2, data.table(epoch = epoch, cost2 = c))
  
  dw     <- (a - y) * i * logistic_gradient(z)
  db     <- (a - y) *     logistic_gradient(z)
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the next set of weight and bias assumtions
w <- 2.00  # Initial assumption for weight
b <- 2.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 200 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart3 <- rbind(chart3, data.table(epoch = epoch, cost3 = c))
  
  dw     <- (a - y) * i * logistic_gradient(z)
  db     <- (a - y) *     logistic_gradient(z)
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the last set of weight and bias assumtions
w <- 3.00  # Initial assumption for weight
b <- 3.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 200 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart4 <- rbind(chart4, data.table(epoch = epoch, cost4 = c))
  
  dw     <- (a - y) * i * logistic_gradient(z)
  db     <- (a - y) *     logistic_gradient(z)
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}


# Plot the three versions to show the issue of vanishing gradient
ggplot() +
  geom_line(data = chart1, aes(x = epoch, y = cost1), linetype=1) + 
  geom_line(data = chart2, aes(x = epoch, y = cost2), linetype=2) + 
  geom_line(data = chart3, aes(x = epoch, y = cost3), linetype=3) + 
  geom_line(data = chart4, aes(x = epoch, y = cost4), color = "red") + 
  theme_bw()


