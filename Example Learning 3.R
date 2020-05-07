library(data.table)
library(ggplot2)

l <- 0.15  # The learning Rate lambda
i <- 1.00  # Input learning data
y <- 0.00  # Output Learning data

chart1 <- data.table(epoch = numeric(), cost1 = numeric())
chart2 <- data.table(epoch = numeric(), cost2 = numeric())
chart3 <- data.table(epoch = numeric(), cost3 = numeric())
chart4 <- data.table(epoch = numeric(), cost4 = numeric())

# Define our Cost Function which is now cross entropy
costf <- function (a, y){ 
  c <- y * log(a) + (1 - y) * log(1 - a)
  return(-c)
  }

logistic <- function (z){
  return(1/(1+exp(-z)))
}

# Run the iteration with the first set of weight and bias assumtions
w <- 0.0  # Initial assumption for weight
b <- 0.0  # Initial assumption for beta
epoch <- 1
while (epoch %% 100 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart1 <- rbind(chart1, data.table(epoch = epoch, cost1 = c))
  
  dw     <- (a - y) * i # Note the new formulae for cross entropy
  db     <- (a - y)
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the next set of weight and bias assumtions
w <- 1.00  # Initial assumption for weight
b <- 1.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 100 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart2 <- rbind(chart2, data.table(epoch = epoch, cost2 = c))
  
  dw     <- (a - y) * i 
  db     <- (a - y) 
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the next set of weight and bias assumtions
w <- 2.00  # Initial assumption for weight
b <- 2.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 100 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart3 <- rbind(chart3, data.table(epoch = epoch, cost3 = c))
  
  dw     <- (a - y) * i 
  db     <- (a - y) 
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}

# Run the iteration with the last set of weight and bias assumtions
w <- 3.00  # Initial assumption for weight
b <- 3.00  # Initial assumption for beta
epoch <- 1
while (epoch %% 100 != 0) {
  z      <- w * i + b
  a      <- logistic(z)
  c      <- costf(a, y)
  chart4 <- rbind(chart4, data.table(epoch = epoch, cost4 = c))
  
  dw     <- (a - y) * i 
  db     <- (a - y) 
  w      <- w - dw * l
  b      <- b - db * l
  epoch  <- epoch + 1
}


# Plot the three versions
ggplot() + 
  geom_line(data = chart1[cost1 < 4], aes(x = epoch, y = cost1), linetype=1) + 
  geom_line(data = chart2[cost2 < 4], aes(x = epoch, y = cost2), linetype=2) + 
  geom_line(data = chart3[cost3 < 4], aes(x = epoch, y = cost3), linetype=3) + 
  geom_line(data = chart4[cost4 < 4], aes(x = epoch, y = cost4), color = "red") + 
  theme_bw()


