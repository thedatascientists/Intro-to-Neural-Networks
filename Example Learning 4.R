library(data.table)
library(ggplot2)

# The learning Rate lambda
l <- 0.15
# Initial assumption for weights
W <- matrix(c(0,0), nrow = 1)
# Initial assumption for beta
b <- 1
# Learning data to explain the nor gate
I <- matrix(c(0L,0L,0L,1L,1L,0L,1L,1L), nrow = 2)
# Output Learning data
y <- as.vector(c(1L,0L,0L,0L))  

chart <- data.table(epoch = numeric(), cost = numeric())
epoch <- 1

# Define our Cost Function but now using vector dot products
costf <- function (a, y) { 
  cost <- as.vector(a) - as.vector(y)
  cost <- cost %*% cost
  cost <- cost / 2
  return(as.numeric(cost))
}

ReLU <- function(z){
  z[z<0] <- 0
  return(z)
}

ReLU_Slope <- function(z){
  z[z<0] <- 0
  z[z>0] <- 1
  return(z)
}

# Create a vector version of bias with the same number of observation in the input
# n is the number of inputs
n <- dim(I)[2]
B <- rep(b,n)

while (epoch %% 125 != 0) {
  
  z     <- as.vector(W %*% I) + B
  a     <- ReLU(z)
  c     <- costf(a, y)
  chart <- rbind(chart, data.table(epoch = epoch, cost = c))
  
  dw1   <- (a-y) %*% (ReLU_Slope(z) * I[1,])
  dw2   <- (a-y) %*% (ReLU_Slope(z) * I[2,])
  db    <- (a-y) %*% (ReLU_Slope(z))
  W[1]  <- W[1] - dw1 * l
  W[2]  <- W[2] - dw2 * l
  b     <- b - db * l
  B     <- rep(b,n)
  epoch <- epoch + 1
}

ggplot(data = chart, aes(x = epoch, y = cost)) +
  geom_line() + theme_classic()

cat(sprintf("Cost:\t%+2.4f\nWeight:\t{%+2.2f , %+2.2f}\nBias:\t%+2.2f\n", c, W[1], W[2], b))
