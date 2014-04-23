# Script using the Gaussian process regression functions from wk6funcs.r.

source("wk6funcs.r")

x <- matrix (c(  1,  2,  3,  4,  7,  8,  9), ncol=1)
y <-         c( 12,  9, 10,  4,  9, 11, 13)

plot (x, y, pch=20, xlim=c(0,10), ylim=c(0,15))


# Covariance function to use.  Takes a vector x1 (actually a scalar here)
# and a matrix X2 (with only one column here) as arguments, and returns 
# the vector of covariances of x1 with rows of X2.

covf <- function (x1, X2, h)
{
  eta <- h[2]
  rho <- h[3]
  
  as.vector (10^2 + eta^2 * exp(-((x1-X2)/rho)^2))
}

cat("\n")

h <- gp_find_hypers (x, y, covf, c(1,1,1))

print(h)

g <- matrix (seq(0,10,length=101), ncol=1)

p <- gp_predict (x, y, g, covf, h)

#lines (g, p)

cat("\n")

h2 <- gp_find_hypers (x, y, covf, c(0.2,3,0.2))

print(h2)

p2 <- gp_predict (x, y, g, covf, h2)

#lines (g, p2)

cat("\n")

h3 <- gp_find_hypers (x, y, covf, c(1,1,0.2))

print(h3)

p3 <- gp_predict (x, y, g, covf, h3)

lines (g, p3)