# Gaussian process regression functions.


# CREATE THE COVARIANCE MATRIX FOR TRAINING RESPONSES.  The arguments are
# the matrix of training inputs, the covariance function (taking an input
# vector and a matrix as arguments), and the vector of hyperparameters.
# The first element in the hyperparameter vector is the residual 
# variance, which is added to the diagonal of the covariance matrix,
# but which is ignored by the covariance function.  The result is the
# matrix of covariances for responses in training cases.

gp_cov_matrix <- function (x_train, covf, hypers)
{
  n <- nrow(x_train)
  
  C <- matrix(NA,n,n)
  #C <- covf(x_train,x_train,hypers)
  for (i in 1:n)
  {
    C[i,] <-covf(x_train[i,],x_train,hypers)
  }
   
  
  
  #print(dim(C))
  diag(C) <- diag(C) + hypers[1]^2
  
  C
}


# PREDICT THE RESPONSE FOR TEST CASES.  The arguments are the matrix of
# inputs for training cases, the vector of responses for training cases,
# the matrix of inputs for test cases, and the covariance function and
# hyperparameter vector as described for gp_cov_matrix.  The result is
# a vector of predictions of the responses for test cases (the predictive
# mean).

gp_predict <- function (x_train, y_train, x_test, covf, hypers)
{
  n <- nrow(x_train)
  
  C <- gp_cov_matrix(x_train,covf,hypers)
  b <- solve(C,y_train)
  #print(dim(b))
  r <- numeric(nrow(x_test))
  k <- matrix(NA,1,nrow(x_train))
  for (i in 1:nrow(x_test))
  { 
    k<-covf(x_test[i,],x_train,hypers)#250 element vector
   
    r[i] <- k %*% b
  }
  
  return (r)
}
gp_predict_average <- function (x_train, y_train, x_test,trFactor,covf, hypers)
{
  n <- nrow(x_train)
 
  k <- matrix(NA,1,nrow(x_train))
   
  k<-covf(x_test,x_train,hypers)#250 element vector
  
  r<- k %*% trFactor
  
  
  return (r)
}


# FIND THE LOG LIKELIHOOD BASED ON RESPONSES IN THE TRAINING SET.
# The arguments are the matrix of inputs for training cases, the
# vector of responses for training cases, and the covariance function
# and hyperparameter vector as described for gp_cov_matrix.  The
# result is the log of the probability density for the training
# responses (with all normalizing factors included).

gp_log_likelihood <- function (x_train, y_train, covf, hypers)
{
  n <- nrow(x_train)
  
  C <- gp_cov_matrix(x_train,covf,hypers)
  #print (dim(C))
  b <- solve(C,y_train)
 
  #print (t(y_train)%*%b/2)
  - (n/2)*log(2*pi) - determinant(C)$modulus/2 - (t(y_train) %*% b)/2
}


# FIND HYPERPARAMETER VALUES MAXIMIZING LOG PROBABILITY.  Arguments
# are the matrix of training inputs, the vector of training responses,
# the covariance function (as for gp_cov_matrix), and a vector of
# initial values for the hyperparameters.  (Any additional arguments are
# passed on to the nlm function.) The result is a vector of values
# for the hyperparameters that should usually at least a locally maximize 
# the likelihood.
#
# The maximization is done in terms of the log likelihood, with hyperparameter 
# h transformed to sqrt(h-0.01) in order to avoid problems that would arise
# with hyperparameters that are negative or near zero (less than 0.01).

gp_find_hypers <- function (x_train, y_train, calc_cov, hypers0, ...)
{
  m <- nlm (
    function (h) - gp_log_likelihood(x_train,y_train,calc_cov, h^2+0.01), 
    sqrt(hypers0-0.01),
    ... )
  
  cat ("Maximum log likelihood:", -m$minimum, "\n")
  
  m$estimate^2 + 0.01
}