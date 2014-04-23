# Script using the Gaussian process regression functions from wk6funcs.r.

source("helperFunctions.r")
library("rbenchmark")
training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}


# Square covariance function

square_cov <- function (x1,X2, h)
{
  gamma <- h[2]
  rho <- h[3]
  result <- matrix(NA,1,nrow(X2))
  difference <- t(t(X2)-x1)
  rows<-nrow(difference)
  difference <- difference^2
  sqSums <- (rowSums(difference))*rho^2
  result<- 100^2 + gamma^2 * exp(-(sqSums))
 
  return (result)

}

# Absolute covariance function

absolute_cov<-function (x1,X2, h)
{
  gamma <- h[2]
  rho <- h[3]
  result <- matrix(NA,1,nrow(X2))
  difference <- t(t(X2)-x1)
  rows<-nrow(difference)
 
  absSums <- (rowSums(abs(difference)))*rho
  result<- 100^2 + gamma^2 * exp(-(absSums))
  

  return (result)
  
}
# Combined covariance function

combined_cov<-function (x1,X2, h)
{
  gamma1 <- h[2]  
  rho <- h[3]
  gamma2 <- h[4]
  
  absResult <- matrix(NA,1,nrow(X2))
  squareResult <- matrix(NA,1,nrow(X2))
  difference <- t(t(X2)-x1)
  rows<-nrow(difference)
  
  absSums <- (rowSums(abs(difference)))*rho
  absResult<- 100^2 + gamma1^2 * exp(-(absSums))
  
   difference <- difference^2
  sqSums <- (rowSums(difference))*rho^2
  squareResult<- 100^2 + gamma2^2 * exp(-(sqSums))
 
  
  return (absResult+squareResult)
  
}


find_square_error <- function(pred_y,test_y)
{
  error <- 0
  rows <- nrow(test_y)
  error <- (pred_y-test_y)^2
  return (mean(error))
  
  
}

cross_validation <- function(trainx,trainy,covf,hypers)
{
  hyperVals <- numeric(length(hypers))
  for (v in 1:10)
  {
    #cat("\nUsing cases",(v-1)*25+1,"to",v*25,"for validation set:\n")
    val.ix = ((v-1)*25+1):(v*25)
    print(val.ix)
    hyper <- gp_try_hypers(trainx,trainy,val.ix,covf,hypers)
    hyperVals <- hyperVals+hyper
  }
  hypers <- hyperVals/10
  print(hypers)
  hypers
    
}

gp_try_hypers <- function(trainx,trainy,val.ix,covf,hypers0, ...)
{
  model <- nlm(function(h) predict_values(trainx,trainy,val.ix,covf, h^2+0.01), 
               sqrt(hypers0-0.01),
               ... )
  
  cat ("Minimum square error :", model$minimum, "\n")
  
  model$estimate^2 + 0.01
}

predict_values<-function(trainx,trainy,val.ix,covf,hyperparam)
{
  x <-trainx[-val.ix,] #training
  y <- as.matrix(trainy[-val.ix])
  testx<-trainx[val.ix,] #cross validation set
  testy<-as.matrix(trainy[val.ix])
 
  predicted_y <- gp_predict (x, y, testx,covf, hyperparam)
  square_error <- find_square_error(predicted_y,testy)
  square_error
  
  
}

importance_sampling <-function(trainx,trainy,testx,testy,covf)
{
  set.seed(225)
  num_samples <- 1000
  
  noise_dist <- rnorm(num_samples,-1,1)
  gamma_dist <- rnorm(num_samples,0,1)
  gamma_dist2 <- rnorm(num_samples,0,1)
  rho_dist <- rnorm(num_samples,0,1)
  num_tests <- nrow(testx)
  
  net_pred_y <-matrix(NA,num_tests,1)
  hypers <- matrix(NA,1,4)
  likelihood <- matrix(NA,1,num_samples)
  trFactor <- matrix(NA,num_samples,nrow(trainx))
  
  for(j in 1:num_samples)
  {
    hypers[1] <- exp(noise_dist[j])
    hypers[2] <- exp(gamma_dist[j])
    hypers[3] <- exp(rho_dist[j])
    hypers[4] <- exp(gamma_dist2[j])
    likelihood[j] <- exp(gp_log_likelihood(trainx,trainy,covf, hypers))
    C <- gp_cov_matrix(trainx,covf,hypers)
   
    trFactor[j,] <- solve(C,trainy)
  }
  marginal <- rowSums(likelihood)
  print (marginal)
  #CovMatrix <- gp_cov_matrix(trainx,covf,hypers)
  
  for(i in 1:num_tests)
  {
    
    weighted_pred_sum <- 0
   # cat("Test number ",i,"\n")
    for(j in 1:num_samples)
    {
      hypers[1] <- exp(noise_dist[j])
      hypers[2] <- exp(gamma_dist[j])
      hypers[3] <- exp(rho_dist[j])
      hypers[4] <- exp(gamma_dist2[j])
      

      predicted_y <- gp_predict_average (trainx, trainy, testx[i,],trFactor[j,],covf, hypers)
      weighted_pred_sum <- weighted_pred_sum + predicted_y*likelihood[j]
    }
    net_pred_y[i] <- (weighted_pred_sum)/marginal
  }
  square_error <- find_square_error(net_pred_y,testy)
  return (square_error)
}

scale_data <- function(xvalues)
{
  xvalues[,1] <- xvalues[,1]/10
  xvalues[,7] <- xvalues[,7]/10
  xvalues
  
}

