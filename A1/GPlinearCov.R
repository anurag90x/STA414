# function to load the data into matrices
training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}

# function to find the covariance betwween a particular training example and the other examples in the training set
covf <-function(xvector,trainingx)
{
  rows <- nrow(trainingx)
  response_variance <- 100^2 #variance of the response
  temp_vector = numeric(rows) #covariance of 1 training example with all other training examples
  for(j in 1:rows)
  {
    dot_product = response_variance*(xvector %*% trainingx[j,])+response_variance
    temp_vector[j] = dot_product 
  }
  return (temp_vector)
} 
  
# function to find the covariance matrix for a linear kernel
lin_cov <- function(x_values,covf,noise_variance)
{
  rows <- nrow(x_values)
  cov_kernel = matrix(NA,rows,rows)
 
  
  for ( i in 1:rows)
  {
    cov_kernel[i,] <- covf(x_values[i,],x_values)
  }

  #covariance function with noise variance
  cov_kernel <- diag(rows)*noise_variance+cov_kernel
  return (cov_kernel)
  
}

# function to estimate the predictive density based on the covariances of the responses
gp_predict <- function (x_train, y_train, x_test, covf)
{
  n <- nrow(x_train)
  
  C <- lin_cov(x_train,covf,1)
  b <- solve(C,y_train) #C-1y
 
  r <- numeric(nrow(x_test)) #the vector of 'mean' values for the different gaussians
  
  for (i in 1:nrow(x_test))
  {
    #compute covariance of test case against the training set
    k <- covf(x_test[i,],x_train)
    r[i] <- k %*% b # expected value of the conditional distribution for this test case
  }
  
  return (r)
}

find_square_error <- function(pred_y,test_y)
{
  error <- 0
  rows <- nrow(test_y)
  error <- (pred_y-test_y)^2
  return (mean(error))
  
  }


# load the data
training_1x <- training_data("train1x.txt")
training_1y <- training_data("train1y.txt")
testx <- training_data("testx.txt")
testy <- training_data("testy.txt")
pred_gpm_y <- gp_predict(training_1x,training_1y,testx,covf)
sq_error_lingpm <- find_square_error(pred_gpm_y,testy)