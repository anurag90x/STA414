source("pca.r")
training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}

fit_logistic <- function(trnx,trny)
{
  logistic <- glm(trny~trnx,family="binomial")
  return (logistic$coeff)
}

predict_class <- function(coeff,valx)
{
  validation <- cbind(1,valx) #add a column of 1s for the intercept term
  linear_mat <- coeff %*% t(validation)
  prob_mat <- 1/(1+exp(-linear_mat))
  return (prob_mat)
}

find_minus_log_likelihood <- function(trnx,trny,lrcoeff)
{ 
  mod_y <- 2*(trny)-1
  mod_trnx <- cbind(1,trnx) #add a column of 1s for the intercept term  
  linear_mat <- lrcoeff %*% t(mod_trnx)
  num_train <- nrow(trnx)
  log_ll <- 0
  

  for ( i in 1:num_train)
  {
    if (trny[i]==0)
    {
      prob_term <- 1/(1+exp(linear_mat[i]))
    }
    else
    {
      prob_term <- 1/(1+exp(-linear_mat[i]))
    }
    log_ll <- log_ll+log(prob_term)
  }
  print (-log_ll/length(trny))
  
}


find_error <- function(prob_mat,valy)
{
  class_pred <- round(prob_mat)
  error <- 0
  for( i in 1:length(valy))
  {
    if (class_pred[i]!=valy[i])
    {
      error <- error + 1
    }
  }
  return (error/length(valy))
}







