# Functions file . Implements EM,loglikelihood, prediction and finding error
# methods
#
#





# Function to read the data from a file and return it as a matrix

training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}


# Function to perform the initial M step to calculate the starting values
# for the mixing prop. and the thetas.


set_initial_parameters <- function(trainx,trainy,alpha,K,num_components)
{
  seedval <- runif(1,1,50)
  
  # seed is a random value from the unif. dist.
  
  set.seed(seedval)
 
  # Initialize the responsibility matrix at the beginning.
  # Based on the label, we get the component indices and initialize the
  # correct range of component values using the uniform dist. between 1 & 2.
  
  tr_rows <- nrow(trainx)
  resp_matrix <- matrix(0,tr_rows,num_components)
  for(i in 1:tr_rows)
  {
    label <- trainy[i] #corress. label for the given training case
    
    indices<- as.numeric(unlist(get_start_end(K,label)))
    start<-indices[1]
    end<-indices[2]
    
    resp_matrix[i,start:end] <-runif(end-start+1,1,2)
    normalization <- sum(resp_matrix[i,])
    resp_matrix[i,start:end] <-  resp_matrix[i,start:end]/normalization
  }
  
  
  
  # each row of the r matrix coress. to the responsibility of each data item with the components.
  # For example, training instance 1 might match to digit 2 which would mean that it has non zero responsibilities for
  # r[1 ,11:15] and the other values are 0
  
  # Based on these first responsibilites, get an initial estimate of the parameters.
  
  param <- M_step(trainx,resp_matrix,alpha,num_components)
 
 # Return this initial estimate
 
  list("theta_matrix"=param$theta_matrix,"mixing_vector"=param$mixing_vector)
}




# Function to run EM, according to a specified number of iterations

runEM <- function(trainx,trainy,K,alpha,num_iters)
{
  
  # These are the initial values of the parameters obtained by the first M step 
  
  
  num_components <- K*10 # K * number of classes
  
  parameters <- set_initial_parameters(trainx,trainy,alpha,K,num_components)
  
  # initial values of the parameters obtained by running the M step, the first time.
  
  mixing_vec <- parameters$mixing_vector
  
  theta_mat <- parameters$theta_matrix 
  
 
  for(i in 1:num_iters)
  {
    #cat("Next iteration of EM.. \n")
    
    calculateLogLikelihood(trainx,trainy,mixing_vec,theta_mat,alpha,K,num_components) # calculate log likelihood
    
    resp_mat <- E_step(trainx,trainy,mixing_vec,theta_mat,K,num_components) # do for each training example
    
    parameters <- M_step(trainx,resp_mat,alpha,num_components) # update the parameters
    
    mixing_vec <- parameters$mixing_vector # get the mixing props.
    
    theta_mat <- parameters$theta_matrix # get the theta values
  }
  
 
  
  
  # The maximum likelihood estimates of the parameters are returned
  
  list("theta_matrix"=theta_mat,"mixing_vector"=mixing_vec,"resp_matrix"=resp_mat)
}

# Function to calculate the log likelihood for the data.


calculateLogLikelihood <- function(trainx,trainy,mixing_vector,theta_matrix,alpha,K,num_components)
{
  num_training <- nrow(trainx)
  log_likelihood <- 0
  for(i in 1:num_training)
  {
    label_value <- trainy[i] 
    
    indices<- as.numeric(unlist(get_start_end(K,label_value)))
    start<-indices[1]
    end<-indices[2]
    
    # relevant mixing prop. values
    
    mixing_values_vec <- mixing_vector[start:end]
    joint_prob <-0
  
    for(k in 0:(end-start))
    {
      theta_vec <- theta_matrix[start+k,]
      alt_theta_vec <- 1-theta_vec
      feature_product <- prod((theta_vec^trainx[i,])*(alt_theta_vec^(1-trainx[i,])))
      joint_prob <- joint_prob + mixing_values_vec[k+1]*feature_product
      
    }
    log_likelihood <- log_likelihood + log(joint_prob)
    
    
  }
  penalty <- calculatePenalty(alpha,theta_matrix)
  cat("Without Penalty \n")
  print (log_likelihood)
 
  cat("With Penalty \n")
  print (log_likelihood+penalty)
  
}

# Function to calculate the log penalty function

calculatePenalty <- function(alpha,theta_matrix)
{
  log_theta <- log(theta_matrix)
  log_alt_theta <- log(1-theta_matrix)
  
  penalty <- alpha*(sum(log_theta)+sum(log_alt_theta))
  return (penalty)
  
  
  
}

# Functin for the E step. Basically, calculate the values of all the responsibilities.
# based on the component values. Depending on the label, select the correct component
# range and then for each vector of theta in the matrix in that range, raise to the. training
# vector and do the 1-theta raised to 1-training vector. Multiply each of the pairs
# and then do a prod of the result to essentially multiply all 196 features in the binomial
# form, for that component. Do this for all of the k components relevant to the training
# example. Also, keep adding these values, to get the marginal. Divide in the end.


E_step <- function(trainx,trainy,mixing_vector,theta_matrix,K,num_components)
{
  
  num_training <- nrow(trainx)
  resp_matrix <- matrix(0,num_training,num_components)
  for(i in 1:num_training)
  {
    label <- trainy[i]
    
    # the starting and ending indices corres. to the label specified
    
    indices<- as.numeric(unlist(get_start_end(K,label)))
    start<-indices[1]
    end<-indices[2]
    
    # vector of relevant mixing proportions.
    mixing_values_vec <- mixing_vector[start:end]
    # loop through the range
    for(k in 0:(end-start))
    {
      theta_vec <- theta_matrix[start+k,]
      alt_theta_vec <- 1-theta_vec
      
      # the product of all the 196 sets of (theta(k,j))^x(i,j) * (1-theta(k,j))^(1-x(i,j))
      
      feature_product <- prod((theta_vec^trainx[i,])*(alt_theta_vec^(1-trainx[i,])))
      
      resp_matrix[i,start+k] <- mixing_values_vec[k+1]*feature_product
     
      
    
      
    }
    # update the correct range in the responsibility matrix
    
    resp_matrix[i,] <-  (resp_matrix[i,])/sum(resp_matrix[i,])
    
  }
  
  return (resp_matrix)
  
  
}

# M step of EM. Update the parameters- theta and mixing props.

M_step<-function(trainx,resp_matrix,alpha,num_components)
{
 #update parameter values
  
 mixing_vec <- update_mixing_prop(trainx,resp_matrix)
 theta_mat <- update_thetas(trainx,resp_matrix,alpha,num_components)
 list("theta_matrix"=theta_mat,"mixing_vector"=mixing_vec)
}




# Function to update the mixing proportions of each component
# This is computed using the same concept as in gaussian mixture
# models. By summing the responsibilities for all training examples
# across each component we get the mixing prop of each component.

update_mixing_prop <- function(trainx,resp_matrix)
{
  num_tr <- nrow(trainx)
  mixing_vector <- colSums(resp_matrix)
  mixing_vector <- mixing_vector/num_tr
  return (mixing_vector)
  
  
}


# Function to update the theta values. It extracts the responsibility
# across all training examples, and multiplies it with the entire training mat.
# to help compute the new values of theta.

update_thetas <- function(trainx,resp_matrix,alpha,num_components)
{
  transpose_train <- t(trainx)  # 196x800 matrix
  
  theta_matrix <- matrix(0,num_components,ncol(trainx))
  for (k in 1:num_components)
  {
    
    resp_vector <- resp_matrix[,k]  # a 800x1 vector for a particular component
    weighted_sum_vector <- alpha +( transpose_train %*% resp_vector )
    denominator <- 2*alpha+sum(resp_vector)
    
    # add responsibilities for all training 
    # examples across that component
    
    theta_matrix[k,] <- (weighted_sum_vector)/(denominator)
  }
  
  return (theta_matrix)
  
} 

# Predict the class of each test case example. This method returns the class
# probability matrix for each test case. It finds the responsibilites of each
# component for generating the data point and then adds the respective components
# to get the class probabilities for each point.

predictClass<-function(testx,mixing_vector,theta_matrix,num_components)
{
  num_tests <- nrow(testx)
  
  # responsibilites for each test case
  predictive_resp_mat <- matrix(NA,num_tests,num_components)
  # class prob. for each test case
  class_prob <- matrix(NA,num_tests,num_classes)
  pred_y <- matrix(NA,num_tests,1)
  for(i in 1:num_tests)
  {
    prob <- 0
    feature_product <- matrix(NA,1,num_components)
    for(k in 1:num_components)
    {
      #compute responsibility for each component
      theta_vec <- theta_matrix[k,]
      alt_theta_vec <- 1-theta_vec
      feature_product <- prod((theta_vec^testx[i,])*(alt_theta_vec^(1-testx[i,])))
      predictive_resp_mat[i,k]<-mixing_vector[k]*feature_product  #numerator
      prob <- prob + mixing_vector[k]*feature_product #marginal, across all components
    }
    # find the resp. by dividing by the marginal
    
    predictive_resp_mat[i,]<- predictive_resp_mat[i,]/sum( predictive_resp_mat[i,])
    
    #each row of class_prob contains the total prob for each of the 10 classes
    
    class_prob[i,]<-unname(tapply( predictive_resp_mat[i,], (seq_along(predictive_resp_mat[i,])-1) %/% (num_components/num_classes), sum))
  }
  # class probability matrix
  return(class_prob)
}




# Function that finds the max probability from the class prob matrix and uses
# it to make a prediction for each test case

make_prediction<-function(class_probability,testy)
{
  num_tests <- nrow(testy) 
  pred_y <- matrix(0,num_tests,1)
  for(i in 1:num_tests)
  {
    max_class <- (which.max(class_probability[i,])-1)
    # because R indices start from 1. If max_class is 1 then the answer is 0 really
    pred_y[i] <-max_class
  }
  return (find_error(pred_y,testy))
}


# Function to find the error between the predicted and actual values.
# Returns the percentage error


find_error<-function(pred_y,testy)
{
  error <- 0
  for(i in 1:nrow(testy))
  {
    if(pred_y[i,]!=testy[i,])
    {
      error <- error + 1
    }
  }
  
  return (error/nrow(testy))
    
}




# Function that returns the starting and ending indices corres. to a particular 
# label value.

get_start_end<-function(K,label)
{
  start <- K*label+1
  end <- K*label+K
  return (list(start,end))
}



