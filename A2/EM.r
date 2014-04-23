# Function to read the data from a file and return it as a matrix

training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}


# Function to perform the initial M step to calculate the starting values
# for the mixing prop. and the thetas.


set_initial_parameters <- function(trainx,trainy,seedval)
{
  set.seed(seedval)

  initialize_resp_mat(trainx,trainy)
  
  
  # each row of the r matrix coress. to the responsibility of each data item with the components.
  # For example, training instance 1 might match to digit 2 which would mean that it has non zero responsibilities for
  # r[1 ,11:15] and the other values are 0
  
  M_step(trainx)
}

# Function to run EM, according to a specified number of iterations

runEM <- function(trainx,trainy,num_iters)
{
  
  for(i in 1:num_iters)
  {
    cat("Next iteration of EM.. \n")
    calculateLogLikelihood(trainx,trainy) # calculate log likelihood
    E_step(trainx,trainy) # do for each training example
    parameters <- M_step(trainx) # update the parameters
  }
  
}

# Function to calculate the log likelihood for the data.


calculateLogLikelihood <- function(trainx,trainy)
{
  num_training <- nrow(trainx)
  log_likelihood <- 0
  for(i in 1:num_training)
  {
    label_value <- trainy[i] 
    indices<- as.numeric(unlist(get_start_end(label_value)))
    start<-indices[1]
    end<-indices[2]
    
    mixing_values_vec <- mixing_vector[start:end]
    prob <-0
  
    for(k in 0:(end-start))
    {
      theta_vec <- theta_matrix[start+k,]
      alt_theta_vec <- 1-theta_vec
      feature_product <- prod((theta_vec^trainx[i,])*(alt_theta_vec^(1-trainx[i,])))
      prob <- prob + mixing_values_vec[k+1]*feature_product
      
    }
    log_likelihood <- log_likelihood + log(prob)
    
    
  }
  penalty <- calculatePenalty()
  #cat("Without Penalty \n")
  #print (log_likelihood)
 # cat("With Penalty \n")
  #print (log_likelihood+penalty)
  return (log_likelihood+penalty)
}

# Function to calculate the log penalty function

calculatePenalty <- function()
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


E_step <- function(trainx,trainy)
{
  
  num_training <- nrow(trainx)
  for(i in 1:num_training)
  {
    label <- trainy[i]
    
    # the starting and ending indices corres. to the label specified
    indices<- as.numeric(unlist(get_start_end(label)))
    start<-indices[1]
    end<-indices[2]
    
    # vector of relevant mixing proportions.
    mixing_values_vec <- mixing_vector[start:end]
    #feature_product <-matrix(0,1,(end-start+1)) #5 component vector
    prob <- 0
    # loop through the range
    for(k in 0:(end-start))
    {
      theta_vec <- theta_matrix[start+k,]
      alt_theta_vec <- 1-theta_vec
      
      # the product of all the 196 sets of (theta(k,j))^x(i,j) * (1-theta(k,j))^(1-x(i,j))
      
      feature_product <- prod((theta_vec^trainx[i,])*(alt_theta_vec^(1-trainx[i,])))
      
      resp_matrix[i,start+k] <- mixing_values_vec[k+1]*feature_product
     
      
      prob <- prob + mixing_values_vec[k+1]*feature_product #marginal
    
      
    }
    # update the correct range in the responsibility matrix
    resp_matrix[i,] <<-  (resp_matrix[i,])/prob
    
    #resp_matrix[i,start:end] <<-  (mixing_values_vec*feature_product)/prob
  }
  
  return (resp_matrix)
  
  
}

# M step of EM. Update the parameters- theta and mixing props.

M_step<-function(trainx)
{
  #update parameter values
  update_mixing_prop(trainx)
  update_thetas(trainx)
  #return (list(mixing_vector,theta_matrix))
}

# Function to update the mixing proportions of each component
# This is computed using the same concept as in gaussian mixture
# models. By summing the responsibilities for all training examples
# across each component we get the mixing prop of each component.

update_mixing_prop <- function(trainx)
{
  num_tr <- nrow(trainx)
  mixing_vector <<- colSums(resp_matrix)
  mixing_vector <<- mixing_vector/num_tr
  #return (mixing_vector)
  
  
}


# Function to update the theta values. It extracts the responsibility
# across all training examples, and multiplies it with the entire training mat.
# to help compute the new values of theta.

update_thetas <- function(trainx)
{
  transpose_train <- t(trainx)#196x800 matrix
  
  
  for (k in 1:num_components)
  {
    
    resp_vector <- resp_matrix[,k] #a 800x1 vector for a particular component
    weighted_sum_vector <- alpha + transpose_train %*% resp_vector 
    denominator <- 2*alpha+sum(resp_vector)
    # add responsibilities for all training 
    # examples across that component
    theta_matrix[k,] <<- (weighted_sum_vector)/(denominator)
  }
  
  return (theta_matrix)
  
} 

# Predict the class of each test case example. This method returns the class
# probability matrix for each test case. It finds the responsibilites of each
# component for generating the data point and then adds the respective components
# to get the class probabilities for each point.

predictClass<-function(testx)
{
  num_tests <- nrow(testx)
  
  # responsibilites for each test case
  pred_resp <- matrix(NA,num_tests,num_components)
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
      feature_product[k] <- prod((theta_vec^testx[i,])*(alt_theta_vec^(1-testx[i,])))
      pred_resp[i,k]<-mixing_vector[k]*feature_product[k]#numerator
      prob <- prob + mixing_vector[k]*feature_product[k]
    }
    # find the resp. by dividing by the marginal
    pred_resp[i,]<-pred_resp[i,]/prob
    #each row of class_prob contains the total prob for each of the 10 classes
    class_prob[i,]<-unname(tapply(pred_resp[i,], (seq_along(pred_resp[i,])-1) %/% (num_components/num_classes), sum))
  }
  # class probability matrix
  return(class_prob)
}


# Function to average the prediction for different trials. Basically, compute
# the class probabilities for 10 different runs of EM, each seeded with a different value
# (the responsibility initialization part) and then add the matrices and average the values.
# Thus you get a matrix with the average class probabilities for teach training ex.
# and then find the max. prob. for each class.


averagePrediction<-function(testx)
{
  # seed values obtained from a uniform dist.
  seed_values <- runif(10,1,50)
  num_trials <- 10
  num_tests <- nrow(testx)
  class_prob <- matrix(0,num_tests,num_classes)
  pred_y <- matrix(NA,num_tests,1)
  
  for(i in 1:num_trials)
  {
    # set diff. init. values
    set_initial_parameters(trainx,trainy,seed_values[i])
    runEM(trainx,trainy,10)
    # add all the class probability matrices
    class_prob <- class_prob+predictClass(testx) 
    
  }
  # average it out
  class_prob <- class_prob/num_trials
  
  # predict based on which has the highest probability
  
  for(i in 1:num_tests)
  {
    max_class <- (which.max(class_prob[i,])-1)
    # because R indices start from 1. If max_class is 1 then the answer is 0 really
    pred_y[i] <-max_class
  }
  
  return(pred_y)
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


# Initialize the responsibility matrix at the beginning.
# Based on the label, we get the component indices and initialize the
# correct range of component values using the uniform dist. between 1 & 2.

initialize_resp_mat <- function(trainx,trainy)
{
  #set.seed(225)
  tr_rows <- nrow(trainx)
  for(i in 1:tr_rows)
  {
    label <- trainy[i] #corress. label for the given training case
    indices<- as.numeric(unlist(get_start_end(label)))
    start<-indices[1]
    end<-indices[2]
    resp_matrix[i,start:end] <<-runif(end-start+1,1,2)
    normalization <- sum(resp_matrix[i,])
    resp_matrix[i,start:end] <<-  resp_matrix[i,start:end]/normalization
  }
  
  
}
# Function that returns the starting and ending indices corres. to a particular 
# label value.

get_start_end<-function(label)
{
  start <- K*label+1
  end <- K*label+K
  return (list(start,end))
}

initialize_q <- function()
{
 
  digit_values <- c(0:num_digits-1) #digits 0 to 9
  for(i in 1:num_digits)
  {
    start <- K*digit_values[i]+1
    end <- K*digit_values[i]+K
    q_matrix[i,start:end] <<-1
    
  }
  
  # At this point, each q vector contains the 1s corressponding to the digit. So 1 corress. to q[1,6:10] is 1
  # whereas all the other values are 0
  
}


trainx <- training_data("trn.txt")
trainy <- training_data("trnlab.txt")

testx <- training_data("tst.txt")
testy <- training_data("tstlab.txt")

#num_digits <<- 10 #10 different digits
K <<- 5
num_features <<- ncol(trainx)
num_classes <<- 10
num_components <<- K*num_classes
alpha <<- 0.05

#q_matrix <<- matrix(0,num_digits,num_components)
resp_matrix <<- matrix(0,nrow(trainx),num_components)
theta_matrix <<- matrix(NA,num_components,num_features)
mixing_vector <<- matrix(0,1,num_components)


#set_initial_parameters(trainx,trainy,1,1)
#logll <- calculateLogLikelihood(trainx,trainy)
#runEM(trainx,trainy,1)

pred_classes <- averagePrediction(testx)
#pred_classes <- predictClass(testx) 
error <- find_error (pred_classes,testy)
