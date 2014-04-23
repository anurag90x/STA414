# Script file to call EM

source("EM2.r")

show_digit <- function (im)
{
  image (1:14, 1:14, t(apply (matrix(im,14,14,byrow=TRUE), 2, rev)),
         col=gray(seq(0,1,length=100)),
         zlim=c(0,1))
}


# Function to average the prediction for different trials. Basically, compute
# the class probabilities for 10 different runs of EM, each seeded with a different value
# (the responsibility initialization part) and then add the matrices and average the values.
# Thus you get a matrix with the average class probabilities for teach training ex.
# and then find the max. prob. for each class.


averagePrediction<-function(testx,trainx,trainy,alpha,K,num_iters)
{
  # seed values obtained from a uniform dist.
  num_trials <- 1
  num_tests <- nrow(testx)
  num_classes <- 10
  
  num_components <- num_classes*K
  class_prob <- matrix(0,num_tests,num_classes)
  pred_y <- matrix(NA,num_tests,1)
  error_vector <- matrix(NA,num_trials,1)
  
  
  for(i in 1:num_trials)
  {
    
    mle_params <- runEM(trainx,trainy,K,alpha,num_iters)
    # add all the class probability matrices
    trial_class_prob <- predictClass(testx,mle_params$mixing_vector,mle_params$theta_matrix,num_components)
    class_prob <- class_prob+trial_class_prob
    
    error_vector[i] <- make_prediction(trial_class_prob,testy)
    
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
  
  # error for each of 10 runs
  print(error_vector)
  return(pred_y)
}


trainx <- training_data("trn.txt")
trainy <- training_data("trnlab.txt")

testx <- training_data("tst.txt")
testy <- training_data("tstlab.txt")

K <- 5
num_features <- ncol(trainx)
num_classes <- 10
num_components <- K*num_classes
num_iters <- 30
alpha <- 0.05

#sink("testoutput3",append = TRUE)
pred_classes <- averagePrediction(testx,trainx,trainy,alpha,K,num_iters)
#sink()
error <- find_error (pred_classes,testy)