The assignment details are outlined in the pdf

To run EM on the dataset use callEM.r. This file contains information to change the number of components to be used as well as the number of iterations to run EM and the number of trials(in case of averaging predictions across multiple EM runs)

K <- 5 # number of components per class
num_features <- ncol(trainx)
num_classes <- 10
num_components <- K*num_classes
num_iters <- 30 # the number of iterations for 1 trial of EM
alpha <- 0.05

The output will show the log likelihood and the log likelihood with the penalty alternatingly. The final output once that is over is the fractional error based on the predicted values for the test set and the actual values	
