# NEURAL NETWORK REGRESSION MODEL WITH ONE HIDDEN LAYER.

# The networks handled by these functions have one layer of hidden
# units, with tanh as the activation function.  The networks are used
# for predicting a real-valued response, modelled as having Gaussian
# noise. Hence maximum likelihood estimates can be found by minimizing
# squared error.
#
# The networks parameters (weights) are in four groups: w1 for weights
# on the input to hidden connections, used in computing the summed
# input into a hidden unit, w1_0 for the constants added to this sum,
# w2 for the weights on the hidden to output connections, used to
# compute the sum that is the output of the network, and w2_0 for the
# constant added this sum.
#
# The network parameters are sometimes stored as a list with named
# elements w1, w1_0, w2, and w2_0, and sometimes as a single vector
# containing all these parameters, in that order.  Conversion between
# these representations is done with R's unlist and relist functions.


# SKELETON FOR LIST OF NETWORK PARAMETERS.  Returns a skeleton suitable for
# use with "relist" for converting a vector of parameters for a network with
# p inputs and m hidden units to a list.

mlp_skeleton <- function (p,m)
{ list (w1=matrix(0,p,m), w1_0=rep(0,m), w2=rep(0,m), w2_0=0)
}


# FIT MLP NETWORK USING SIMPLE GRADIENT DESCENT.  The arguments are a vector
# of real responses for training cases, a matrix of inputs for training cases,
# the number of hidden units, the learning rate, and the number of iterations
# of gradient descent to do.  
#
# Note that the input variables and response variables should be scaled to
# have close to mean zero and standard deviation one, so that a single learning
# rate is roughly appropriate for all weights.
#
# The initial network weights are randomly chosen independently from Gaussian 
# distributions with mean zero and standard deviation 0.01, except that w2_0
# is set to the mean response in the training cases.
#
# The result returned is a list with element P giving the matrix of predictions
# for each iteration (rows) and training case (columns), element E giving the 
# average squared error for each iteration, and element W giving the network
# parameters for each iteration (rows) in vector form.  Note that these 
# results are for states after each iteration, with initial values not included.

mlp_train <- function (y, X,Xval,yval, m, eta, iters,lambda=0.01)
{
  n <- nrow(X)
  p <- ncol(X)
  
  skel <- mlp_skeleton(p,m)
  
  E <- rep(NA,iters)
  E_val <- rep(NA,iters)
  W <- matrix(NA,iters,length(unlist(skel)))
  P <- matrix(NA,iters,n)
  P_val <- matrix(NA,iters,nrow(Xval))
  validation_error <- matrix(NA,iters)
  
  w <- c (rnorm(length(unlist(skel))-1,0,0.01), mean(y))
  wl <- relist(w,skel)
  
  fw <- mlp_forward(X,wl)
  
  for (iter in 1:iters)
  { 
   # bk <- mlp_backward(y,X,wl,fw)
    bk <- mlp_backward(y,X,wl,fw)
    gr <- mlp_grad(X,skel,fw,bk,wl,lambda)
    
    w <- w - eta*unlist(gr)
    wl <- relist(w,skel)
    W[iter,] <- w
    
    fw <- mlp_forward(X,wl)
    fwval <- mlp_forward(Xval,wl)
    P[iter,] <- fw$o
    P_val[iter,] <- fwval$o
    E[iter] <- find_log_likelihood(fw$o,y,lambda,wl)
    E_val[iter] <- find_log_likelihood(fwval$o,yval,0,wl)
  }
  
  list (E=E, P=P, W=W,E_val=E_val,P_val=P_val)
}



mlp_train_checkgrad <- function (y, X, m, eta, iters,lambda=0.01)
{
  n <- nrow(X)
  p <- ncol(X)
  print(p)
  
  skel <- mlp_skeleton(p,m)
  
  E <- rep(NA,iters)
  W <- matrix(NA,iters,length(unlist(skel)))
  P <- matrix(NA,iters,n)
  
  w <- c (rnorm(length(unlist(skel))-1,0,0.01), mean(y))
  wl <- relist(w,skel)
  
  fw <- mlp_forward(X,wl)
  diff_mat <- matrix(NA,iters,length(w))
  
  for (iter in 1:iters)
  { 
    cat("Next iteration \n")
    grad_vec <- mlp_check_grad(y,X, skel, w, epsilon=0.001,lambda)
    gr <- grad_vec$grad
    w <- w - eta*unlist(gr)
    wl <- relist(w,skel)
      
  }
  
  list (E=E, P=P, W=W,Diff=diff_mat)
}

find_error <- function(values,valy)
{
  prob_mat <- 1/(1+exp(-values))
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



find_log_likelihood <- function(pred,trny,lambda,wl)
{ 
 
  num_train <- length(pred)
  log_ll <- 0

  
  for ( i in 1:num_train)
  {
    #print(pred[i])
    if (trny[i]==0)
    {
      prob_term <- 1/(1+exp(pred[i]))
    }
    else
    {
      prob_term <- 1/(1+exp(-pred[i]))
    }
    log_ll <- log_ll+log(prob_term)
  }
  
  #print (-log_ll)
  log_ll <- -log_ll+lambda*sum(wl$w1^2)
  #print (log_ll)
  return (log_ll)
  
}

# FORWARD NETWORK COMPUTATIONS.  Given the matrix of inputs and the network
# parameters in list form, this function returns a list with a matrix of hidden 
# unit inputs (s) for all training cases, a matrix of hidden unit values (h) for
# all training cases, and a matrix (with one column) of output unit values (o)
# for all training cases.

mlp_forward <- function (X, wl)
{
  n <- nrow(X)
  p <- ncol(X)
  
  s <- matrix(wl$w1_0,n,length(wl$w1_0),byrow=TRUE) + X %*% wl$w1
  h <- tanh(s)
  o <- h %*% wl$w2 + wl$w2_0
  
  list (s=s, h=h, o=o)
}


# BACKWARD NETWORK COMPUTATIONS.  Given the training responses, training inputs,
# network parameters in list form, and results of the forward computation, this
# function returns a list of matrices of partial derivatives with respect to 
# unit values, for each training case.

mlp_backward <- function (y, X, wl, fw)
{
  num_y  <- length(y)
  dE_do <- matrix(NA,num_y,1)
  for (i in 1:num_y)
  {
    temp<- (fw$o[i])
    
    if (y[i]==1)
    {
      dE_do[i]<- -(1/(1+exp(temp)))
        
      
    }
    else
    {      
      
      dE_do[i]<- (exp(temp)/(1+exp(temp)))
      
    }
    
  }
 
  dE_dh <- dE_do %*% wl$w2
  dE_ds <- (1-fw$h^2) * dE_dh
  
  list (dE_do=dE_do, dE_dh=dE_dh, dE_ds=dE_ds)
}


# COMPUTE GRADIENT OF LOG LIKELIHOOD WITH RESPECT TO PARAMETERS.  Takes
# as arguments the matrix of inputs, the skeleton for the parameter list, and 
# the results of the forward and backward computations.  Returns the gradient
# of the squared error in list form.

mlp_grad <- function (X, skel, fw, bk,wl,lambda)
{
  p <- ncol(X)
  gr <- skel
  gr$w2_0 <- sum (bk$dE_do)
  gr$w2 <- t(fw$h) %*% bk$dE_do
  gr$w1_0 <- colSums(bk$dE_ds)  
  
  
  for (j in 1:p)
  { gr$w1[j,] <- (X[,j] %*% bk$dE_ds)+2*lambda*wl$w1[j,]
  }
  
  
  gr
}


# CHECK THAT GRADIENTS ARE CORRECT.

mlp_check_grad <- function (y, X, skel, w, epsilon=0.0001,lambda)
{
  grd <- unlist(skel)
  print (length(w))
  
  for (i in 1:length(w))
  { 
    #print(i)
    w1 <- w
    w1[i] <- w1[i]-epsilon
    w2 <- w
    w2[i] <- w2[i]+epsilon
    
    o1 <-  mlp_forward(X,relist(w1,skel))$o
    E1 <- find_log_likelihood (o1,trny,lambda,relist(w1,skel))
    o2 <-  mlp_forward(X,relist(w2,skel))$o
    E2 <- find_log_likelihood (o2,trny,lambda,relist(w2,skel))
    
    
    grd[i] <- (E2-E1) / (2*epsilon)
  }
  
  wl <- relist(w,skel)
  fw <- mlp_forward (X, wl)
  bk <- mlp_backward (y, X, wl, fw)
  gr <- mlp_grad (X, skel, fw, bk,wl,lambda)
  #cat("Print whether the numbers are close to 0 \n")
  #print (sum(round(unlist(gr)-unlist(grd))==rep(0,length(w)),na.rm=TRUE)==length(w))
  sink("trueshit.txt",append=TRUE)
  
  print (sum((unlist(gr)-unlist(grd))<0.0001,na.rm=TRUE)==length(w))
  cat("\n")
  sink()
 # sink("diff.txt",append=TRUE)
  print(unlist(gr)-unlist(grd))
  #sink()
  
  
  
  list ("gradest"= (grd), "grad"=gr, "diff"= unlist(gr)-unlist(grd))
}