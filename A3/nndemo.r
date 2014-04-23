
source("neuralnet.r")
source("pca.r")
# Fit neural network model to data, and plot squared error and sum of
# absolute values of weights over training run.
training_data <- function(data)
{
  
  result <- read.table(data,head=FALSE)
  
  return(as.matrix(result))
}


find_pred_error <- function(class_pred,valy)
{
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

find_test_classes <- function(testx,weights,skel,testy)
{
  wl <- relist(weights,skel)
  lin_basis <- mlp_forward(testx,wl)
  log_ll <- find_log_likelihood(lin_basis$o,testy,0,wl) 
  cat("Log probability of test set \n")
  print(log_ll/length(testy))
  test_pred <- round(1/(1+exp(-1*lin_basis$o)))
  return (test_pred)
  
}



try_different_components<- function()
{
  
  m <- 10   # number of hidden units
  lambda <- 0.1
  error <- c(0,0,0)
  test_error <- c(0,0,0)
  max_iters <- 5000
  est_ll_mat <- matrix(NA,iters,1)
  val_ll_mat <- matrix(NA,iters,1)
  pca <- c(40)
  for(i in 1:length(pca))
  {
    eigenvec_est <- pca.vectors(trnx_est,pca[i])
    trnx_est_proj <- pca.proj(eigenvec_est,trnx_est)
    
    eigenvec_val <- pca.vectors(trnx_val,pca[i])
    trnx_val_proj <- pca.proj(eigenvec_est,trnx_val)
    
    tstx_proj <- pca.proj(eigenvec_est,tstx)
    
    skel <- mlp_skeleton(pca[i],m)
    
    # setting a seed value
    set.seed(200)
    
    
    iters <- c(1:max_iters)
    fit <- mlp_train (trny_est,trnx_est_proj,trnx_val_proj,trny_val, m, 0.001, length(iters),lambda)
    
    est_ll_mat <- fit$E
    val_ll_mat <- fit$E_val
    
    min_index <- which.min(fit$E_val) # Min. minus log ll for validation
    
    print (fit$E_val[min_index])
    
    fwval_best <- fit$P_val[min_index,]
    predictions <- round(1/(1+exp(-fwval_best)))
    
   
    test_pred<- find_test_classes(tstx_proj,fit$W[min_index,],skel,tsty)   
    
    
  
    error[i] <- find_pred_error(predictions,trny_val)
    test_error[i] <- find_pred_error(test_pred,tsty)
    
    
  }
  print (error)
  print (test_error)
  return (list("Est"=est_ll_mat,"Val"=val_ll_mat,"vale"=error,"teste"=test_error))
  
}

trnx <- training_data("a3trnx.txt")
trny <- training_data("a3trny.txt")


tstx <- training_data("a3tstx.txt")
tsty <- training_data("a3tsty.txt")

trnx_est <- trnx[1:1000,]
trnx_val <- trnx[1001:1300,]
trny_est <- trny[1:1000]
trny_val <- trny[1001:1300]


m <- 10 #number of hidden units
eta <- 0.001
pca <- c(40)
iters <- 400


eigenvec_est <- pca.vectors(trnx_est,40)
trnx_est_proj <- pca.proj(eigenvec_est,trnx_est)

#trnx_val_proj <- pca.proj(eigenvec_est,trnx_val)

log_ll_mats <- try_different_components()
#mlp <- mlp_train_checkgrad(trny_est, trnx_est_proj, m, eta, iters,0.01)
  


