#EM algorithm for a guassian mixture model. 
#reads and converts data into a matrix.
matrix_m <- function (data)
{
  new_data <- read.table(data, head=FALSE)
  new_data <- as.matrix(new_data)
  return (new_data)
}


#function to help me index my matrix.
index_class <- function (label, k)
{
  index <- (label + 1: label + k)
  return (index)
}

#initializing the responsibility matrix.
res_matrix <- function (train_data, y, k)
{
  set.seed(225)
  cols_train <- ncol(train_data)
  rows_train <- nrow(train_data)
  new_matrix <- matrix(0, rows_train, (k %*% 10))
  for (i in 1:rows_train)
  {
    
    label <- y[i]
    k_start <- k * (y[i])+1 #start number dependent on class
    k_end <- k*y[i] + k #end number dependent on class
    new_matrix[i,k_start:k_end] <- runif(5, 1, 2)
    new_matrix[i,k_start:k_end] <-  new_matrix[i,k_1:k_end]/sum(new_matrix[i,k_1:k_end])
   
  }
  return (new_matrix)
} 

#initializing the theta matrix.
theta <- function (res, k, alpha)
{
  
  numerator <- alpha + (t(res) %*% train_x)
  denom <- (2*alpha + colSums(res))
  theta_mat <- numerator
  total <- k * 10
  
  for(l in 1:total)
  {
    theta_matrix[l,] <<- numerator[l,]/denom[l]
  }
  return (theta_matrix)
}


#new mixing proportions by using m step.
new_mixing_props <- function(train_x)
{
  num_train_x <- nrow(train_x)
  mixed_vector <- colSums(res_mat)
  mixing_prop <- (mixed_vector/num_train_x)
  return (mixing_prop)
}

#log likelihood function.
log_likelihood <- function(train_x, y, k_num, theta_mat)
{
  
  diff_theta <- 1 - (theta_mat)
  diff_train <- 1 - (train_x)
  k <- matrix(NA, 50, 1)
  mix_props <- new_mixing_props(train_x) #calling to get estimate mixing proportions
  log_likelihood <- 0
  for (i in 1:nrow(train_x))
  {
    summation <- 0
    label<-y[i]
    k_start <- k_num * (y[i]) + 1
    k_end <- k_num*y[i] + k_num
    for(k in 1:50)
    {
      if(k>k_start && k<k_end)
      {
      prod <- 1 #setting 'prod' or the binomial product summation.
      for(j in 1:196)
      {
        prod<- prod %*% (theta_mat[k,j]^train_x[i,j])*((1-theta_mat[k,j])^(1-train_x[i,j]))
      }
      summation <- summation + mix_props[k,]*prod
      
      }
        
    }
    log_likelihood<- log_likelihood + log(summation)
  }
  return (log_likelihood)
}

#function written to carry out the e-step.
e_step <- function(y, theta_mat, mix_props)
{
  k <- matrix(NA, 50, 1)
  
  for (i in 1:nrow(train_x))
  {
    summation <- 0
    label<-y[i]
    k_1 <- components * (y[i])+1
    k_end <- components*y[i]+components
    for(k in 1:50)
    {
      if(k>k_1 && k<k_end)
      {
        prod <- 1
        for(j in 1:196)
        {
          prod<- prod %*% (theta_mat[k,j]^train_x[i,j])*((1-theta_mat[k,j])^(1-train_x[i,j]))
        }
        summation <- summation + mix_props[k,]*prod
        res_mat[i,k] <- (prod/summation)
      }
    }
  }
  return (e_step)
}

#method to choose 5 components in a vector in order for use in the prediction function.
five_comps <- function(vector)
{
  for (i in 1:length(vector))
  {
    vec <- vector[5*(i):5*(i)+5]
    sum_vec <- sum(vec)
  }
  return(sum_vec)
}

#prediction function.
pred <- function(test_x, k)
{
  num_rows_test <- nrow(test_x)
  pred_matrix <- matrix(NA, num_rows_test, 1)
  pred_res <- matrix(NA, num_rows_test, k*10)
  class_prob <- matrix(NA, num_rows_test, 10)
  mix_vector <- matrix(0, 1, k*10)
  for (i in 1:num_rows_test)
  {
    prob <- 0
    feat_matrix <- matrix(NA, 1, k*10)
    for (k in 1:k*10)
    {
      theta_vector <- theta_mat[k,]
      diff_theta_vector <- 1 - (theta_vector)
      feat_matrix[k]<- prod((theta_vector^test_x[i,])*(diff_theta_vector^(1-test_x[i,])))
      pred_res[i,k] <- mix_vector[k] * feat_matrix[k]
      prob <- prob + mixed_vector[k]*feat_matrix[k]
    }
    
    pred_res[i,] <- pred_res[i,] / prob
    class_prob[i,] <- five_comps(pred_res[i,])
    
  }
  for (i in num_rows_test)
  {
    max_class <- (which.max(class_prob[i,])-1)
    pred_matrix[i,] <- max_class
                  
  }
  return (pred_matrix)
}


#finding the error. 
finding_error <- function(pred_y, test_y)
{
  error <- 0
  num_rows_test_y <- nrow(test_y)
  for (i in 1:num_rows_test_y)
  {
    if (pred_y[i,] != test_y[i,])
    {
      error <- error + 1
    }
  }
  return (error/num_rows_test_y)
}


#average prediction with number of trials set as 10.
avg_pred <- function(test_x, num_trials, k)
{
  new_matrix <- matrix(NA, num_trials, 1)
  for (i in 1:num_trials)
  {
    new_matrix[i,] <- pred(test_x, k)
  }
  sum <- rowSums(new_matrix)
  return (sum/num_trials)
}

#EM steps given the number of iterations.
EMalg <- function(train_x, y, components, k, alpha, num_iter)
{
  res <- res_matrix(train_x, y, 5)
  for (i in 1:num_iter)
  {
    theta_mat <- theta(res, k, alpha)
    mix_props <- new_mix_props (train_x)
    calculate_log_likelihood <- log_likelihood(train_x, y, k, theta_mat)
    res <- e_step (y, theta_mat, mix_props)
  }
}
  

#calls for retrieving data.
train_x <- matrix_m('trn.txt')
label_y <- matrix_m('trnlab.txt')
test_x <- matrix_m('tst.txt')
test_y <- matrix_m('tstlab.txt')


#res_mat <- res_matrix(train_x, label_y, 5)
#theta_mat <- theta (train_x, label_y, 5, 0.05)
#n_mix_props <- new_mixing_props(train_x)
ll <- log_likelihood(train_x, label_y, 5)
e_step <- e_step(train_x, label_y, 5)
#pred_k <- pred(test_x, 5)

