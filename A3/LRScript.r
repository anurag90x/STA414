source("LR.r")

trnx <- training_data("a3trnx.txt")
trny <- training_data("a3trny.txt")

tstx <- training_data("a3tstx.txt")
tsty <- training_data("a3tsty.txt")

trnx_est <- trnx[1:1000,]
trnx_val <- trnx[1001:1300,]
trny_est <- trny[1:1000]
trny_val <- trny[1001:1300]

pca_vals <- c(40)

#sink("LRoutput.txt",append=TRUE)
for ( i in 1:length(pca_vals))
{
  
  cat("Using number of components - ")
  print (pca_vals[i])
  eigenvec_est <- pca.vectors(trnx_est,pca_vals[i])
  trnx_est_proj <- pca.proj(eigenvec_est,trnx_est)
  
  #eigenvec_val <- pca.vectors(trnx_val,pca_vals[i])
  trnx_val_proj <- pca.proj(eigenvec_est,trnx_val)
  
  tstx_proj <- pca.proj(eigenvec_est,tstx)
  
  
  lrfit_coeff <- fit_logistic(trnx_est_proj,trny_est)
  sink("coeff.txt")
  print(lrfit_coeff)
  sink()
  cat("Log likelihood for training \n")
  find_minus_log_likelihood(trnx_est_proj,trny_est,lrfit_coeff)
  cat("Log likelihood for validation\n")
  find_minus_log_likelihood(trnx_val_proj,trny_val,lrfit_coeff)
  cat("Log likelihood for test set\n")
  find_minus_log_likelihood(tstx_proj,tsty,lrfit_coeff)
  
  cat("Error for prediction ")
  prob_mat <- predict_class(lrfit_coeff,trnx_val_proj)
  error <- find_error(prob_mat,trny_val)
  print (error)
  
  cat("Error for test set prediction ")
  prob_mat_test <- predict_class(lrfit_coeff,tstx_proj)
  error_test <- find_error(prob_mat_test,tsty)
  print (error_test)
  
  
  
}
#sink()