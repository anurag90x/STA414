source("GPsquareExponentialCov.r")
library("rbenchmark")

# load the data
x <- training_data("train1x.txt")
y <- training_data("train1y.txt")
# load the data
x2 <- training_data("train2x.txt")
y2 <- training_data("train2y.txt")

testx <- training_data("testx.txt")
testy <- training_data("testy.txt")


sink("outputdata.txt",append=TRUE)

# Test 1 - Square error for square covariance function
hyperparam <- gp_find_hypers (x, y, square_cov, c(4,4,4))
hyperparam_tr2 <- gp_find_hypers (x2, y2, square_cov, c(4,4,4))

average_time_predicted <- benchmark(replications = 5,predicted_y <- gp_predict (x, y, testx,square_cov, hyperparam))
average_time_predicted_tr2 <- benchmark(replications = 5,predicted_y_tr2 <- gp_predict (x2, y2, testx,square_cov, hyperparam_tr2))

square_error <- find_square_error(predicted_y,testy)
square_error_tr2 <- find_square_error(predicted_y_tr2,testy)

cat("SQUARE COVARIANCE UNSCALED DATA \n")
print (square_error)
print (square_error_tr2)
print(average_time_predicted$elapsed/average_time_predicted$replications)
print(average_time_predicted_tr2$elapsed/average_time_predicted_tr2$replications)






# Test 1 - Square error for square covariance function

x<-scale_data(x)
x2<-scale_data(x2)
testx <- scale_data(testx)

hyperparam <- gp_find_hypers (x, y, square_cov, c(4,4,4))
hyperparam_tr2 <- gp_find_hypers (x2, y2, square_cov, c(4,4,4))

average_time_predicted <- benchmark(replications = 5,predicted_y <- gp_predict (x, y, testx,square_cov, hyperparam))
average_time_predicted_tr2 <- benchmark(replications = 5,predicted_y_tr2 <- gp_predict (x2, y2, testx,square_cov, hyperparam_tr2))

square_error <- find_square_error(predicted_y,testy)
square_error_tr2 <- find_square_error(predicted_y_tr2,testy)

cat("SQUARE COVARIANCE SCALED DATA \n")
print (square_error)
print (square_error_tr2)
print(average_time_predicted$elapsed/average_time_predicted$replications)
print(average_time_predicted_tr2$elapsed/average_time_predicted_tr2$replications)






# Test 1 - Square error for absolute covariance function
hyperparam_abs <- gp_find_hypers (x, y, absolute_cov, c(4,4,4))
hyperparam_abs_tr2 <- gp_find_hypers (x2, y2, absolute_cov, c(4,4,4))

average_time_predicted_abs <- benchmark(replications = 5,predicted_y_abs <- gp_predict (x, y, testx,absolute_cov, hyperparam_abs))
average_time_predicted_abs_tr2 <- benchmark(replications = 5,predicted_y_abs_tr2 <- gp_predict (x2, y2, testx,absolute_cov, hyperparam_abs_tr2))

square_error_abs <- find_square_error(predicted_y_abs,testy)
square_error_abs_tr2 <- find_square_error(predicted_y_abs_tr2,testy)

cat("ABSOLUTE COVARIANCE SCALED DATA \n")
print (square_error_abs)
print (square_error_abs_tr2)
print(average_time_predicted_abs$elapsed/average_time_predicted_abs$replications)
print(average_time_predicted_abs_tr2$elapsed/average_time_predicted_abs_tr2$replications)




# Test 1 - Square error for combined covariance function
hyperparam_combo <- gp_find_hypers (x, y, combined_cov, c(4,4,4,4))
hyperparam_combo_tr2 <- gp_find_hypers (x2, y2, combined_cov, c(4,4,4,4))

average_time_predicted_combined <- benchmark(replications = 5,predicted_y_combined <- gp_predict (x, y, testx,combined_cov, hyperparam_combo))
average_time_predicted_combined_tr2 <- benchmark(replications =5,predicted_y_combined_tr2 <- gp_predict (x2, y2, testx,combined_cov, hyperparam_combo_tr2))

square_error_combined <- find_square_error(predicted_y_combined,testy)
square_error_combined_tr2 <- find_square_error(predicted_y_combined_tr2,testy)


cat("COMBINED COVARIANCE SCALED DATA \n")
print (square_error_combined)
print (square_error_combined_tr2)
print(average_time_predicted_combined$elapsed/average_time_predicted_combined$replications)
print(average_time_predicted_combined_tr2$elapsed/average_time_predicted_combined_tr2$replications)


# Test 2 - Cross validation error 


average_time_cv <- benchmark(replications = 1,cross_validation_set <- cross_validation(x,y,square_cov,c(4,4,4)))
average_time_cv_tr2 <- benchmark(replications = 1,cross_validation_set_tr2 <- cross_validation(x2,y2,square_cov,c(4,4,4)))

average_time_cv_abs <- benchmark(replications = 1,cross_validation_abs <- cross_validation(x,y,absolute_cov,c(4,4,4)))
average_time_cv_abs_tr2 <- benchmark(replications = 1,cross_validation_abs_tr2 <- cross_validation(x2,y2,absolute_cov,c(4,4,4)))


average_time_cv_combined <- benchmark(replications = 1,cross_validation_combined <- cross_validation(x,y,combined_cov,c(4,4,4,4)))
average_time_cv_combined_tr2 <- benchmark(replications = 1,cross_validation_combined_tr2 <- cross_validation(x2,y2,combined_cov,c(4,4,4,4)))


cat("CROSS VALIDATION ERRORS \n")
print(average_time_cv$elapsed/average_time_cv$replications)
print (average_time_cv_tr2$elapsed/average_time_cv_tr2$replications)
print (average_time_cv_abs$elapsed/average_time_cv_abs$replications)
print (average_time_cv_abs_tr2$elapsed/average_time_cv_abs_tr2$replications)
print (average_time_cv_combined$elapsed/average_time_cv_combined$replications)
print (average_time_cv_combined_tr2$elapsed/average_time_cv_combined_tr2$replications)

# Test 3 Importance Sampling

average_time_samp <- benchmark(replications = 1, sample_sq_error <- importance_sampling(x,y,testx,testy,square_cov))
average_time_samp_tr2 <- benchmark(replications = 1, sample_sq_error_tr2 <- importance_sampling(x2,y2,testx,testy,square_cov))

average_time_samp_abs <- benchmark(replications = 1, sample_abs_error <- importance_sampling(x,y,testx,testy,absolute_cov))
average_time_samp_abs_tr2 <- benchmark(replications = 1, sample_abs_error_tr2 <- importance_sampling(x2,y2,testx,testy,absolute_cov))

average_time_samp_combo <- benchmark(replications = 1, sample_comb_error <- importance_sampling(x,y,testx,testy,combined_cov))
average_time_samp_combo_tr2 <- benchmark(replications = 1, sample_comb_error_tr2 <- importance_sampling(x,y,testx,testy,combined_cov))


cat("Square error values for sampling \n ")
print(sample_sq_error)
print(sample_sq_error_tr2)
print(sample_abs_error)
print(sample_abs_error_tr2)
print(sample_comb_error)
print(sample_comb_error_tr2)


cat("Importance Sampling Errors for 1000 points \n")
print(average_time_samp$elapsed/average_time_samp$replications)
print (average_time_samp_tr2$elapsed/average_time_samp_tr2$replications)
print (average_time_samp_abs$elapsed/average_time_samp_abs$replications)
print (average_time_samp_abs_tr2$elapsed/average_time_samp_abs_tr2$replications)
print (average_time_samp_combo$elapsed/average_time_samp_combo$replications)
print (average_time_samp_combo_tr2$elapsed/average_time_samp_combo_tr2$replications)


sink()