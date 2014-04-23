

training_data <- function(data)
{
 
  result <- read.table(data,head=FALSE)
 
  return(as.matrix(result))
}

# funciton to fit the linear model
fit_lm <- function(x_values,y_values)
{
  #model <-lm.fit(y_values~x_values[,1]+x_values[,2]+x_values[,3]+x_values[,4]+x_values[,5]+x_values[,6]+x_values[,7]+x_values[,8],data=censusinfo)
  model <-lm(y_values~x_values)  
  #summary(model)
  return (coef(model))
}

# function to estimate average square error
find_square_error <- function(coeff,test_x,test_y)
{
  error <- 0
  
  end_ind <- ncol(test_x)+1
  for(j in 1:nrow(test_x))
  {
    pred_y <- coeff[2:end_ind] %*% test_x[j,]+coeff[1]
    
    error <- (pred_y - test_y[j])^2+error
  }
  
  average_sq_error <- error/nrow(test_x)
  return (average_sq_error)
  
  
}

# load the data
training_1x <- training_data("train1x.txt")
training_1y <- training_data("train1y.txt")


training_2x <- training_data("train2x.txt")
training_2y <- training_data("train2y.txt")


testx <- training_data("testx.txt")
testy <- training_data("testy.txt")

sink("SimpleLinearModel.txt",append=TRUE)

cat("Linear model -TRAINING SET 1\n")

coeff <- fit_lm(training_1x,training_1y)
sq_error <- find_square_error(coeff,testx,testy)
print(sq_error)

cat("Linear model -TRAINING SET 2\n")



coeff_tr2 <- fit_lm(training_2x,training_2y)
sq_error_tr2 <- find_square_error(coeff_tr2,testx,testy)
print(sq_error_tr2)




cat("Linear model SQUARE BASIS -TRAINING SET 1\n")

# square basis functions
square_basis <- cbind(training_1x,training_1x^2)
coeff_sq <- fit_lm(square_basis,training_1y)
sq_testx <- cbind(testx,testx^2)
sq_error_sqbasis <- find_square_error(coeff_sq,sq_testx,testy)
print(sq_error_sqbasis)


cat("Linear model SQUARE BASIS -TRAINING SET 2\n")


square_basis_tr2 <- cbind(training_2x,training_2x^2)
coeff_sq_tr2 <- fit_lm(square_basis_tr2,training_2y)
sq_testx_tr2 <- cbind(testx,testx^2)
sq_error_sqbasis_tr2 <- find_square_error(coeff_sq_tr2,sq_testx_tr2,testy)
print(sq_error_sqbasis_tr2)


cat("Linear model SQUARE AND CUBE BASIS -TRAINING SET 1\n")


# cube basis functions
cube_basis <- cbind(training_1x,training_1x^2,training_1x^3)
coeff_cube <- fit_lm(cube_basis,training_1y)
cube_testx <- cbind(testx,testx^2,testx^3)
cube_error_sqbasis <- find_square_error(coeff_cube,cube_testx,testy)
print(cube_error_sqbasis)



cat("Linear model SQUARE AND CUBE BASIS -TRAINING SET 2\n")



cube_basis_tr2 <- cbind(training_2x,training_2x^2,training_2x^3)
coeff_cube_tr2 <- fit_lm(cube_basis_tr2,training_2y)
cube_testx_tr2 <- cbind(testx,testx^2,testx^3)
cube_error_sqbasis_tr2 <- find_square_error(coeff_cube_tr2,cube_testx_tr2,testy)
print(cube_error_sqbasis_tr2)

sink()

