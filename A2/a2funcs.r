show_digit <- function (im)
{
  image (1:14, 1:14, t(apply (matrix(im,14,14,byrow=TRUE), 2, rev)),
         col=gray(seq(0,1,length=100)),
         zlim=c(0,1))
}



do_em <- function (images, labels, K, alpha, iterations)
{

#	set.seed(5)	
	frequencies <- matrix(0, 10,1)

	for (i in (1:length(labels)))
	{
		value <- labels[i]+1
		frequencies[value] = frequencies[value] + 1;
	}

	frequencies <- frequencies/nrow(labels)

	q <- matrix (0, K*10, 10)

	for (i in (1:10))
	{	
		index <- (i-1)*K+1;
		q[index:(index+K-1),i] <- 1

	}

	r <- matrix(NA, nrow(images), K*10)

	for (i in (1:nrow(r)))
	{
    
		r[i,] = runif(K*10, 1, 2)
		y <- labels[i]
		r[i,] = r[i,] * q[,(y+1)]
		r[i,] = r[i,]/sum(r[i,])
	}

	pi <- matrix(NA, K*10, 1)
	theta <- matrix(NA, K*10, ncol(images))
  
	
  #print (r)
	for (t in (1:iterations))
	{

    
	  #M Step
	  for (k in (1:(K*10)))
	  {
	    sum_Rk <- sum(r[,k])
	    pi[k] = sum_Rk/nrow(r)
	    
	    for (j in (1:ncol(images)))
	    {
	      theta[k, j] = (alpha + r[,k] %*% images[,j])/(2*alpha + sum_Rk)
	    }
	    
	  }

		#E step
		for (i in (1:nrow(images)))
		{

			y <- labels[i]

			for (k in (1:(K*10)))
			{
				if (q[k,(y+1)]==1)
				{
					temp <- prod(theta[k,]^(images[i,]) * (1-theta[k,])^(1-images[i,]))
					r[i,k] <- pi[k] *exp(sum(log(temp)))
				}
				
			}

			r[i,] <- r[i,] / sum(r[i,])
		}
    
		#print (pi)
		likelihood <- log_likelihood(images, labels, K, pi, q, theta, r, alpha)
		cat ("\nlikelihood: ")
		print(likelihood$likelihood)
		cat ("likelihood with penalty: ")
		print(likelihood$likelihood_with_penalty)
		



	}

 list("theta"=theta, "pi" = pi, "r"=r, "frequencies" = frequencies, "q" = q)

}

predict <- function (images, labels, K, pi, q, theta, frequencies)
{
	mix_prob <- matrix(NA, K*10, 1)
	class_prob <- matrix(NA, 10, 1)
	predictions <- matrix(NA, 10, 1)
	results <- matrix (NA, nrow(images), 1)

	for (j in (1:nrow(images)))
	{
		for (k in (1:(K*10)))
		{
			temp <- theta[k,]^(images[j,]) * (1-theta[k,])^(1-images[j,])
			mix_prob[k] <- pi[k] *exp(sum(log(temp)))
		}

		for (i in (1:10))
		{
			class_prob[i] <- sum(mix_prob*q[,i])
		}

		norm <- t(frequencies) %*% class_prob

		for (i in (1:10))
		{
			predictions[i] <- frequencies[i]*class_prob[i]/norm
		}

		res <- order(predictions)
		results[j] <- res[length(res)]-1
	}

	write.table(results, "result.txt")
	results
}




log_likelihood <- function(images, labels, K, pi, q, theta, r, alpha)
{

	likelihood_matrix <- matrix (0, nrow(images), 1)
	penalty <- 0

	for (i in (1:nrow(images)))
	{
		y <- labels[i]

		for (k in (1:(K*10)))
		{
			if (q[k,(y+1)]==1)
			{

			   temp <- prod(theta[k,]^(images[i,]) * (1-theta[k,])^(1-images[i,]))
         # this is the change really. You were adding the log values of each mix. component and then log of temp.
         # But that is essentially, then adding the logs of the (pi*probability) for the 5 components as well.
         # But you really want to find the log of the sum of the 5 components. Also, you were multiplying the responsibility
         # what that is, is then really the expectation of the log probability wrt the responsibilities. What you want is the log
         # of the joint probability of the data (which is the marginal along the components)
			   likelihood_matrix[i] <- likelihood_matrix[i] + pi[k]*temp
			}

		}
		likelihood_matrix[i] <- log( likelihood_matrix[i])

	}


	for (k in (1:(K*10)))
	{
		penalty <- penalty + sum(log(theta[k,])) + sum (log(1-theta[k,]))
	}

	likelihood <- sum(likelihood_matrix)

	list ("likelihood" = likelihood,  "likelihood_with_penalty" = likelihood+alpha*penalty)

}

err_calc <- function (trainx, trainy, testx, testy)
{
	alpha <- 0.05
	K <- 5
  # i just did this here, because i wasnt sure what your error thing was doing from the other script.
	iterations <- 30

	ret <- do_em (trainx, trainy, K, alpha, iterations)
	pi <- ret$pi
	q <- ret$q
  
	theta <- ret$theta
	frequencies <- ret$frequencies
	r <- ret$r


	results <- predict  (testx, testy, K, pi, q, theta, frequencies)
	count <- 0

	for (i in (1:nrow(testy)))
	{
		if (results[i] == testy[i])
		{
			count <- count + 1
		}
	}

	count/nrow(testx)*100
}