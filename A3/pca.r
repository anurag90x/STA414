# FUNCTIONS FOR PRINCIPAL COMPONENTS ANALYSIS.
#
# Written by Radford Neal.
#
# Note that R also has built-in "princomp" and "prcomp" functions.  The
# functions here are provided so that you can more easily see the details 
# of how it's done.  Also, these functions handle data where there are more 
# variables than cases (handled by prcomp but not by princomp), and they
# seem to be faster than the built-in functions (at least up to R 3.0.3).


# FIND PRINCIPAL COMPONENT VECTORS.  The first argument is the data - either
# a data frame, or a matrix with rows corresponding to cases and columns to 
# variables.  This procedure handles data with fewer cases than variables or
# fewer variables than cases appropriately, returning as many principal 
# components as the minimum of the two.  The number of principal components 
# can be further reduced with the "k" argument.  The "center" and "scale" 
# arguments say whether the mean should be subtracted from each variable
# (default TRUE) and whether each variables should be divided by the standard 
# deviation (default FALSE).  The value returned is a list with element "e"
# being the principal component directions (eigenvectors), element "v" being
# the corresponding eigenvalues, element "shift" being the amount by which
# each variable was shifted (minus its mean, or zero), and element "mul"
# being the amount by which each variable was multiplied (reciprocal of
# its standard deviation, or one).

pca.vectors <- function (X, k=NULL, center=TRUE, scale=FALSE)
{ 
  X <- as.matrix(X)
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Center and/or scale the variables, as requested.
  
  shift <- 0
  mul <- 1
  
  if (center || scale)
  { X <- scale(X,center=center,scale=scale)
    if (center) shift <- -attr(X,"scaled:center")
    if (scale)  mul <- 1/attr(X,"scaled:scale")
  }
  
  # Find principal components from eigenvectors, choosing what to find
  # eigenvectors of based on n<p or n>=p.
  
  if (n<p)
  { m <- X %*% t(X)
    ev <- eigen(m,symmetric=TRUE)
    if (is.null(k))
    { e <- t(X) %*% ev$vectors
    }
    else
    { e <- t(X) %*% ev$vectors[,1:k]
    }
  }
  else # n >= p
  { m <- t(X) %*% X
    ev <- eigen(m,symmetric=TRUE)
    e <- ev$vectors
  }
  
  v <- ev$values / n
  
  # Select just k principal components if k is specified (otherwise all).
  
  if (!is.null(k))
  { e <- e[,1:k]
    v <- v[1:k]
  }
  
  # Make sure directions (eigenvectors) have length one.
  
  for (i in 1:ncol(e))
  { e[,i] <- e[,i] / sqrt(sum(e[,i]^2))
  }
  
  # Return the results as a list.
  
  list (e=e, v=v, shift=shift, mul=mul)
}


# FIND PROJECTIONS OF DATA ONTO FIRST K PRINCIPAL COMPONENTS.  The first
# argument is the list of principal component vectors, shift, and scale
# factors returned by pca.vectors.  The second argument is the data to
# project (a matrix or data frame), which might or might not be the same
# data that was used to find the principal components.  The third argument
# is the number of principal components to use, which must be no more than
# the number in "pc" (default is to use all that are in "pc").

pca.proj <- function (pc, X, k=ncol(pc$e))
{
  if (k>ncol(pc$e))
  { stop("Asking for more principal components than are available")
  }
  
  X <- as.matrix(X)
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Shift and scale the data in the same way as was done when finding the
  # principal components.
  
  if (!all(pc$shift==0) || !all(pc$mul==1))
  { shift <- rep(pc$shift,length.out=p)
    scale <- rep(pc$mul,length.out=p)
    for (i in 1:p)
    { X[,i] <- (X[,i]+shift[i])*scale[i]
    }
  }
  
  # Return the projections on the first k principal components.
  
  if (k==ncol(pc$e))
  { Y <- X %*% pc$e
  }
  else
  { Y <- X %*% pc$e[,1:k]
  }
  
  rownames(Y) <- rownames(X)
  
  Y
}