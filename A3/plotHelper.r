min_log_val <- c(56.22,51.22,50.96,44.83,44.13,43.99,47.73,46.18,44.4)
#error_val <- c()
iterations <-c(800,1500,3000,800,1500,3000,800,1500,300)
pca_comp<- c(rep(10,3),rep(20,3),rep(40,3))
log_frame <- data.frame(pca_comp,iterations,min_log_val)
attach(log_frame);
plot(iterations,min_log_val,col=c("red","blue","green")[pca_comp]);detach(log_frame)
#> attach(DF); plot(x, y, col=c("red","blue","green")[z]); detach(DF)
