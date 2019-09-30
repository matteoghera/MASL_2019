sigmoid <- function(x){
  1/(1+exp(-x))
}

log_likelihood <- function(beta, X, y){
  return((-1/m)*sum(y*log(sigmoid(X%*%beta)) + (1-y)*log(1-sigmoid(X%*%beta))))
}

newton <- function(X, y, beta, num_iter){
  library(numDeriv)
  library(MASS)
  J_hist<-vector()
  
  for(i in 1:num_iter){
    score <- (1/m)*(t(X)%*%(sigmoid(X%*%beta) - y)) # calcolo della score function
    H <- hessian(log_likelihood, beta, method = "complex", X = X, y = y)
    #H contiene le derivate seconde della log-verosimiglianza
    beta <- beta - ginv(H)%*%score # passo iterativo del metodo di newton
    J_hist[i] <- log_likelihood(beta, X, y)
  }
  result <- list(beta, J_hist)
  return(result)
}

set.seed(11)
x <- matrix(rnorm(400), ncol = 4)
y <- round(runif(100, 0, 1)) 
m <- length(y)
X<-cbind(rep(1, 100), x)
beta<-rep(0, 5)


num_iter = 20
result <- newton(X, y, beta, num_iter)
beta <- result[[1]]
print(beta)

cost_hist <- result[[2]]
plot(1:num_iter, cost_hist, type = 'l')

pred <- sigmoid(X%*%beta)
pred <- ifelse(pred > .5, 1, 0)
accuracy <- mean(pred == y)
print(accuracy) 


