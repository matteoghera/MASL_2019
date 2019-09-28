############################
#log likelihood di una normale N(mu, sigma)
############################

likelihood <- function(data, sigma, mu){
  dev =sum((data-mu)^2)
  return(((2*pi*sigma^2)^(-length(data)*0.5))*pi^(-0.5*dev*1/sigma^2))
}

log_likelihood <- function(data, sigma, mu){
  dev =sum((data-mu)^2)
  return(-length(data)*0.5*log(2*pi*sigma^2)-0.5*dev*1/sigma^2)
}


####################
#fine dichiarazione
####################
set.seed(42)
n = 10
mean = 3.14
sd = 0.22
sequence = rnorm(n = n, mean = mean, sd = sd)
if(mean>=0){
  mu = seq(0,mean, by =0.01 )
}else{
  mu = seq(mean,0, by =-0.01)
}



likelihoodVal = c()
for (i in 1:length(mu)) {
  likelihoodVal[i] = likelihood(sequence, sigma, mu[i])
}

loglikelihoodVal = c()
for (i in 1:length(mu)) {
  loglikelihoodVal[i] =  logLikelihood(sequence, sigma, mu[i])
}


qplot(x=p,
      y = likelihoodVal,
      geom = "line",
      main = "Verosomiglianza",
      xlab = "mu",
      ylab = "Likelihood")

par(las = 1, cex.lab = 1.2)

qplot(x=p,
      y = loglikelihoodVal,
      geom = "line",
      main = "Log-verosomiglianza",
      xlab = "mu",
      ylab = "log-likelihood")

imax <- which.max(loglikelihoodVal) #cerca l' indice del valore p che massimizza la log
mu_MLE <- mu[imax] # recupera il valore di p per cui la log like è massima
mu_MLE
