##################################
#log likehood di una bernulli B(p)
##################################

########################
#dichiarazione funzioni
########################
likelihood <- function(data, p){
  sum_xi = 0
  for (i in 1:length(data)) {
      sum_xi = sum_xi+data[i]
  }
  return ((p^(sum_xi))*(1-p)^(length(data)-sum_xi))
}

logLikelihood <- function(data, p){
  sum_xi = 0
  for (i in 1:length(data)) {
    sum_xi = sum_xi+data[i]
  }
  return (sum_xi*log(p)+log(1-p)*(length(data)-sum_xi))
}

####################
#fine dichiarazione
####################
set.seed(42)
sequence <- rbinom(n = 100, size = 1, prob = 0.2)
p <- seq(0, 1, by = 0.001)

likelihoodVal = c()
for (i in 1:length(p)) {
  likelihoodVal[i] = likelihood(sequence,p[i])
}

loglikelihoodVal = c()
for (i in 1:length(p)) {
  loglikelihoodVal[i] =  logLikelihood(sequence,p[i])
}


qplot(x=p,
      y = likelihoodVal,
      geom = "line",
      main = "Verosomiglianza",
      xlab = "p",
      ylab = "Likelihood")

par(las = 1, cex.lab = 1.2)

qplot(x=p,
      y = loglikelihoodVal,
      geom = "line",
      main = "Log-verosomiglianza",
      xlab = "p",
      ylab = "log-likelihood")

imax <- which.max(loglikelihoodVal) #cerca l' indice del valore p che massimizza la log
p_MLE <- p[imax] # recupera il valore di p per cui la log like è massima
p_MLE
