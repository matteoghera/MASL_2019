#esercizio slide: Introduction to MASL

myPlot <- function(myModel, column) {
  require(ggplot2)
  #recupero le informazioni del modello
  info_reg = paste("Adj R2 =",signif(summary(myModel)$adj.r.squared, 4),
                   "Intercetta =",signif(myModel$coef[[1]],4 ),
                   "Pendenza =",signif(myModel$coef[[2]], 4),
                   "P-value"= signif(summary(myModel)$coef[2,4], 4))
  #costruisco il grafico
  plot = ggplot(myModel$model, aes_string(x = names(myModel$model)[column], y =names(myModel$model)[1]))
  plot = plot + geom_point()+ stat_smooth(method = "lm", col = "red")
  plot = plot + labs(title = info_reg)
  return (plot)
}

#impostatione dei parametri del modello e della simulazione MC
n = 1000
nsim = 100
sigma = 2
b0 = -1 # intercetta del modello
b1 = 2 # coefficiente di reg per X1
b2 = 1 # coefficiente di reg l'interazzione tra X1 e X2
#fine impostazione dei parametri
for (i in 1:nsim) {
  # generazione dei dati e del modello
  set.seed(42+i)
  X1 = rnorm(n, sd = 2)
  X2 = rnorm(n, sd = 2)
  eps = rnorm(n)
  Y = -1+2*X1^3+(X1)^2*X2+eps
  X = data.frame("Y"= Y, "X1" = X1, "X2" = X2 )
  # fine generazione
  #split dei dati
  split = sample(1:length(X1),1)
  training = data.frame("Y"=Y[seq(from = 1, to = split)], "X1"=X1[seq(from = 1, to = split)]
                        ,"X2"=X2[seq(from = 1, to = split)])
  test = data.frame("Y"=Y[seq(from = split+1, to = length(X1))], "X1"= X1[seq(from = split+1, to = length(X1))]
                    ,"X2"=X2[seq(from = split+1, to = length(X1))])
  #fine split data
  #stima del modello con i dati di training
  mod0 = lm(Y~I(X1^i)+X2*X1, data = training)
}

#summary&plotting 
summary(mod0)
plot(mod0)
plot = myPlot(mod0,2)
plot


