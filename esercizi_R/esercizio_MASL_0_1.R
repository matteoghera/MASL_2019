#esercizio slide: Introduction to MASL

#impostatione dei parametri del modello e della simulazione MC
n = 1000
nsim = 100
sigma = 2
b0 = -1 # intercetta del modello
b1 = 2 # coefficiente di reg per X1
b2 = 1 # coefficiente di reg l'interazzione tra X1 e X2
#fine impostazione dei parametri

# generazione dei dati e del modello
set.seed(42)
X1 = rnorm(n, sd = 2)
X2 = rnorm(n, sd = 2)
eps = rnorm(n)
Y = -1+2*X1^3+(X1)^2*X2+eps
# fine generazione

#split dei dati
split = sample(1:length(X1),1)
training = data.frame("Y"=Y[seq(from = 1, to = split)], "X1"=X1[seq(from = 1, to = split)]
                      ,"X2"=X2[seq(from = 1, to = split)])
test = data.frame("Y"=Y[seq(from = split+1, to = length(X1))], "X1"= X1[seq(from = split+1, to = length(X1))]
                  ,"X2"=X2[seq(from = split+1, to = length(X1))])

data.frame()

#stima del modello e previsioni con il test set
mod0 = lm(Y~X1^4+(X1)^2*X2, data = training)
summary(mod0)
yhat = predict(mod0, test)

#plotting
plot(mod0)