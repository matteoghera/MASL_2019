source('~/eserciziMasl/esercizio2.R') #richiama lo script precedente
X = matrix(c(X0,X1), nrow = n, ncol = 2) #crea una matrice di dim nx2 con i vettori X0 e X1
colnames(X) = c("X0", "X1") # rinomino le colonne
betaHat = solve(t(X)%*%X)%*%t(X)%*%Y # stima dei coefficienti di regressione con OLS
yhat = X%*%betaHat # calcolo i valori adattati
epsilonHat = Y-betaHat # calcolo dei residui