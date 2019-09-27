n = 1000 # dichiaro la lunghezza massima degli elementi
X0 = rep(1,1000) # creao un vettore di dimensione n di soli 1
X1 = rnorm(n) # X1 è un vettore di dimensione n in cui gli elementi sono generati attraverso una norm. standard
epsilon = rnorm(n) # idem con patate
Y = 2*X0+2*X1+epsilon # creo una v.a che combinazione lineare delle precendi
meanY = round(mean(Y), digits = 2) # ottengo la media di Y e l'arrotondo sulla seconda cifra dopo la virgola
sdY = round(sd(Y), digits = 2) # idem ma per deviazione standard