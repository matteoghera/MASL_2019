##############################
# Monte carlo simulazione
###############################
# serve per la selezione del modello, l'accuratezza di algoritmo per la predizione,
# per vere se un stimatore è corretto

n = 500
sigma = 2
b0 = 2
b1 = -0.1 #-1
b2 = 0
set.seed(42)
X1 = rnorm(n = n, mean = 0, sd = sigma)
X2 = rnorm(n = n, mean = 0, sd = sigma)
epsilon = rnorm(n = n)
y = b0+b1*X1+b2*X2+epsilon

mod0 = lm(y ~ X1+X2) #stima del primo modello di regressione con OLS
# la variabile dip viene separata con il tilde, le covariate del modello
# vengono segnalate con il +
summary(mod0) #output stile stata del modello
names(summary(mod0)) #il dataframe è una lista
summary(mod0)[4]
#modo alternativo per recuperare la tabella, summary(mod0)$coefficients
b1_pvalue = summary(mod0)$coefficients[2,4]
b2_pvalue = summary(mod0)$coefficients[3,4]





nsim = 5000 #numero di simulazioni per ripete il metodo di montecarlo
b1_pvalue = c()
b2_pvalue = c()


#set.seed(42+i) questo è un trucco, supponiamo che ti servono i dati alla 5 iter.
# bene basta dopo impostare il seme a 42+5 e recuperi i dati della simulazione
# relativa alla 5 iterazione.
for (i in 1:nsim) {
  set.seed(42+i)
  X1 = rnorm(n = n, mean = 0, sd = sigma)
  X2 = rnorm(n = n, mean = 0, sd = sigma)
  epsilon = rnorm(n = n)
  y = b0+b1*X1+b2*X2+epsilon
  
  mod0 = lm(y ~ X1+X2) 
  summary(mod0) #output stile stata del modello
  b1_pvalue[i] = summary(mod0)$coefficients[2,4]
  b2_pvalue[i] = summary(mod0)$coefficients[3,4]
}

length(b1_pvalue)

hist(b1_pvalue) # la distribuzione si concentra solo su 0e+00, ci sta dicendo
# che il p-value è vicino a 0, quindi b1 è significativo (rifiuto H0)
# la distribuzione ottenuta è quasi uniforme, quindi significa che
# il b2 è 0 pertanto non è significativo (conferma che b2 è 0, accetto H0)

# il pvalue non ci da la certezza che H0 è falsa, ma quando è piccolo conviene scommettere
# su H1 piuttosto che H0, mentre quando è grande conviene scommetere su H0
#Quindi per dire che H0 è falsa e quindi H1 è vera, ci conviene avere i primi istogrammi
#alla estrema sinistra alti, mentre per dire che H0 è vera e H1 falsa ci conviene
#avere gli istogrammi alla estrema destra alti, quando gli istogrammi sono uniforme
#l'incertezza domina.