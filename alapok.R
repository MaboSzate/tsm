# Beolvasás
require(xlsx)
datauj <- read.xlsx('C:/Users/andra/Downloads/adatbazis.xlsx', sheetName = "2022")
data <- read.xlsx('C:/Users/andra/Downloads/adatbazis.xlsx', sheetName = "2011 (prior)")

#üres sorok törlése
data2uj <- datauj[rowSums(is.na(datauj)) != ncol(datauj),] 
data2 <- data[rowSums(is.na(data)) != ncol(data),] 
datauj <- data2uj
data <- data2

# Lin. regresszió a priorra
model <- lm(kozepfok ~ foglalk + vallalkozas + wc, data = data)
summary(model)

#Ulam
require(rethinking)
require(dplyr)
d = select(datauj, kozepfok, foglalk, vallalkozas, wc)
m <- ulam(alist(
  kozepfok~dnorm(mu, sigma),
  mu<-a + b1*foglalk + b2*vallalkozas + b3*wc,
  a~dnorm(0.60151,0.03994),
  b1~dnorm(0.14834,0.05924),
  b2~dnorm(0.28915, 0.04762),
  b3~dnorm(-1.32839,0.07291),
  sigma~dunif(0,0.15)
), data=d, chains=4)
precis(m)
plot(extract.samples(m))
trankplot(m)
pairs(m)
traceplot(m, chain=1)

#Ábrázolás
require(tidybayes)
require(tidybayes.rethinking)
pred = predicted_draws(m, datauj, ndraws = 1000)
n <- length(pred$.prediction)/1000
pred$input <- rep(1:n, each=1000)
mean_predictions <- aggregate(pred$.prediction, by=list(pred$input), FUN=mean)
plot(datauj$kozepfok, mean_predictions$x)
predlin = predict(model, datauj)
plot(datauj$kozepfok, predlin)