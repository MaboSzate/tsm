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

# Bayes-i modell
require(rethinking)
flist<-alist(
  kozepfok~dnorm(mu, sigma),
  mu<-a + b1*foglalk + b2*vallalkozas + b3*wc,
  a~dnorm(0.60151,0.03994),
  b1~dnorm(0.14834,0.05924),
  b2~dnorm(0.28915, 0.04762),
  b3~dnorm(-1.32839,0.07291),
  sigma~dunif(0.09,0.1)
)
model<-map(flist, data=datauj)

precis(model)