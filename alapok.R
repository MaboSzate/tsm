# Beolvasás
datauj <- read.xlsx('C:/Users/andra/Downloads/adatbazis.xlsx', sheetName = "2022")
data <- read.xlsx('C:/Users/andra/Downloads/adatbazis.xlsx', sheetName = "2011 (prior)")úű

#üres sorok törlése
data2uj <- datauj[rowSums(is.na(datauj)) != ncol(datauj),] 
data2 <- data[rowSums(is.na(datauj)) != ncol(datauj),] 
datauj <- data2uj
data <- data2

# Lin. regresszió a priorra
model <- lm(kozepfok ~ X15alatt + foglalk + haziorvos + gimnazium + vallalkozas + wc, data = data)
summary(model)
