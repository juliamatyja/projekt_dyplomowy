
# WCZYTANIE DANYCH

library(readxl)
dane_licencjat <- read_excel("Desktop/PRACA LICENCJACKA/DANE/dane_licencjat.xlsx")

# ZAŁOŻENIA

# wartości odstające

zmienne <- dane_licencjat[, paste0("x", 1:8)]
mahalanobis <- mahalanobis(zmienne, colMeans(zmienne), cov(zmienne))
liczba_zmiennych <- ncol(zmienne)
prog <- qchisq(0.99, df = liczba_zmiennych)
dane_licencjat$obserwacja_odstajaca <- mahalanobis > prog
which(dane_licencjat$obserwacja_odstajaca)

dane_licencjat2 <- dane_licencjat[-16, ] 
zmienne2 <- dane_licencjat2[, paste0("x", 1:8)]

# wielowymiarowa normalność

library(MVN)
mvn(zmienne2, mvnTest = "hz")
mvn(zmienne2, mvnTest = "mardia")

# silne współzależności

library(psych)
KMO(zmienne2)
zmienne3 <- zmienne2[, !(names(zmienne2) %in% "x8")]
KMO(zmienne3)

cortest.bartlett(zmienne3)

library(corrplot)
korelacje <- cor(zmienne3)
corrplot(korelacje, 
         method = "color",    
         type = "upper",       
         addCoef.col = "black",
         tl.col = "black",    
         col = colorRampPalette(c("white", "#333333"))(200),
         number.cex = 0.85)

# współliniowość

VIF <- function(data) {
  wartosci_vif <- numeric(ncol(data))
  names(wartosci_vif) <- colnames(data)
  
  for (i in 1:ncol(data)) {
    y <- data[, i]
    x <- data[, -i]
    dane <- as.data.frame(cbind(y, x))
    colnames(dane)[1] <- "y"
    model <- lm(y ~ ., data = dane)
    r2 <- summary(model)$r.squared
    wartosci_vif[i] <- 1 / (1 - r2)
  }
  
  return(round(wartosci_vif, 2))
}

VIF(zmienne3)

# PCA

zmienne_stand <- scale(zmienne3)
pca <- prcomp(zmienne_stand, center = TRUE)
summary(pca)
dane_svd <- svd(zmienne_stand)
lambda <- (dane_svd$d / sqrt(max(1, nrow(zmienne3) - 1)))^2
lambda

wariancje <- pca$sdev^2
plot(wariancje, 
     type = "b",          
     xlab = "Składowe główne", 
     ylab = "Wariancja",
     pch = 1,           
     col = "black")

# FA

fa <- principal(zmienne_stand, nfactors = 3, rotate = "varimax", scores = TRUE)
fa$loadings        
fa$Vaccounted 

ładunki <- round(as.matrix(fa$loadings), 6)
kwadrat <- ładunki^2
przeskalowane <- apply(kwadrat, 2, function(x) round(x / sum(x), 6))

expl_var <- round(fa$Vaccounted[1, ], 4)  

ss_loadings <- fa$Vaccounted[1, ]          
total <- sum(ss_loadings)       
expl_tot <- ss_loadings / total  
round(expl_tot, 4)

ładunek_czynnikowy <- rbind(ładunki, expl_var, expl_tot)
kwadrat_przeskalowany <- rbind(przeskalowane, NA, NA)

ładunek_czynnikowy
kwadrat_przeskalowany

library(writexl)
zmienne_stand <- as.data.frame(zmienne_stand)
ścieżka <- file.path(Sys.getenv("HOME"), "Desktop")
write_xlsx(zmienne_stand, path = file.path(ścieżka, "zmienne_stand.xlsx"))

