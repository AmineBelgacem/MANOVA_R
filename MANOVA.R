# install.packages("rstatix")
#install.packages("formattable")
library(rstatix)
library(formattable)

# 1 - Introduction


x1  <- c(35,35,40,10,6,20,35,35,35,30)
x2  <- c(3.5,4.9,3,2.8,2.7,2.8,4.6,10.9,8,1.6)
x3  <- c(2.8,2.7,4.38,3.21,2.73,2.81,2.88,2.9,3.28,3.2)
fac <-c(1,1,1,1,1,2,2,2,2,2)
df  <- data.frame(x1,x2,x3,fac)

#---1. Transformation de la variable fac en facteur
#-> variable factorielle
df$fac <- as.factor(df$fac) 

#-> nb de facteur (= 2)
k <- nlevels(df$fac)

#-> nb de variables
n_var <- ncol(df) - 1

#-> Nombre d'observations 
N <- nrow(df)

# 2 - Calcul des moyennes et la matrice 
# des variances covariances par multiplication matricielle

j <- matrix(1,nrow=1,ncol=N)
J <- matrix(1,nrow=N,ncol=N)
X <- matrix(c(x1,x2,x3), nrow=10, byrow=F)

m <- (1/N)%*%j%*%X
m
s2 <- (1/(N-1))*(t(X)%*%X-t(X)%*%((1/N)*J)%*%X)
s2
# 3 - Développement de la méthode de comparaison
# 3.1 : Calculs intermédiaires

#On filtre notre data base en fonction de la colonne facreur
d1 <- df %>% filter(fac ==1)
d2 <- df %>% filter(fac ==2)

#On transforme nos dataframes en matrices
d1 <- matrix(c(d1$x1,d1$x2,d1$x3), nrow=5, byrow=F )
d2 <- matrix(c(d2$x1,d2$x2,d2$x3), nrow=5, byrow=F )

#On recrée les variables dont on a besoin pour faire nos sous dataframes
n1 <- nrow(d1)
j1 <- matrix(1,nrow=1,ncol=n1)
J1 <- matrix(1,nrow=n1,ncol=n1)

n2 <- nrow(d2)
j2 <- matrix(1,nrow=1,ncol=n2)
J2 <- matrix(1,nrow=n2,ncol=n2)

#Calcul moyenne de nos sous dataframes
m1 <- (1/n1)%*%j1%*%d1
m2 <- (1/n2)%*%j2%*%d2

#Calcul matrice des variances covariances
s2d1 <- (1/(n1-1))*(t(d1)%*%d1-t(d1)%*%((1/n1)*J1)%*%d1)
s2d2 <- (1/(n2-1))*(t(d2)%*%d2-t(d2)%*%((1/n2)*J2)%*%d2)

#Création de nos deux listes 
G1 <- list(d1,m1,n1,s2d1)
G1 <- list(G1.data=d1,G1.mean=m1,G1.n=n1,G1.S2=s2d1)

G2 <- list(d2,m2,n2,s2d2)
G2 <- list(G2.data=d2,G2.mean=m2,G2.n=n2,G2.S2=s2d2)

#création de la liste 
L <- list(G1,G2)
L

#3.2 

#Test homogénéité



#3.2.1 : Calcul Sd²

Sd2 <- ((G1$G1.n -1)*G1$G1.S2 + (G2$G2.n-1)*G2$G2.S2)/(G1$G1.n+G2$G2.n-2)
Sd2

#3.2.2 :  Calcul du T² de Hotteling (sous H0)

Ty <- t(m1-m2) #Transposé de (Y1 - Y2)
Y  <- (m1-m2) #(Y1 - Y2)
T2I <- solve(((1/G1$G1.n)+(1/G2$G2.n))*Sd2) #matrice inverse du facteur du milieu


T2 <- Y%*%T2I%*%Ty
T2

#3.2.3 :Statistiques et conclusion

Fobs <- ((n1+n2-3-1)/((n1+n2-2)*3))*T2
Fobs

pvalue <- df(Fobs,3,6)
pvalue
# 3.2.4 : Calcul du T² de Hotteling (sous H0) avec la fonction MANOVA

model <- lm(cbind(x1, x2, x3) ~ fac, df)
Manova(model, test.statistic = "Wilks")

1- pf(Fobs,3,6, lower.tail = T)

#4

df2 <- data.frame(subset(iris, iris$Species != 'virginica'))
df2$Species <- factor(df2$Species )

