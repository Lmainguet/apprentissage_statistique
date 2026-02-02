install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("ggplot2")
library("factoextra")
library(tidyverse)
library(RColorBrewer)
data = load("Documents/app statistique/apprentissage statistique/TP1/regression-dataset.Rdata")
fit = lm(y~x)
summary(fit)
predict = predict(fit, newdata=data.frame("x"=x.grid))
lm(y~poly(x, 3, rax=TRUE))
plot(x, residuals())

x.grid=seq(min(x), max(x), by=0.01)
fx.grid= predict(fit3, newdata=data.frame("x"=x.grid))
plot(grid)

#q3
dag.list=c(1:5, 8,10,15,20)
library(RColorBrewer)
cols = brewer.pal(lenght(deg.list), "Set1")
x.grid=seq(0,10, by=0.01)
par(mfrow = c(2,2))
plot(x,y, pch=19, main="modele")
for(i in seq(lenght(deg.list))){
  d=deg.list[i]
  fit = lm(y~poly(x,d,raw=TRUE))
  preds = predict(fit, newdata=data.frame("x"=x.grid))
  lines(x.grid, preds, col = cols[i], lwd=2)
}

deg.list= c(i:5, 5, 10, 15, 20)
MSE= c()
for(d in deg.list){
  fit = lm(y-poly(x, d, raw=TRUE))
  mse=mean((y-fit$fitted.values)^2)
  cat("*** degre", d, ":MSE on training data = ", mse, "***\n")
  MSE=c(MSE,mse)
}
plot(deg.list, MSE, type="b")

#prediction de x test sur x y
for(i in seq(lenght(deg.list))){
  d=deg.list[i]
  fit = lm(y~poly(x,d,raw=TRUE))
  preds = predict(fit, newdata=data.frame("x"=x.test))
  mse=mean((y.test-preds)^2)
  MSE=c(MSE.test,mse)
}



#il faut trouver le juste milllieu entre biais et variance. 
#faut que la mse soit faible (le pourcentage d'erreur)
#et que la variance soient haute(le pourcentage de bonne reponse). 
#c'est valable pour l'apprentissage suppervisé mais pas sur les reseau de neurones

plot(deg.list, ylim=range(range(c(MSE, MSE.test)), MSE, type="b"))

###exercice 4
install.packages("MVN")
library(MASS)
library(rockchalk)
mu=c(2,3)
mvrnorm(n = 200, mu, Sigma, tol = 1e-06, empirical = FALSE)
X0 <- MASS::mvrnorm(n=200, mu = c(0,0,0), Sigma = diag(3))
modelname(x, conditions = NULL, ...) <- value

mv = function(X, mu, S){
  p = length(mu)
  return( 1/( sqrt((2*pi)^p*det(S)) ) * exp( -0.5 * t(X-mu) %*% solve(S) %*% (X-mu)) )
}



##Exercice 2
getwd()
y=as.character(read.table("TP1/nci.label")$V1)
X =read.table("TP1/nci.data")
X =t(X)
#upprimez l’échantillon de classe inconnue et les échantillons relatifs à des expériences de reproductibilité (leur nom contient le terme "repro")
ind1 = grep("repro", y)
ind2 = which(y == "UNKNOWN")
ind.rm = c(ind1, ind2)
y = y[-ind.rm]
y = factor(y)
X = X[-ind.rm,]

#Combien de composantes principales obtenez-vous ?
scale = scale(X, center = TRUE, scale = TRUE)
res.pca <- PCA(scale, graph = TRUE)


##Exercice 3
data = load("TP1/digits-3.Rdata")
print(data)
image(x = data)
cols = gray(seq(1,0,length.out=256))
X = apply(I, 3, function(x){as.vector(x)})
X = t(X)
