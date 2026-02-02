## tp2 
# exo 1 

load("/home/icm1/mainguel/Documents/apprentissage_statistique/TP2/spectra.Rdata")
# question 2 - plot dataset
#Représentez sur une même figure l’ensemble des 215 courbes, avec un code couleur indiquant leur catégorie. En pratique on ignore la catégorie des groupes car on est dans un
#cas d’apprentissage non-supervisé. Dans le cadre du TP on souhaite évaluer la qualité des
#méthodes on connait donc les catégories. Les données sont dites labellisées.
colors <- rainbow(length(unique(y)))[as.factor(y)]

# Create the plot with the first line
plot(X[1,], type = "l", col = colors[1], ylim = range(X), 
     main = "dataset", xlab = "channel index", ylab = "signal intensity")

# Add the remaining lines
for(i in 2:nrow(X)){
  lines(X[i,], col = colors[i])
}

# question 3 - calculer la matrice de distance Euclidienne
d = dist(X)

# question 4 -  ACP sur les données et visualisez le nuage de points des individus dans
#le premier plan factoriel. Vous pouvez ajouter un code couleur reflétant la catégorie des
#individus.
pca = prcomp(X)
plot(-pca$x[,1], -pca$x[,2], col = y, pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA")

# question 5 - represent it in 2d using cmdscale
ds = cmdscale(d, 2)
plot(ds[,1], ds[,2], col = y, xlab = "dim 1", ylab = "dim 2", main = "projection of Euclidean  distance", pch = 19)

# compare to PCA
#"On note que la distance Euclidienne ne permet pas de bien discriminer les deux catégories de courbes. Le code suivant compare les résultats obtenus par \texttt{cmdscale} à partir de la matrice de distance Euclidienne à l'ACP. Les résultats sont bien identiques. On note néanmoins que l'ACP étant définie à un signe près, il a fallu ici utiliser un facteur négatifs sur les composantes principales pour que les nuages de points soient effectivement superposables."
pca = prcomp(X)
par(mfrow = c(1,2))
plot(-pca$x[,1], -pca$x[,2], col = y, pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA")
plot(ds[,1], ds[,2], col = y, xlab = "dim 1", ylab = "dim 2", main = "CMDS", pch = 19)

# la les courbe qui se ressemble beaucoup sont tres proche donc la correlation est proche de 1.
# le probleme c'est que si les points sont proche sur le graphique , la distance est prochde de 0.
# on va faire la transposé pour que les valeurs proches de 0 soient proche et celle proche de 1 soient differentes.
# 1) compute correlation matrix
C = cor(t(X))
# 2) build a distance object as 1 - C
dC = as.dist(1-C)
# represent it in 2D
dsC = cmdscale(dC, 2)
plot(dsC[,1], dsC[,2], col = y, xlab = "dim 1", ylab = "dim 2", main = "projection of correlation  distance", pch = 19)
