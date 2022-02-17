
install.packages("NbClust")
install.packages("cluster")
install.packages("e1071")
install.packages("mclust")
install.packages("fpc")
install.packages("factoextra")

library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra)

options(scipen = 0)
datos <- read.csv("C:/Users/Mustella 3D/Downloads/movies.csv");
wwwww<- data.frame(datos[,c(1,2,8,9,18,20:25)])

wss <- (nrow(datos[,c(1,2,8,9,18,20:25)])-1)*sum(apply(datos[,c(1,2,8,9,18,20:25)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datos[,c(1,2,8,9,18,20:25)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Numero de clusters",  ylab="Dentro de los grupos suma de cuadrados")

#k-medias
conjunto<-datos
datosCompleto<-datos[complete.cases(conjunto),]
km<-kmeans(datos[,c(1,2,8,9,18,20:25)],3,iter.max =100)
conjunto$grupo<-km$cluster

g1<- conjunto[conjunto$grupo==1,]
prop.table(table(g1$budget))*100
nrow(g1)
summary(g1)

g2<- conjunto[conjunto$grupo==2,]
prop.table(table(g2$budget))*100
g3<- conjunto[conjunto$grupo==3,]
prop.table(table(g3$budget))*100

plotcluster(datos[,c(1,2,8,9,18,20:25)],km$cluster)

#Clustering jerarquico
hc<-hclust(dist(datos[,c(1,2,8,9,18,20:25)])) 
plot(hc)
rect.hclust(hc,k=3) 
groups<-cutree(hc,k=3) 
conjunto$gruposHC<-groups


g1HC<-conjunto[conjunto$gruposHC==1,]
g2HC<-conjunto[conjunto$gruposHC==2,]
g3HC<-conjunto[conjunto$gruposHC==3,]

#Mixture of gaussians
mc<-Mclust(datos[,c(1,2,8,9,18,20:25)],20)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-conjunto[conjunto$mxGau==1,]
g2MC<-conjunto[conjunto$mxGau==2,]
g3MC<-conjunto[conjunto$mxGau==3,]


silkm<-silhouette(km$cluster,dist(datos[,c(1,2,8,9,18,20:25)]))
mean(silkm[,3])

silch<-silhouette(groups,dist(datos[,c(1,2,8,9,18,20:25)]))
mean(silch[,3]) 

silmg<-silhouette(mc$classification,dist(datos[,c(1,2,8,9,18,20:25)]))
mean(-silmg[,3])