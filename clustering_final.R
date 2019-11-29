setwd("C:/Users/Kevin/Desktop/acer/project")
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)
library(factoextra)
library(dtwclust)
library(grid)
library(ggcorrplot)
library(recommenderlab)
library(fields)
library(reshape2)

source("preprocessingEDA_final.r")

stats = fread("stats_g.csv",sep = ",",header = T)
duo = datapreprocessing_stats_duo(stats)

#k-medoids
#standardized data
duo_s = apply(duo[,5:ncol(duo)],2,scale)
duo_s = as.data.table(cbind(duo[,1:4],duo_s))


#k-medoids without standardized
kmedoid = pam(duo[,5:ncol(duo)],k=3)
# kmedoid = pam(duo[,c(12,17,23,32,36)],k=3)
plot(kmedoid)
fviz_cluster(kmedoid, stand = T, geom = "point",ellipse=F)
fviz_nbclust(duo[,5:ncol(duo)], pam, method = "wss")#3 #Choose the number of cluster


#k-medoids with standardized
kmedoid_s = pam(duo_s[,5:ncol(duo_s)],k=4) 
# kmedoid_s = pam(duo_s[,c(12,17,23,32,36)],k=3)
plot(kmedoid_s)
fviz_cluster(kmedoid_s, stand = T,geom = "point",ellipse=F)
fviz_nbclust(duo_s[,5:ncol(duo)], pam, method = "wss")#4 #Choose the number of cluster

#PCA
pca = prcomp(formula = ~., data = duo[,5:ncol(duo)], scale= T)
summary(pca)
PoV <- pca$sdev^2/sum(pca$sdev^2) #proportion variation explained by each PC
pca_df = as.data.frame(pca$x)
pca_df$cluster = as.factor(kmedoid_s$clustering)
loading_df = as.data.frame(pca$rotation)
head(pca_df)

#Clustering outcome visualization by PC1~PC5
ggplot(pca_df,aes(x=PC1,y=PC2,color=cluster))+geom_point()+ #1 vs 2
  ggtitle("Cluster plot")+
  xlab(paste("PC1(",as.character(round(100*PoV[1],2)),"%)",sep = ""))+
  ylab(paste("PC2(",as.character(round(100*PoV[2],2)),"%)",sep = ""))
ggplot(pca_df,aes(x=PC2,y=PC3,color=cluster))+geom_point()+ #2 vs 3
  ggtitle("Cluster plot")+
  xlab(paste("PC2(",as.character(round(100*PoV[2],2)),"%)",sep = ""))+
  ylab(paste("PC3(",as.character(round(100*PoV[3],2)),"%)",sep = ""))  
ggplot(pca_df,aes(x=PC1,y=PC3,color=cluster))+geom_point()+ #1 vs 3
  ggtitle("Cluster plot")+
  xlab(paste("PC1(",as.character(round(100*PoV[1],2)),"%)",sep = ""))+
  ylab(paste("PC3(",as.character(round(100*PoV[3],2)),"%)",sep = ""))  
ggplot(pca_df,aes(x=PC1,y=PC4,color=cluster))+geom_point()+ #1 vs 4
  ggtitle("Cluster plot")+
  xlab(paste("PC1(",as.character(round(100*PoV[1],2)),"%)",sep = ""))+
  ylab(paste("PC4(",as.character(round(100*PoV[4],2)),"%)",sep = ""))  
ggplot(pca_df,aes(x=PC1,y=PC5,color=cluster))+geom_point()+ #1 vs 5
  ggtitle("Cluster plot")+
  xlab(paste("PC1(",as.character(round(100*PoV[1],2)),"%)",sep = ""))+
  ylab(paste("PC5(",as.character(round(100*PoV[5],2)),"%)",sep = ""))  
ggplot(pca_df,aes(x=PC3,y=PC4,color=cluster))+geom_point()+ #3 vs 4  #garbage
  ggtitle("Cluster plot")+
  xlab(paste("PC3(",as.character(round(100*PoV[3],2)),"%)",sep = ""))+
  ylab(paste("PC4(",as.character(round(100*PoV[4],2)),"%)",sep = ""))  


#See loadings for PC1~PC5 
dotchart(sort(pca$rotation[,1]),main="Loading Plot for PC1",xlab="Variable Loadings",col="red")
dotchart(sort(pca$rotation[,2]),main="Loading Plot for PC2",xlab="Variable Loadings",col="blue")
dotchart(sort(pca$rotation[,3]),main="Loading Plot for PC3",xlab="Variable Loadings",col="darkgreen")
dotchart(sort(pca$rotation[,4]),main="Loading Plot for PC4",xlab="Variable Loadings",col="purple")
dotchart(sort(pca$rotation[,5]),main="Loading Plot for PC5",xlab="Variable Loadings",col="darkblue")

#Biplot for PC1 and PC2
fviz_pca_biplot(pca,habillage = kmedoid_s$clustering,col.var = "red",geom="point",pointshape = 16)+
  labs(x = paste("PC1(",as.character(round(100*PoV[1],2)),"%)",sep = "")
    , y = paste("PC2(",as.character(round(100*PoV[2],2)),"%)",sep = ""))

#Decide how many PCs to use
fviz_eig(pca, addlabels = TRUE) #3~5
