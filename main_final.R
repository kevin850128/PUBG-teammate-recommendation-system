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
source("recommendation_final.r")

stats = fread("stats_g.csv",sep = ",",header = T)
matchplayer = fread("matchplayer_g.csv",sep = ",",header = T)

main_UBCF = function(stats,matchplayer,nn,player,n,simmethod){
  duo = datapreprocessing_stats_duo(stats)
  match_duo = datapreprocessing_matchplayer_duo(matchplayer)
  allplayers = union(match_duo$accountId,match_duo$teammateId1)
  accm = produce_accmatrix(match_duo)
  mcount = produce_countmatrix(match_duo)
  m = accm/mcount
  cluster_number = clustering_standardized(duo)
  cluster_outcome = produce_clustertable(duo,cluster_number)
  clusterplayer = unique(duo$accountId)
  predictlist = UBCF(m,accm,mcount,nn,player,n,simmethod,cluster_outcome,clusterplayer,allplayers)
  return(predictlist)
}

main_IBCF = function(matchplayer,nn,player,n,simmethod){
  match_duo = datapreprocessing_matchplayer_duo(matchplayer)
  accm = produce_accmatrix(match_duo)
  mcount = produce_countmatrix(match_duo)
  m = accm/mcount
  predictlist = IBCF(m,nn,player,n,simmethod)
  return(predictlist)
}


main_UBCF(stats,matchplayer,10,"account.010fab4f1d34494390a82e8e4d7ef857",5,"Euclidean")
main_IBCF(matchplayer,10,"account.010fab4f1d34494390a82e8e4d7ef857",5,"Euclidean")

#============================================================================================#
#toy example

#example1
#test whether clutering recommendation function works or not 
mat3 = matrix(c(NA,NA,3,NA,1,
  NA,NA,NA,4,NA,
  3,NaN,NA,NA,2,
  NA,4,NA,NA,NA,
  1,NA,2,NA,NA),byrow = T,nrow=5)
rownames(mat3) = c("u1","u2","u3","u4","u5")
colnames(mat3) = c("u1","u2","u3","u4","u5")
accmat3 = mat3
mcount3 = mat3
mcount3[!is.na(mcount3)] = 1
my_clusterplayer3 = c("u1","u2","u3","u4","u5")
my_allplayers3 = c("u1","u2","u3","u4","u5")
my_cluster_outcome3 = cbind(c("u1","u2","u3","u4","u5"),c("4","2","3","4","1"))
my_cluster_outcome3 = as.data.table(my_cluster_outcome3)
colnames(my_cluster_outcome3) = c("accountId","cluster")

UBCF(mat3,accmat3,mcount3,3,"u4",2,"Euclidean",my_cluster_outcome3,my_clusterplayer3,my_allplayers3)
IBCF(mat3,3,"u4",2,"Euclidean")


#example2
#the example in my final presentation
mat_f = matrix(c(NA,NA,NA,10,22,NA,34,
  NA,NA,31,19,46,NA,16,
  NA,31,NA,8,3,18,5,
  10,19,8,NA,1,41,NA,
  22,46,3,1,NA,13,NA,
  NA,NA,18,41,13,NA,27,
  34,16,5,NA,NA,27,NA),byrow = T,nrow=7)
rownames(mat_f) = c("Kevin","Jessie","Lena","Franklin","Bruce","Eileen","Flora")
colnames(mat_f) = c("Kevin","Jessie","Lena","Franklin","Bruce","Eileen","Flora")
accm_f = mat_f
mcount_f = mat_f
mcount_f[!is.na(mcount_f)] = 1
my_clusterplayer_f = c("Kevin","Jessie","Lena","Franklin","Bruce","Eileen","Flora")
my_allplayers_f = c("Kevin","Jessie","Lena","Franklin","Bruce","Eileen","Flora")
my_cluster_outcome_f = cbind(c("Kevin","Jessie","Lena","Franklin","Bruce","Eileen","Flora"),
  c("4","2","3","4","1","2","3"))
my_cluster_outcome_f = as.data.table(my_cluster_outcome_f)
colnames(my_cluster_outcome_f) = c("accountId","cluster")


UBCF(mat_f,accm_f,mcount_f,3,"Kevin",6,"Euclidean",my_cluster_outcome_f,my_clusterplayer_f,my_allplayers_f)
#see the detail of steps in UBCF 
pmat_f = as(mat_f,"realRatingMatrix")
sim_f = similarity(pmat_f,method="Euclidean")
simmat_f = as(sim_f,"matrix")
mat_f[is.na(mat_f)] = 0
simmat_f[is.na(simmat_f)] = 0
user_sim_f = simmat_f[1,2:7] * (mat_f[2:7,]!=0)
unrate_index_f = (mat_f[1,]==0) & (names(mat_f[1,])!=rownames(mat_f)[1])
unrate_sim_f = user_sim_f[,unrate_index_f]
nnindex_f = apply(unrate_sim_f ,2,order,decreasing = T)[1:3,]
mostsim_sim_f = draw_sim(unrate_sim_f,nnindex_f)
mostsim_rate_f = draw_rate(mat_f,unrate_sim_f,nnindex_f)
prediction_f = apply(mostsim_sim_f * mostsim_rate_f,2,sum) / apply(mostsim_sim_f,2,sum)
userrate_f = c(prediction_f,mat_f[1,][names(mat_f[1,])!=rownames(mat_f)[1] & !unrate_index_f])
sort(userrate_f)



IBCF(mat_f,3,"Kevin",6,"Euclidean")
#see the detail of steps in IBCF
pmat_f = as(mat_f[2:7,],"realRatingMatrix")
sim_f = similarity(pmat_f,method="Euclidean",which="items")
simmat_f = as(sim_f,"matrix")
mat_f[is.na(mat_f)] = 0
simmat_f[is.na(simmat_f)] = 0
user_sim_f = simmat_f * (mat_f[1,]!=0)
unrate_index_f = (mat_f[1,]==0) & (names(mat_f[1,])!=rownames(mat_f)[1])
unrate_sim_f = user_sim_f[,unrate_index_f]
nnindex_f = apply(unrate_sim_f,2,order,decreasing = T)[1:3,]
mostsim_sim_f = draw_sim(unrate_sim_f,nnindex_f)
mostsim_rate_f = draw_rate_i(1,mat_f,nnindex_f)
prediction_f = apply(mostsim_sim_f * mostsim_rate_f,2,sum) / apply(mostsim_sim_f,2,sum)
userrate_f = c(prediction_f,mat_f[1,][names(mat_f[1,])!=rownames(mat_f)[1] & !unrate_index_f])
sort(userrate_f)
