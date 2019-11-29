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

source("recommendation_final.r")

datapreprocessing_stats_duo = function(rawdata){
  cols = c("assists","boosts","dBNOs","damageDealt","headshotKills","heals"
    ,"kills","revives","rideDistance","roadKills","suicides",
    "swimDistance","teamKills","timeSurvived","top10s","vehicleDestroys",
    "walkDistance","weaponsAcquired","wins")
  rawdata[,c("rankPointsTitle","longestTimeSurvived","losses"):= NULL]
  mydata = as.data.table(
    rawdata %>% 
      filter(roundsPlayed!=0) %>%
      mutate_at(.vars = cols, .funs=list(pg = ~./roundsPlayed))
  )
  duo = mydata[gameMode=="duo",-cols,with=FALSE]
  return(duo)
}

stats = fread("stats_g.csv",sep = ",",header = T)
duo = datapreprocessing_stats_duo(stats)
#============================================================================#

###duo(data from stats)
##EDA
ggplot(duo,aes(x=roundsPlayed))+geom_histogram(color="black", fill="lightgreen") #games dist
rpc = data.table(x=sort(unique(duo$roundsPlayed)),
  y=cumsum(table(duo$roundsPlayed)/length(duo$roundsPlayed)))
ggplot(rpc,aes(x=x,y=y))+geom_step(color="red")+
  labs(title = "Cdf of games",x="games",y="cdf")
ggplot(duo,aes(x=wins_pg))+geom_histogram(color="black", fill="lightgreen") #win rate dist
wpgc = data.table(x=sort(unique(duo$wins_pg)),
  y=cumsum(table(duo$wins_pg)/length(duo$wins_pg)))
ggplot(wpgc,aes(x=x,y=y))+geom_step(color="red")+
  labs(title = "Cdf of wins_pg",x="wins_pg",y="cdf")
ggplot(duo,aes(x=days))+geom_histogram(color="black", fill="lightgreen") #days dist



ggplot(duo,aes(x=roundsPlayed,y=wins_pg))+geom_point(color="black",fill="red",pch=21)
ggplot(duo[(assists_pg<1)],aes(x=assists_pg,y=wins_pg))+ #affect by outlier
  geom_point(color="black",fill="red",pch=21)+
  geom_smooth(method = "lm")
ggplot(duo,aes(x=kills_pg,y=wins_pg))+geom_point(color="black",fill="red",pch=21)+
  geom_smooth(method = "lm")
ggplot(duo,aes(x=boosts_pg,y=wins_pg))+geom_point(color="black",fill="red",pch=21)+
  geom_smooth(method = "lm")
ggplot(duo[heals_pg<4.5],aes(x=heals_pg,y=wins_pg))+ #affect by outlier
  geom_point(color="black",fill="red",pch=21)+
  geom_smooth(method = "lm")
ggplot(duo,aes(x=suicides_pg,y=teamKills_pg))+geom_point(color="black",fill="red",pch=21)+
  geom_smooth(method = "lm")

cormatrix = cor(duo[,5:ncol(duo)])
ggcorrplot(cormatrix)
#===================================================================================#

###match_duo(data from matchplayer)
##EDA
datapreprocessing_matchplayer_duo = function(rawdata){
  match_duo = rawdata[gameMode=="duo",-c("teammateId2","teammateId3")]
  return(match_duo)
}

matchplayer = fread("matchplayer_g.csv",sep = ",",header = T)
match_duo = datapreprocessing_matchplayer_duo(matchplayer)

sum(match_duo$teammates==0) #number of no teammate
length(unique(match_duo$matchId))

match_duo %>% group_by(matchId) %>% count(matchId,sort = T)
match_duo[matchId=="1578716d-05dc-42cf-9641-fe7197d687a9",]#a game that has many observations

#===================================================================================#
##intersection of each pair of players 

#have to run for a long time

# accm = produce_accmatrix(match_duo)
# mcount = produce_countmatrix(match_duo)
# m = accm/mcount
# intersection = c()
# cbn = combn(nrow(m), 2)
# for(i in 1:(dim(cbn)[2])){
#   intersection[i] = sum((!is.na(m[cbn[1,i],])) * (!is.na(m[cbn[2,i],])))
# }
# intersection = as.data.frame(intersection)
# ggplot(intersection,aes(x=intersection))+geom_bar(color="black", fill="lightgreen")

