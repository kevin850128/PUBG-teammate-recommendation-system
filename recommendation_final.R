#clutering for standardized data
clustering_standardized = function(data){ #data=duo
  data_s = apply(data[,5:ncol(data)],2,scale)
  data_s = as.data.table(cbind(data[,1:4],data_s))
  kmedoid_s = pam(data_s[,5:ncol(data_s)],k=4) #best number of cluster is 4
  cluster_number = kmedoid_s$clustering
  return(cluster_number)
}

#summarize the k-medoids clutering resulit into a table
produce_clustertable = function(data,cluster_number){ 
  #data=duo, cluster_number=output from clustering_standardized()
  clusterplayer = unique(data$accountId)
  cluster_outcome = cbind(data$accountId,cluster_number)
  cluster_outcome = as.data.table(cluster_outcome)
  colnames(cluster_outcome) = c("accountId","cluster")
  return(cluster_outcome)
}

#find the best teammate cluster for a player
bestcluster_teammate = function(m,accm,mcount,cluster_outcome,clusterplayer){
  rl = which(!is.na(m),arr.ind = T)
  container = matrix(0,nrow=1,ncol=6)
  for(j in 1:dim(rl)[1]){
    r = rownames(m)[rl[j,1]]
    c = colnames(m)[rl[j,2]]
    if((r %in% clusterplayer) & (c %in% clusterplayer)){
      if(as.numeric(cluster_outcome[accountId==r,cluster]) > 
          as.numeric(cluster_outcome[accountId==c,cluster])){
        clus = c(cluster_outcome[accountId==c,cluster],cluster_outcome[accountId==r,cluster])
      }else{
        clus = c(cluster_outcome[accountId==r,cluster],cluster_outcome[accountId==c,cluster])
      }
      container = rbind(container,c(r,c,accm[r,c],mcount[r,c],clus))
    }
  }
  container = container[-1,]
  container = as.data.table(container)
  colnames(container) = c("accountId","teammateId","accwinplace","gamecount","aclus","tclus")
  container$accwinplace = as.numeric(container$accwinplace)
  container$gamecount = as.numeric(container$gamecount)
  cluster_ref = container %>% group_by(aclus,tclus) %>%
    summarise(mean_winplace = sum(accwinplace)/sum(gamecount))
  cluster_ref_mat = dcast(cluster_ref,aclus~tclus,value.var=c("mean_winplace")) %>% as.matrix
  cluster_ref_mat = cluster_ref_mat[,-1] %>% apply(2, as.numeric)
  cluster_ref_mat[lower.tri(cluster_ref_mat)] = t(cluster_ref_mat)[lower.tri(cluster_ref_mat)]
  cluster_ref_vector = apply(cluster_ref_mat,2,function(x){which(x==min(x,na.rm = T),arr.ind = T)})
  return(cluster_ref_vector)
}

#male a matrix accumulate ranks of each combination of players 
produce_accmatrix = function(data){ #data=match_duo
  allplayers = union(data$accountId,data$teammateId1)
  allplayers = allplayers[allplayers!=""]
  n = length(allplayers)
  accm = matrix(0,n,n) #accumulated winplace matrix
  rownames(accm) = allplayers
  colnames(accm) = allplayers
  for(i in 1:nrow(data)){
    if(data[i,teammates] == 0){next}
    else{
      irow = data[i,accountId]
      icol = data[i,teammateId1]
      accm[irow,icol] = accm[irow,icol]+data[i,winPlace]
      accm[icol,irow] = accm[icol,irow]+data[i,winPlace]
    }
  }
  return(accm)
}

#male a matrix count games played of each combination of players 
produce_countmatrix = function(data){ #data=match_duo
  allplayers = union(data$accountId,data$teammateId1)
  allplayers = allplayers[allplayers!=""]
  n = length(allplayers)
  mcount = matrix(0,n,n) #game count matrix
  rownames(mcount) = allplayers
  colnames(mcount) = allplayers
  for(i in 1:nrow(data)){
    if(data[i,teammates] == 0){next}
    else{
      irow = data[i,accountId]
      icol = data[i,teammateId1]
      mcount[irow,icol] = mcount[irow,icol]+1
      mcount[icol,irow] = mcount[icol,irow]+1
    }
  }
  return(mcount)
}

#create similarity matrix for players
sim_mat_UBCF = function(m,simmethod){ #m=rank matrix, simmethod=method to calculate similarity
  pm = as(m,"realRatingMatrix")
  sm = similarity(pm,method=simmethod)
  sm = as(sm,"matrix")
  return(sm)
}

#create similarity matrix for teammates
sim_mat_IBCF = function(trainindex,m,simmethod){ 
  #trainindex = indexes for all players and teammates except the player to be recommend
  #m=rank matrix, simmethod=method to calculate similarity
  pm = as(m[trainindex,],"realRatingMatrix")
  sm = similarity(pm,method=simmethod,which="items")
  sm = as(sm,"matrix")
  return(sm)
}

#create a matrix that store similarities for calculation
draw_sim = function(mat,topnindex){ #mat = unrank_similarity matrix, topnindex = the most n similar indexes 
  mymat_sim = topnindex
  for(k in colnames(topnindex)){
    mymat_sim[,k] = mat[topnindex[,k],k]
  }
  return(mymat_sim)
}

#create a matrix that store ranks for UBCF(player-based) calculation
draw_rate = function(mat,unrate_sim,topnindex){ #mat = rank matrix, topnindex = the most n similar indexes
  mymat_rate = topnindex
  for(k in colnames(topnindex)){
    mymat_rate[,k] = mat[rownames(unrate_sim)[topnindex[,k]],k]
  }
  return(mymat_rate)
}

#create a matrix that store ranks for IBCF(teammate-based) calculation
draw_rate_i = function(i,mat,topnindex){ #i = the index of the player to be recommended
  #mat = rank matrix, topnindex = the most n similar indexes
  mymat_rate = topnindex
  for(k in colnames(topnindex)){
    mymat_rate[,k] = mat[i,topnindex[,k]]
  }
  return(mymat_rate)
}

#player-based recommendation
UBCF = function(m,accm,mcount,nn,player,n,simmethod,cluster_outcome,clusterplayer,allplayers){
  trainindex = which(rownames(m)!=player)
  i = which(rownames(m)==player) #the index of the player to be recommended
  predictlist = list()
  sim = sim_mat_UBCF(m,simmethod)
  cluster_ref_vector = bestcluster_teammate(m,accm,mcount,cluster_outcome,clusterplayer)
  m[is.na(m)] = 0
  sim[is.na(sim)] = 0
  #clustering recommendation
  if(sum(sim[i,]) == 0 & rownames(m)[i] %in% clusterplayer & (n-length(m[i,][m[i,]!=0]))>0){
    belong = cluster_outcome[accountId==rownames(m)[i],cluster]
    teammate_belong = as.character(cluster_ref_vector[belong])
    possible_teammate = cluster_outcome[cluster==teammate_belong ,accountId]
    candidate_teammate = intersect(possible_teammate,allplayers)
    rec_vec = apply(m[candidate_teammate,,drop=FALSE],1,function(x){sum(x)/length(which(x!=0))})
    rec_vec = rec_vec[rec_vec<=25] #only pick rank<25
    if(length(rec_vec)<=(n-length(m[i,][m[i,]!=0]))){ 
      prediction = rec_vec
    }
    else{prediction = rec_vec[sample(length(rec_vec),size = n-length(m[i,][m[i,]!=0]))]}
    userrate = c(prediction,m[i,][names(m[i,])!=rownames(m)[i] & m[i,]!=0])
    result = sort(userrate)
  }
  #traditional UBCF recommendation
  else{
    user_sim = sim[i,trainindex] * (m[trainindex,]!=0)
    unrate_index = (m[i,] == 0) & (names(m[i,])!=rownames(m)[i]) 
    unrate_sim = user_sim[,unrate_index]
    nnindex = apply(unrate_sim,2,order,decreasing = T)[1:nn,]
    mostsim_sim = draw_sim(unrate_sim,nnindex)
    mostsim_rate = draw_rate(m,unrate_sim,nnindex)
    prediction = apply(mostsim_sim * mostsim_rate,2,sum) / apply(mostsim_sim,2,sum)
    userrate = c(prediction,m[i,][names(m[i,])!=rownames(m)[i] & !unrate_index])
    result = sort(userrate)
  }
  if(length(result) < n){output = result} #prevent from showing NA
  else{output = result[1:n]}
  predictlist[[rownames(m)[i]]] = output  
  return(predictlist)
}

#teammate-based recommendation
IBCF = function(m,nn,player,n,simmethod){
  trainindex = which(rownames(m)!=player)
  i = which(rownames(m)==player) #testindex
  predictlist = list()
  sim = sim_mat_IBCF(trainindex,m,simmethod)
  m[is.na(m)] = 0
  sim[is.na(sim)] = 0
  user_sim = sim * (m[i,]!=0)
  unrate_index = (m[i,] == 0) & (names(m[i,])!=rownames(m)[i])
  unrate_sim = user_sim[,unrate_index]
  nnindex = apply(unrate_sim,2,order,decreasing = T)[1:nn,]
  mostsim_sim = draw_sim(unrate_sim,nnindex)
  mostsim_rate = draw_rate_i(i,m,nnindex)
  prediction = apply(mostsim_sim * mostsim_rate,2,sum) / apply(mostsim_sim,2,sum)
  userrate = c(prediction,m[i,][names(m[i,])!=rownames(m)[i] & !unrate_index])
  result = sort(userrate)
  if(length(result) < n){output = result} #prevent from showing NA
  else{output = result[1:n]}
  predictlist[[rownames(m)[i]]] = output
  return(predictlist)
}
