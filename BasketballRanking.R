library(dplyr)
p3<-65581.66667/166216.2778
p2<-274157.6667/624653
p1<-(1-p3-p2)
MaxMOV<-32
MOVChain<-matrix(rep(0,(2*MaxMOV-1)^2),nrow=2*MaxMOV-1,byrow=TRUE)
rhs <- c(rep(0,2*MaxMOV-1-3),p3/2,(p3+p2)/2,1/2)

for(i in seq(1,(2*MaxMOV-1))){
  MOVChain[i,i]<-1
  if(i+3<=(2*MaxMOV-1)){
    MOVChain[i,i+3] <- -p3/2
  }
  if(i-3>0){
    MOVChain[i,i-3] <- -p3/2
  }
  if(i+2<=(2*MaxMOV-1)){
    MOVChain[i,i+2] <- -p2/2
  }
  if(i-2>0){
    MOVChain[i,i-2] <- -p2/2
  }
  if(i+1<=(2*MaxMOV-1)){
    MOVChain[i,i+1] <- -p1/2
  }
  if(i-1>0){
    MOVChain[i,i-1] <- -p1/2
  }
  
}
Expecteds<-solve(MOVChain,rhs)


# #pulling data from Kaggle CSV
allscores <- read.csv("~/Senior/Ranking Project/Kaggle Data/RegularSeasoncompactResults.csv", header=FALSE)
teams <- read.csv("~/Senior/Ranking Project/Kaggle Data/TeamCodes.csv", header=TRUE)
names(allscores)<-c("Year","Day","Team1","Home1","Score1","Team2","Score2","Home2", "OT")
names(teams)<-c("Label","Team")
scores<-subset(allscores, Year > 2016)


# #pulling data from Massey Ratings Site
# scores <- read.csv("https://www.masseyratings.com/scores.php?s=298892&sub=11590&all=1&mode=3&format=1", header=FALSE)
# teams <- read.csv("https://www.masseyratings.com/scores.php?s=298892&sub=11590&all=1&mode=3&format=2", header=TRUE)
# names(scores)<-c("Year","Day","Team1","Home1","Score1","Team2","Home2","Score2")
# names(teams)<-c("Label","Team")



A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))
b=rep(1,length(teams$Team))

#max_points=max(c(max( scores$Score1 ),max( scores$Score2 )))
#max_points=100

for(i in 1:length(scores$Team1) ){
  
  if(abs(scores$Score1[i]-scores$Score2[i])<MaxMOV){
    Share1=Expecteds[scores$Score1[i]-scores$Score2[i]+MaxMOV]
  }  else{
    if(scores$Score1[i]>scores$Score2[i]){
      Share1=1
    } else{
      Share1=0
    }
  }
  
  
  Share2=1-Share1
  
  A[ match(scores$Team1[i],teams$Label)  ,match(scores$Team2[i], teams$Label)  ]=A[ match(scores$Team1[i], teams$Label) ,match(scores$Team2[i],teams$Label)  ]+Share2;
  A[ match(scores$Team2[i], teams$Label) ,match(scores$Team1[i],teams$Label)  ]=A[ match(scores$Team2[i], teams$Label) , match(scores$Team1[i], teams$Label)   ]+Share1;
  A[ match(scores$Team1[i], teams$Label) ,match(scores$Team1[i], teams$Label)  ]=A[ match(scores$Team1[i],teams$Label) ,match(scores$Team1[i],teams$Label)  ]+Share1;
  A[ match(scores$Team2[i],teams$Label) ,match(scores$Team2[i], teams$Label)  ]=A[ match(scores$Team2[i], teams$Label) ,match(scores$Team2[i], teams$Label)  ]+Share2;
  if( i%%10 ==0 ){
    print(c(i,Share1,Share2))
  }
}
image(A)
library(igraph)
row.names(A) <- teams$Team;
colnames(A) <- teams$Team;
game_graph <-graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE, diag = FALSE)

A_unnormed <- A

for(i in 1:length(teams$Team)){
  if(sum(A[i,])!=0){ 
    A[i,]=A[i,]/sum(A[i,])
  }
}
#rank<-sol/norm(sol,"2")

#Put in to deal with the fact that we don't neccessarily have one communication class.

library(expm)
Rating<-t(b)%*% (A)
for( n in 1:1000 ){
  Rating <- Rating %*% A
}



#Rating<-rowSums( eigen(t(A))$vectors[,eigen(t(A))$values==1])*64/sum(eigen(t(A))$values==1)
rankedteams<-data.frame(teams,as.numeric(Rating))
rankedteams<-rankedteams[ order(Rating,decreasing=TRUE), ]
rankings<-cbind(seq(1,length(rankedteams$Team)),rankedteams[2:3])
names(rankings)<-c("Ranking",names(rankings)[2],"Rating")
row.names(rankings)<-seq(nrow(rankings))
write.csv(rankings, paste("FBS MOV RW ", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)
A_RW<-A





