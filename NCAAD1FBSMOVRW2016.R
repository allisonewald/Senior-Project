
library(readr)
pTD<-0.760
pFG<-0.240 # ignore safeties and assume all TD=7
MaxMOV<-39
MOVChain<-matrix(rep(0,(2*MaxMOV-1)^2),nrow=2*MaxMOV-1,byrow=TRUE)
rhs <- c(rep(0,2*MaxMOV-1-7),rep(pTD/2,4),rep(1/2,3))
for(i in seq(1,(2*MaxMOV-1))){
  MOVChain[i,i]<-1
  if(i+3<=(2*MaxMOV-1)){
    MOVChain[i,i+3] <- -pFG/2
  }
  if(i-3>0){
    MOVChain[i,i-3] <- -pFG/2
  }
  if(i+7<=(2*MaxMOV-1)){
    MOVChain[i,i+7] <- -pTD/2
  }
  if(i-7>0){
    MOVChain[i,i-7] <- -pTD/2
  }
  
}
Expecteds<-solve(MOVChain,rhs)
#pulling data from Massey Site
scores <- read.csv("http://www.masseyratings.com/scores.php?s=286577&sub=11604&all=1&mode=2&format=1", header=FALSE)
teams <- read_csv("http://www.masseyratings.com/scores.php?s=286577&sub=11604&all=1&mode=2&format=2", col_names = FALSE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2")
names(teams)<-c("Label","Team")

A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))
b=rep(1,length(teams$Team))
#diag(A)=rep(2,length(diag(A)))

#max_points=max(c(max( scores$Score1 ),max( scores$Score2 )))
max_points=100

for(i in 1:length(scores$Team1) ){
  
  if(abs(scores$Score1[i]-scores$Score2[i])<39){
    Share1=Expecteds[scores$Score1[i]-scores$Score2[i]+MaxMOV]
  }  else{
    if(scores$Score1[i]>scores$Score2[i]){
      Share1=1
    } else{
      Share1=0
    }
  }
  
  
  Share2=1-Share1
  
  A[ scores$Team1[i] ,scores$Team2[i]  ]=A[ scores$Team1[i] ,scores$Team2[i]  ]+Share2;
  A[ scores$Team2[i] ,scores$Team1[i]  ]=A[ scores$Team2[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team1[i] ,scores$Team1[i]  ]=A[ scores$Team1[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team2[i] ,scores$Team2[i]  ]=A[ scores$Team2[i] ,scores$Team2[i]  ]+Share2;
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


#Use Colley
A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))
b=rep(1,length(teams$Team))
diag(A)=rep(2,length(diag(A)))

for(i in 1:length(scores$Team1) ){
  A[ scores$Team1[i] ,scores$Team2[i]  ]=A[ scores$Team1[i] ,scores$Team2[i]  ]-1;
  A[ scores$Team2[i] ,scores$Team1[i]  ]=A[ scores$Team2[i] ,scores$Team1[i]  ]-1;
  A[ scores$Team1[i] ,scores$Team1[i]  ]=A[ scores$Team1[i] ,scores$Team1[i]  ]+1;
  A[ scores$Team2[i] ,scores$Team2[i]  ]=A[ scores$Team2[i] ,scores$Team2[i]  ]+1;
  
  if(abs(scores$Score1[i]-scores$Score2[i])<MaxMOV){
    Share1=Expecteds[scores$Score1[i]-scores$Score2[i]+MaxMOV]
  }  else{
    if(scores$Score1[i]>scores$Score2[i]){
      Share1=1
    } else{
      Share1=0
    }
  }
  
  
  Share2=-Share1
  
  
  if(abs(scores$Score1[i]-scores$Score2[i]))
    b[ scores$Team1[i] ]=b[ scores$Team1[i] ]- Share2
  b[ scores$Team2[i] ]=b[ scores$Team2[i] ]- Share1
}

Rating=solve(A,b)


rankedteams<-cbind(teams,as.numeric(Rating))
rankedteams<-rankedteams[ order(Rating,decreasing=TRUE), ]
rankings<-cbind(seq(1,length(rankedteams$Team)),rankedteams[2:3])
names(rankings)<-c("Ranking",names(rankings)[2],"Rating")
row.names(rankings)<-seq(nrow(rankings))
write.csv(rankings, paste("FBS MOV Colley ", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

