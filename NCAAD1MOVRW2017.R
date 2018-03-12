library(dplyr)
p3<-590235/1495946.5
p2<-2467419/5621877
p1<-(1-p3-p2)
MaxMOV<-31
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
#pulling data from Massey Site
scores <- read.csv("~/Senior/Ranking Project/Kaggle Data/RegularSeasoncompactResults.csv", header=FALSE)
teams <- read.csv("~/Senior/Ranking Project/Kaggle Data/TeamCodes.csv", header=TRUE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2", "OT")
names(teams)<-c("Label","Team")

A=matrix(rep(0,length(teams[,2])^2),nrow=length(teams[,2]))
b=rep(1,length(teams[,2]))
#diag(A)=rep(2,length(diag(A)))

#max_points=max(c(max( scores$Score1 ),max( scores$Score2 )))
max_points=100

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
  A[ scores$Team1[i] ,scores$Team2[i]  ]=A[ scores$Team1[i] ,scores$Team2[i]  ]+Share2;
  A[ scores$Team2[i] ,scores$Team1[i]  ]=A[ scores$Team2[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team1[i] ,scores$Team1[i]  ]=A[ scores$Team1[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team2[i] ,scores$Team2[i]  ]=A[ scores$Team2[i] ,scores$Team2[i]  ]+Share2;
  #   roundsleft= ceiling((max_points-scores$Score1[i])/3)+ceiling((max_points-scores$Score2[i])/3);
  #   probs=matrix(rep(0,107^2),nrow=107)
  #   probs[ scores$Score1[i]+1,scores$Score2[i]+1]=1
  #   for(j in 1:roundsleft){
  #     probs<-FBMatrixUpdate(probs)
  #   }
  #   Share1=sum(probs[101:107,])
  #   Share2=sum(probs[,101:107])
  
  
  #b[ scores$Team1[i] ]=b[ scores$Team1[i] ]+(Share1-Share2)/2
  #b[ scores$Team2[i] ]=b[ scores$Team2[i] ]+(Share2-Share1)/2
  
  #b[ scores$Team1[i] ]=b[ scores$Team1[i] ]-(pbinom(max_points-scores$Score1[i],roundsleft,.5)-pbinom(max_points-scores$Score1[i],roundsleft,.5,lower.tail=FALSE))/2;
  #b[ scores$Team2[i] ]=b[ scores$Team2[i] ]-(pbinom(max_points-scores$Score2[i],roundsleft,.5)-pbinom(max_points-scores$Score2[i],roundsleft,.5,lower.tail=FALSE))/2;
  if( i%%10 ==0 ){
    print(c(i,Share1,Share2))
  }
  
}
A_unnormed <- A
for(i in 1:length(teams[,2])){
  if(sum(A[i,])!=0){ 
    A[i,]=A[i,]/sum(A[i,])
  }
}

library(expm)
b=t(rep(1,length(teams[,2])))
library(expm)
Rating<-b%*% (A)
for( n in 1:1000 ){
  Rating <- Rating %*% A
}
#rank<-sol/norm(sol,"2")
#Rating<-eigen(t(A))$vectors[,1]
#Rating<-Rating/sum(Rating)*dim(teams)[1]/2
rankedteams<-cbind(teams,as.numeric(Rating))
rankedteams<-rankedteams[ order(Rating,decreasing=TRUE), ]
rankings<-cbind(seq(1,length(rankedteams$Team)),rankedteams[2:3])
names(rankings)<-c("Ranking",names(rankings)[2],"Rating")
row.names(rankings)<-seq(nrow(rankings))
write.csv(rankings, paste("D1 MOV RW ", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

predict_info <- full_join(rankings, teams, by="Team")