#Use Massey
library(readr)
#pulling data from Massey Site
scores <- read.csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=1", header=FALSE)
teams <- read_csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=2", col_names = FALSE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2")
names(teams)<-c("Label","Team")


A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))
b=rep(0,length(teams$Team))


for(i in 1:length(scores$Team1) ){
  A[ scores$Team1[i] ,scores$Team2[i]  ]=A[ scores$Team1[i] ,scores$Team2[i]  ]-1;
  A[ scores$Team2[i] ,scores$Team1[i]  ]=A[ scores$Team2[i] ,scores$Team1[i]  ]-1;
  A[ scores$Team1[i] ,scores$Team1[i]  ]=A[ scores$Team1[i] ,scores$Team1[i]  ]+1;
  A[ scores$Team2[i] ,scores$Team2[i]  ]=A[ scores$Team2[i] ,scores$Team2[i]  ]+1;
 
  Share1=scores$Score1[i] -scores$Score2[i]

  Share2=-Share1
  
  
  b[ scores$Team1[i] ]=b[ scores$Team1[i] ]+ Share1
  b[ scores$Team2[i] ]=b[ scores$Team2[i] ]+ Share2
}

A[9, ]= rep(1,9)
b[9]=0

Rating=solve(A,b)


rankedteams<-cbind(teams,as.numeric(Rating))
rankedteams<-rankedteams[ order(Rating,decreasing=TRUE), ]
rankings<-cbind(seq(1,length(rankedteams$Team)),rankedteams[2:3])
names(rankings)<-c("Ranking",names(rankings)[2],"Rating")
row.names(rankings)<-seq(nrow(rankings))
write.csv(rankings, paste("IIAC Massey ", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)







