#Use Random Walker
library(readr)
library(tidyverse)
#pulling data from Massey Site
scores <- read.csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=1", header=FALSE)
teams <- read_csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=2", col_names = FALSE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2")
names(teams)<-c("Label","Team")

num_regular<- scores %>% mutate(RegularSeason = Date < 20170220) %>% group_by(RegularSeason) %>% 
  count() %>% filter(RegularSeason) %>% .$n #Takes out games from tournement

#Use Random Walker
nw=rep(0, length(teams$Team))
nl=rep(0, length(teams$Team))
p = .75
A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))


for( i in 1:num_regular){
  ## Calculate p based on MOV
  
  if(scores$Score1[i]>scores$Score2[i]){
    A[scores$Team1[i],scores$Team1[i]]=A[scores$Team1[i],scores$Team1[i]]-(1-p)
    A[scores$Team2[i],scores$Team2[i]]=A[scores$Team2[i],scores$Team2[i]]-(p)
    A[scores$Team1[i],scores$Team2[i]]=A[scores$Team1[i],scores$Team2[i]]+p
    A[scores$Team2[i],scores$Team1[i]]=A[scores$Team2[i],scores$Team1[i]]+(1-p)

  } else{
    A[scores$Team2[i],scores$Team2[i]]=A[scores$Team2[i],scores$Team2[i]]-(1-p)
    A[scores$Team1[i],scores$Team1[i]]=A[scores$Team1[i],scores$Team1[i]]-(p)
    A[scores$Team2[i],scores$Team1[i]]=A[scores$Team2[i],scores$Team1[i]]+p
    A[scores$Team1[i],scores$Team2[i]]=A[scores$Team1[i],scores$Team2[i]]+(1-p)

  }
}


A

e<- eigen(A)
library(tidyverse)
near(e$values,0)

ranke<-e$vectors[,length(teams$Team)]
ranke <- ranke/sum(ranke)*length(ranke)

rankedteams <- teams %>% mutate(rating=Re(ranke)) %>% arrange(desc(rating)) %>% mutate(ranking = row_number())

