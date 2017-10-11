#Use Random Walker
library(readr)
#pulling data from Massey Site
scores <- read.csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=1", header=FALSE)
teams <- read_csv("http://masseyratings.com/scores.php?s=292154&sub=12191&all=1&mode=2&format=2", col_names = FALSE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2")
names(teams)<-c("Label","Team")

#Use Random Walker

if(scores$Score1[i]>scores$Score2[i]){
  nw++
} else{
  nl++
}

p = .75
A=matrix(rep(0,length(teams$Team)^2),nrow=length(teams$Team))
b=rep(1,length(teams$Team))
diag(A)=rep((-(1-p)nw-pnl),legnth(diag(A)))



e<- eigen(D)
library(tidyverse)
near(e$values,0)

e$vectors[,4]
e$vectors[,4]/sum(e$vectors[,4])*4

