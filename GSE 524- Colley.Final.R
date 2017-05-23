# Colley Project: Final Part

# by: Jack Markavage and Sam Donnelly

# Final part

rm(list=ls())

# Loading the data frame created in part 1
load("~/allfootball.rdata")

# Creating a function with season as its only argument
# Returns the ranks of all D1 teams for that particular season
ColleyRank=function(x){
  all$take=0
  
  n=nrow(all)
  for(i in 1:n){
    if(all$season[i]==x){
      all$take[i]=1
    }
  }
  all=all[all$take==1,]
  
  a=nrow(all)
  ColMatrix=matrix(0,nrow=a,ncol=a)
  
  for(i in 1:a) {
    for (j in all$opponent[[i]]) {
      ColMatrix[i,j]=ColMatrix[i,j]-1
    }
  }
  diag(ColMatrix)=2+all$wins+all$losses
  
  myvector=vector()
  for(i in 1:a){
    myvector[i]=1+((all$wins[i]-all$losses[i])/2)
  }
  
  score=solve(ColMatrix,myvector)
  solution=data.frame(name=all$team,score=score)
  solution=solution[order(solution$score,decreasing=T),]
  rownames(solution)=1:nrow(solution)
  return(solution)
}

