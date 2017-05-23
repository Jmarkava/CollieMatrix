load("~/ncaa_data.rdata")
str(df)


A=matrix(0,nrow=nrow(df),ncol=nrow(df))
  
  for(i in 1:nrow(df)) {
    for (j in df$opponents[[i]]) {
      A[i,j]=A[i,j]-1
    }
  }
  diag(A)=2+df$wins+df$losses

myvector=vector()
for(i in 1:nrow(df)){
  myvector[i]=1+((df$wins[i]-df$losses[i])/2)
}

solve(A,myvector)

solution=data.frame(name=df$teams,score=solve(A,myvector))
solution=solution[order(solution$score,decreasing=T),]

print(solution)





