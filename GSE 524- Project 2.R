load("ncaa_data.rdata")

# Setting my variables
r=(1+df$wins)/(2+(df$losses+df$wins))

eff.wins=vector()
for(i in 1:nrow(df)) {
  eff.wins[i]=((df$wins[i]-df$losses[i])/2)+(sum(r[df$opponents[[i]]]))
}

r.new=(1+eff.wins)/(2+(df$losses+df$wins))

# Setting the variables i just created into a while loop to run until the iteration is greater than .0001
while(max(r.new-r)>.00001){
  r=r.new
  for(i in 1:nrow(df)) {
    eff.wins[i]=((df$wins[i]-df$losses[i])/2)+(sum(r[df$opponents[[i]]]))
  }
  r.new=(1+eff.wins)/(2+(df$losses+df$wins))
  i=i+1
}

# Creating a data frame to display the solution
solution= data.frame(Teams =df$teams, Rank = r.new)
solution= solution[order(solution$Rank, decreasing=T),]
rownames(solution)=1:120

solution