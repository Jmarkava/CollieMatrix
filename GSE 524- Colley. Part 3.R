# Colley Project: Final Part

# by: Jack Markavage and Sam Donnelly

rm(list=ls())

# the set length oh charecters for each variable in the data sets
fl=c(10,28,4,28,4,16)
all=data.frame(season=(0),team=(0),wins=(0),losses=(0),opponent=(0))

for(season in 1961:2010){
  data=read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",season,"gms.txt",sep=""),fl,col.names=c("Date","AwayTeam","AwayScore","HomeTeam","HomeScore","Location"))[-6]
  
  # Removing the blanck Spaces
  data$HomeTeam=gsub(" ","",data$HomeTeam)
  data$AwayTeam=gsub(" ","",data$AwayTeam)
  
  #Removing the () around some teams b\c it Automatically flags them for D2, and subsequent removal
  data$HomeTeam=gsub("\\(","",data$HomeTeam)
  data$HomeTeam=gsub("\\)","",data$HomeTeam)
  data$AwayTeam=gsub("\\(","",data$AwayTeam)
  data$AwayTeam=gsub("\\)","",data$AwayTeam)
  
  #Removing division 2 teams
  data$remove=0
  for(i in 1:nrow(data)){
    if((length(grep(data$AwayTeam[i],data$AwayTeam))+length(grep(data$AwayTeam[i],data$HomeTeam)))<6|(length(grep(data$HomeTeam[i],data$HomeTeam))+length(grep(data$HomeTeam[i],data$AwayTeam)))<6){
      data$remove[i]=1
    }
  }
  data=data[data$remove==0,]
  
  #Creating rows for if home team won, lost, or tied
  data$homewin=(0)
  data$homeloss=(0)
  data$tie=(0)
  
  # Removing ties from the data
  for(i in 1:nrow(data)){
    if(data$HomeScore[i]>data$AwayScore[i]){
      data$homewin[i]=1
    }else{
      if(data$HomeScore[i]<data$AwayScore[i]){
        data$homeloss[i]=1
      }
    }
  }
  for(i in 1:nrow(data)){
    if(data$HomeScore[i]==data$AwayScore[i]){
      data$tie[i]=1
    }
  }
  data=data[data$tie==0,]
  
  # Creating a new data frame so i can get the unique names of teams from the data
  teams=data.frame(data$AwayTeam,data$HomeTeam)
  teams=c(t(teams))
  
  #creating the data set
  myframe=data.frame(season=season,team=unique(teams),wins=0,losses=0,tempid=0,opponent=0) 
  m=as.numeric(length(myframe$team))
  myframe$tempid=1:m
  myframe$opponent=as.list(myframe$opponent)
  
  # Computing the number of wins and losses per unique team
  n=as.numeric(nrow(data))
  
  data$HomeTeam=factor(data[,4],levels=levels(myframe[,2]))
  for(i in 1:m){
    for(j in 1:n){
      if(myframe$team[i]==data$HomeTeam[j]){
        if(data$homewin[j]==1){
          myframe$wins[i]= myframe$wins[i]+1
        }else{
          myframe$losses[i]= myframe$losses[i]+1
        }
      }
    }
  }
  data$AwayTeam=factor(data[,2],levels=levels(myframe[,2]))
  for(i in 1:m){
    for(j in 1:n){
      if(myframe$team[i]==data$AwayTeam[j]){
        if(data$homeloss[j]==1){
          myframe$wins[i]= myframe$wins[i]+1
        }else{
          myframe$losses[i]= myframe$losses[i]+1
        }
      }
    }
  }
  
  # getting the ID's in the data
  data$homeid=0
  data$awayid=0
  
  for(i in 1:m){
    for(j in 1:n){
      if(myframe$team[i]==data$HomeTeam[j]){
        data$homeid[j]=myframe$tempid[i]
      }else{
        if(myframe$team[i]==data$AwayTeam[j]){
          data$awayid[j]=myframe$tempid[i]
        }
      }
    }
  }
  # getting the List of opponents
  for(i in 1:m){
    for(j in 1:n){
      if(myframe$tempid[i]==data$awayid[j]){
        myframe$opponent[[i]]=c(myframe$opponent[[i]],data$homeid[j])
      }else{
        if(myframe$tempid[i]==data$homeid[j]){
          myframe$opponent[[i]]=c(myframe$opponent[[i]],data$awayid[j])
        }
      }
    }
  }
  # Removing the first value from each opponent vector as they are all 0
  for(i in 1:m){
    myframe$opponent[[i]]=myframe$opponent[[i]][-1]
  }
  
  myframe=myframe[-5]
  
  all=rbind(all,myframe)
} 
all=all[-1,]
rownames(all)=1:nrow(all)

save(all,file="allfootball.rdata")

