get_transitions<-function(ed){
c<-0
file<-c()
for(i in 1:(length(ed)-1)){
  c<-c+1
  transition<-rbind(ed[[i]],ed[[i+1]])
  
  if(colSums(transition)[2]%%2==0){
    if(transition[1,2]%%2==0){
      belong<-"own"
    }
    if(transition[1,2]%%2==1){
      belong<-"other"
    }
    if(belong=="own"){
      if(transition[1,2]==transition[2,2]){
        direction<-"within"
      }
      if(transition[1,2]!=transition[2,2]){
        direction<-"between"
      }
    }
    if(belong=="other"){
      if(transition[1,1]==transition[2,1]){
        direction<-"within"
      }
      if(transition[1,1]!=transition[2,1]){
        direction<-"between"
      }
    }
  }
  if(colSums(transition)[2]%%2==1){
    if(transition[1,1]==transition[1,1]&(transition[1,2]-transition[2,2])^2==1&(max(transition[,2])%%2==0))
      {
      belong<-"intra"
      direction<-"cell"
    }
    if(!(transition[1,1]==transition[1,1]&(transition[1,2]-transition[2,2])^2==1&(max(transition[,2])%%2==0)))
      {
      belong<-"skip"
      direction<-" "
    }
  }
  file[c]<-paste(belong,direction,sep="-")
}
return(file)
}

DATA<-list()
for(k in 1:100){
Data<-list()
for(j in 1:50){
  a<-search_domination(game,4)
  ed<-a$eye_movement
  trans<-get_transitions(a$eye_movement)
  data_row<-table(trans)
  Data[[j]]<-data_row[nv]
}
results<-matrix(unlist(Data),ncol=length(nv),byrow=TRUE)
results[is.na(results)]<-0
DATA[[k]]<-colMeans(results)
}  
results<-matrix(unlist(DATA),ncol=length(nv),byrow=TRUE)
par(mfrow=c(2,3))
hist(results[,1])
hist(results[,2])
hist(results[,3])
hist(results[,4])
hist(results[,5])
hist(results[,6])


DATA<-list()
for(k in 1:100){
Data<-list()
for(j in 1:50){
  a<-search_domination(game,1)
  ed<-a$eye_movement
  trans<-get_transitions(a$eye_movement)
  data_row<-table(trans)
  Data[[j]]<-data_row[nv]
}
results<-matrix(unlist(Data),ncol=length(nv),byrow=TRUE)
results[is.na(results)]<-0
DATA[[k]]<-colMeans(results)
}  
results<-matrix(unlist(DATA),ncol=length(nv),byrow=TRUE)
par(mfrow=c(2,3))
for(i in 1:length(nv)){
hist(results[,i],main=nv[i])
}




results[is.na(results)]<-0

hist(results[,1])
hist(results[,2])
hist(results[,3])
hist(results[,4])
hist(results[,5])





