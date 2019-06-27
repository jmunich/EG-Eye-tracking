install.packages("rjags")
install.packages("coda")
require(rjags)
require(coda)

mt_simulated_data
names(mt_simulated_data)[c(24:26,30:32,15:17,34:37)]
A<-rowSums(mt_simulated_data[,c(3,8,33)])
B<-rowSums(mt_simulated_data[,c(4:7,9:11,15:17,21,27)])
C<-rowSums(mt_simulated_data[,c(24:26,30:32,15:17,34:37)])
D<-mt_simulated_data[,38]

rule<-mt_simulated_data$rule
reduced_data<-as.data.frame(cbind(A,B,C,D))

game<-rep(c(rep(FALSE,4),rep(TRUE,5)),3000)

er0<-.1
er1<-.15

ns<-rowSums(cbind(A,B,C,D))

for(k in 1:length(ns)){
  if(game){
    er<-er0
  }
  if(!game){
    er<-er1
  }
  for(i in 1:length(reduced_data[1,])){
    for(j in 1:length(reduced_data[k,i])){
      skill<-runif(1,0,1)
      if(skill<er){
        reduced_data[k,i]<-reduced_data[k,i]-1
        
        
      }  
    }
  }
}

meltData <- reshape2::melt(data.frame(rule,reduced_data))

for(i in unique(meltData$rule)){
  boxplot(data=meltData[meltData$rule==i,], value~variable, main=i,xaxt="n")
  abline(v=c(1:length(r_names)),h=0,col="grey")
  axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
}
