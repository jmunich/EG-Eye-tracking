eye_output<-simulate_search(3000,1,n=1)

t_simulated_data<-eye_output$transition_data
mt_simulated_data<-eye_output$meta_transition_data
mt_simulated_data[,-c(1,2)]<-mt_simulated_data[,-c(1,2)]/rowSums(mt_simulated_data[,-c(1,2)])

rule_means<-aggregate(mt_simulated_data[,-c(1,2)],by=list(mt_simulated_data$rule),mean)
rule_sd<-aggregate(mt_simulated_data[,-c(1,2)],by=list(mt_simulated_data$rule),sd)

data_output<-t(rule_means)
c_names<-data_output[1,]
r_c_names<-c("Optimist","Pessimist","Naive","Altruist","K-1","K-2","D-1","D-2","Nash")
data_output<-data_output[-1,]
r_names<-rownames(data_output)

data_output <- mapply(data_output, FUN=as.numeric)
data_output <- matrix(data_output,ncol=length(c_names))

colnames(data_output)<-c_names
rownames(data_output)<-r_names
data_output<-data_output[,r_c_names]

boxplot_data<-reshape2::melt(mt_simulated_data[,-1])



pdf("rplot.pdf")

library("RColorBrewer")
require("plotrix")
gradcol1<-brewer.pal(n = 8, name = "YlOrRd")[-c(1:4)]
gradcol2<-brewer.pal(n = 9, name = "YlGnBu")[-c(1:4)]
gradcol<-c(gradcol1,gradcol2)


par(mar=c(7,2,3,2))
plot(c(1:length(r_names)),data_output[,1],type="l",ylim=c(0,max(data_output)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion average")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 1:length(c_names)){
points(c(1:length(r_names)),data_output[,i],type="l",col=gradcol[i],lwd=2)
}

legend("topleft", c_names[c(1,6,7,8,2,9,3,4,5)],fill=gradcols)

for(i in c(1:length(c_names))){
  plot(c(1:length(r_names)),data_output[,i],type="l",ylim=c(0,max(data_output)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main=paste(r_c_names[i]," proportion average",sep=""))
  abline(v=c(1:length(r_names)),h=0,col="grey")
  axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
  points(c(1:length(r_names)),data_output[,i],type="l",col=gradcols[i],lwd=2)
}

data_output<-t(rule_sd)
c_names<-data_output[1,]
r_c_names<-c_names[c(1,6,7,8,2,9,3,4,5)]
data_output<-data_output[-1,]
r_names<-rownames(data_output)

data_output <- mapply(data_output, FUN=as.numeric)
data_output <- matrix(data_output,ncol=length(c_names))

plot(c(1:length(r_names)),data_output[,1],type="l",ylim=c(0,max(data_output)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion SD")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 1:length(c_names)){
points(c(1:length(r_names)),data_output[,i],type="l",col=gradcol[i],lwd=2)
}

legend("topleft", c_names[c(1,6,7,8,2,9,3,4,5)],fill=gradcols)


for(i in 1:length(c_names)){
plot(c(1:length(r_names)),data_output[,i],type="h",ylim=c(0,max(data_output)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main=paste(r_c_names[i]," proportion SD",sep=""))
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
points(c(1:length(r_names)),data_output[,i],type="h",col=gradcols[i],lwd=2)
}
dev.off()

names(mt_simulated_data)[-c(1,2)]

  
for(i in 3:38){  
boxplot(mt_simulated_data[,c(1,2)] ~ rule, data = mt_simulated_data)
}


meltData <- reshape2::melt(mt_simulated_data[,-1])

for(i in unique(meltData$rule)){
  boxplot(data=meltData[meltData$rule==i,], value~variable, main=i,xaxt="n")
  abline(v=c(1:length(r_names)),h=0,col="grey")
  axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
}

a<-search_k.level(game,2)
animate_search(a)
