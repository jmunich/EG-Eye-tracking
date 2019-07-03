library("RColorBrewer")
require("plotrix")

### Get average scores per group and chance proportions
all_ns<-matrix(rowMeans(data_output[,c("Optimist","Pessimist","Naive","Altruist")]),ncol=1)
all_ns_ea<-matrix(rowMeans(data_output[,c("Optimist","Pessimist","Naive")]),ncol=1)
all_s<-matrix(rowMeans(data_output[,c("K-1","K-2","D-1","D-2","Nash")]),ncol=1)
all_s_ek1<-matrix(rowMeans(data_output[,c("K-2","D-1","D-2","Nash")]),ncol=1)
row.names(all_ns)<-row.names(all_ns_ea)<-row.names(all_s)<-row.names(all_s_ek1)<-row.names(data_output)

chances_raw<-chance_meta_transition_probabilities(3,3,1)
chances1<-chances_raw[rownames(data_output)]
r_names<-row.names(data_output)




### Get colours for plotting
gradcol1<-brewer.pal(n = 8, name = "YlOrRd")[-c(1:4)]
gradcol2<-brewer.pal(n = 9, name = "YlGnBu")[-c(1:4)]
type_col<-brewer.pal(n = 4, name = "PuOr")
gradcol<-c(gradcol1,gradcol2)
all_ns_col<-brewer.pal(n = 8, name = "YlOrRd")[4]
all_ns_ea_col<-brewer.pal(n = 8, name = "YlOrRd")[3]
all_s_col<-brewer.pal(n = 9, name = "YlGnBu")[4]
all_s_ek1_col<-brewer.pal(n = 9, name = "YlGnBu")[3]


### Plot all decision rules

par(mar=c(9,2,3,2))
plot(c(1:length(r_names)),data_output[,1],type="l",ylim=c(0,max(data_output)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion average: All")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 1:length(data_output[1,])){
  points(c(1:length(r_names)),data_output[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_output[,i],pch=19,col=gradcol[i],lwd=2)
}

legend("topleft", colnames(data_output),fill=gradcol,col="white")

### Plot non-strategic decision rules

par(mar=c(9,2,3,2))
plot(c(1:length(r_names)),data_output[,1],type="l",ylim=c(0,max(data_output)),
     col=gradcol[1],lwd=2,xaxt="n",
     xlab="",main="Proportion average: non-strategic")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 1:4){
  points(c(1:length(r_names)),data_output[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_output[,i],pch=19,col=gradcol[i],lwd=2)
}
points(c(1:length(r_names)),all_ns,type="l", lwd=3,col=all_ns_col,lty=2)
points(c(1:length(r_names)),all_ns, pch=15,col=all_ns_col,lty=2)

points(c(1:length(r_names)),all_ns_ea,type="l", lwd=3,col=all_ns_ea_col,lty=2)
points(c(1:length(r_names)),all_ns_ea, pch=15,col=all_ns_ea_col,lty=2)

points(c(1:length(r_names)),chances1,type="l", lwd=3,col="black",lty=1)
points(c(1:length(r_names)),chances1, pch=15,col="black",lty=1)


legend("topright", c(colnames(data_output)[1:4],"Combined","Combined excl. Altruist","Chance"),
       fill=c(gradcol[1:4],all_ns_col,all_ns_ea_col,"black"),col="white")

### Plot strategic decision rules

par(mar=c(9,2,3,2))
plot(c(1:length(r_names)),data_output[,5],type="l",ylim=c(0,max(data_output)),col=gradcol[5],lwd=2,xaxt="n",xlab="",main="Proportion average: strategic")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 5:9){
  points(c(1:length(r_names)),data_output[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_output[,i],pch=19,col=gradcol[i],lwd=2)
}

points(c(1:length(r_names)),all_s,type="l",lwd=3,col=all_s_col,lty=2)
points(c(1:length(r_names)),all_s,pch=15,lwd=3,col=all_s_col,lty=2)

points(c(1:length(r_names)),all_s_ek1,type="l",lwd=3,col=all_s_ek1_col,lty=2)
points(c(1:length(r_names)),all_s_ek1,pch=15,lwd=3,col=all_s_ek1_col,lty=2)

points(c(1:length(r_names)),chances1,type="l", lwd=2,col="black",lty=1)
points(c(1:length(r_names)),chances1, pch=15,col="black",lty=1)


legend("topleft", c(colnames(data_output)[5:9],"Combined"),fill=c(gradcol[5:9],all_s_col))

### Compare strategic to non-strategic
diff<-all_ns-all_s
all_ns<-all_ns[order(diff)]
all_s<-all_s[order(diff)]

plot(c(1:length(r_names)),all_ns,type="l", lwd=3,
     col=all_ns_col, xaxt="n",xlab="",
     ylim=c(-max(data_output),max(data_output)), main="Proportion average: non-strategic vs strategic")

abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names[order(diff)],las=2,cex.axis=.7)
points(c(1:length(r_names)),all_ns,type="l",lwd=3,col=all_ns_col)
points(c(1:length(r_names)),all_ns,pch=15,lwd=3,col=all_ns_col)
points(c(1:length(r_names)),all_s,type="l",lwd=3,col=all_s_col)
points(c(1:length(r_names)),all_s,pch=15,lwd=3,col=all_s_col)
points(c(1:length(r_names)),all_ns-all_s,type="l",lwd=3,col="grey1",lty=3)
points(c(1:length(r_names)),all_ns-all_s,pch=19,lwd=3,col="grey1",lty=3)

points(c(1:length(r_names)),chances1[order(diff)],type="l",col="green",lwd=.1)
points(c(1:length(r_names)),chances1[order(diff)],pch=19,col="green",lwd=.1)

legend("topleft", c("Non-strategic","Strategic","Difference"),fill=c(all_ns_col,all_s_col,"grey1"))





### Plot ordered differences
all_ns<-matrix(rowMeans(data_output[,c(1:4)]),ncol=1)
rownames(all_ns)<-rownames(all_ns)<-rownames(data_output)


differences<-all_ns-all_s
benchmark<-(sqrt(differences^2)/2)+.005

dif_names<-row.names(differences)
A<-all_ns>benchmark&all_s<=benchmark
B<-all_ns>benchmark&all_s>benchmark
C<-all_ns<=benchmark&all_s>benchmark
D<-all_ns<=benchmark&all_s<=benchmark
mt_group<-differences
mt_group[A]<-1
mt_group[B]<-2
mt_group[C]<-3
mt_group[D]<-4
ABCD<-mt_group[,1]
mt_group<-mt_group[order(differences[,1]),]
orders<-order(differences[,1])[order(mt_group)]

data_output_ordered<-data_output[orders,]
dif_names<-dif_names[orders]
differences<-matrix(differences[orders],ncol=1)

plot(c(1:length(r_names)),differences[,1],type="l", lwd=3,
     col="grey1", xaxt="n",xlab="",
     ylim=c(-max(data_output),max(data_output)), main="Differences: non-strategic - strategic (d=relative difference benchmark)")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),dif_names,las=2,cex.axis=.7)

for(i in 1:length(data_output_ordered[1,])){
  if(i>4){
    data_output_ordered[,i]<--data_output_ordered[,i]
  }
  #  if(i==4){
  #    next
  #  }
  points(c(1:length(r_names)),data_output_ordered[,i],type="l",col=gradcol[i],lwd=.1)
  points(c(1:length(r_names)),data_output_ordered[,i],pch=19,col=gradcol[i],lwd=.1)
}
points(c(1:length(r_names)),differences[,1],pch=15, lwd=3,
       col="grey1")
points(c(1:length(r_names)),differences[,1],type="l", lwd=3,
       col="grey1")

for(i in 1:4){
  points(which(ABCD[orders]==i),rep(max(data_output),length(which(ABCD[orders]==i))),col=type_col[i],pch=15)
  points(which(ABCD[orders]==i),rep(max(data_output),length(which(ABCD[orders]==i))),type="l",lwd=5,col=type_col[i])
}

points(c(1:length(r_names)),chances,type="l",col="green",lwd=.1)
points(c(1:length(r_names)),chances,pch=19,col="green",lwd=.1)

legend("bottomleft", c("A: S<d;NS>d","B: S>=d;NS>d","C: S>=d;NS<d","D: S<d;NS<d"),fill=type_col)




### Produce boxplots per game
chances2<-chances_raw[unique(boxplot_data$variable)]

c<-0
for(i in unique(boxplot_data$rule)){
  c<-c+1
#  chances2<-chances2_0+(2*(sqrt(n_mt[i]*chances2_0*(1-chances1))/n_mt[i]))
  boxplot(data=boxplot_data[boxplot_data$rule==i,], value~variable, main=i,xaxt="n", col=gradcol[c], ylim=c(0,max(boxplot_data$value)))
  axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
  points(c(1:length(r_names)),chances2,type="l", lwd=1,col="black",lty=1)
  points(c(1:length(r_names)),chances2, pch=15,col="black",lty=1)
  legend("topright","Chance",pch = 15, lty = 1)
}

n_mt


dev.off()

