library("RColorBrewer")
gradcol1<-brewer.pal(n = 8, name = "YlOrRd")[-c(1:4)]
gradcol2<-brewer.pal(n = 9, name = "YlGnBu")[-c(1:4)]
type_col<-brewer.pal(n = 4, name = "PuOr")
gradcol<-c(gradcol1,gradcol2)

data_groups<-simulate_search_get_data(eye_output,uni=FALSE)$data

chances_raw<-chance_meta_transition_probabilities(3,3,1)
chances1<-chances_raw[rownames(data_groups)]
data_groups<-data_groups[order(chances1),]

set.seed(1)

k.o<-kmeans(data_groups, centers = 4, nstart = 10)

G1<-names(k.o$cluster)[k.o$cluster==1]
G2<-names(k.o$cluster)[k.o$cluster==2]
G3<-names(k.o$cluster)[k.o$cluster==3]
G4<-names(k.o$cluster)[k.o$cluster==4]

namlist<-list(
  G1[order(G1)],
  G2[order(G2)],
  G3[order(G3)],
  G4[order(G4)]
)

Namlist<-namlist[order(unlist(lapply(namlist,function(x){x[1]})))]

namA<-Namlist[[1]]
namB<-Namlist[[2]]
namC<-Namlist[[3]]
namD<-Namlist[[4]]

ord<-unlist(Namlist)
data_groups<-data_groups[ord,]
r_names<-rownames(data_groups)
chances1<-chances_raw[rownames(data_groups)]

par(mar=c(9,2,3,2),mfrow=c(1,1))
plot(c(1:length(r_names)),data_groups[,1],type="l",ylim=c(min(data_groups),max(data_groups)+.4),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion average: All")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=.7)
for(i in 1:length(data_groups[1,])){
  points(c(1:length(r_names)),data_groups[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_groups[,i],pch=19,col=gradcol[i],lwd=2)
}

#legend("topleft", colnames(data_groups),fill=gradcol,col="white", ncol=2)

points(c(1:length(r_names)),chances1,type="l",col="black",lwd=2)
points(c(1:length(r_names)),chances1,pch=19,col="black",lwd=2)

group<-rep(c(1:4),lengths(Namlist))
names(group)<-unlist(Namlist)

points(c(1:length(group)),rep(max(data_groups),length(group))+.1,col=type_col[group],pch=15,cex=1.5)
legend("topleft",LETTERS[1:4],horiz = TRUE, fill=unique(type_col), bg="white")



par(mar=c(5,2,3,2),mfrow=c(1,2))

par(mar=c(4,2,2,2),mfrow=c(1,1))

data_groups_3<-simulate_search_get_data(eye_output,uni=TRUE)$data
r_names<-rownames(data_groups_3)
chances_raw_2<-chance_meta_transition_probabilities(3,3,0)
chances1<-chances_raw_2[rownames(data_output)]

plot(c(1:length(r_names)),data_groups_3[,1],type="l",ylim=c(min(data_groups_2),max(data_groups_2)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion average: All")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=2,cex.axis=1)
for(i in 1:length(data_groups_3[1,])){
  points(c(1:length(r_names)),data_groups_3[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_groups_3[,i],pch=19,col=gradcol[i],lwd=2)
}

points(c(1:length(r_names)),chances1,type="l",col="black",lwd=4)
points(c(1:length(r_names)),chances1,pch=19,col="black",lwd=4)

legend("top", c(colnames(data_groups),"Chance"),fill=c(gradcol,"black"),col="white", ncol=5)

data_groups_2<-aggregate(data_groups[names(group),],by=list(group),sum)[,-1]
chances3<-chances_raw[names(group)]
chances3<-aggregate(chances3,by=list(group),sum)[,-1]

rownames(data_groups_2)<-LETTERS[1:4]
r_names<-rownames(data_groups_2)

plot(c(1:length(r_names)),data_groups_2[,1],type="l",ylim=c(min(data_groups_2),max(data_groups_2)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Proportion average: All")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=1,cex.axis=1)
for(i in 1:length(data_groups_2[1,])){
  points(c(1:length(r_names)),data_groups_2[,i],type="l",col=gradcol[i],lwd=2)
  points(c(1:length(r_names)),data_groups_2[,i],pch=19,col=gradcol[i],lwd=2)
}

points(c(1:length(r_names)),chances3,type="l",col="black",lwd=4)
points(c(1:length(r_names)),chances3,pch=19,col="black",lwd=4)



#### Chances

par(mar=c(9,2,3,2),mfrow=c(1,1))
plot(c(1:length(names(chances1))),chances1,type="l",ylim=c(min(data_groups),max(chances1)),col="black",lwd=2,xaxt="n",xlab="",main="Chance meta-transitions")
abline(v=c(1:length(names(chances1))),h=0,col="grey")
axis(1,at=c(1:length(names(chances1))),names(chances1),las=2,cex.axis=.9)
points(c(1:length(names(chances1))),chances1,col="black",pch=19, lwd=2)


plot(c(1:length(r_names)),chances3,type="l",ylim=c(min(data_groups_2),max(data_groups_2)),col=gradcol[1],lwd=2,xaxt="n",xlab="",main="Chance meta-transitions per meta-transition group")
abline(v=c(1:length(r_names)),h=0,col="grey")
axis(1,at=c(1:length(r_names)),r_names,las=1,cex.axis=1)
points(c(1:length(r_names)),chances3,type="l",col="black",lwd=4)
points(c(1:length(r_names)),chances3,pch=19,col="black",lwd=4)
