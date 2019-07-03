par(mfrow=c(1,1),mar=c(2,2,2,2))

eye_data<-list(list(c(3,1),c(3,2)),
               list(c(1,2),c(2,2)),
               list(c(1,2),c(1,6)),
               list(c(1,1),c(1,5)),
               list(c(1,1),c(2,1)),
               list(c(2,3),c(2,6)))

game<-generate_game(3,3,1:9)

insearch<-matrix(unlist(eye_data),ncol=2,byrow = TRUE)

shift_is<-ifelse(insearch[,2]%%2!=0,-.5,0)
insearch[,1]<-insearch[,1]+shift_is

xcor<-rep(c(1:length(game[1,])),each=length(game[,1]))
ycor<-rep(1:c(length(game[,1])),length(game[1,]))
shift_cor<-ifelse(xcor%%2!=0,-.5,0)  
ycor<-ycor+shift_cor

half<-mean(length(insearch[,1]))

plot(insearch[,2],insearch[,1],ylab="",xlab="",
     xlim=c(.3,max(xcor)+.3),ylim=c(.3,max(ycor)+.3),col="white",
     xaxt="n", yaxt="n", axes=FALSE)

mtext("Player 1", side=3, line=1.5,cex=1, font=16)
mtext("Player 2", side=2, line=.5,cex=1, font=16)

ys<-rep((length(game[,1])):0,each=2)
xs<-rep(c(2,0),length(game[,1])+1)
lowY<-min(ys)+.25
highY<-max(ys)+.25
lowX<-min(xs)+.5
lines(c(lowX,lowX),c(lowY,highY),col="gray")
for(i in 1:(length(game[1,])/2)){
  lowX<-lowX+2
  lines(c(lowX,lowX),c(lowY,highY),col="gray")
  lines(xs+.5,ys+.25,col="gray")
  xs<-xs+2
}
lines(c(lowX,lowX),c(lowY,highY),col="gray4")
lines(c(.5,lowX),c(lowY,lowY),col="gray4")

gradcol<-color.gradient(c(0,1,1),0,c(1,1,0),nslices=length(insearch[,1]))
seq1<-1:length(insearch[,1])
seq<-seq1[c(TRUE,FALSE)]
for(i in seq){
  lines(insearch[c(i,i+1),2],insearch[c(i,i+1),1],col=gradcol[i], lwd=2)
}

text(xcor,ycor,as.vector(game),cex=1.2, font=16)
axis(3,at=((1:length(game[1,]))[c(TRUE,FALSE)])+.5,pos=max(ycor)+.2,col=NA,tck=0,font=16,labels=as.character(c(1:(length(game[1,])/2))),las=0,cex=.7,cex.axis=.5)
axis(2,at=(1:length(game[,1]))-.25,pos=.5,col=NA,tck=0,font=16,labels=as.character(c(1:length(game[,1]))),las=2,cex=.7,cex.axis=.5)

seq2<-seq1[c(FALSE,TRUE)]
text(insearch[seq2,2]+.25,insearch[seq2,1],c(1:6),cex=1.2, font=17)
points(insearch[seq2,2]+.25,insearch[seq2,1],cex=4,lwd=3)
