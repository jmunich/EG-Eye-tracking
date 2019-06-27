plot_eye<-function(eye_object=1){
  require(plotrix)
  if(length(eye_object)==1){
    eye_object<-list(eye_movement<-list(c(99,99),c(99,99)),eye_choice<-"NULL",game<-generate_game(3,3,1:9))
    names(eye_object)<-c("eye_movement","eye_choice","game")
  }
  eye_data<-eye_object$eye_movement
  game<-eye_object$game
  choice<-eye_object$eye_choice
  
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
  mtext("Time", side=1, line=1,cex=.5, font=16)
  
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
  
  gradcol<-color.gradient(c(0,1,1),0,c(1,1,0),nslices=30*length(insearch[,1]))
  color.scale.lines(insearch[,2],insearch[,1],c(0,1,1),0,c(1,1,0),colvar=1:length(insearch[,1]),lwd=2)
  color.legend(.5,.01,lowX,.1,"",rect.col=gradcol,cex=1,align="rb",gradient="x")
  mtext(paste("Choice = ",eye_object$eye_choice,sep=""), side=3, line=.5, cex=.7, font=16)
  text(xcor,ycor,as.vector(game),cex=1.2, font=16)
  axis(3,at=((1:length(game[1,]))[c(TRUE,FALSE)])+.5,pos=max(ycor)+.2,col=NA,tck=0,font=16,labels=as.character(c(1:(length(game[1,])/2))),las=0,cex=.7,cex.axis=.5)
  axis(2,at=(1:length(game[,1]))-.25,pos=.5,col=NA,tck=0,font=16,labels=as.character(c(1:length(game[,1]))),las=2,cex=.7,cex.axis=.5)
}

animate_search<-function(eye_object,time=.5){
  eye_data<-eye_object$eye_movement

  par(mfrow=c(1,1),mar = c(2, 2, 2, 2),oma=c(2,0,0,0))
  for(i in 1:length(eye_data)){
    eye_object$eye_movement<-eye_data[c(1:i)]
    plot_eye(eye_object)
    Sys.sleep(time)
  }
}

animate_search_gif<-function(eye_object,time=.5){
  eye_data<-eye_object$eye_movement
  par(mfrow=c(1,1),mar = c(2, 2, 2, 2),oma=c(2,0,0,0))
  for(i in 1:length(eye_data)){
    frame<-paste(c("Frame_",i,".png"),collapse="")
    png(frame)
    eye_object$eye_movement<-eye_data[c(1:i)]
    plot_eye(eye_object)
    dev.off()
  }
}

game<-generate_game(3,3,1:9)
e_o<-search_k.level(game,2)
animate_search_gif(e_o)
