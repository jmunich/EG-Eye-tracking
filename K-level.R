search_k.level<-function(game,k){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  eye_movement<-list()
  # Get sequence of searched choices
  # k=0; I am naive, k=1; I respond to naive; k=2; I respond to k=1; k=3; I respond to k=2
  if((k%%2)!=0&(k>0)){
    outcome<-search_k.1(game)
    s<-(k-1)/2
  }
  if((k%%2)==0|k==0){
    outcome<-search_naive(game)
    s<-k/2
  }
  an_choice<-outcome[[1]]
  eye_choice<-outcome[[2]]
  flag_1<-outcome[[4]]
  eye_movement<-outcome$eye_movement
  ind<-length(eye_movement)+1
  
  if(k>1){
    for(i in 1:s){
      memory<-c()
      search_seq_other<-sample(c_other,length(c_other),replace=FALSE)
      for(j in 1:length(search_seq_other)){
        loc<-c(search_seq_other[j],c_own_other[eye_choice])
        memory[j]<-game[loc[1],loc[2]]
        eye_movement[[ind]]<-loc
        ind<-ind+1
      }
      memory<-memory[order(search_seq_other)]
      
      an_choice<-my_max(game[,c_own_other[eye_choice]])[[1]]
      eye_choice<-my_max(memory)[[1]]
      flag_1<-flag_1+my_max(game[,c_own_other[eye_choice]])[[2]]
      
      search_seq_bet<-sample(c_own_own,length(c_own_own),replace=FALSE)
      memory<-c()
      for(j in 1:length(search_seq_bet)){
        loc<-c(c_other[eye_choice],search_seq_bet[j])
        memory[j]<-game[loc[1],loc[2]]
        eye_movement[[ind]]<-loc
        ind<-ind+1
      }
      memory<-memory[order(search_seq_bet)]
      
      an_choice<-my_max(game[c_other[eye_choice],c_own_own])[[1]]
      eye_choice<-my_max(memory)[[1]]
      flag_1<-flag_1+my_max(game[c_other[eye_choice],])[[2]]
    }
  }
  flag<-flag_1>0
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}