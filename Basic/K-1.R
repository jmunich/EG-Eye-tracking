search_k.1<-function(game){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  g_other<-transpose_game(game)
  
  eye_movement<-list()
  outcome<-search_naive(g_other)
  flag_1<-outcome[[4]]
  an_choice_ot<-outcome[[1]]
  eye_choice_ot<-outcome[[2]]
  eye_movement<-transpose_eye(outcome$eye_movement,g_other)[[1]]
  ind<-length(eye_movement)+1
  
  memory<-c()
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace=FALSE)
  for(i in 1:length(c_own_own)){
    loc<-c(an_choice_ot[1],search_seq_bet[i])
    memory[i]<-game[loc[1],loc[2]]
    eye_movement[[ind]]<-loc
    ind<-ind+1
  }
  memory<-memory[order(search_seq_bet)]
  
  an_choice<-my_max(memory)[[1]]
  eye_choice<-my_max(memory)[[1]]
  flag<-(flag_1+my_max(memory)[[2]])!=0
  
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}