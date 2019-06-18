search_naive<-function(game){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  eye_movement<-list()
  # Get sequence of searched choices
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace=FALSE)
  # Search each choice to find local maximum, then compare the local maxima
  ind<-0
  meta_memory<-c(rep(0,length(search_seq_bet)))
  for(i in 1:length(search_seq_bet)){
    memory<-c()
    search_seq_in<-sample(c_other,length(c_other),replace=FALSE)
    for(j in 1:length(search_seq_in)){
      loc<-c(search_seq_in[j],search_seq_bet[i])
      memory[j]<-game[loc[1],loc[2]]
      ind<-ind+1
      eye_movement[[ind]]<-loc
    }
    meta_memory[i]<-sum(memory)
  }    
  meta_memory<-meta_memory[order(search_seq_bet)]
  
  eye_choice<-my_max(meta_memory)[[1]]  
  sum_mat<-colSums(game_own(game))
  an_choice<-my_max(sum_mat)[[1]]
  flag<-my_max(sum_mat)[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}
