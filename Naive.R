search_naive<-function(game, retain="null"){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  all_c_own_own<-c_own_own
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  if(length(retain)!=1){
    c_own_own<-c_own_own[retain[[2]]]
    c_own_other<-c_own_other[retain[[2]]]
    c_other<-c_other[retain[[1]]]
  }
  
  eye_movement<-list()
  # Get sequence of searched choices
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace=FALSE)
  # Search each choice to find local maximum, then compare the local maxima
  ind<-0
  meta_memory<-c(rep(0,length(all_c_own_own)))
  for(i in search_seq_bet){
    memory<-c()
    counter<-0
    search_seq_in<-sample(c_other,length(c_other),replace=FALSE)
    for(j in search_seq_in){
      loc<-c(j,i)
      counter<-counter+1
      memory[counter]<-game[loc[1],loc[2]]
      ind<-ind+1
      eye_movement[[ind]]<-loc
    }
    m_column<-which(all_c_own_own==i)
    meta_memory[m_column]<-sum(memory)
  }    
  eye_choice<-my_max(meta_memory)[[1]]  
  sum_mat<-colSums(game[c_other,c_own_own])
  an_choice<-which(all_c_own_own==c_own_own[my_max(sum_mat)[[1]]])
  flag<-my_max(sum_mat)[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}
