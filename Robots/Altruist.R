search_altruist<-function(game){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  eye_movement<-list()
  # Get sequence of searched choices
  search_seq_bet<-sample(c_own_other,length(c_own_other),replace=FALSE)
  # Search each choice to find local maximum, then compare the local maxima
  ind<-0
  vals<-c(rep(c(1:length(c_other)),each=2),rep(c(0,1),length(c_other)))
  locations<-matrix(vals,ncol=2,byrow=FALSE)
  meta_memory<-c()
  for(i in 1:length(search_seq_bet)){
    memory<-0
    search_seq_in<-sample(c(1:(2*length(c_other))),2*length(c_other),replace=FALSE)
    for(j in 1:length(search_seq_in)){
      loc<-c(locations[search_seq_in[j],1],locations[search_seq_in[j],2]+search_seq_bet[i])
      memory<-memory+game[loc[1],loc[2]] ## Could add SVO weight here
      ind<-ind+1
      eye_movement[[ind]]<-loc
    }
    meta_memory[i]<-memory
  }    
  meta_memory<-meta_memory[order(search_seq_bet)]
  
  eye_choice<-my_max(meta_memory)[[1]]  
  sum_mat<-colSums(game)
  sum_mat<-colSums(matrix(sum_mat,nrow=2,byrow=FALSE))
  an_choice<-my_max(sum_mat)[[1]]
  flag<-my_max(sum_mat)[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag, game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}
