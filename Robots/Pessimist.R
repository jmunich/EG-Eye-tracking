search_pessimist<-function(game){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  eye_movement<-list()
  # Get sequence of searched choices
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace=FALSE)
  # Search each choice to find local maximum, then compare the local maxima
  ind<-0
  meta_memory<-c()
  loc_meta_memory<-list()
  for(i in 1:length(search_seq_bet)){
    memory<-c()
    memory_loc<-list()
    search_seq_in<-sample(c_other,length(c_other),replace=FALSE)
    for(j in 1:length(search_seq_in)){
      loc<-c(search_seq_in[j],search_seq_bet[i])
      memory[j]<-game[loc[1],loc[2]]
      memory_loc[[j]]<-loc
      ind<-ind+1
      eye_movement[[ind]]<-loc
    }
    meta_memory[i]<-memory[my_min(memory)[[1]]]
    loc_meta_memory[[i]]<-memory_loc[[my_min(memory)[[1]]]]
  }    
  eye_maximize<-search_maximum(meta_memory,loc_meta_memory)
  eye_choice<-which(c_own_own%in%eye_maximize$eye_choice)  
  if(length(eye_choice)>1){
    eye_choice<-sample(eye_choice,1)
  }
  eye_movement_max<-eye_maximize$eye_movement
  if(identical(eye_movement[length(eye_movement)],eye_movement_max[1])){
    eye_movement_max<-eye_movement_max[-1]
  }
  eye_movement<-c(eye_movement,eye_movement_max)
  an_choice<-my_max(apply(game_own(game),2,max))[[1]]
  flag<-my_max(apply(game_own(game),2,max))[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}
