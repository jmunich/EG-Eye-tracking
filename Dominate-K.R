search_domination<-function(game,k){
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  
  if((k%%2)==0|k==0){
    g_output<-domination(game)
    
    an_game<-g_output$an_game
    eye_game<-g_output$eye_game
    an_retain<-g_output$an_retain
    eye_retain<-g_output$eye_retain
    c_other<-g_output$choices_other
    eye_movement<-g_output$eye_movement
    s<-k/2
    ind<-length(eye_movement)
  }
  if((k%%2)!=0&(k>0)){
    an_game<-game
    eye_game<-game
    an_retain<-rep(TRUE,ncol(game)/2)
    eye_retain<-rep(TRUE,ncol(game)/2)
    c_other<-rep(TRUE,nrow(game)/2)
    eye_movement<-list()
    s<-(k-1)/2
    ind<-0
  }
  
  for(i in 1:s){
    if(i==0){
      next
    }
    if(sum(c_other)==1|sum(eye_retain)==1){
      break
    }
    an_t_game<-transpose_game(an_game)
    eye_t_game<-transpose_game(eye_game)
    
    an_g_output<-domination(an_t_game)
    eye_g_output<-domination(eye_t_game)
    
    an_game<-an_g_output$an_game
    eye_game<-eye_g_output$eye_game
    an_retain<-an_retain&an_g_output$an_retain
    eye_retain<-eye_retain&eye_g_output$eye_retain
    c_other<-c_other&g_output$choices_other
    
    eye_movement<-c(eye_movement,transpose_eye(g_output$eye_movement,an_t_game)$eye_data)
    
    if(sum(c_other)==1|sum(eye_retain)==1){
      break
    }
    
    an_t_game<-transpose_game(an_game)
    eye_t_game<-transpose_game(eye_game)
    
    an_g_output<-domination(an_t_game)
    eye_g_output<-domination(eye_t_game)
    
    an_game<-an_g_output$an_game
    eye_game<-eye_g_output$eye_game
    an_retain<-an_retain&an_g_output$an_retain
    eye_retain<-eye_retain&eye_g_output$eye_retain
    c_other<-c_other&g_output$choices_other
    
    eye_movement<-c(eye_movement,g_output$eye_movement)
  }
  if(sum(eye_retain)==1){
    eye_choice<-c_own_own[eye_retain]
    an_choice<-c_own_own[an_retain]
    an_flag<-FALSE
  }
  
  if(sum(c_other)==1&sum(eye_retain)!=1){
    eye_choice<-my_max(eye_game[c_other,c_own_own])[[1]]
    eye_flag<-my_max(eye_game[c_other,c_own_own])[[2]]
    an_choice<-my_max(an_game[c_other,c_own_own])[[1]]
    an_flag<-my_max(an_game[c_other,c_own_own])[[2]]
  }
  
  if(sum(c_other)!=1&sum(eye_retain)!=1){
    an_dom_output<-search_naive(an_game)
    eye_dom_output<-search_naive(eye_game)
    an_choice<-an_dom_output$an_choice
    eye_choice<-an_dom_output$eye_choice
    an_flag<-an_dom_output$flag  
    eye_flag<-eye_dom_output$flag
    eye_movement<-c(eye_movement,eye_dom_output$eye_movement)
  }
  
  flag<-an_flag
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}
