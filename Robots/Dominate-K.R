search_domination<-function(game,k){
  
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  
  even<-(k%%2)==0
  odd<-(k%%2)!=0
  
  for(t in c(1,2)){
    type<-3
    eye_retain<-list(rep(TRUE,length(c_other)),rep(TRUE,length(c_own_own)))
    an_retain<-list(rep(TRUE,length(c_other)),rep(TRUE,length(c_own_own)))  
    d_game<-game  
    
    if(t==1){
      eye_movement<-list()
    }
    
    if(even){
      d_output<-domination(d_game,eye_retain,an_retain)
      eye_retain<-d_output$eye_retain[c(2,1)]
      an_retain<-d_output$an_retain[c(2,1)]
      eye_movement<-c(eye_movement,d_output$eye_movement)
      s<-k/2
    }
    if(odd){
      s<-((k-1)/2)+1
    }
    retains<-eye_retain
    if(t==2){
      retains<-an_retain  
    }
    for(i in 1:s){
      if(k==0){
        type<-0
        break
      }
      
      retain_t0<-retains
      ### Solved already (0)?
      if(sum(retains[[1]])==1|sum(retains[[2]])==1){
        type<-0
        break
      }
      d_game<-transpose_game(d_game)
      d_output<-domination(d_game,retains,retains)
      retains<-d_output$eye_retain[c(2,1)]
      if(t==2){
        retains<-d_output$an_retain[c(2,1)]
      }
      t_eye_movement<-transpose_eye(d_output$eye_movement,d_output$game)
      if(t==1){
        eye_movement<-c(eye_movement,t_eye_movement$eye_data)
      }      
      ### Solved already (1)?
      if(sum(retains[[1]])==1|sum(retains[[2]])==1){
        type<-1
        break
      }
      d_game<-transpose_game(d_game)
      d_output<-domination(d_game,retains,retains)
      retains<-d_output$eye_retain[c(2,1)]
      if(t==2){
        retains<-d_output$an_retain[c(2,1)]
      }
      t_eye_movement<-d_output$eye_movement
      if(t==1){
        eye_movement<-c(eye_movement,t_eye_movement)
      }
      ### No solution (0)?
      if(identical(retain_t0,retains)){
        type<-0
        break
      }
    }
    if(type==0){
      retains<-retains[c(2,1)]
    }
    if(sum(retains[[2]])==1){
      choice<-which(retains[[2]]==TRUE)
      d_output<-list(choice,NULL)
      names(d_output)<-c("eye_choice","eye_movement")
      if(t==2){
        choice<-list(which(retains[[2]]==TRUE),FALSE)
        names(d_output)<-c("an_choice","flag")
      }
    }  
    if(sum(retains[[1]])==1&sum(retains[[2]])>1){
      values<-d_game[retains[[1]],c_own_own[retains[[2]]]]
      columns<-c_own_own[retains[[2]]]
      locations<-list()
      for(i in 1:sum(retains[[2]])){
        locations[[i]]<-c(c_other[retains[[1]]],columns[i])
      }
      m_output<-search_maximum(values,locations)
      eye_choice<-which(c_own_own%in%m_output$eye_choice)
      if(length(eye_choice)>1){
        eye_choice<-sample(eye_choice,1)
      }
      d_output<-list(eye_choice,m_output$eye_movement)
      names(d_output)<-c("eye_choice","eye_movement")
      if(t==2){
        highest<-my_max(values)[[1]]
        choices<-which(retains[[2]]==TRUE)
        an_choice<-choices[highest]
        flag<-my_max(values)[[2]]
        d_output<-list(an_choice,flag)
        names(d_output)<-c("an_choice","flag")
      }
    }  
    if(sum(retains[[1]])>1&sum(retains[[2]])>1){
      d_output<-search_naive(d_game, retain=retains)
    } 
    if(t==1){
      output_1<-list(d_output$eye_choice,c(eye_movement,d_output$eye_movement))
      names(output_1)<-c("eye_choice","eye_movement")
    }  
    if(t==2){
      output_2<-list(d_output$an_choice,d_output$flag)
      names(output_2)<-c("an_choice","flag")
    }  
  }
  output<-list(output_2$an_choice,output_1$eye_choice,output_1$eye_movement,output_2$flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}