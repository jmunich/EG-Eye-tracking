## Find index of the highest/lowest value.  If two or more highest/lowest values are equal, 
# selects one at random. The function also returns the number of maxima/minima.

my_max<-function(x){
  maxima<-which(x == max(x))
  npos<-length(maxima)>1
  if(npos){
    maximum<-sample(maxima,1)
  }else{
    maximum<-maxima
  }
  output<-list(maximum,npos)
  return(output)
}

my_min<-function(x){
  minima<-which(x == min(x))
  npos<-length(minima)>1
  if(npos){
    minimum<-sample(minima,1)
  }else{
    minimum<-minima
  }
  output<-list(minimum,npos)
  return(output)
}

## Generates random payoff vector of payoffs for any game with p1 available choices 
# for player 1 (horizontal) and player 2 (vertical). Payoff values are taken from
# vector y

generate_game<-function(p1,p2,y){
  payoff<-sample(y,2*p1*p2,replace = TRUE)
  game<-matrix(payoff,p1,2*p2)
  return(game)
}

## Split existing game into own and other's payoffs.

game_own<-function(game){
  payoff_own<-game[,c(FALSE,TRUE)]
  return(payoff_own)
}

game_other<-function(game){
  payoff_other<-game[,c(TRUE,FALSE)]
  return(payoff_other)
}

## Utilities for transposing games (perspective switching)

transpose_game<-function(game){
  t_own<-t(game_own(game))
  t_other<-t(game_other(game))
  t_game<-matrix(0,nrow=length(game[1,])/2,ncol=length(game[,1])*2)
  t_game[,c(TRUE,FALSE)]<-t_own
  t_game[,c(FALSE,TRUE)]<-t_other
  return(t_game)
}

transpose_eye<-function(eye_data,game){
  map<-game
  indicator<-1:length(map)
  map[,]<-indicator
  map_t<-transpose_game(map)
  
  dict_old<-list()
  dict_new<-list()
  
  for(i in indicator){
    v_old<-as.vector(which(map==i,arr.ind = TRUE))
    dict_old[[i]]<-v_old
    v_new<-as.vector(which(map_t==i,arr.ind = TRUE))
    dict_new[[i]]<-v_new
  }
  
  for(i in 1:length(eye_data)){
    svec<-c()
    for(j in 1:length(dict_old)){
      svec[j]<-sum(dict_old[[j]]==eye_data[[i]])==2
    }
    eye_data[i]<-dict_new[svec]
  }
  t_game<-transpose_game(game)
  output<-list(eye_data,t_game)
  names(output)<-c("eye_data","t_game")
  return(output)
}

## Utility for eye-search in finding the maximum in pre-selected subset of cells

search_maximum<-function(meta_memory,loc_meta_memory){
  memory<-meta_memory
  loc_memory<-loc_meta_memory
  ### Order the examination, so that the viewer starts in the most recent cell
  search_seq_post<-c(length(loc_meta_memory),sample(1:(length(loc_meta_memory)-1),length(loc_meta_memory)-1,rep=FALSE))
  loc_meta_memory<-loc_meta_memory[search_seq_post]
  meta_memory<-meta_memory[search_seq_post]
  possible_space<-search_seq_post
  retain<-rep(TRUE,length(meta_memory))
  
  search<-c()
  for(k in search_seq_post){
    if(!k%in%possible_space){
      next
    }
    possible_space<-possible_space[-1]
    if(length(possible_space)==0){
      break
    }
    search_seq_bet<-sample(possible_space,length(possible_space),replace = FALSE)
    search<-c(search,k,search_seq_bet)
    for(l in search_seq_bet){
      greater<-meta_memory[k]>meta_memory[l]
      equal<-meta_memory[k]==meta_memory[l]
      if(greater){
        retain[k]<-retain[k]*TRUE
        retain[l]<-retain[l]*FALSE
        possible_space<-possible_space[-which(possible_space==l)]
      }
      if(equal){
        retain[k]<-retain[k]*TRUE
        retain[l]<-retain[l]*TRUE
      }
      if(!(greater|equal)){
        retain[k]<-retain[k]*FALSE
        retain[l]<-retain[l]*TRUE
      }
    }
  }
  retain<-retain==1
  eye_movement<-loc_memory[search]
  choices<-unlist(loc_meta_memory)[c(FALSE,TRUE)]
  choice_order<-order(choices)
  retain<-retain[choice_order]
  choices<-choices[choice_order]
  eye_choice<-choices[retain]
  output<-list(eye_choice,eye_movement)
  names(output)<-c("eye_choice","eye_movement")
  return(output)  
}
