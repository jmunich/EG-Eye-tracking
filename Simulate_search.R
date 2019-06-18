## Finds index of the highest value.  If two or more highest values are equal, 
# selects one at random. The function also returns the number of maxima.

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

### Non-strategic players

## Optimist player selects the choice to maximize highest possible outcome

choice_optimist<-function(game){
  choice<-my_max(apply(game_own(game),1,max))
  return(choice)
}

search_optimist<-function(game){
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
    meta_memory[i]<-max(memory)
  }    
  meta_memory<-meta_memory[order(search_seq_bet)]
  
  eye_choice<-my_max(meta_memory)[[1]]  
  an_choice<-my_max(apply(game_own(game),2,max))[[1]]
  flag<-my_max(apply(game_own(game),2,max))[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}

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
    meta_memory[i]<-min(memory)
  }    
  meta_memory<-meta_memory[order(search_seq_bet)]
  
  eye_choice<-my_max(meta_memory)[[1]]  
  an_choice<-my_max(apply(game_own(game),2,min))[[1]]
  flag<-my_max(apply(game_own(game),2,min))[[2]]
  output<-list(an_choice,eye_choice,eye_movement,flag, game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","game")
  return(output)
}

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

domination<-function(game){
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace = FALSE)
  possible_space<-search_seq_bet
  
  empty_col<-colSums(game[,possible_space])!=0
  empty_row<-rowSums(game)!=0
  
  possible_space<-possible_space[empty_col]
  c_other<-c_other[empty_row]
  
  retain<-rep(1,length(c_own_own))
  eye_movement<-list()
  ind<-0
  
  for(i in 1:(length(search_seq_bet)-1)){
    if(!search_seq_bet[i]%in%possible_space){
      next
    }
    possible_space<-possible_space[-1]
    
    if(length(possible_space)==0){
      next
    }
    
    for(j in 1:length(search_seq_bet)){
      if((!search_seq_bet[j]%in%possible_space)|(search_seq_bet[i]==search_seq_bet[j])){
        next
      }
      search_seq_in<-sample(c_other,length(c_other),replace = FALSE)
      memory<-c()
      equal<-c()
      for(k in 1:length(search_seq_in)){
        loc<-c(search_seq_in[k],search_seq_bet[i])
        loc1<-c(search_seq_in[k],search_seq_bet[j])
        memory[k]<-game[loc[1],loc[2]]>game[loc1[1],loc1[2]]
        equal[k]<-game[loc[1],loc[2]]==game[loc1[1],loc1[2]]
        
        ind<-ind+1
        eye_movement[[ind]]<-loc
        ind<-ind+1
        eye_movement[[ind]]<-loc1
      }
      if(sum(equal)<length(c_other)&(sum(c(memory,equal))==length(c_other))){
        memory[!memory]<-equal[!memory]
      }
      if(sum(memory)==length(c_other)){
        r0<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[i])
        r1<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[j])
        retain[r0]<-retain[r0]*TRUE
        retain[r1]<-retain[r1]*FALSE
        possible_space<-possible_space[-which(possible_space==search_seq_bet[j])]
      }
      if((sum(memory)<length(c_other))&(sum(memory)>0)){
        r0<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[i])
        r1<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[j])
        retain[r0]<-retain[r0]*TRUE
        retain[r1]<-retain[r1]*TRUE
      }
      if(sum(memory)==0){
        r0<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[i])
        r1<-which(search_seq_bet[order(search_seq_bet)]==search_seq_bet[j])
        retain[r0]<-retain[r0]*FALSE
        retain[r1]<-retain[r1]*TRUE
      }
    }
  }
  
  an_retain<-c()
  for(n in 1:length(c_own_own)){
    greater<-game[c_other,c_own_own[n]]<game[c_other,c_own_own[-n],drop=FALSE]
    equal<-game[c_other,c_own_own[n]]==game[c_other,c_own_own[-n],drop=FALSE]
    not_all_equal<-colSums(equal)!=length(c_other)
    greater[,not_all_equal]<-greater[,not_all_equal]|equal[,not_all_equal]
    
    dominated<-sum(colSums(greater)==length(c_other))
    an_retain[n]<-ifelse(dominated==0,TRUE,FALSE)
  }
  
  retain<-retain==1
  
  an_game<-game
  eye_game<-game
  an_game[,!rep(an_retain,each=2)]<-0
  eye_game[,!rep(retain,each=2)]<-0
  
  output<-list(an_game,eye_game,an_retain,retain,empty_row,eye_movement)
  names(output)<-c("an_game","eye_game","an_retain","eye_retain","choices_other","eye_movement")
  return(output)
}

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


game<-generate_game(4,3,1:9)

a<-search_domination(game,4)

animate_search(a$eye_movement,a$game,.5)



choice_dominance<-function(game,k){
  own<-game_own(game)
  other<-game_other(game)
  xind<-c(1:length(own[,1]))
  yind<-c(1:length(own[1,]))
  
  
  for(z in 1:k){
    if(length(xind)>1&length(yind)>1){
      
      dom_other<-c()
      for(i in 1:length(other[1,])){
        doms<-c()
        for(j in 1:length(other[1,])){
          tvec<-other[,i]<other[,j]
          doms[j]<-ifelse(sum(tvec)==length(other[,1]),1,0)
        }
        dom_other[i]<-sum(doms)
      }
      
      exclude_other<-which(dom_other>0)
      
      if(length(exclude_other)>0){
        xind<-xind[-exclude_other]
        own<-own[,-exclude_other]
        other<-other[,-exclude_other]
      }
      
    }
    
    if(length(xind)>1&length(yind)>1){
      
      dom_own<-c()
      for(i in 1:length(own[,1])){
        doms<-c()
        for(j in 1:length(own[,1])){
          tvec<-own[i,]<own[j,]
          doms[j]<-ifelse(sum(tvec)==length(own[1,]),1,0)
        }
        dom_own[i]<-sum(doms)
      }
      exclude_own<-which(dom_own>0)
      
      if(length(exclude_own)>0){
        yind<-yind[-exclude_own]
        own<-own[-exclude_own,]
        other<-other[-exclude_own,]
      }
      
    }
    
    if(length(yind)==1){
      choice<-yind
      return(choice)
      #break
    }
    
    if(length(xind)==1){
      choice<-my_max(own)
      return(choice)
      #break
    }
    
    if(length(xind)>1&length(yind)>1){
      choice<-my_max(rowSums(own))
      return(choice)
      #break
    }
  }
  
}

choice_nash<-function(game){
  p1<-length(game[,1])
  p2<-length(game[1,])/2
  own<-game_own(game)
  other<-game_other(game)
  
  own_mat<-own
  for(i in 1:length(own_mat[1,])){
    vec<-own_mat[,i]
    crit<-max(vec)
    own_mat[,i]<-ifelse(vec==crit,1,0)
  }
  
  ot_mat<-other
  for(i in 1:length(ot_mat[,1])){
    vec<-ot_mat[i,]
    crit<-max(vec)
    ot_mat[i,]<-ifelse(vec==crit,1,0)
  }
  
  eq_mat<-own_mat*ot_mat
  
  choice<-which(rowSums(eq_mat)>0)
  return(choice)
}

########################################################################################
# values: what values can the payoff have
# player1: Choices available to player 1
# player2: Choices available to player 2
# sel1: index of decision rule that should have solutions distinct from the rest
# decision rules are (respectively): non-strategic(optimist, pessimist, altruist, naive)
#                                  ; strategic(nash, k1, k2, domination1, domination2)
ngames<-20
values<-c(1:9)
player1<-3
player2<-3

sel1<-c(1:4)


glist<-list()
slist<-list()
for(i in 1:ngames){
  mark<-TRUE
  mark2<-1
  while(mark|(mark2!=0)){
    game<-generate_game(player1,player2,values)
    
    opt<-choice_optimist(game)
    pes<-choice_pessimist(game)
    alt<-choice_altruist(game)
    nai<-choice_naive(game)
    nash<-choice_nash(game)
    k1<-choice_k.level(game,1)
    k2<-choice_k.level(game,2)
    d1<-choice_dominance(game,1)
    d2<-choice_dominance(game,2)
    
    solutions<-list(opt, pes, alt, nai, nash, k1, k2, d1, d2)
    mark<-(length(solutions)!=sum(lengths(solutions)))|(length(solutions[[5]])==0)
    choices<-unlist(solutions)
    mark2<-sum(solutions[sel1]%in%solutions[-sel1],solutions[-sel1]%in%solutions[sel1])
  }
  
  solnames<-c("Optimist","Pessimist","Altruist","Naive","Nash", "k1","k2","d1","d2")
  solvec<-unlist(solutions)
  gvec<-as.vector(t(game))
  names(solvec)<-solnames
  glist[[i]]<-gvec
  slist[[i]]<-solvec
}
usegame<-do.call(rbind,glist)
usesol<-do.call(rbind,slist)

write.csv(usegame, "games.csv", row.names=FALSE)



plot_eye<-function(eye_data, game){
  require(plotrix)
  insearch<-matrix(unlist(eye_data),ncol=2,byrow = TRUE)
  
  shift_is<-ifelse(insearch[,2]%%2!=0,-.5,0)
  insearch[,1]<-insearch[,1]+shift_is
  
  xcor<-rep(c(1:length(game[1,])),each=length(game[,1]))
  ycor<-rep(c(length(game[,1]):1),length(game[1,]))
  shift_cor<-ifelse(xcor%%2!=0,-.5,0)  
  ycor<-ycor+shift_cor
  
  half<-mean(length(insearch[,1]))
  
  plot(insearch[,2],insearch[,1],ylab="",xlab="",
       xlim=c(.3,max(xcor)+.3),ylim=c(.3,max(ycor)+.3),col="white",
       xaxt="n", yaxt="n", axes=FALSE)
  mtext("Player 1", side=3, line=.5,cex=.7, font=16)
  mtext("Player 2", side=2, line=.5,cex=.7, font=16)
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
  
  text(xcor,ycor,as.vector(game),cex=1.2, font=16)
}

par(mfrow=c(2,3),mar = c(3, 3, 3, 3),oma=c(3,0,6,0))
l<-2
ft<-16
cx<-.8


for(i in 1){
  a<-generate_game(3,3,c(1:9))
  plot_eye(search_optimist(a)[c(3)],a)
  mtext("Optimist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_pessimist(a)[c(3)],a)
  mtext("Pessimist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_altruist(a)[c(3)],a)
  mtext("Altruist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_naive(a)[c(3)],a)
  mtext("Naive", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_k.level(a,k=1)[c(3)],a)
  mtext("K-1", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_k.level(a,k=2)[c(3)],a)
  mtext("K-2", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
}
mtext("Information search in strategic choice", outer = TRUE, cex = 1.5,font=ft, line=2.5)
mtext("Decision rule-based simulations", outer = TRUE, cex = 1.2,font=ft, line=1)

animate_search<-function(eye_object,time){
  game<-eye_object$game
  eye_data<-eye_object$eye_movement
  choice<-eye_object$eye_choice
  par(mfrow=c(1,1),mar = c(2, 2, 2, 2),oma=c(2,0,0,0))
  for(i in 1:length(eye_data)){
    plot_eye(eye_data[c(1:i)],game)
    Sys.sleep(time)
  }
  mtext(paste("Choice = ",choice,sep=""), side=3, line=-.5, cex=.7, font=16)
}
a<-search_k.level(game,2)
animate_search(a,.5)
