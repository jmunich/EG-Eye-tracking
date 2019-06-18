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
