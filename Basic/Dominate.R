domination<-function(game, eye_retain_0=FALSE, an_retain_0=FALSE){
  
  
  all_c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  all_c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  all_c_other<-c(1:length(game[,1]))
  
  if(length(eye_retain_0)==1){
    if(!eye_retain_0){
      eye_retain_0<-list(rep(TRUE,length(all_c_other)),rep(TRUE,length(all_c_own_own)))
    }
  }
  
  if(length(an_retain_0)==1){
    if(!an_retain_0){
      an_retain_0<-list(rep(TRUE,length(all_c_other)),rep(TRUE,length(all_c_own_own)))
    }
  }
  
  c_own_own<-all_c_own_own[eye_retain_0[[2]]]
  c_own_other<-all_c_own_other[eye_retain_0[[2]]]
  c_other<-all_c_other[eye_retain_0[[2]]]
  
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace = FALSE)
  possible_space<-search_seq_bet
  
  retain<-eye_retain_0[[2]]
  eye_movement<-list()
  
  ind<-0
  for(i in search_seq_bet){
    if(!i%in%possible_space){
      next
    }
    possible_space<-possible_space[-1]
    if(length(possible_space)==0){
      break
    }
    for(j in search_seq_bet){
      if(!j%in%possible_space){
        next
      }
      search_seq_in<-sample(c_other,length(c_other),replace = FALSE)
      memory<-c()
      equal<-c()
      lower<-c()
      counter<-0
      for(k in search_seq_in){
        loc<-c(k,i)
        loc1<-c(k,j)
        
        counter<-counter+1
        memory[counter]<-game[loc[1],loc[2]]>game[loc1[1],loc1[2]]
        equal[counter]<-game[loc[1],loc[2]]==game[loc1[1],loc1[2]]
        lower[counter]<-game[loc[1],loc[2]]<game[loc1[1],loc1[2]]
        
        
        ind<-ind+1
        eye_movement[[ind]]<-loc
        
        ind<-ind+1
        eye_movement[[ind]]<-loc1
        
        if(sum(memory)>0&sum(lower)>0){
          break
        }
      }
      r0<-which(all_c_own_own==i)
      r1<-which(all_c_own_own==j)
      if(sum(memory+equal)==length(c_other)&sum(equal)!=length(c_other)){
        retain[r0]<-retain[r0]*1
        retain[r1]<-retain[r1]*0
        possible_space<-possible_space[-which(possible_space==j)]
      }
      if(((sum(memory+equal)!=length(c_other))&(sum(memory)>0))|sum(equal)==length(c_other)){
        retain[r0]<-retain[r0]*1
        retain[r1]<-retain[r1]*1
      }
      if(sum(memory)==0){
        retain[r0]<-retain[r0]*0
        retain[r1]<-retain[r1]*1
        break
      }
    }
  }
  
  c_own_own<-all_c_own_own[an_retain_0[[2]]]
  c_own_other<-all_c_own_other[an_retain_0[[2]]]
  c_other<-all_c_other[an_retain_0[[1]]]
  
  an_retain<-an_retain_0[[2]]
  for(n in 1:length(c_own_own)){
    greater<-game[c_other,c_own_own[n]]<game[c_other,c_own_own[-n],drop=FALSE]
    equal<-game[c_other,c_own_own[n]]==game[c_other,c_own_own[-n],drop=FALSE]
    not_all_equal<-colSums(equal)!=length(c_other)
    greater[,not_all_equal]<-greater[,not_all_equal]|equal[,not_all_equal]
    
    dominated<-sum(colSums(greater)==length(c_other))
    where<-which(all_c_own_own==c_own_own[n])
    an_retain[where]<-ifelse(dominated==0,TRUE,FALSE)
  }
  
  eye_retain_own<-retain==1
  eye_retain_other<-eye_retain_0[[1]]
  an_retain_other<-eye_retain_0[[1]]
  eye_retains<-list(eye_retain_other,eye_retain_own)
  an_retains<-list(an_retain_other,an_retain)
  
  output<-list(eye_retains,an_retains,eye_movement,game)
  names(output)<-c("eye_retain","an_retain","eye_movement","game")
  return(output)
}
