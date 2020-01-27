search_nash<-function(game){
  # Get indices of payoffs
  c_own_own<-c(1:length(game[1,]))[c(FALSE,TRUE)]
  c_own_other<-c(1:length(game[1,]))[c(TRUE,FALSE)]
  c_other<-c(1:length(game[,1]))
  
  eye_movement<-list()
  
  # Best response of other to my choices
  
  t_game<-transpose_game(game)
  t_c_own_own<-c(1:length(t_game[1,]))[c(FALSE,TRUE)]
  t_c_own_other<-c(1:length(t_game[1,]))[c(TRUE,FALSE)]
  t_c_other<-c(1:length(t_game[,1]))
  t_search_seq_in<-sample(t_c_other,length(t_c_other),replace = FALSE)
  
  select<-list()
  count_i<-0
  for(i in t_search_seq_in){
  values<-t_game[i,c_own_own]
  columns<-c_own_own
  locations<-list()
  count_j<-0
  for(j in columns){
    count_j<-count_j+1
    locations[[count_j]]<-c(i,columns[count_j])
  }
  m_output<-search_maximum(values,locations)
  t_eye_movement<-transpose_eye(m_output$eye_movement,t_game)
  eye_movement<-c(eye_movement,t_eye_movement$eye_data)
  locs<-locations[which(c_own_own%in%m_output$eye_choice)]
  for(l in 1:length(locs)){
  count_i<-count_i+1
  select[[count_i]]<-locs[[l]]
  }
  }
  other_best<-transpose_eye(select,t_game)
  other_best<-other_best$eye_data
  
  for(i in 1:length(other_best)){
    other_best[[i]]<-as.integer(other_best[[i]]+c(0,1)) 
  }
  
  
  search_seq_bet<-sample(c_own_own,length(c_own_own),replace = FALSE)
  select<-list()
  count_i<-0
  for(i in c_other){
  values<-game[i,c_own_own]
  columns<-c_own_own
  locations<-list()
  count_j<-0
  for(j in columns){
    count_j<-count_j+1
    locations[[count_j]]<-c(i,columns[count_j])
  }
  m_output<-search_maximum(values,locations)
  eye_movement<-c(eye_movement,m_output$eye_movement)
  locs<-locations[which(c_own_own%in%m_output$eye_choice)]
  for(l in 1:length(locs)){
  count_i<-count_i+1
  select[[count_i]]<-locs[[l]]
  }
  }
  own_best<-select

  nash<-own_best[own_best%in%other_best]
  if(length(nash)>1){
    nash<-nash[sample(1:length(nash),1)]
  }
  no_nash<-FALSE
  if(length(nash)==0){
    no_nash<-TRUE
    naive_out<-search_naive(game)
    eye_movement<-c(eye_movement, naive_out$eye_movement)
    nash<-list(c(0,c_own_own[naive_out$eye_choice]))
  }
  eye_choice<-which(c_own_own==nash[[1]][2])
  
  
  an_mat_own<-matrix(0,ncol=length(c_own_own),nrow=length(c_other))
  an_mat_other<-matrix(0,ncol=length(c_own_own),nrow=length(c_other))
  for(i in 1:length(an_mat_own[,1])){
    own_payoff<-game[c_other[i],c_own_own]
    maxima<-which(own_payoff==max(own_payoff))
    an_mat_own[i,maxima]<-1
  }
  for(i in 1:length(an_mat_other[1,])){
    other_payoff<-game[c_other,c_own_other[i]]
    maxima<-which(other_payoff==max(other_payoff))
    an_mat_other[maxima,i]<-1
  }
  an_mat_nash<-an_mat_own+an_mat_other
  an_nash<-which(an_mat_nash==2,arr.ind = TRUE)
  flag<-FALSE
  if(dim(an_nash)[1]==0){
    an_nash<-c()
  }

  if(!is.null(dim(an_nash)[1])){
    if(dim(an_nash)[1]>1){
    an_nash<-an_nash[sample(1:length(an_nash[,1]),1),]
    flag<-TRUE
    }
  }
  
  if(length(an_nash)==2){
    an_choice<-an_nash[2]
  }
  if(length(an_nash)==0){
    an_naive_out<-search_naive(game)
    an_choice<-an_naive_out$an_choice
  }
  
  names(an_choice)<-NULL

  output<-list(an_choice,eye_choice,eye_movement,flag,no_nash,game)
  names(output)<-c("an_choice","eye_choice","eye_movement","flag","no_nash","game")
  return(output)
}