get_transitions<-function(eye_object=NULL,eye_data=NULL,is.object=FALSE,n=1){
  if(is.object){
    eye_data<-eye_object$eye_movement
  }
  
  c<-0
  file<-c()
  for(i in 1:(length(eye_data)-1)){
    c<-c+1
    transition<-rbind(eye_data[[i]],eye_data[[i+1]])
    
    if(colSums(transition)[2]%%2==0){
      if(transition[1,2]%%2==0){
        belong<-"own"
      }
      if(transition[1,2]%%2==1){
        belong<-"other"
      }
      if(belong=="own"){
        if(transition[1,2]==transition[2,2]){
          direction<-"-within"
        }
        if(transition[1,2]!=transition[2,2]){
          direction<-"-between"
        }
      }
      if(belong=="other"){
        if(transition[1,1]==transition[2,1]){
          direction<-"-within"
        }
        if(transition[1,1]!=transition[2,1]){
          direction<-"-between"
        }
      }
    }
    
    if(colSums(transition)[2]%%2==1){
      mark<-TRUE
      if((transition[1,1]==transition[2,1])&((transition[1,2]-transition[2,2])^2==1)&(max(transition[,2])%%2==0))
      {
        mark<-FALSE
        belong<-"intra"
        direction<-"cell"
      }
      if(mark)
      {
        belong<-"skip"
        direction<-""
      }
    }
    file[c]<-paste(belong,direction,sep="")
    
  }
  meta_transitions<-c()
  for(i in 1:(length(file)-n)){
    meta_transitions[i]<-paste(file[c(i:(i+n))],collapse = "_")
  }
  output<-list(file,meta_transitions)
  names(output)<-list("transitions","meta_transitions")
  return(output)
}

initiate_meta_transitions<-function(n=1){
  names<-c("intracell","own-within","own-between","other-within","other-between","skip")
  
  meta_transitions<-names
  meta_list<-list()
  
  for(j in 1:n){
    if(n==0){
      break
    }
    for(i in 1:length(meta_transitions)){
      meta_transitions_2<-paste(meta_transitions[i],names,sep="_")
      meta_list[[i]]<-meta_transitions_2
    }
    meta_transitions<-unlist(meta_list)
  }
  df<-matrix(c(1:length(meta_transitions)),ncol=length(meta_transitions))
  df<-as.data.frame(df)
  names(df)<-meta_transitions
  df<-df[FALSE,]
  output<-list(meta_transitions,df)
  names(output)<-c("labels","data_frame")
  return(output)
}

write_eye_data<-function(eye_list,n=1){
  init<-initiate_meta_transitions(n)
  var_names<-init[[1]]
  df<-init[[2]]
  for(i in 1:length(eye_list)){
    observation<-get_transitions(eye_data = eye_list[[i]],n=n)[[2]]
    obvec<-c()
    counter<-0
    for(j in var_names){
      counter<-counter+1
      obvec[counter]<-sum(observation==j)
    }  
    df<-rbind(df,obvec)
  }
  names(df)<-var_names
  return(df)
}


simulate_search<-function(games,repetitions,n=1){
  #### Set number of games
  games<-games
  #### Set number of repetitions per game
  same_game<-repetitions
  
  
  eye_data_list<-list()
  game_list<-list()
  game_vec<-c()
  rule_vec<-c()
  name_vec<-c("Optimist","Pessimist","Altruist","Naive","K-1","K-2","D-1","D-2","Nash")
  for(i in 1:games){
    a<-generate_game(3,3,c(1:9))
    game_list[[i]]<-a
    eye_list<-list()
    for(k in 1:same_game){
      eye_list[[1]]<-search_optimist(a)$eye_movement
      eye_list[[2]]<-search_pessimist(a)$eye_movement
      eye_list[[3]]<-search_altruist(a)$eye_movement
      eye_list[[4]]<-search_naive(a)$eye_movement
      eye_list[[5]]<-search_k.level(a,k=1)$eye_movement
      eye_list[[6]]<-search_k.level(a,k=2)$eye_movement
      eye_list[[7]]<-search_domination(a,k=1)$eye_movement
      eye_list[[8]]<-search_domination(a,k=2)$eye_movement
      eye_list[[9]]<-search_nash(a)$eye_movement
      eye_data_list<-c(eye_data_list, eye_list)
      game_vec<-c(game_vec,rep(i,9))
      rule_vec<-c(rule_vec,name_vec)
    }
  }
  
  mdf<-write_eye_data(eye_data_list,n=0)
  mdf_mt<-write_eye_data(eye_data_list,n=n)
  data<-cbind(game_vec,rule_vec,mdf)
  data_mt<-cbind(game_vec,rule_vec,mdf_mt)
  names(data)[c(1,2)]<-c("game","rule")
  names(data_mt)[c(1,2)]<-c("game","rule")
  output<-list(data,data_mt,game_list)
  names(output)<-c("transition_data","meta_transition_data", "games")
  return(output)
}

add_postion<-function(positions,walks){
  new_walks<-list()
  for(i in 1: length(walks)){
    collect<-list()
    c<-0
    for(j in 1:length(positions)){
      if(identical(walks[[i]][length(walks[[i]])],positions[[j]])){
        next
      }
      c<-c+1
      collect[[c]]<-c(walks[[i]],positions[[j]])
    }
    new_walks<-c(new_walks,collect)
  }
  return(new_walks)
}  

impossible_meta_transitions<-function(p1,p2,n=1){
  g_rows<-1:p1
  g_cols<-1:(2*p2)
  
  positions<-list()
  c<-0
  for(i in g_rows){
    for(j in g_cols){
      c<-c+1
      positions[[c]]<-list(c(g_rows[i],g_cols[j]))
    }
  }

  walks<-positions
  for(j in 1:(n+1)){
    walks<-add_postion(positions,walks)
  }

  frequencies<-write_eye_data(walks,n=n)
  impossible<-names(frequencies)[colSums(frequencies)==0]
  return(impossible)
}

chance_meta_transition_probabilities<-function(p1,p2,n){
  # All possible transitions
  F_1all<-(2*(p1*p2))*((2*(p1*p2))-1)
  # All possible meta-transitions
  F_2all<-(2*(p1*p2))*(((2*(p1*p2))-1)^(n+1))
  # Intracell transitions
  F_inc<-2*(2*p1*p2)
  # Other within; own within
  F_otw<-(p1*p2)*(p2-1)
  F_oww<-(p1*p2)*(p1-1)
  # Other between; own between
  F_otb<-(p1*p2)*(p2*(p1-1))
  F_owb<-(p1*p2)*(p1*(p2-1))
  # Skip
  F_ski<-F_1all-(F_inc+F_otw+F_oww+F_otb+F_owb)
  ## Meta vectors
  V_inc<-c(1,(p1-1)/2,(p1*(p2-1))/2,(p2-1)/2,(p2*(p1-1))/2)
  V_oww<-c(1,(p1-1),(p1*(p2-1)),0,0)
  V_owb<-c(1,(p1-1),(p1*(p2-1)),0,0)
  V_otw<-c(1,0,0,(p2-1),(p2*(p1-1)))
  V_otb<-c(1,0,0,(p2-1),(p2*(p1-1)))
  V_ski<-c(1,(p1-1)/2,(p1*(p2-1))/2,(p2-1)/2,(p2*(p1-1))/2)
  C_ski<-((2*(p1*p2))-1)-c(sum(V_inc),sum(V_oww),sum(V_owb),sum(V_otw),sum(V_otb),sum(V_ski))
  # Matrix with probability of each meta-transition
  mtmat<-cbind(rbind(V_inc,V_oww,V_owb,V_otw,V_otb,V_ski),C_ski)
  invec<-c(F_inc,F_oww,F_otb,F_otw,F_otb,F_ski)
  
  # Probability of meta-transition group
  transition_names<-c("intracell","own-within","own-between","other-within","other-between","skip")

  colnames(mtmat)<-rownames(mtmat)<-names(invec)<-transition_names
  transitions<-initiate_meta_transitions(n)$labels  
  transition_list<-strsplit(transitions,"_")  
  t_probability<-c()
  for(i in 1:length(transition_list)){
    if(n==0){
      t_probability<-invec
      break
    }
    t_prob<-invec[transition_list[[i]][1]]
    for(j in 1:(length(transition_list[[i]])-1)){
      t_prob<-t_prob*mtmat[transition_list[[i]][j],transition_list[[i]][j+1]]
    }
    t_probability[i]<-t_prob
  }  
  t_probability<-t_probability/F_2all
  names(t_probability)<-transitions
  return(t_probability)
}

simulate_search_get_data<-function(eye_output,uni=FALSE){
  ## Get transition data (hash the second line to have frequencies instead of proportions)
  t_simulated_data<-eye_output$transition_data
  t_simulated_data[,-c(1,2)]<-t_simulated_data[,-c(1,2)]/rowSums(t_simulated_data[,-c(1,2)])
  
  ## Get meta-transition data (hash the second line to have frequencies instead of proportions)
  # First, get rid of impossible meta-transitons
  if(!uni){
  exclude<-impossible_meta_transitions(3,3,n)
  mt_simulated_data<-eye_output$meta_transition_data
  mt_simulated_data<-mt_simulated_data[,!names(mt_simulated_data)%in%exclude]
  mt_simulated_data[,-c(1,2)]<-mt_simulated_data[,-c(1,2)]/rowSums(mt_simulated_data[,-c(1,2)])
  }
  ## Get mean proportions of meta-transitions per decision rule
  if(uni){
    mt_simulated_data<-t_simulated_data
  }
  
  number_mt<-rowSums(mt_simulated_data[,-c(1,2)])
  n_mt_means<-aggregate(number_mt,by=list(mt_simulated_data$rule),mean)
  n_mt<-as.integer(n_mt_means[,2])
  names(n_mt)<-n_mt_means[,1]
  
  
  rule_means<-aggregate(mt_simulated_data[,-c(1,2)],by=list(mt_simulated_data$rule),mean)
  rule_sd<-aggregate(mt_simulated_data[,-c(1,2)],by=list(mt_simulated_data$rule),sd)
  data_output<-t(rule_means)
  c_names<-data_output[1,]
  r_c_names<-c("Optimist","Pessimist","Naive","Altruist","K-1","K-2","D-1","D-2","Nash")
  data_output<-data_output[-1,]
  r_names<-rownames(data_output)
  data_output <- mapply(data_output, FUN=as.numeric)
  data_output <- matrix(data_output,ncol=length(c_names))
  colnames(data_output)<-c_names
  rownames(data_output)<-r_names
  data_output<-data_output[,r_c_names]
  
  ## Get long data for box-plots
  boxplot_data<-reshape2::melt(mt_simulated_data[,-1])
  output<-list(data_output,boxplot_data)
  names(output)<-c("data","boxplot_data")
  return(output)
}
