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
