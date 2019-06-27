par(mfrow=c(3,3),mar = c(3, 3, 3, 3),oma=c(3,0,6,0))
l<-2
ft<-16
cx<-.8


for(i in 1){
  a<-generate_game(3,3,c(1:9))
  plot_eye(search_optimist(a))
  mtext("Optimist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_pessimist(a))
  mtext("Pessimist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_altruist(a))
  mtext("Altruist", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_naive(a))
  mtext("Naive", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_k.level(a,k=1))
  mtext("K-1", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_k.level(a,k=2))
  mtext("K-2", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_domination(a,k=1))
  mtext("D-1", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_domination(a,k=2))
  mtext("D-2", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
  plot_eye(search_nash(a))
  mtext("Nash", side = 3, adj = 0.05, line = l, font=ft, cex=cx)
}
mtext("Information search in strategic choice", outer = TRUE, cex = 1.5,font=ft, line=2.5)
mtext("Decision rule-based simulations", outer = TRUE, cex = 1.2,font=ft, line=1)

#animate_search(search_domination(a,k=2),.25)
