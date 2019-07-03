#### Set the number of games, the number of repetitions per game and n for transition n_grams
n<-1
eye_output<-simulate_search(1000,1,n=n)

## Get transition data (hash the second line to have frequencies instead of proportions)
t_simulated_data<-eye_output$transition_data
t_simulated_data[,-c(1,2)]<-t_simulated_data[,-c(1,2)]/rowSums(t_simulated_data[,-c(1,2)])

## Get meta-transition data (hash the second line to have frequencies instead of proportions)
# First, get rid of impossible meta-transitons
exclude<-impossible_meta_transitions(3,3,n)
mt_simulated_data<-eye_output$meta_transition_data
mt_simulated_data<-mt_simulated_data[,!names(mt_simulated_data)%in%exclude]
mt_simulated_data[,-c(1,2)]<-mt_simulated_data[,-c(1,2)]/rowSums(mt_simulated_data[,-c(1,2)])

## Get mean proportions of meta-transitions per decision rule
#for transitions unhash:
mt_simulated_data<-t_simulated_data

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

