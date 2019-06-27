library(runjags)
run <- c("Basic_functions.R", "Dominate.R", "Naive.R", "Dominate-K.R", "K-1.R", "K-level.R", "Nash.R", "Optimist.R",
         "Pessimist.R", "Altruist.R", "Get_data_functions.R")
lapply(run, source)
n<-1

# get data
eye_output <- simulate_search(games = 20, repetitions = 5, n = n)

# get metatransitions
exclude <- impossible_meta_transitions(3,3,n)
mt_simulated_data <- eye_output$meta_transition_data
mt_simulated_data <- mt_simulated_data[,!names(mt_simulated_data)%in%exclude]

(nam <- names(mt_simulated_data))

(namA <- nam[c(3, 8, 25)])
(namB <- nam[c(4:7, 9:11, 13:15, 17, 21)])
(namD <- nam[30])
(namC <- nam[!nam %in% c("game", "rule", namA, namB, namD)])

dataToFit <- data.frame(games = mt_simulated_data$game, rule = mt_simulated_data$rule,
                        A = rowSums(mt_simulated_data[,namA]),
                        B = rowSums(mt_simulated_data[,namB]),
                        C = rowSums(mt_simulated_data[,namC]),
                        D = mt_simulated_data[[namD]])


# from Model description.Rmd
# Number of choices per player
C<-3
# All possible transitions
F_1all<-(2*(C^2))*((2*(C^2))-1)
# All possible meta-transitions
F_2all<-(2*(C^2))*(((2*(C^2))-1)^2)
# Intracell transitions
F_inc<-2*(2*C^2)
# Other within; own within
F_otw<-F_oww<-(C^2)*(C-1)
# Other between; own between
F_otb<-F_owb<-(C^2)*(C*(C-1))
# Skip
F_ski<-F_1all-(F_inc+F_otw+F_oww+F_otb+F_owb)
## Meta vectors
V_inc<-c(1,(C-1)/2,(C*(C-1))/2,(C-1)/2,(C*(C-1))/2)
V_oww<-c(1,(C-1),(C*(C-1)),0,0)
V_owb<-c(1,(C-1),(C*(C-1)),0,0)
V_otw<-c(1,0,0,(C-1),(C*(C-1)))
V_otb<-c(1,0,0,(C-1),(C*(C-1)))
V_ski<-c(1,(C-1)/2,(C*(C-1))/2,(C-1)/2,(C*(C-1))/2)
C_ski<-((2*(C^2))-1)-c(sum(V_inc),sum(V_oww),sum(V_owb),sum(V_otw),sum(V_otb),sum(V_ski))
# Matrix with probability of each meta-transition
mtmat<-cbind(rbind(V_inc,V_oww,V_owb,V_otw,V_otb,V_ski),C_ski)
invec<-c(F_inc,F_oww,F_otb,F_otw,F_otb,F_ski)
pmat<-(mtmat*invec)/F_2all
# Probability of meta-transition group
Pa<-pmat[1,1]+pmat[1,6]+pmat[6,1]
Pb<-sum(pmat[1,c(2:5)])+sum(pmat[c(2:5),1])+sum(pmat[c(2:3),c(2:3)])
Pc<-sum(pmat[6,c(2:5)])+sum(pmat[c(2:5),6])+sum(pmat[c(4:5),c(4:5)])
Pd<-pmat[6,6]
pvec<-c(Pa,Pb,Pc,Pd)

# add error transitions
dataToFit$type <- ifelse(dataToFit$rule %in% c("Altruist", "Pessimist", "Naive", "Optimist"), "NS", "S")
dataToFit$nClean <- rowSums(dataToFit[, LETTERS[1:4]])
dataToFit$error <- ifelse(dataToFit$type == "NS", rbeta(nrow(dataToFit), 20, 100), rbeta(nrow(dataToFit), 30, 100))
dataToFit$noErrors <- rbinom(nrow(dataToFit), dataToFit$nClean, dataToFit$error)
errors <- t(rmultinom(nrow(dataToFit), dataToFit$noErrors, pvec))
dataToFit$n <- rowSums(errors) + dataToFit$nClean

dataToFit$AE <- dataToFit$A + errors[,1]
dataToFit$BE <- dataToFit$B + errors[,2]
dataToFit$CE <- dataToFit$C + errors[,3]
dataToFit$DE <- dataToFit$D + errors[,4]


# define the model
model <- "
model{
  for (i in 1:nTrials){
    x[i,] ~ dmulti(p[,s[i]], n[i])
    s[i] ~ dcat(w)
  }
  
  expB0 <- exp(b0 + 0.5*b1)
  expB1 <- exp(b0 - 0.5*b1)
  
  # non strategic p
  p[1,1] <- (1-e[1]) * (1 / (1+ expB0)) + e[1] * pvec[1]
  p[2,1] <- (1-e[1]) * (expB0 / (1+ expB0)) + e[1] * pvec[2]
  p[3,1] <- e[1]*pvec[3]
  p[4,1] <- e[1]*pvec[4]
  
  # strategitc p
  p[1,2] <- e[2] * pvec[1]
  p[2,2] <- (1-e[2]) * (expB1 / (1+ expB1)) + e[2] * pvec[2]
  p[3,2] <- (1-e[2]) * (1 / (1+ expB1)) + e[2] * pvec[3]
  p[4,2] <- e[2]*pvec[4]
  
  for(i in 1:4){
    p[i,3] <- pvec[i]
  }
  
  # priors
  w ~ ddirich(c(1, 1, 1))
  b0 ~ dnorm(0, 1)
  b1 ~ dnorm(0, 1)I(0, 10)
  
  for(i in 1:2){
    e[i] ~ dbeta(2, 10)
  }
}
"

dat <- list(x = as.matrix(dataToFit[,c("AE", "BE", "CE", "DE")]),
            n = as.integer(dataToFit$n),
            nTrials = nrow(dataToFit), pvec = pvec)

fit <- run.jags(model = model, monitor = c("e", "b0", "b1", "w", "s", "p"), data = dat, summarise = FALSE,
                n.chains = 2, sample = 1000)

w <- summary(fit, vars = "w")
s <- summary(fit, vars = "s")
table(estimated = s[,"Mode"], real = dataToFit$type)
table(estimated = s[,"Mode"], real = dataToFit$rule)

p <- summary(fit, vars = "p")

plot(fit, vars = "w")
plot(fit, vars = c("e", "b0", "b1"))
plot(fit, vars = c("p"))

# Filtering altruists in a separate group probably helps a little;
# however, we still have trouble getting nice chains;

modelMultinomialMixture <- "
model{
  for (i in 1:nTrials){
    x[i,] ~ dmulti(p[, s[i]], n[i])
    s[i] ~ dcat(w)
  }
  w ~ ddirich(alphaW[1:nGroup])
  
  for(i in 1:nGroup){
    p[1:4, ] ~ ddirich(alphaP[1:4])
  }
}
"
dat$nGroup <- 5
dat$alphaW <- rep(1, dat$nGroup)
dat$alphaP <- rep(1, 4)


fit <- run.jags(model = modelMultinomialMixture, monitor = c("w", "s", "p"), data = dat,
                summarise = FALSE,
                n.chains = 2, sample = 1000)

w <- summary(fit, vars = "w")
s <- summary(fit, vars = "s")
table(estimated = s[,"Mode"], real = dataToFit$type)
table(estimated = s[,"Mode"], real = dataToFit$rule)

p <- summary(fit, vars = "p")

plot(fit, vars = "w")
plot(fit, vars = c("p"))
