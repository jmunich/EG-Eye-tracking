library(runjags)
run <- c("Basic/Basic_functions.R", "Basic/Dominate.R", "Robots/Naive.R", "Robots/Dominate-K.R", "Basic/K-1.R", "Robots/K-level.R", "Robots/Nash.R", "Robots/Optimist.R",
         "Robots/Pessimist.R", "Robots/Altruist.R", "Basic/Get_data_functions.R")
lapply(run, source)
n<-1

# get data for classification
eye_output <- simulate_search(games = 20, repetitions = 5, n = n)

# get metatransitions for determining meta-transition groups
exclude <- impossible_meta_transitions(3,3,n)
mt_simulated_data <- eye_output$meta_transition_data
mt_simulated_data <- mt_simulated_data[,!names(mt_simulated_data)%in%exclude]

eye_output_2<-simulate_search(games = 3000, repetitions = 1, n = n)
data_groups<-simulate_search_get_data(eye_output_2,uni=FALSE)$data
chances_raw<-chance_meta_transition_probabilities(3,3,1)


### The formal model is based on the following k-means. If there is a problem, one cause might be different ordering of clusters by kmeans
set.seed(1)

k.o<-kmeans(data_groups, centers = 4, nstart = 10)

G1<-names(k.o$cluster)[k.o$cluster==1]
G2<-names(k.o$cluster)[k.o$cluster==2]
G3<-names(k.o$cluster)[k.o$cluster==3]
G4<-names(k.o$cluster)[k.o$cluster==4]

namlist<-list(
  G1[order(G1)],
  G2[order(G2)],
  G3[order(G3)],
  G4[order(G4)]
)

Namlist<-namlist[order(unlist(lapply(namlist,function(x){x[1]})))]

(namA<-Namlist[[1]])
(namB<-Namlist[[2]])
(namC<-Namlist[[3]])
(namD<-Namlist[[4]])


chances<-chances_raw[unlist(Namlist)]
(pvec<-aggregate(chances,by=list(rep(c(1:4),lengths(Namlist))),sum)[,-1])

dataToFit <- data.frame(games = mt_simulated_data$game, rule = mt_simulated_data$rule,
                        A = rowSums(mt_simulated_data[,namA]),
                        B = rowSums(mt_simulated_data[,namB]),
                        C = rowSums(mt_simulated_data[,namC]),
                        D = rowSums(mt_simulated_data[,namD]))



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
  
  expB0 <- exp(b0)

  # Altruist
  p[1,1] <- pvec[1]
  p[2,1] <- pvec[2]
  p[3,1] <- pvec[3]
  p[4,1] <- pvec[4]
  
  # Non-strategic
  p[1,2] <- e[1] * pvec[1]
  p[2,2] <- pvec[2]
  p[3,2] <- pvec[3]
  p[4,2] <- (1-e[1]) + e[1]*pvec[4]

  # Nash + domination
  p[1,3] <- e[2] * pvec[1]
  p[2,3] <- (1-e[2]) + e[2] * pvec[2]
  p[3,3] <- pvec[3]
  p[4,3] <- pvec[4]

  # K-1
  p[1,4] <- e[3] * pvec[1]
  p[2,4] <- pvec[2]
  p[3,4] <- (1-e[3]) + e[3] * pvec[3]
  p[4,4] <- pvec[4]
  
  # K-2
  p[1,5] <- (1-e[4]) * (expB0 / (1+ expB0)) + e[4] * pvec[1]
  p[2,5] <- pvec[2]
  p[3,5] <- pvec[3]
  p[4,5] <- (1-e[4]) * (1 / (1+ expB0)) + e[4] * pvec[4]

  
  # priors
  w ~ ddirich(c(1, 1, 1, 1, 1))
  b0 ~ dnorm(0, 1)
  # b1 ~ dnorm(0, 1)I(0, 10)
  
  for(i in 1:4){
    e[i] ~ dbeta(2, 10)
  }
}
"

dat <- list(x = as.matrix(dataToFit[,c("AE", "BE", "CE", "DE")]),
            n = as.integer(dataToFit$n),
            nTrials = nrow(dataToFit), pvec = pvec)

fit <- run.jags(model = model, monitor = c("e", "b0", "w", "s", "p"), data = dat, summarise = FALSE,
                n.chains = 2, sample = 1000)

w <- summary(fit, vars = "w")
s <- summary(fit, vars = "s")
table(estimated = s[,"Mode"], real = dataToFit$type)
table(estimated = s[,"Mode"], real = dataToFit$rule)

p <- summary(fit, vars = "p")

plot(fit, vars = "w")
plot(fit, vars = c("e", "b0"))
plot(fit, vars = c("p"))

summary(fit)
# Filtering altruists in a separate group probably helps a little;
# however, we still have trouble getting nice chains;

modelMultinomialMixture <- "
model{
  for (i in 1:nTrials){
    x[i,] ~ dmulti(p[s[i], ], n[i])
    s[i] ~ dcat(w)
  }
  w ~ ddirich(alphaW[1:nGroup])
  
  for(i in 1:nGroup){
    p[i,(1:4)] ~ ddirich(alphaP[(1:4)])
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
