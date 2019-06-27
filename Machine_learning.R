ml_data<-mt_simulated_data[,-1]
gdt<-lapply(eye_output$games,as.vector)
gdt<-lapply(gdt,function(x){rep(x,9)})
games<-matrix(unlist(gdt),ncol=18,byrow=TRUE)
ml_data$rule<-as.factor(ml_data$rule)
ml_data_games<-cbind(ml_data,games)

library(caret)
intrain <- createDataPartition(y = ml_data$rule, p = 0.65, list = FALSE)
training <- ml_data[intrain,]
testing <- ml_data[-intrain,]

set.seed(12345)
# Training with classification tree
library(rpart)
modfit.rpart <- rpart(rule ~ ., data=training, method="class", xval = 4)
print(modfit.rpart, digits = 3)

predictions1 <- predict(modfit.rpart, testing, type = "class")

confusionMatrix(predictions1, testing$rule)

