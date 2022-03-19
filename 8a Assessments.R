
#8. Machine Learning Assessments

library(dslabs)
mnist <- read_mnist()

str(mnist)

length(mnist)


#Assessment

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(type) %>% summarize(PctFemale = mean(sex == "Female"))

y_hat <- ifelse(dat$type == "inclass","Female","Male") 
y <- dat$sex

mean(y == y_hat)

Conf_mat <- table(y_hat, y)

#Create y and y hat as factors
y_hat <- ifelse(dat$type == "inclass","Female","Male") %>%
  factor()
y <- dat$sex %>% factor()

sensitivity(data = y_hat, reference =  y)
specificity(data = y_hat, reference =  y)

mean(y == "Female")


#Distance
library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x) #matrix x, dims 189 by 500
#has 189 obs, each with info on 500 genes

d <- dist(tissue_gene_expression$x)

length(d)
dim(as.matrix(d))

as.matrix(d)[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]

image(as.matrix(d))


#Knn

data(heights)
str(heights)

y <- heights$sex
x <- heights$height

set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)

test_data <- heights[test_index,]
train_data <- heights[-test_index,]

ks <- seq(1,101,3)

Run_KNNs <- function(inp){
  
  knn_fit <- knn3(sex ~ height, data = train_data, k = inp)
  y_hat_knn <- predict(knn_fit, test_data, type = "class")
  #confusionMatrix(data=y_hat_knn, reference=test_data$sex)$overall[["Accuracy"]]
  F_meas(data=y_hat_knn,reference=test_data$sex)
  
 # tibble(k=inp, Fs = TestF)
  
}

FScores <- sapply(ks,Run_KNNs)

max(FScores)
ks[which.max(FScores)]



#Q2

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1,sample.kind = "Rounding")

tisdat <- tissue_gene_expression

#2nd way (first argument the matrix of predictors, second argument the vector of outcomes):
x <- as.matrix(tisdat$x) #matrix of predictors
y <- tisdat$y #vector of outcomes
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)

x_test <- x[test_index,]
x_train <- x[-test_index,]

y_test <- y[test_index]
y_train <- y[-test_index]

?createDataPartition


ks <- seq(1,11,2)

Run_KNNs <- function(inp){
  
  knn_fit <- knn3(x_train,y_train,k=inp)
  
 # knn_fit <- knn3(y ~ x, data = train_data, k = inp)
  y_hat_knn <- predict(knn_fit, x_test, type = "class")
  acc<- confusionMatrix(data=y_hat_knn, reference=y_test)$overall[["Accuracy"]]
  #F_meas(data=y_hat_knn,reference=test_data$sex)
  
  tibble(k=inp, Acc = acc)
  
}

Accuracies <- sapply(ks,Run_KNNs)
Accuracies

#Kfold cross validation
library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

min(x_subset)

fit <- train(x_subset,y, method="glm")
fit$results


install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
library(dplyr)

ind <- tt$p.value <= 0.01

sum(ind)

Varnames <- tt %>% row.names
SigVars <- Varnames[ind]

x_subset <- x[ ,SigVars] #Just filter to significant variables

fit <- train(x_subset,y, method="glm")
fit$results


#Knn

fit <- train(x_subset, y, method="knn", tuneGrid = data.frame(k = seq(101,301,25)))
ggplot(fit)


?train

library(dslabs)
str(tissue_gene_expression)


fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method="knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results



#Bootstrapping

library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

?createResample
data.frame(y = mnist_27$train$y) %>% nrow()

ind1 <- indexes[1]
ind1_df <- data.frame(samp = ind1)
ind1_df %>% group_by(Resample01) %>% summarize(cnt = n()) %>% filter(Resample01 %in% c(3,4,7))

ind2 <- indexes[2]
ind2_df <- data.frame(ind2)
ind2_df %>% group_by(Resample01) %>% summarize(cnt = n()) %>% filter(Resample01 %in% c(3,4,7))


#Do above for all indexes

Rep_Cnts <- function(x){
  ind <- indexes[x]
  ind_df <- data.frame(samp = ind)
  colnames(ind_df) <- c('samp')
  ind_df %>% group_by(samp) %>% summarize(cnt = n()) %>% filter(samp == 3)
}

Reps <- seq(1,10)
Tot_3 <- sapply(Reps, Rep_Cnts)


#q3
y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

set.seed(1, sample.kind="Rounding") # if R 3.6 or later

B <- 10000
MC <- replicate(B , {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(MC)
sd(MC)

#Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later

B <- 10
n <- 100
M_star <- replicate(B, {
  y_star <- sample(y, n, replace = TRUE)
  quantile(y_star,0.75)
})

mean(M_star)
sd(M_star)

#Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later

B <- 10000
n <- 100
M_star <- replicate(B, {
  y_star <- sample(y, n, replace = TRUE)
  quantile(y_star,0.75)
})

mean(M_star)
sd(M_star)



#GENERATIVE MODELS
#Q1

library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

df <- data.frame(x) %>% mutate(y=y)
train_lda <- train(y ~ ., method = "lda", data = df)
train_lda

tmp <- train_lda$finalModel$means
class(tmp)

moddat <- as.data.frame(t(train_lda$finalModel$means)) 
colnames(moddat) <- c("Cerebellum","Hippocampus")

moddat %>% ggplot(aes(Cerebellum,Hippocampus)) +
  geom_point() +
  geom_text()



library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

df <- data.frame(x) %>% mutate(y=y)
train_qda <- train(y ~ ., method = "qda", data = df)
train_qda

moddat <- as.data.frame(t(train_qda$finalModel$means)) 
colnames(moddat) <- c("Cerebellum","Hippocampus")




set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

df <- data.frame(x) %>% mutate(y=y)
train_lda <- train(y ~ ., method = "lda", data = df,preProcess="center")
train_lda

tmp <- train_lda$finalModel$means
class(tmp)

moddat <- as.data.frame(t(train_lda$finalModel$means)) 
colnames(moddat) <- c("Cerebellum","Hippocampus")

moddat %>% ggplot(aes(x=Cerebellum,y=Hippocampus,label=rownames(.))) +
  geom_point() +
  geom_text()





# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]


df <- data.frame(x) %>% mutate(y=y)
train_lda <- train(y ~ ., method = "lda", data = df,preProcess="center")
train_lda

tmp <- train_lda$finalModel$means
class(tmp)

moddat <- as.data.frame(t(train_lda$finalModel$means)) 
colnames(moddat) <- c("Cerebellum","Hippocampus")

moddat %>% ggplot(aes(x=Cerebellum,y=Hippocampus,label=rownames(.))) +
  geom_point() +
  geom_text()



#REGRESSION TREES, CLASSIFICATION TREES, RANDOM FORESTS


library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y~.,
             data=dat,
             )
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x,y_hat), col=2)


#Rand forest
library(randomForest)
fit <- randomForest(y~x,data=dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)  


#Adjust to make prediction smoother
library(randomForest)
fit <- randomForest(y~x,data=dat,nodesize=50, maxnodes=25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")





#CARET PACKAGE
head(tissue_gene_expression)

library(rpart)   #NB: RPART is for classification or regression trees according to documentation
df_x <- data.frame(tissue_gene_expression$x)
df <- mutate(df_x,y=tissue_gene_expression$y)


set.seed(1991,sample.kind = "Rounding")
fit <- train(y~. , 
             method="rpart", 
             data= df, 
             tuneGrid=data.frame(cp=seq(0,0.1,0.01))
             ) 
plot(fit)

set.seed(1991,sample.kind = "Rounding")

df_withpred <- mutate(df, y_hat = predict(fit,df,type="raw"))
df_withpred %>% select(y,y_hat) %>% head()

confusionMatrix(df_withpred$y_hat, df_withpred$y)

fit$bestTune 
fit$finalModel

#Q2

set.seed(1991,sample.kind = "Rounding")
fit_rpart <- train(y~. , 
             method="rpart", 
             data= df, 
             tuneGrid=data.frame(cp=seq(0,0.1,0.01)),
             control = rpart.control(minsplit = 0)
) 
plot(fit_rpart)


df_withpred <- mutate(df, y_hat = predict(fit_rpart,data=df))
df_withpred %>% select(y,y_hat) %>% head()

fit_rpart$bestTune 
fit_rpart$finalModel
fit_rpart$results  #can see max accuracy in here is 0.908

confusionMatrix(df_withpred$y_hat, df_withpred$y)  #this shows perfect accuracy...
# which makes sense as minsplit and cp both at zero, this should keep partitioning until every obs has it's own node

#So accuracy out of the results is different... based on bootstrap samples. e.g. 25% of data, 25 samples.
#Just need to figure out how this works. Is it each of the 25 trees scored on the full data to get that 90% accuracy?

#confusionMatrix(data = predict(fit, df), 
 #               reference = df$y)$overall["Accuracy"]


#3
plot(fit$finalModel, margin=0.1)
text(fit$finalModel,cex=0.75) 

#4 Random Forest
getModelInfo("rf")
modelLookup("rf")

set.seed(1991,sample.kind = "Rounding")
fit <- train(y~. , 
             method="rf", 
             data= df, 
             tuneGrid=data.frame(mtry=seq(50, 200, 25)),   #mtry use random selections of variables to build trees
             nodesize=1
) 

fit$bestTune 

imp <- varImp(fit)
  imp
  
#Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

imp$importance %>% filter(rownames(imp$importance) %in% tree_terms) %>% arrange(desc(Overall))



#TITANIC EXERCISES
###################

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


y <- titanic_clean$Survived

set.seed(42,sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p=0.2, list=FALSE)

train_set <- titanic_clean[-test_index,]
test_set <-titanic_clean[test_index,]

str(train_set)

#Avg survival % in training set
train_set %>% mutate(survived_num = as.numeric(Survived) - 1) %>% summarize(Mean = mean(survived_num), Sum= sum(survived_num))
test_set %>% mutate(survived_num = as.numeric(Survived) - 1) %>% summarize(Mean = mean(survived_num), Sum= sum(survived_num))

#Q2
train_set <- train_set %>% mutate(survived_num = as.numeric(Survived) - 1)
test_set <- test_set %>% mutate(survived_num = as.numeric(Survived) - 1)

set.seed(3,sample.kind = "Rounding")
Guess1 <- as.factor(sample(c(0,1), nrow(test_set), replace=TRUE))

confusionMatrix(Guess1,test_set$Survived)$overall["Accuracy"]
confusionMatrix(Guess1,test_set$Survived)


#Q3a
str(train_set)
train_set %>% group_by(Sex) %>% summarize(Mean_survived = mean(survived_num),Sum= sum(survived_num), tot=n())

Guess2 <- test_set %>% mutate(srv_pred = as.factor(ifelse(Sex=="male",0,1))) %>% select(srv_pred)
confusionMatrix(Guess2$srv_pred,test_set$Survived)

#Q4a
train_set %>% group_by(Pclass) %>% summarize(Mean_survived = mean(survived_num),Sum= sum(survived_num), tot=n())

#Q4b
Guess3 <- test_set %>% mutate(srv_pred = as.factor(ifelse(Pclass==1,1,0))) %>% select(srv_pred)
confusionMatrix(Guess3$srv_pred,test_set$Survived)

#Q4c
train_set %>% group_by(Sex,Pclass) %>% summarize(Mean_survived = mean(survived_num),Sum= sum(survived_num), tot=n())

#Q4d
Guess4 <- test_set %>% mutate(srv_pred = as.factor(ifelse((Sex=="female" & Pclass==1) | 
                                                          (Sex=="female" & Pclass==2)
                                                          ,1,0))) %>% select(srv_pred)
confusionMatrix(Guess4$srv_pred,test_set$Survived)


#Q6
F_meas(data = Guess2$srv_pred, reference = test_set$Survived)    
F_meas(data = Guess3$srv_pred, reference = test_set$Survived)   
F_meas(data = Guess4$srv_pred, reference = test_set$Survived)   
?F_meas

#Q7 - LDA
set.seed(1,sample.kind = "Rounding")
str(train_set)
fit_LDA <-  train(Survived ~ Fare,
                 method="lda",
                 data=train_set)

confusionMatrix(predict(fit_LDA,test_set),test_set$Survived)

#Q7 - QDA
set.seed(1,sample.kind = "Rounding")
fit_QDA <-  train(Survived ~ Fare,
                  method="qda",
                  data=train_set)

confusionMatrix(predict(fit_QDA,test_set),test_set$Survived)

?caret::train

#Q8 - Logistic Regression
#a)
set.seed(1,sample.kind = "Rounding")
fit_Logistic <-  train(Survived ~ Age,
                  method="glm",
                  data=train_set)

confusionMatrix(predict(fit_Logistic,test_set),test_set$Survived)


#b)
set.seed(1,sample.kind = "Rounding")
fit_Logistic2 <-  train(Survived ~ Sex + Pclass + Fare + Age,
                       method="glm",
                       data=train_set)

confusionMatrix(predict(fit_Logistic2,test_set),test_set$Survived)

#c)
set.seed(1,sample.kind = "Rounding")

train_set_orig <- train_set %>% select(!survived_num)

fit_Logistic3 <-  train(Survived ~ .,
                        method="glm",
                        data=train_set_orig)

confusionMatrix(predict(fit_Logistic3,test_set),test_set$Survived)


#9 - KNN
set.seed(6,sample.kind = "Rounding")
fit_knn <-  train(Survived ~ .,
                  method="knn",
                  tuneGrid = data.frame(k=seq(3,51,2)),
                  data=train_set_orig)
plot(fit_knn)
fit_knn$bestTune

confusionMatrix(predict(fit_knn,test_set),test_set$Survived)



#10
set.seed(8,sample.kind = "Rounding")

control <- trainControl(method="cv",number=10,p=0.9)
fit_knn_cv <- train(Survived~., 
                    method="knn",
                    data=train_set_orig,
                    tuneGrid = data.frame(k=seq(3,51,2)), 
                    trControl=control)

plot(fit_knn_cv)
fit_knn_cv$bestTune

confusionMatrix(predict(fit_knn_cv,test_set),test_set$Survived)


#11 - Classification Tree
set.seed(10,sample.kind = "Rounding")

fit_ctree <- train(Survived~., 
                    method="rpart",
                    data=train_set_orig,
                    tuneGrid = data.frame(cp=seq(0,0.05,0.002)))

plot(fit_ctree)
fit_ctree$bestTune

confusionMatrix(predict(fit_ctree,test_set),test_set$Survived)


plot(fit_ctree$finalModel, margin=0.1)
text(fit_ctree$finalModel,cex=0.75) 


fit_ctree$finalModel


#12 - Random Forest
set.seed(14,sample.kind = "Rounding")

fit_rf <- train(Survived~., 
             method="rf", 
             data= train_set_orig, 
             tuneGrid=data.frame(mtry=seq(1:7)),   #mtry use random selections of variables to build trees
             ntree=100
) 

fit_rf$bestTune
confusionMatrix(predict(fit_rf,test_set),test_set$Survived)

imp <- varImp(fit_rf)
imp



#SECTION 6.
#Ensembles

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")


library(caret)
library(dslabs)
library(tidyverse)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


length(mnist_27$test$y)
length(models)

#Repeat predict models
p <- seq(1:length(models))
JoinDat <- map_df(p, function(p){
  y_hat <- factor(predict(fits[[p]], mnist_27$test ))
  data.frame(method = models[p],
       y_hat = y_hat)
})

#Make data wide
library(tidyr)
JoinPreds <- JoinDat %>% group_by(method) %>% mutate(Id= row_number()) %>% spread(key=method, value=y_hat) %>% select(-Id)

nrow(JoinPreds)
ncol(JoinPreds)

#Get accuracy for each method in a single data frame

NmList <- colnames(JoinPreds)

GetAccs <- map_df(p, function(p){
  Acc <- confusionMatrix(JoinPreds[[p]],mnist_27$test$y)$overall["Accuracy"]
  data.frame(method = NmList[p],
             Acc = Acc)
})

GetAccs %>% summarize(avg = mean(Acc))

#Get most common prediction for each observation

JoinDat2 <- JoinDat %>% group_by(method) %>% mutate(Id= row_number())
Tmp <- JoinDat2 %>% group_by(Id,y_hat) %>% summarize(n=n()) %>% slice(which.max(n))

confusionMatrix(Tmp$y_hat,mnist_27$test$y)$overall["Accuracy"]

Acc <- fits$results$Accuracy



#Get min accuracies from cross validation of training of models
p <- seq(1:length(models))
GetMinAccs <- map_df(p, function(p){
  Acc <- min(fits[[p]]$results$Accuracy)
  data.frame(method = models[p],
             Acc = Acc)
})

GetMinAccs %>% summarize(avg = mean(Acc))



#Filter to min accuracy models greater than 0.8. Get most common prediction for each observation

#i.e. only use naive bayes, knn, gamloess, qda, and rf

MinMethsList <- GetMinAccs %>% filter(Acc >= 0.8) %>% select(method) 
MinMethsList <- as.character(MinMethsList[,1])

#Get most common prediction for each observation

MinMethsList

JoinDat2_alt <- JoinDat %>% filter(method %in% MinMethsList) %>% group_by(method) %>% mutate(Id= row_number())
Tmp_alt <- JoinDat2_alt %>% group_by(Id,y_hat) %>% summarize(n=n()) %>% slice(which.max(n))
confusionMatrix(Tmp_alt$y_hat,mnist_27$test$y)$overall["Accuracy"]




