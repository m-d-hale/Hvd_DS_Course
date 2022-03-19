
#8a. Machine Learning Assessments
library(dslabs)
library(dplyr)
library(tidyverse)
data(heights)
heights
str(heights)
class(heights)
class("male")
heights$sex[777]
heights[777,1]
max(heights$height)
i <- which.max(heights$height)
heights$height[i]
which.min(heights$height)
mean(heights$height)
median(heights$height)


index <- heights$sex == "Male"
length(heights$sex[index]) / length(heights$sex)

index <- heights$height > 78
length(heights$height[index])

index <- heights$height > 78 & heights$sex == "Female"
length(heights$height[index])



#q7
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

head(test_index)

#Single predictor, single variable
y_hat_Train <- ifelse(train$Sepal.Length>5,"virginica","versicolor") %>%
  factor(levels=levels(train$Species))
y_train <- train$Species

mean(y_train == y_hat_Train) 



?seq
?sapply
?map_dbl

#Look for maxing sepal.length

Var <- train$Petal.Length #Sepal.Width Petal.Length Petal.Width
min <- min(Var)
max <- max(Var)
Rng <- seq(min,max,by=0.1)
accuracy <- map_dbl(Rng, function(x){
  y_hat_Train <- ifelse(Var>x,"virginica","versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_train == y_hat_Train) 
})
max(accuracy)
Rng[which.max(accuracy)] #Gives cut-off of 4.7 on Petal.Length as best

#See how 4.7 cut-off does on the test set
y_Test <- test$Species
y_hat_Test <- ifelse(test$Petal.Length>4.7,"virginica","versicolor") %>%
  factor(levels=levels(test$Species))
mean(y_Test == y_hat_Test) 


#Q10

Var <- test$Petal.Width #Sepal.Length Sepal.Width Petal.Length Petal.Width
min <- min(Var)
max <- max(Var)
Rng <- seq(min,max,by=0.1)
accuracy <- map_dbl(Rng, function(x){
  y_hat_Test <- ifelse(Var>x,"virginica","versicolor") %>%
    factor(levels=levels(test$Species))
  mean(y_Test == y_hat_Test) 
})
max(accuracy)

#Q11
plot(iris,pch=21,bg=iris$Species)

#4.7 best Petal.Length cut-off

Var <- train$Petal.Width #Sepal.Width Petal.Length Petal.Width
min <- min(Var)
max <- max(Var)
Rng <- seq(min,max,by=0.1)
accuracy <- map_dbl(Rng, function(x){
  y_hat_Train <- ifelse(Var>x,"virginica","versicolor") %>%
    factor(levels=levels(train$Species))
  mean(y_train == y_hat_Train) 
})
max(accuracy)
Rng[which.max(accuracy)] #1.5 the best petal width cut-off


#See how 4.7 petal.length or 1.5 petal.width cut-off does on the test set
y_Test <- test$Species
y_hat_Test <- ifelse((test$Petal.Length>4.7| test$Petal.Width>1.5) ,"virginica","versicolor") %>%
  factor(levels=levels(test$Species))
mean(y_Test == y_hat_Test) 




#Comprehension Q1
(0.85*0.02) / (0.017 + 0.098)
0.017+0.003+0.098+0.882


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test) #prob that test is positive

#Where test is negative... what probability is there of having the disease
index <- test == 0
mean(disease[index])

#Where test is +ve what probability is there of having the disease
index <- test == 1
mean(disease[index])


mean(disease[index])/mean(disease)

library(dslabs)
data("heights")
heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex=="Male")) %>%
qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g= cut(height,quantile(height,ps), include.lowest=TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)



Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  # MISSING CODE
  qplot(x, y, data =.)



#Linear Reg Q1
library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

head(dat)


set.seed(1, sample.kind="Rounding") 
All_RMSEs <- replicate(n , {
  index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  train_set <- dat[-index,]
  test_set <- dat[index,]
  mod <- lm(y ~ x,data=train_set)
  y_hat <- predict(mod, test_set)
  root_mse <- sqrt(mean((test_set$y - y_hat)^2))
})

mean(All_RMSEs)
sd(All_RMSEs)

#Linear Reg Q2

Mattfunc <- function(x){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = x, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  All_RMSEs <- replicate(100 , {
    index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    train_set <- dat[-index,]
    test_set <- dat[index,]
    mod <- lm(y ~ x,data=train_set)
    y_hat <- predict(mod, test_set)
    root_mse <- sqrt(mean((test_set$y - y_hat)^2))
  })
  
  c(mean(All_RMSEs),sd(All_RMSEs))
  
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding") 
sapply(n, Mattfunc)


#Linear Reg Q4

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1, sample.kind="Rounding") 
All_RMSEs <- replicate(n , {
  index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  train_set <- dat[-index,]
  test_set <- dat[index,]
  mod <- lm(y ~ x,data=train_set)
  y_hat <- predict(mod, test_set)
  root_mse <- sqrt(mean((test_set$y - y_hat)^2))
})

mean(All_RMSEs)
sd(All_RMSEs)


#Linear Reg Q6

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
train_set <- dat[-test_index,]
test_set <- dat[test_index,]

mod1 <- lm(y ~ x_1,data=train_set)
mod2 <- lm(y ~ x_2,data=train_set)
mod3 <- lm(y ~ x_1 + x_2,data=train_set)

y_hat1 <- predict(mod1, test_set)
y_hat2 <- predict(mod2, test_set)
y_hat3 <- predict(mod3, test_set)

sqrt(mean((test_set$y - y_hat1)^2))
sqrt(mean((test_set$y - y_hat2)^2))
sqrt(mean((test_set$y - y_hat3)^2))


#Linear Reg Q8

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
train_set <- dat[-test_index,]
test_set <- dat[test_index,]

mod1 <- lm(y ~ x_1,data=train_set)
mod2 <- lm(y ~ x_2,data=train_set)
mod3 <- lm(y ~ x_1 + x_2,data=train_set)

y_hat1 <- predict(mod1, test_set)
y_hat2 <- predict(mod2, test_set)
y_hat3 <- predict(mod3, test_set)

sqrt(mean((test_set$y - y_hat1)^2))
sqrt(mean((test_set$y - y_hat2)^2))
sqrt(mean((test_set$y - y_hat3)^2))


#Logistic Regression Q1

set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(mu_1 = 2, n = 1000, p = 0.5, 
                      mu_0 = 0, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat2 <- make_data(mu_1 = 3)


set.seed(1, sample.kind="Rounding")
mu_1 <- seq(0,3,len=25)

mh_dats <- sapply(mu_1, make_data) #This works - created 25 training and test sets

#Testing logistic for a single dataset
head(mh_dats[1,1]) #1st training set
head(mh_dats[2,1]) #1st test set (row 2, col1)
str(mh_dats[,1]$train) # 1st training set

fit_glm <- glm(y ~ x, data=mh_dats[,1]$train, family="binomial")
p_hat_glm <- predict(fit_glm, newdata=mh_dats[,1]$test)
y_hat_glm <- ifelse(p_hat_glm > 0.5, 1, 0)  %>% factor(levels = levels(mh_dats[,1]$test$y))
Mat <- confusionMatrix(data=y_hat_glm, reference = mh_dats[,1]$test$y)
Mat$overall[["Accuracy"]]

#Multiple datasets
Run_Models <- function(mh_dt){
  TrainSet <- mh_dats[,mh_dt]$train
  TestSet <- mh_dats[,mh_dt]$test
  fit_glm <- glm(y ~ x, data=TrainSet, family="binomial")
  p_hat_glm <- predict(fit_glm, newdata=TestSet)
  y_hat_glm <- ifelse(p_hat_glm > 0.5, 1, 0)  %>% factor(levels = levels(TestSet$y))
  Mat <- confusionMatrix(data=y_hat_glm, reference = TestSet$y)
  Mat$overall[["Accuracy"]]
}

DatNums <- seq(1,25)
mh_mods <- sapply(DatNums, Run_Models)

#Plot 
data.frame(delta=mu_1, res=mh_mods) %>% ggplot(aes(delta,res)) + 
  geom_point()





#SMOOTHING
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")


str(dat)

range(dat$date) #3 years of data... 36 months
span = 2/36

index <- sum(is.na(dat$deaths))

dat2 <- dat %>% filter(!is.na(deaths))
str(dat2)


fit <- loess(deaths ~ as.numeric(date), degree=1, span = span, data=dat2)
dat %>% mutate(smooth=predict(fit,as.numeric(date)), day=yday(date),year=as.character(year(date))) %>%
  ggplot(aes(day,smooth,color=year)) +
  geom_line(lwd=2)


head(dat)

dat %>% ggplot(aes(date,deaths)) + 
  geom_point() +
  geom_smooth(color="red", method="loess", span=span,method.args = list(degree=1))


library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()



length(mnist_27$train$x_2)
span = 10/80
fit <- loess(as.numeric(y) ~ x_2, degree=1, span=span, data=mnist_27$train)

mnist_27$train %>% mutate(smooth=predict(fit)) %>%
  ggplot(aes(x_2,smooth)) +
  geom_line(lwd=2)



#MATRICES

x <- matrix(rnorm(1000),100,10)
dim(x)
nrow(x)
ncol(x)


x_tmp <- sweep(x,2,1:nrow(x),"+")
head(x)
head(x_tmp)


y <- matrix(1:15,5,3)
y_tmp <- sweep(y,1,1:nrow(y),"+")

y_tmp2 <- y + seq(nrow(y))


y_tmp <- sweep(y,2,1:ncol(y), FUN = "+")


str(mnist$train)


mn2 <- mnist$train$images
mean(mn2)
mn2[mn2 <= 50 | mn2 >=205] <- 0
mn2[mn2 > 0] <- 1
mean(mn2)

mean(mnist_train_alt3)
