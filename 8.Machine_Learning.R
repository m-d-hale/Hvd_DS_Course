
#Course 8: Machine Learning

#Supervised Machine Learning:
#Building models on data where we know the outcome
#using it to predict in future when we don't know the outcome

#Predicting Y (predictor, dependent) as a function of Xs (covariates, features, independent variables -> all synonyms)


#For categorical outcomes, Y can be any one of K classes
# k can vary widely - e.g. if trying to predict numbers from 
#handwritten numbers, then each digit can be one of 10 outcomes (0-9)
#For speech recognition, could be any word - much bigger problem!

#When outcome is categorical, refer to machine learning task as "classification"
#Predictions will be categorical, and they will either be correct or incorrect

#When outcome is continuous, refer to ML task as "prediction"
#In this case the prediction will not be right or wrong
#But there will be a level of error between the prediction and the actual outcome

#For either though, the predicted output of the model is y-hat



#Example on handwritten zip codes in US
#Each digit of handwritten zip code translated into 28x28 
#images. i.e. 784 pixels in total. 
#For each pixel,a score between 0 (white) and 255 black is obtained

#So outcome is a digit from 0-9
#And there are 784 features, each with a value from 0-255

library(dplyr)
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

#Basic simple example - predicting sex using height
y <- heights$sex
x <- heights$height

#Caret package includes createDataPartition to allow split into training and test sets

set.seed(2,sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)

train_set <- heights[-test_index,]
test_set <-heights[test_index,]

#Develop algorithm, then report 'Overall accuracy': 
#the proportion of cases that were correctly predicted in the test set

#To start, just randomly guess the output
#(best to code categorical outputs as factors)
y_hat <- sample(c("Male","Female"),
                length(test_index), replace=TRUE) %>%
  factor(levels = levels(test_set$sex)) #stores var sex as factor (recommended for categorical vars in ML)

#Then get overall accuracy (proportion of times the model gave the right answer)
mean(y_hat == test_set$sex) #in this case about 52% of the time


#Should be able to do better as we know males on avg are slightly taller than females

heights %>% group_by(sex) %>%
  summarize(mean(height),sd(height))

#So to start have the model predict male, if the height is above 2 SDs below the avg male height

y_hat <- ifelse(x>62,"Male","Female") %>%
  factor(levels=levels(test_set$sex))

mean(y == y_hat) #accuracy now up to 79.3%

#Can try different cut-offs rather than 2 SDs (or 62 inches)

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male","Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

#plot the results
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 

max(accuracy) #best accuracy is 83.6%
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff #which occurs with a cut-off of 64 inches

#Test this on our test set too...
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


#Confusion Matrix

#Overall accuracy can be a deceptive measure.

#Confusion matrix more complete
table(predicted = y_hat, actual = test_set$sex)

#Shows accuracy for male v different to female (high for male, low for female)
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

#OVerall accuracy just looks ok because there are more males than females in the data
prev <- mean(y=="Male")
prev #77% of the data is male

#sensitivity: ability of an algorithm to predict a positive outcome when the actual outcome is positive
#specificity: ability of an algorithm not to predict a positive when the actual outcome is not positive

#Elements in confusion matrix:
   #True Positive (TP) : Actual Positive; Predicted Positive
   #False Positive (FP) : Actual Negative; Predicted Positive
   #False Negative (FN): Actual Positive; Predicted Negative
   #True Negative (TN): Actual Negative; Predicted Negative

#Sensitivity = TP / (TP + FN)  OR ... TP / All Actual Positives
     #Aka True Positive Rate (TPR)
     #Aka Recall

#Specificity = TN / (TN + FP)  OR ... TN / All Actual Negatives
     #Aka True Negative Rate (TNR)

#Another way of quantifying specificity...
    # = TP / (TP + FP)    OR  ... TP / All Predicted Positives
    # AkA Precision
    # Aka Positive Predictive Value (PPV)
    # Precision depends on prealent, since higher prevalence implies you can get higher precision even when guessing

#confusion matrix expects factors as inputs, and first level considered the positive outcome
confusionMatrix(data = y_hat, reference = test_set$sex)
#Gives all the figures previously mentioned...


#Good to understand sensitivity and specificity
#But in certain circumstances having single metric helps e.g. optimisation
#Balanced Accuracy is an average of sensitivity and specificity


#Because sensitivity and specificity are rates, is more appropriate to compute harmonic average
# F1-Score = 1/ (0.5 * (1/recall + 1/precision))

#Often see is written as: 2 x (precision x recall)/(precision + recall)


#Whether sensitivity or specificity are more important depends on context
#e.g. if big loss (e.g.train malfunction) better for test to be overly cautious

#F1-score can be weighted based on whether sensitivity or specificity is more important
#Define beta to represent how much more important sensitivity is vs specificity
#Weighted F-Score = 1 
# / ((Beta^2/(1+Beta^2)) x (1/recall)) + ((1/(1+Beta^2)) x (1/recall))

#F_meas function in caret package computes weighted F score above
#with beta defaulting to 1

#Re-doing prediction algorithm maximising F score, instead of overall accuracy:

cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male","Female") %>%     #creates a vector of "Male" or "Female" prediction
    factor(levels = levels(test_set$sex))                        #enumerates prediction as a factor (NB: levels of test set sex used to indicate vector of unique values that x might have taken)
  F_meas(data = y_hat, reference = factor(train_set$sex))        #generates F1-score. #Reference = vector of actuals
})

?factor
?levels
?map_dbl
levels(test_set$sex)
?F_meas

#Plot F values for the different cut-offs
data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line() 

max(F_1) #Max F score is 0.61
which.max(F_1) #index of max F1 (i.e. row number)

best_cutoff <- cutoff[which.max(F_1)] #cut-off driving max F_1 : 66 inches
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

#Better balance of sensitivity and specificity. Now do much better than guessing
confusionMatrix(data = y_hat, reference = test_set$sex)




#Prevalence matters in practice - even high sensitivity and specificity may not be useful in practice when prevalence near to 0 or 1

#Gives example where prevalence in data v different from population, and high sensitivity can still give v low precision 
#(i.e. low prob of actual having disease if model says it does). Uses Bayes Theorem to show this.


#Receiver Operating Characteristic curve and precision-recall curves

#When comparing guessing previously, used 50-50. But could use different %. e.g. 90% male.
#But comes at cost of lower sensitivity
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)


#... Can choose different cut-offs and compare sensitivity/specificity
#ROC curve plots sensitivity (True Positive Rate) against 1 - Specificitiy (the False Positive Rate)



# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

#Add cut-offs used as labels to the ROC curve
library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)


#Issue with ROC: neither sensitivity or specificity depend on prevalence

#In caeses where prevalence matters, can make a precision-recall plot instead

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})


#If making Male the positive, instead of female, the ROC curve stays the same, but the precision-recall curve looks different
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#Linear Regression (which can be considered a machine learning algorithm)

library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

#Predict son's height 
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#First guess -> use average son height in training set
avg <- mean(train_set$son)
avg #avg of 70

#Mean squared error of this estimate:
mean((avg - test_set$son)^2) #6.03

#Now using linear regression
fit <- lm(son ~ father, data=train_set)
fit$coef

#Gives an estimate of conditional expectaion of son's height, given father's height

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2) #reduces mean squared error to 4.63


#Predict function v useful for Machine Learning applications
#Used to score a new dataset with a model
#So don't need to write out formula. Can use

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2) #same answer of 4.63

#NB: predict doesn't always return objects of the same type
#depends on whether being used for a model produced by lm, glm or something else
#Help files for predict.model will help, e.g.:
?predict.lm
?predict.glm



#LOGISTIC REGRESSION

library(dslabs)
data("heights")
str(heights)

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)

head(test_index)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

head(train_set)
head(test_set)

#Define Y=1 for females, Y=0 for males. X = height

#Conditional probability of being female, if height = 66 inches...
train_set %>% filter(round(height)==66) %>%
  summarize(mean(sex=="Female"))
#So gives 24% chance of being female if height is 66 (round to 1 nearest inch`1`)
#i.e. conditional probability given height is 66


heights %>%
  mutate(x=round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x,prop)) +
  geom_point()

#Start with linear regression....
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)

#USe decision rule of predicting female if p(female) > 50%

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat,test_set$sex)
#Gives prediction accuracy of 78.5%


#But forecasts outside 0-1 range. Therefore logistic transformation
#Fitted using maximum likelihood estimation, rather than least squares

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)


glm_fit <- train_set %>%
  mutate(y = as.numeric(sex=="Female")) %>%
  glm(y ~ height, data=., family="binomial")


?predict.glm
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
#NB: Type = response to get predicted probabilities
#Default is to return the logistic transform values (i.e just the linear predictors evaluated)

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat_logit, test_set$sex)

#i.e. prediction accuracy (using 0.5 as cut-off) up to 79.9%. Bit better
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


#Multiple predictors. Going back to digit example.
#Simplified example just looking at proportion of black
#in upper left and lower right quadrant, and seeing if 
#we can predict whether the number is a 2 or a 7


#Looking at min and max instances in top left quadrant
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)


data("mnist_27")

#create chart
str(mnist_27)
head(mnist_27)
mnist_27$train %>% ggplot(aes(x_1,x_2,color=y)) +
  geom_point()

#Looking at min and max instances in bottom right quadrant
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)


#fit logistic reg
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

#Create decision rule: >0.5 predict a 7; <0.5 predict a 2

p_hat_glm <- predict(fit_glm, newdata=mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data=y_hat_glm, reference = mnist_27$test$y)
#Gives accuracy of 79%

#Constructed dataset, so is a true conditional probability
#Can access and plot here:
mnist_27$true_p %>% ggplot(aes(x_1,x_2,fill=p)) +
  geom_raster()

#Improving the plot:
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 


#Then plotting the 0.5 probability line from the logistic,
#and shows it's a straight line, so doesn't pick up the curve 
#in the true conditional probability
#A limitation to logistic regression
#NB: Is this true if non-linear terms used in the GLM??

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)


#SMOOTHING (aka curve fitting; aka low band pass filtering)

#Used because assumption is the 'signal' is smooth
#But actual data includes signal + noise

#Example from 2008 election used...
data("polls_2008")
qplot(day, margin, data = polls_2008)


#Bin smoothing - grouping data points into strata in which the 
#value of f(x) can be assumed to be constant

#e.g grouping data into weeks

#So basically picking a time and saying +/- 3.5 days
#can we grouped together into the same bucket

#The 7 days in the group known as the window size, 
#the bandwidth or the span

#And average of y values in the span seen as a good estimate for f(x)

#Bin smoothing looks to take the average for each point of x
#in the window size around x


# bin smoothers
span <- 7  #i.e. 7 day window
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


#Can weight more distant points from the centre less in the smoothing...
#The functions from which the weights are calculated are called kernels
#Bin smoothing is a specific type of kernel, as anythiing outside the window is weighted 0, everything inside
#is weighted 1/N (where N is the number of observations in the window)
#But instead of this 'box' type of approach, you can apply other distributions - e.g. normal distributions
#to reduce the weight of observations that are further from the x in question

# Normal distrib kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")



#Local weighted regression (Loess)

#Issue with bin smoothing is that for the f(x) to be approx constant, you need small windows. Therefore
#reduces data points in each window to average. Therefore imprecise estimates of the trend (volatility)
#Loess permits us to consider larger windows
#Adjust the 'constant in the window' assumption to 'linear in the window' (i.e. linear but, not necessarily horizontal)
#So Loess is basically fitting lots of linear a+bx lines in each window
#One other difference - Loess doesn't use a window of x, but instead uses the same number of observations around x
#Is controlled via the span argument, which expects a proportion for loess. e.g. if 100 obs, and 0.1 entered
#Then each regression will be based on the 10 closest observations for each x

#Fitting isn't straight OLS - is a weighted function being minimised.

#And the kernel for the weights isn't a normal distribution, but something called Tukey tri-weight.
#(basically distribution has less in the tails and is more concentrated around x)

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#Another difference - the regressions can be fitted robustly. This basically fits one regression, finds 
#the outliers, then down weights them for a subsequent regression.
#To use method, it needs: family = "symmetric"


#Taylor's theorem, the says all smooth functions are linear if you look close enough, also says that all functions
#can be approximated by parabolas, and you don't need to look as closely as the linear approximation
#i.e. can use larger windows by fitting parabolas (/quadratics) in each window - and this is actually the default procedure in loess
#degree=1 option tells loess to fit polynomials of degree 1 (i.e. lines)
#So the default is degree=2 (i.e. fitting a quadratic function in each window)

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

#NB: loess is the method used by default if using geom_smooth() as part of ggplot
#Quite straightforward to change the span etc and improve the fit

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))



#MATRICES

#Digits example again.

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

#Often convenient to save the predictors in a matrix and the outcome in a vecttor
#Rather than using a data frame
#mnist data is set up in this way

#The main reason for using matrices is that certain mathematical operations needed to 
#develop efficient code can be performed using techniques from a branch of mathematics 
#called linear algebra. Linear algebra and matrix notation are key elements of the 
#language used in academic papers describing machine learning techniques. 

str(mnist)
class(mnist$train$images)

#60k predictors in the dataset. Therefore sample first 1000
#predictors and first 1k labels

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]


#Joining 2 vectors of same length to create vector
x1 <- seq(1,5)
x2 <- seq(6,10)
x1_2 <- cbind(x1,x2)

#To get dimension (rows x cols) of a matrix:
dim(x1_2)

#Can't get dim of a vector in R, but can convert vector to matrix and get it from that
dim(as.matrix(x1))


#Transform vector into matrix form using matrix function
#Specify rows and columns for the matrix
#matrix is filled by column - first colum first, then second etc
my_vector <- 1:15
mat <- matrix(my_vector, 5,3)
mat


#If wanting to fill in by row instead of column
#use the byrow argument
mat_t <- matrix(my_vector, 3, 5, byrow=TRUE)
mat_t

#Note - if less elements in vector than in matrix, R will recyle the vector
#until the matrix is filled up, and will do it without warning.
matrix(my_vector,5,5)

#If already have a matrix, and want to transpose it, use the t function
mat_t_alt <- t(mat)
mat_t_alt
identical(mat_t_alt,mat_t) 


str(x)
head(x)

grid <- matrix(x[3,], 28, 28) #create 28*28 matrix to represent the 784 pixels in an image
image(1:28, 1:28, grid) #Takes each entry as an intensity and plots it. 
#Appears upside down given how R plots

# flip the image back
image(1:28, 1:28, grid[, 28:1])

?image

#Sum values of each rows using rowSums function
sums <- rowSums(x)
head(sums)

#Averages of rows can be calculated using rowMeans function
avg <- rowMeans(x)
head(avg)

data.frame(labels=as.character(y),row_averages=avg) %>% ggplot(aes(labels,row_averages)) +
  geom_boxplot()

#Boxplots show figure 1 uses less ink than other numbers from 0-9 (not surprising)

#To get column sums and column means use colSums and colMeans functions

#Package matrixStats adds functions like rowSds and colSds
#performn operations on each row or column v efficiently

#Not too different to sapply and map. Apply the same function repeatedly to part of our object

#Use apply function for matrices
#General form: apply(matrix, dimension, function)
  #NB: the dimension argument needs to be 1 to apply the function to rows 
  #or 2 for columns

#So can replicate the rowMeans function by doing this:
avgs <- apply(x, 1, mean)
head(avgs)

#Like sapply and map, can use any function, including self generated ones
#But they aren't as fast as dedicated functions like rowMeans, colMeans etc


#In dataset of pixels, each column reflects a given pixel (e.g. 1 of the 784 pixels in each image)
#Looking to remove columns that don't vary much (and don't contain much information to help predict where the total image is 0,1,2,3... or 9)
#Using standard deviations of each column for this to get spread in each column (pixel). Can use colSDS matrix function...

library(matrixStats)
sds <- colSds(x)

#Plot distribution of standard devs
as.data.frame(sds) %>% ggplot(aes(sds)) +
  geom_histogram()

#Reminder, to pull 352nd and 353rd columns in a table (and all rows), you use:
test <- x[,c(352,353)]
head(test)

#... and to pull 3rd and 4th rows and all columns:
test <- x[c(3,4),]
head(test)

#And can also use logical indices to determine which rows/columns to keep
#So to only keep columns with total standard deviation > 60...

new_x <- x[,colSds(x)>60]
str(new_x) #So only keeps 314 columns with standard deviations > 60
dim(new_x)

#NB: when subsetting, if selecting one row or column, creates a vector, not a matrix
class(x[,1])  #gives integer
dim(x[,1])   #gives null

#Use drop option (equal to false) to maintain as a matrix
class(x[,1, drop=FALSE])  #gives "matrix" "array"
dim(x[,1,drop=FALSE])   #gives 1000 1


#Functions as.vector to turn matrices into vectors (e.g. 5 x 3 matrix will become a 15 element vector)
mat <- matrix(1:15,5,3)
mat

as.vector(mat)

#So to get histogram of all predictors
qplot(as.vector(x), bins=30, color=I("black"))

#Big dichotomy in values. IF we think anything below a value of 25 is just a smudge then can 
#reset the values to equal zero

new_x <- x
new_x[new_x < 50] <- 0 #So selecting all elements of new_x where new_x < 50, and resetting to zero
qplot(as.vector(new_x), bins=30, color=I("black"))

#Smaller example of the same
mat <- matrix(1:15,5,3)
mat

new_mat <- mat
new_mat[new_mat < 5] <- 0
new_mat

#Or to zero out between 6 and 12
new_mat <- mat
new_mat[new_mat > 6 & new_mat < 12] <- 0
new_mat


#Now binarizing the data. i.e. splitting the intensities in each pixel into 1 (for ink) or 0 (no ink)
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

#Or can use logicals (TRUE/FALSE) and multiply by 1 to coerce into numbers
bin_X <- (x > 255/2)*1
head(bin_X)

str(bin_x)
str(bin_X)


#Last task - standardising. 
#Start with substracting vector from matrix
#When doing this if you've got a matrix with 1000 rows, and subtract
#a vector with 1000 elements, it'll take the first element of the 
#vector from every element in the first row of the matrix; 
#the second element of the vector from the every element in the
#second row of the matrix etc etc

#Same holds true for other arithmetic operations

#So to get entries normalised (i.e. each element in a column, 
#less the mean of the column, and divided by the standard dev of the column
(x - rowMeans(x)/rowSds(x))

#Note .. this wouldn't work if you wanted to scale across columns
#Would need to transpose the matrix first and transpose back
#e.g.
t(t(x) - colMeans(x))

#Sweep function can also be used. Takes each entry of a vector
#and subtracts from a matrix

X_mean_0 <- sweep(x,2,colMeans(x)) #NB: 2 means columns
# So this gets the means in each column and subtracts
#that from each element in the respective column

#Can also use sweep for other arithmetic operations...


x_standardized <- sweep(x,2,colSds(x),FUN = "/")


#For matrix multiplication use:  %*%
#e.g. cross product:
t(x) %*% x

#or get the same using crossprod function:
crossprod(x)

#to get the inverse used solve. e.g. to undo the crossprod:
solve(crossprod(x))

#qr decomposition
qr(x)




#DISTANCE - pretty straightforward if on cartesian plan (i.e. 2 dimensions); 
#trickier in other higher dimension data which can't be visualised


set.seed(0, sample.kind = "Rounding")
if(!exists("mnist")) mnist <-  read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,] #predictors
y <- mnist$train$labels[ind]  #labels


y[1:3] #So first 3 written numbers are a 7, 7 and a 2

x_1 <- x[1,] #first obs values for 784 predictors
x_2 <- x[2,] #Second obs values for 784 predictors
x_3 <- x[3,] #third obs

#Distances
sqrt(sum((x_1 - x_2)^2)) #2080
sqrt(sum((x_1 - x_3)^2)) #2252
sqrt(sum((x_2 - x_3)^2)) #2643

#So distances between the first and second obs (7 and 7) are closer
#than the distance between the first and third or second and third

#Can compute the same faster using matrix algebra - cross product function
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

#dist function - computes distance between each row (i.e. each observation)
d <- dist(x)
class(d)   #object d is of class "dist"

#Several machine learning related functions in R that take objects of class dist as input

#Or can coerce into a matrix:
as.matrix(d)[1:3,1:3]   #So matches the calcs produced manually above

#visualise the distances
image(as.matrix(d))

#order the distance by labels (i.e. the actual numbers written)
image(as.matrix(d)[order(y), order(y)])

#Shows digits that are the same (i.e. the ys) are closer to each other
#Also, appears to be more uniformity in how 7s are drawn vs 2s


#Distance between predictors (need to transpose the matrix, then us dist)

d <- dist(t(x))
dim(as.matrix(d)) #generates a 784 by 784 matrix

#Just looking at pixel 492...
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))
#So can see it's closest mathematically to the closest pixels


#K-nearest neighbour
#Machine learning algorithm is related to bin smoothin (but is easier to adapt to multiple dimensions)

#For any point for which we want to estimate the conditional probability, 
#we look at the k nearest points and take the average of these points
#Set of points used to compute the avg referred to as a neighbourhood

#Large k - gives smoother estimates.

#Comparing knn with logistic regression

library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5,7,2))
confusionMatrix(data=y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#Gives an accuracy of 0.76

#Going to use knn3, which comes with the caret package
?knn3
#So can call knn3 in a couple of ways:

#1st way (data frame plus formula):
knn_fit <- knn3(y ~ . , data=mnist_27$train) #NB: y ~ . means use all predictors to predict y

#2nd way (first argument the matrix of predictors, second argument the vector of outcomes):
x <- as.matrix(mnist_27$train[,2:3]) #matrix of predictors
y <- mnist_27$train$y #vector of outcomes
knn_fit2 <- knn3(x,y)

#1st way quicker to write, but when using large dataset will want to use the matrix approach

#Note, default k is 5.Can write explicitly like:
knn_fit <- knn3(y ~ ., data=mnist_27$train, k=5)


#predict function for knn produces either a probability for each class
# or it can produce the outcome that maximises the probability

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
#type = class gives a prediction of the outcomes

confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]

#Gives accuracy of 0.815, so an improvement vs logistic regression



#Over-training and oversmoothing

#We have higher accuracy when we predict on a training set than when we 
#compare on a test set. Per below:

#TRaining set:
y_hat_knn <- predict(knn_fit, mnist_27$train, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$train$y)$overall["Accuracy"]

#Test set (lower accuracy):
y_hat_knn <- predict(knn_fit, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]

#Over-training at it's worst when k=1. Just uses each obs as the prediction

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#So almost perfect in training set, but in test set, not so much : 0.74
#Pattern is just the same as the training set pattern exactly...


#Testing with k=401...
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall[["Accuracy"]]

#Accuracy up to 0.79, but still not great
#Gets rid of the noise, but doesn't pick up the signal either
#Call this oversmoothing

#Picking a good value of k
ks <- seq(3,251,2) #all odd numbers between 3 and 251
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k=k, train = train_error, test = test_error)
  
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)


#plot
accuracy %>% ggplot() + 
  geom_line(aes(x=k,y=test),col="blue",size=1 ) +
  geom_line(aes(x=k,y=train), col="red",size=1) +
  ylab("% Accuracy") 


#BUT - USED TEST SET TO GET TO K ; SHOULDN'T DO IT!!!


#Therefore: K-fold cross validation
#See notes


#Bootstrapping....

#Create population income data
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income) #about $45k
m

#Estimating median with the sample median
set.seed(1, sample.kind="Rounding")
N <- 250
X <- sample(income, N)
M<- median(X)
M  #$42k median just using a single sample

library(gridExtra)

#Can we construct a CI around the median?
#Monte Carlo simulation shows the distrib of the sample median is approximately normal...

B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

#So can get mean and standard errors of the sample median
mean(M)
sd(M)

#Problem in practice is not having the population distributions
#In past for the means, have used Central Limit Theorem
#but here interested in the median.

#Bootstrap permits us to approximate a monte carlo simulation
#without access to the entire distribution
#Construct bootstrap samples
B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))


#Gives a quite different result to (incorrectly) using
#the central limit theorem:
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

#If we know the distribution is normal, can use the boostrap
#to estimate mean and SD and form a CI in this way
mean(M) + 1.96 * sd(M) * c(-1,1)
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)



#GENERATIVE MODELS

#1) Naive Bayes

library(caret)
library(dplyr)
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#conditional distribution of height given sex approx normal
#(i.e. approx normal for male group and female group individually)
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

#prevalence (pi) estimated from data:
pi <- train_set %>%
  summarize(pi=mean(sex == "Female")) %>%
  .$pi
pi

#Use the above to get the rule
x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1-pi)) #Naive bayes probability estimates
p_hat_bayes %>% head()

#Looks a lot like logistic regression curve fit
tmp <- test_set %>% mutate(height2 = round(height), preds = p_hat_bayes, fem_flag = ifelse(sex=="Female",1,0)) %>%
  group_by(height2) %>% summarize(avg_preds = mean(preds),avg_acts = mean(fem_flag)) %>% data.frame()
tmp %>% head()
class(tmp)

tmp %>% ggplot(aes(height2)) +  
  geom_line(aes(y=avg_preds)) +
  geom_point(aes(y=avg_acts))


#Prevalence in sample different to wider population. 
#only 23% women in sample
#So if use rule that cond prov has to be bigger than 0.5 to predict females
#accuracy will be affected by low sensitivity:
y_hat_bayes <- ifelse(p_hat_bayes>0.5, "Female", "Male")
sensitivity(data=factor(y_hat_bayes),reference=factor(test_set$sex))

#Is because algorithm gives more weight to specificity to account for the low prevalence
specificity(data=factor(y_hat_bayes),reference=factor(test_set$sex))


#Naive Bayes allows correction - don't have to use pi hat from the sample
#e.g. in predicting males/females in population, would use something more like 0.5

pi_pop <- 0.5
p_hat_bayes_unbiased <- f1*pi_pop / (f1*pi_pop + f0*(1-pi_pop))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased>0.5,"Female","Male")

sensitivity(data=factor(y_hat_bayes_unbiased),reference=factor(test_set$sex))
specificity(data=factor(y_hat_bayes_unbiased),reference=factor(test_set$sex))


#Quadratic Discriminant Analysis
#################################

#Version of Naive Bayes, assuming conditional probabilities for predictors are multivariate normal
#So example above was qda in fact
#Two predictors example
#Assumption that they're bivariate normal therefore

data("mnist_27")

params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1=mean(x_1), avg_2 = mean(x_2), sd_1=sd(x_1), sd_2=sd(x_2),r=cor(x_1,x_2))
params

train_qda <- train(y ~ ., method="qda", data=mnist_27$train)
y_hat <- predict(train_qda,mnist_27$test)
confusionMatrix(data=y_hat,reference=mnist_27$test$y)$overall["Accuracy"]

#Lower accuracy than with kernel smoothers
#Potentially because of normality assumption
#Doesn't appear to hold so well for the 7s (looks ok for the 2s)

#QDA becomes tougher as the number of predictors rises
#Lots of parameters to estimate
#This formula tells us how many:
#K * (2p + p * (p-1)/2)

#Therefore one assumption to reduce number of parameters to estimate is 
#that the correlation and standard deviation is the same for all classes
#i.e. what a regression model does (without interactions)
#Doing this called 'Linear Discriminant Analysis'

params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
#Lower accuracy than QDA here...


#Add in another digit to predict. This time - 1s (as well as 2s and 7s)
#i.e. so no longer binary outcome - one of 3 levels to predict


if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456)    #use set.seed(3456, sample.kind="Rounding") in R 3.6 or later
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

#Fitting the QDA
train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")


#CLASSIFICATION WITH MORE THAN TWO CLASSES : TREES

#LDA, QDA not meant to be used with datasets with many predictors (too many parameters to estimate)
#K-nearest neighbour or local regression do not have model parameters, but also face issues with multiple predictors: "curse of dimensionality"
#Distance would need calculating in p-dimensional space. Our local windows/neighbourhoods become larger to capture x% of data. 
#So stop being local...


#Classification and Regression Trees (CART)

data("olive")
head(olive)

#Task: Predict region using fatty acid composition
table(olive$region)
olive <- select(olive, -area) #remove area column; not used as predictor
head(olive)

#Using caret package to fit k-nearest neighbour
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)),     #So trying 1,3,5,7,9,11,13,15 neighbours
             data = olive)
ggplot(fit) #So accuracy of 0.97. Good, but should be able to do better.
#Chart shows one fatty acid is only present Southern Italy (eicosenoic). 
#Another separates Northern Italy from Sardinia (linoleic)
#So should be able to perfectly predict

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)


#So can see by eye you can partition the data based on the two predictor variables.
#And turn it into a decision rule. If eicosenoic > 0.065 then predict southern italy
#If not, then if linoleic > 10.535 predict Sardinia, if not predict Northern Italy
#i.e. a decision tree produced

#Regression Trees and Decision Trees operate by partitioning the predictor space in this way
#When the outcome is continuous, they're regression trees
#When the outcome is categorical, they're decision trees (aka classification trees)


#Regression Tree example - 2008 poll data
#First, partition feature space into J non-overlapping Regions, R1-Rj
#Then use average Y (i.e. observed outcome) in each region as Y-hat for the region

#Need to decide on how to partition, and how many partitions...
#Done via algorithm looking to minimise residual sum of squares
#So total sum of yi-yhat squared, for every observation...

#rpart function in rpart package to be used on poll data...
head(polls_2008) #Just table of day and margin
qplot(day, margin, data = polls_2008)

fit <- rpart(margin~., data= polls_2008)  #So predicts margin as a function of all other vars (in this case, just day)

plot(fit, margin=0.1)
text(fit,cex=0.75)   #These two lines of code produce the decision tree that's been fitted. Produces 8 partitions

polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


#Note, every time data is split and two new partitions produced, the residual sum of squares decreases
#Given can keep splitting and can adapt to training data
#i.e. danger of overfitting, and getting down to partitions = to number of observations.

#So algorithm sets a minimum for how much the residual sum of squares must improve for another partition to be added
#This is referred to as the Complexity Parameter (or CP)
#RSS must improve by a factor of CP for the new partition to be added

#Algorithm also sets a minimum number of observations to be partitioned
#minsplit argument in the rpart function ... default is 20 (i.e. if less than 20 obs, another partition won't be attempted)

#Also sets a minimum of the number of observations in each partition
#minbucket argument in rpart function. default is minsplit / 3, rounded to closest integer....

#So if CP to zero and minsplit to 2, the algorithm will keep going until each observation is its own partition

fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#How to pick CP? Can use cross-validation like with any other tuning parameter
train_rpart <- train(margin ~ ., method="rpart", tuneGrid=data.frame(cp=seq(0,0.05,len=25)),data=polls_2008)
ggplot(train_rpart)


# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#PRUNING
#Can snip off partitions that do not meet a CP criterion.

pruned_fit <- prune(fit, cp=0.01)
plot(pruned_fit, margin=0.1)
text(pruned_fit,cex=0.75)


#CLASSIFICATION TREES (AKA DECISION TREES)
#Used when the outcome is categorical

#So rather than taking average at end of each node, now in the partitions we predict with the class that has the
#majority vote in each node (i.e. the class that appears most)

#Also can't use residual sum of squares...

#Gini index and entropy are two common metrics (see notes)
#Both metrics seek to partition observations into subsets that have the same class
#If all one class in a partition, Gini and entropy get a value of zero.

train_rpart <- train(y~.,
                     method="rpart",
                     tuneGrid = data.frame(cp= seq(0.0,0.1,len=25)),
                     data=mnist_27$train
                     )
plot(train_rpart)

confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
#So accuracy of 0.82

#This is better than logistic regression in this case, but not as good as the kernel methods

#Plotting shows that the boundary can't be smoothed.

#Pros: easy to understand, to interpret. Easy to visualise. And sometimes model human processes (e.g. decisions to triage 
#potential heart attack patients).

#Cons: greedy approach via recursive partitioning is a bit harder to train than KNN or linear reg.
#Rarely the best performing method as not very flexible and susceptible to changes in the training data

#Random forests try to improve on some of those shortcomings


#RANDOM FORESTS
################

#Core Idea: Improve prediction performance and reduce instability by averaging multiple decision/classification trees or regression trees
#i.e. a forest of multiple decision trees

#Two key features:
  #1) bootstrap aggregation, or 'bagging'. 
        #Build many decision trees based on training set. 
        #For every observation in the test set use each decision tree to predict the outcome. 
        #Then combine the predictions - done in different way for continuous vs categorical outcomes
            #For continuous, just average the predictions for that observation
            #For categorical use the majority vote (class that appears most) for that observation

#To generate multiple trees in the first place - use booststrap - i.e. sampling with replacement from the training set
#Then build decision tree to each of the boostrapped training sets.

library(randomForest)
fit <- randomForest(margin~., data=polls_2008)
plot(fit)  #For the randomForest function, the plot of the object created plots Error vs number of trees


polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#Creates predictions that look overfitted.
#Can tweak using the carat package. Rborist method is another for fitting random forests...

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Can control the smoothness of the random forest in several ways. 
#One is to limit the size of each node
#Two we can use a random selection of features to consider when building each tree in the forest. This reduces correlation between trees in the forest
  #the argument for this tuning parameter in the randfomForest function is mtry. But each random forest implementation has a different name

#Downside of random forest is loss of interpretability
#However, is a measure called variable importance that helps us interpret the results (basically shows which predictor vars are most important)
#The caret package includes the function varImp that extracts variable importance from any model in which the calc is implemented



#Classification with more than two classes and the caret Package
#################################################################

#Lots of different packages out there for ML, all with different syntax. Caret tries to provide consistency
#Currently includes 238 different methods (see websites saved in bookmarks)

#NB: to run models, still need to install the packages needed for each model. Caret won't do this automatically.

#Caret also provides a function that performs cross validation for us

#Training models using caret can be done with v similar syntax
library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)   #logistic regression
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)   #knn

#Making predictions can use output of function directly. V similar syntax...
y_hat_glm <- predict(train_glm,mnist_27$test,type="raw")
y_hat_knn <- predict(train_knn,mnist_27$test,type="raw")

data.frame(pred = y_hat_glm)

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]


#Tuning parameters with caret
  #When an algorithm includes a tuning parameter, train automatically uses cross-validation to decide among a few default values
  #Link saved in favourites shows which tuning parameters are optimised
  #Or can use following code for info about each model

getModelInfo("knn")  # Full info
modelLookup("knn")  #Summary info: shows that the parameter that is optimised is k

#Can see the results of cross validation using ggplot, highlight parameter
ggplot(train_knn,highlight=TRUE)
plot(train_knn)   #NB: Just plotting the output from the model training seems to do the same... i.e. in this case shows the number
#of neighbours (k) vs accuracy, so ends up using 9 neighbours

#Default bootstrap cross validation is 25 bootstrap samples comprised of 25% of the observations
#For knn method, default is to try k= 5, 7 and 9
#To try other k, need to use the tuneGrid parameter in the train function
#Has to be supplied as a dataframe, where the column name matches the parameters in the model lookup output

#e.g. to change k in knn ... data.frame(k=seq(9,67,2)) ... i.e. 9 to 67 in increments of 2

#Just need to be aware that this would be 30 versions of k, with 25 bootstrap samples for each k ... i.e. 750 knn models fitted
#So do need to think about runtimes...

train_knn <- train(y~., 
                   method="knn",
                   data=mnist_27$train,
                   tuneGrid = data.frame(k=seq(9,71,2))
                   )
ggplot(train_knn,highlight=TRUE)   #Visually shows best K is more like 30...

train_knn$bestTune  #Shows the exact best k : 29
train_knn$finalModel #To access the best performing model, which is what is used if using the model to predict on a new dataset

#So checking the accuracy on the test set...
confusionMatrix(predict(train_knn,mnist_27$test,type="raw"), mnist_27$test$y)$overall["Accuracy"]
#Shows accuracy of 0.84.

#To change the way cross validation is done in the training function (the method or what have you) use trainControl function.
#And call it trControl in the train function

#e.g. to use 10-fold cross validation (10 validation samples with 10% of observations each)
control <- trainControl(method="cv",number=10,p=0.9)
train_knn_cv <- train(y~., method="knn",data=mnist_27$train,tuneGrid = data.frame(k=seq(9,71,2)), trControl=control)

plot(train_knn_cv)
#More variable plot using less samples with smaller % of obs in...

#Can also pull out standard deviations around each parameter for each k tried...
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))


#Plotting the conditional probabilities for each obs vs x1 and x2 and overlaying the model shows a good looking model, but
#not very smooth
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])


#Check caret documentation. Shows gamLoess can be used to smooth the boundary of the knn prediction
#Need gam package for this.

modelLookup("gamLoess")   #shows 2 parameters to optimize in this method. Span and degree.

#Note, even to change span and leave degree alone, the data frame provided must have a column for both degree and span

grid <- expand.grid(span=seq(0.15,0.65,len=10),degree=1)   #expand.grid to basically produce cartesian joined data frame from factors

#?expand.grid

train_loess <- train(y~.,
                     method="gamLoess",
                     tuneGrid=grid,
                     data=mnist_27$train)

#plot(train_loess) #So span of about 0.38 the most optimal in terms of accuracy
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


#So delivers 0.85 accuracy in test. Performs similarly to knn. But conditional probability estimate is smoother than knn.

#Note, not all parameters optimised via tuning. e.g. regressions optimise coeffs using MLE or least squares as standard
#You don't have to enter lots of different coeffs to try via the tuning part of caret package
#important to know which parameters are tunable and which are not



#SECTION 6: MODEL FITTING AND RECOMMENDATION SYSTEMS

mnist <- read_mnist()

names(mnist)

dim(mnist$train$images)

class(mnist$train$labels)  #Vector with classes as integers
table(mnist$train$labels)  #table of frequencies by class integer


# sample 10k rows from training set, 1k rows from test set
set.seed(123,sample.kind = "Rounding")

index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


#Preprocessing MNIST Data   (typical preprocessing: standardizing / transforming predictors; removing ones that aren't useful etc)

#Step 1 - finding features with v little variability (not surprising as some parts of the image that
#rarely contain writing)

library(matrixStats)
sds <- colSds(x)   #gets standard deviation of each of the 784 columns (which represent the 784 pixels in the grid)
qplot(sds,bins=256,color=I("black"))


#Caret package includes a function that recommends features to be removed due to near zero variance

library(Caret)
nzv <- nearZeroVar(x)

image(matrix(1:784 %in% nzv, 28, 28))   #so can see it's all the pixels at the edges that have near zero variance
                                        # given people write their numbers in the middle of the box

#To see how many cols are retained after near zero variance features removed
col_index <- setdiff(1:ncol(x),nzv)  #i.e. checking for cols in x that aren't in nzv
length(col_index) #So 252 pixels out of the 784 retained as useful


#Model Fitting for MNIST data

#Step 1. Add column names to feature matrices - is a requirement of caret package
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)


#Step 2. Knn. Using k-fold cross validation to improve speed

control <- trainControl(method="cv",number=10,p=0.9)
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k=c(1,3,5,7)),
                   trControl = control
                   )

ggplot(train_knn)

#Starting with small data samples
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]


cm$byClass[,1:2]   #Shows outputs (i.e. numbers 0 to 9, with sensitivity and specificity of each)
#So sensitivity lowest for number 8. i.e. of actual 8s written, the knn model predicted 84.6% of them as 8s (lower than
#all other numbers)
#Specificity lowest for number 7. of all the actuals excluding 7, 99.4% were called as not 7 on the test


#Now try random forest

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune


#Set to larger number of trees
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)


y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

#Cross check predictions vs images of the written numbers
install.packages("rafalib")
library(rafalib)
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}


#Variable Importance for random forests
#Not supported in rborist at minute
#Therefore using standard randomforest function


library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x,y,ntree=50)

imp <- importance(rf)
imp

#Then to plot it as an image...
image(matrix(imp,28,28)) #...to see where the important features (pixels) are

#Looking into places we made a mistake in the prediction in either knn or random forest...

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max,1,max)

ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing=TRUE)]

#Images of where we made a mistake on knn...
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 3),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}


#ensembles (ensembling diffent ML algorithms into one)
#e.g. random forest and knn ensemble (using average probability predicted from each):

p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)

p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn) / 2

y_pred <- factor(apply(p,1,which.max)-1)
confusionMatrix(y_pred,y_test)  #So 96% accuracy: i.e. improvement on using Knn or Random Forests alone









