
library(gtools)
library(tidyverse)


#ASSESSMENT 1

#1: Race medals

#Overall perms of the 3 medals from the 8 runners
MedalPerms <- permutations(8,3)
nrow(MedalPerms)

#Perms of 3 medals from 3 Jamaican runners
nrow(permutations(3,3))

6/336

#Monte Carlo sim
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

B <- 10000
RaceRuns <- replicate(B, {
  tmp <- sample(runners,3)  
  tmp1 <- tmp == "Jamaica"
  mean(tmp1) == 1
})
mean(RaceRuns)


#2: Restaurant Mgmt

nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(2,1))
nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
nrow(combinations(6,1)) * nrow(combinations(6,3)) * nrow(combinations(3,1))


NumEntrees <- function(entrees){
  nrow(combinations(entrees,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
}
sapply(1:12, NumEntrees)

NumSides <- function(sides){
  nrow(combinations(6,1)) * nrow(combinations(sides,2)) * nrow(combinations(3,1))
}
sapply(2:12, NumSides)


#3 Cancer vs alcohol/tobacco use

head(esoph)
nrow(esoph)
sum(esoph$ncases)
sum(esoph$ncontrols)

#Summary table by alcohol group
esoph %>% group_by(alcgp) %>% 
  summarize(numcases = sum(ncases),numcontrols = sum(ncontrols),ProbCase = numcases / (numcases + numcontrols))

#Summary table for cases by tobacco group
Summesoph <- esoph %>% filter(ncases >= 1) %>% group_by(tobgp) %>%
  summarize(numcases = sum(ncases))

index1 <- Summesoph$tobgp %in% c('10-19','20-29','30+')
sum(Summesoph$numcases[index1]) / sum(Summesoph$numcases)

#Summary table for controls by tobacco group
Summesoph2 <- esoph %>% filter(ncontrols >= 1) %>% group_by(tobgp) %>%
  summarize(numcontrols = sum(ncontrols))

index2 <- Summesoph2$tobgp %in% c('10-19','20-29','30+')
sum(Summesoph2$numcontrols[index2]) / sum(Summesoph2$numcontrols)



#Summary table for cases by alc group
Summalc <- esoph %>% filter(ncases >= 1) %>% group_by(alcgp) %>%
  summarize(numcases = sum(ncases))

index1 <- Summalc$alcgp == '120+'
Prtopalc <- sum(Summalc$numcases[index1]) / sum(Summalc$numcases)
Prtopalc

#Summary table for cases by tob group
Summtob <- esoph %>% filter(ncases >= 1) %>% group_by(tobgp) %>%
  summarize(numcases = sum(ncases))

index1 <- Summtob$tobgp == '30+'
Prtoptob <- sum(Summtob$numcases[index1]) / sum(Summtob$numcases)
Prtoptob

#Summary table for cases by tob and alc group
Summtobalc <- esoph %>% filter(ncases >= 1) %>% group_by(tobgp,alcgp) %>%
  summarize(numcases = sum(ncases))

index2 <- Summtobalc$tobgp == '30+' & Summtobalc$alcgp == '120+'
Prtoptobalc <- sum(Summtobalc$numcases[index2]) / sum(Summtobalc$numcases)

Prtopalcortob <- Prtopalc + Prtoptob - Prtoptobalc
Prtopalcortob


#Summary table for controls by alc group
Summalc_cntrl <- esoph %>% filter(ncontrols >= 1) %>% group_by(alcgp) %>%
  summarize(numcontrols = sum(ncontrols))

index1 <- Summalc_cntrl$alcgp == '120+'
Prtopalc_ctrl <- sum(Summalc_cntrl$numcontrols[index1]) / sum(Summalc_cntrl$numcontrols)
Prtopalc_ctrl

#Multiple of cases vs controls likelihood of being in highest alc group
Prtopalc / Prtopalc_ctrl


#Summary table for controls by alc group
Summtob_cntrl <- esoph %>% filter(ncontrols >= 1) %>% group_by(tobgp) %>%
  summarize(numcontrols = sum(ncontrols))

index1 <- Summtob_cntrl$tobgp == '30+'
Prtoptob_ctrl <- sum(Summtob_cntrl$numcontrols[index1]) / sum(Summtob_cntrl$numcontrols)
Prtoptob_ctrl

#Summary table for controls by alc & tob group
Summtobalc_cntrl <- esoph %>% filter(ncontrols >= 1) %>% group_by(tobgp,alcgp) %>%
  summarize(numcontrols = sum(ncontrols))

index1 <- Summtobalc_cntrl$tobgp == '30+' & Summtobalc_cntrl$alcgp == '120+'
Prtoptobalc_ctrl <- sum(Summtobalc_cntrl$numcontrols[index1]) / sum(Summtobalc_cntrl$numcontrols)
Prtoptobalc_ctrl

Prtoptoboralc <- Prtopalc_ctrl + Prtoptob_ctrl - Prtoptobalc_ctrl

Prtopalcortob / Prtoptoboralc



#ASSESSMENT 2



#ACT test scores in the USA
#Period 2016-2018

Mean_ACT <- 20.9
SD_ACT <- 5.7

#NB: need to change sample kind as using R version later than R 3.6
set.seed(16,sample.kind = "Rounding")

#Generate simulated data
act_scores <- rnorm(10000,Mean_ACT,SD_ACT)
mean(act_scores)
sd(act_scores)

head(act_scores)

sum(act_scores >= 36)

sum(act_scores >= 30) / length(act_scores)

sum(act_scores <= 10) / length(act_scores)

#Question 2

x <- 1:36

f_x <- dnorm(x,Mean_ACT,SD_ACT)

plot(x,f_x)


#Question 3

z_act_scores = (act_scores - Mean_ACT)/SD_ACT
head(z_act_scores)

sum(z_act_scores > 2) / length(z_act_scores)
1 - pnorm(2)

Mean_ACT + (2 * SD_ACT)

qnorm(0.975,Mean_ACT,SD_ACT)

#Question 4

Genprob <- function(x){
  pnorm(x,Mean_ACT,SD_ACT)
  
}

Genprob(1:36)

qnorm(0.95,Mean_ACT,SD_ACT)

p <- seq(0.01,0.99,0.01)
sample_quantiles <- quantile(act_scores,p)
sample_quantiles

theoretical_quantiles <- qnorm(p,Mean_ACT,SD_ACT)
theoretical_quantiles

plot(theoretical_quantiles, sample_quantiles)
abline(0,1)


#ASSESSMENT 3

n <- 44
p <- 0.2

a <- 1
b <- -0.25

avgexamscore <- n * (a*p + b*(1-p)) #mean of total exam score (guessing on all 44 qs)
avgexamscore_SE <- sqrt(n) * abs(b-a) * sqrt(p*(1-p)) # SE of total exam score (guessing all 44qs)


1 - pnorm(8, avgexamscore,  avgexamscore_SE)


set.seed(21, sample.kind = "Rounding")

B <- 10000
S <- replicate(B , {
  X <- sample(c(a,b) , n, replace=TRUE, c(p,1-p)) #Vector of scores on eqch question for single student
  sum(X) #Sum of the total exam result for a single student
})
mean(S>=8)


#Q2

n <- 44
p <- 0.25

a <- 1
b <- 0

n * (a*p + b*(1-p)) #mean score per q

p <- seq(0.25,0.95,0.05)

EV <- function(prob){
  avg <- n * (a*prob + b*(1-prob))
  se <- sqrt(n) * abs(b-a) * sqrt(prob*(1-prob)) # SE of total exam score (guessing all 44qs)
  1 - pnorm(35,avg,se)
}

out <- sapply(p, EV)
tmp <- data.frame(p = p, result = out)


#Q3

a <- 6
b <- -1
p <- 5/38
n <- 500

a*p + b*(1-p) #expected value of payout for one bet & expected value of average payout over 500 bets
abs(b-a) * sqrt(p*(1-p)) #SE of expected value for one bet
abs(b-a) * sqrt(p*(1-p)) / sqrt(n) #SE of average expected value - 500 bets

avg <- n * (a*p + b*(1-p)) #expected value of summed payout for 500 bets
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p)) #SE of expected value of summed payout for 500 bets

pnorm(0,avg,se)



#BIG SHORT ASSESSMENT

library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

Payout_IfDeath <- -150000
Premium <- 1150
n <- 1000


#Females
p <- death_prob %>% filter(age==50 & sex =="Female") %>% pull(prob)
p

p*Payout_IfDeath + (1-p)*Premium #EV for 1 policy
abs(Payout_IfDeath - Premium)*sqrt(p*(1-p)) #SE of EV for 1 policy

EV_Sum <- n * (p*Payout_IfDeath + (1-p)*Premium) #EV for 1000 policies
SE_Sum <- sqrt(n) * (abs(Payout_IfDeath - Premium)*sqrt(p*(1-p))) #SE of EV for 1000 policies

pnorm(0,EV_Sum,SE_Sum) #Using central limit theorem for prob of losing money on 1000 policies

#Males
p <- death_prob %>% filter(age==50 & sex =="Male") %>% pull(prob)
p

Payout_IfDeath <- -150000
n <- 1000
EV_Sum <- 700000

Premium <- (EV_Sum/n - p*Payout_IfDeath) / (1-p)
Premium

SE_Sum <- sqrt(n) * (abs(Payout_IfDeath - Premium)*sqrt(p*(1-p))) #SE of EV for 1000 policies
SE_Sum

pnorm(0,EV_Sum,SE_Sum) #Prob of losing money

#Pt 3

p <- 0.015 #prob of death
Premium <- 1150
n <- 1000
Payout_IfDeath <- -150000


EV_Sum <- n * (p * Payout_IfDeath + (1-p) * Premium)
EV_Sum

SE_Sum <- sqrt(n) * (abs(Payout_IfDeath - Premium)*sqrt(p*(1-p))) #SE of EV for 1000 policies
SE_Sum

pnorm(0,EV_Sum,SE_Sum) #Prob of losing money
pnorm(-1000000,EV_Sum,SE_Sum) #Prob of losing more than 1m dollars

p <- seq(.01, .03, .001)

LoseMoney <- function(prob){
  EV_Sum <- n * (prob * Payout_IfDeath + (1-prob) * Premium)
  SE_Sum <- sqrt(n) * (abs(Payout_IfDeath - Premium)*sqrt(prob*(1-prob))) 
  pnorm(0,EV_Sum,SE_Sum)
}

S <- sapply(p, LoseMoney)
Ans <- data.frame(probability = p, ProbLoseMoney = S)

#Prob of losing a million...
p <- seq(.01, .03, .0025)

LoseaMillion <- function(prob){
  EV_Sum <- n * (prob * Payout_IfDeath + (1-prob) * Premium)
  SE_Sum <- sqrt(n) * (abs(Payout_IfDeath - Premium)*sqrt(prob*(1-prob))) 
  pnorm(-1000000,EV_Sum,SE_Sum)
}

S <- sapply(p, LoseaMillion)
Ans <- data.frame(probability = p, ProbLoseaMillion = S)

#Pt4

p <- 0.015
set.seed(25, sample.kind = "Rounding")
samp <- sample(c(Payout_IfDeath,Premium),n,prob=c(p,1-p), replace=TRUE)
sum(samp) / 10^6



p <- 0.015 #prob of death
Premium <- 1150
n <- 1000
Payout_IfDeath <- -150000

set.seed(27, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  samp <- sample(c(Payout_IfDeath,Premium),n,prob=c(p,1-p), replace=TRUE)
  sum(samp)
})

mean(S < -1000000)

#Find premium for 5% chance of loss

z <- qnorm(0.05) #the z value at which 1% of the standard normal distrib is less than
x <- -Payout_IfDeath*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

EV <- p * Payout_IfDeath + (1-p) * x
EV_Sum <- n * EV

EV_Sum

set.seed(28, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  samp <- sample(c(Payout_IfDeath,x),n,prob=c(p,1-p), replace=TRUE)
  sum(samp)
})

mean(S < 0)




set.seed(29, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  pr <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  samp <- sample(c(Payout_IfDeath,x),n,prob=c(pr,1-pr), replace=TRUE)
  sum(samp)
})

mean(S)

mean(S < 0)
mean(S < -1000000)
