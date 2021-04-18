

#4. INFERENCE AND MODELLING

library(tidyverse)
library(dslabs)
ds_theme_set()

#function to show random draw from urn. In this case sample of 25 beads.
take_poll(25)

#Want to predict proportion of blue beads in the full urn -> p. p is a parameter, as it relates
#to the population

#Spread is the difference in p vs 1-p (i.e. prob of blue minus prob of red)
# So p - (1-p)  or 2p -1

#25 beads pulled is a sample
#The proportion of blue/red in the sample can be used to estimate the population parameter p.
#Hence and 'estimate'

#Full urn is the population

#p is a random variable. If running 4 samples, get a different p each time

#Define random variable X: 1 if we pick a blue bead at random, 0 if it's red.

#If don't have information about population parameter p, often using sample means and standard
#errors as estimates of the population

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se

#So to get an estimate of a sample mean being with 1% of the population mean:
pnorm(0.01/se) - pnorm(-0.01/se)

#Typically 95% confidence intervals quote...+/- 1.96 * SE


#Example using Monte Carlo to corroborate tools. Issue is we don't actually know p
#One thing we can do is pick a value of p, or several values of p

p <- 0.45
B <- 10000
N <- 1000
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  mean(X)
})

mean(X_hat)
sd(X_hat)

#Histogram and QQ plot
library(dslabs)
library(tidyverse)
library(gridExtra)
p1 <- data.frame(X_hat = X_hat) %>% ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(X_hat = X_hat) %>%
  ggplot(aes(sample = X_hat)) +
  stat_qq(dparams = list(mean = mean(X_hat), sd = sd(X_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)


#For realistic values of p, suggests 100k sample would give prediction almost perfectly...
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

# .... but poll this large is expensive, and also the theory has limits. 
# get biases ... e.g. samples only on people who have phones. People not telling the truth
#about their voting intentions etc etc
# Also not sure who is actually going to end up voting.

#Differences in expected value and actual p is called bias.
#Typical bias in polls is between 1 and 2%


#SECTION 3

#Confidence intervals. Geom_smooth useful for a visual on confidence intervals

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")


#Calculating confidence intervals using samples
p <- 0.45
N <-1000

X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 1.96*SE_hat, X_hat + 1.96*SE_hat)

#To get z values to use for 95% CI can use this (i.e. so 2.5% on each end of the distribution)
qnorm(0.025)
qnorm(0.975)

#for 99%, this
qnorm(0.005)
qnorm(0.995)

#Can Monte Carlo simulate to check if p does fall in the CI based on the sampling 95% of the time.
p <- 0.45
N <-1000
B <- 10000

inside <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 1.96*SE_hat, X_hat + 1.96*SE_hat ) #true/false check if p is in the 95% CI produced by the sample
})

mean(inside) #so about 95%. About 5% of the time p falls outside the confidence interval produced

#NB: confidence intervals are random variables and change with each samples
# p, the population probability, does not change

#So correct language is to say there is a 95% chance that actual, fixed p is somewhere within the confidence interval


#POWER

#Trying to predict the result of an election, then a confidence interval that includes a spread of 0
# (a tie) is not helpful. Doesn't imply a close election - means the sample size is too small
#Power is the probability of detecting an effect when there is a true effect to find.
#Power increases as sample size increases because larger sample size means smaller standard error

#e.g. little power in the following. SE of the spread (2*SE, per notes) is too big...
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-1.96, 1.96)*2*sqrt(X_hat*(1-X_hat)/N)

#P-VALUES

#Computing p-value if pulling 100 balls from an urn, and getting 52 blue.
#NB: example is a bit confusing as words it as prob blue > red by chance... but
#actually test is two tailed, so looking at chance of getting a result as extreme as 52

#Null hypothesis that there's no difference in proportions

#1) Can either do on the sum of balls pulled (i.e. 52)
N <- 100
Blue_Balls <-52
Null_hypothesis_Blue <- 50
Null_hyp_Blue_p <- 0.5
Null_hypothesis_SE <- sqrt(N)*sqrt(Null_hyp_Blue_p*(1-Null_hyp_Blue_p))

z <- (Blue_Balls - Null_hypothesis_Blue) /  Null_hypothesis_SE
1 - (pnorm(z) - pnorm(-z)) #prob of getting less than 48 or greater than 52 balls

#Or 2) can do on spread of proportions (p*(1-p) = 2p - 1 )

Null_hyp_spread = 0
Sample_spread = 0.04 #0.52 - 0.48
Null_hyp_SE <- sqrt(Null_hyp_Blue_p*(1-Null_hyp_Blue_p)/N)
Null_hyp_SE_spread = 2 * Null_hyp_SE

z <- (Sample_spread - Null_hyp_spread) /  Null_hyp_SE_spread
1 - (pnorm(z) - pnorm(-z)) #prob of spread less than -0.02 or greater than +0.02
#Gives same result


#SECTION 4: STATISTICAL MODELS

#Mimicking a set of poll results a week before 2012 presidential election using monte carlo
#In reality Obama won popular vote by 3.9%

d <- 0.039 #The actual result
Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516) #Sample sizes of polls
p <- (d+1)/2  #The actual proportion voting for Obama

confidence_intervals <- sapply(Ns, function(N) {
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p,p)) #replicate 0,1 vector for those voting Obama, for each poll
  X_hat <- mean(X) #Proportion voting Obama in poll
  SE_hat <- sqrt(X_hat*(1-X_hat)/N) #SE of X
  2*c(X_hat,X_hat - 1.96*SE_hat, X_hat + 1.96*SE_hat)-1 #outputs 3 values. Spread = 2p-1. So these are
  #an estimate of the spread, and the min max bounds of a 95% CI around the spread from each poll
})
confidence_intervals

#Stick into a data frame with a row for each poll outcome
polls <- data.frame(poll=1:ncol(confidence_intervals), t(confidence_intervals),sample_size=Ns) #NB: t=transpose
names(polls) <- c("poll","estimate","low","high","sample_size") #This labels the columns. 
polls

#So most polls individually show 0 spread within the 95% confidence interval, even if the mean
#was weighted to Obama

#Nate Silver combined the polls to improve accuracy...
sum(polls$sample_size) #Total of 11269 results across all the 12 polls

#Estimate of the spread using a weighed average of the spreads (weighted by N in each poll)
d_hat <- polls %>%
  summarize(avg=sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg
d_hat #0.0494

#Get p_hat (proportion for Obama estimate) from d_hat, to use to get the SE
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe #0.0185

round(d_hat*100,1)
round(moe*100,1)

#So in this case get 4.9% plus or minus 1.8%



#Statistical Models to predict election results (popular vote)

library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)
head(polls_us_election_2016)

#Filter to national polls within a week of the election.
#Remove polls given a B or less by 538 website (Nate Silver) ... i.e. the polls they don't believe to be reliable

polls <- polls_us_election_2016 %>%
  filter(state=="U.S." & enddate >= "2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) #NB: leaves the non graded polls in the data

#Add spread estimate (Clinton vs Trump)
polls <- polls %>% mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100)

#p = proportion voting for Clinton
#d = spread (Clinton over Trump) ... = 2p-1
#SE of d = 2 * sqrt(p(1-p)/N)   

#Get weighted avg spread
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
d_hat

p_hat <- (1+d_hat)/2
se_d <- 2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe <- 1.96*se_d #To get 95% Conf Interval bounds
moe 

#So spread of 1.43% plus or minus 0.66%

#Actual was 2.1 .. so what happened?


# histogram of the spread shows it not to be normally distributed, and SE appears larger than 0.0066
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)


#Some pollsters taking more than one poll a week...
polls %>% group_by(pollster) %>% summarize(n())

#... plot the most regular poll takers (over 6 per week)
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster,spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# ... So can see the pollster matters. Spread is about right, but averages are quite different
# Pollster bias impacts who they're polling etc and influences results

#Get lat reported poll from each pollster...
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup() #removes the group by

?ungroup

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

#In effect have 15 mean p to work with, but given pollster to pollster variability
#our SE is no longer sqrt(p(1-p)). Is an unknown (sigma)
#Theory tells us we can estimate it using the sample standard deviation though (where our sample
#here is the 15 rows of aggregated data)
#And to get from standard deviation to standard error, divide through by sqrt of 15
#Formula for sample standard dev divides the delta of x to mean x squared, divided by n-1

sd(one_poll_per_pollster$spread) #0.024

results <- one_poll_per_pollster %>%
  summarize(avg=mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start= avg - 1.96*se, end=avg+1.96*se)
round(results*100,1) #Now gives a wider confidence interval of 1.7% to 4.1% which includes the election
#night spread of 2.1%


#5. Bayesian Statistics

#Monte Carlo to try and visualise Bayes' theorem
#Looking at testing for Cystic Fibrosis, which has a 1 in 3900 prevalence (0.00025)
#Sample of 100,000 people simulated

prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"),N,replace=TRUE, prob = c(prev,1-prev))

N_D <- sum(outcome=="Disease") #In our Monte Carlo, how many of the 100,000 people have the disease?
N_D #Answer: 22
N_H <- sum(outcome=="Healthy") #In our Monte Carlo, how many of the 100,000 people have the disease?
N_H #Answer: 99978

#So makes the probability of getting some false positives is quite high (i.e. coming back positive
#on a test, but actually being healthy) even with a v accurate test (e.g. 99% accurate at identifying
#a diseased person or a healthy person)
accuracy <- 0.99
test <- vector("character",N) #Create empty character vector of length N
test[outcome=="Disease"] <- sample(c("+","-"),N_D, replace=TRUE, prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"] <- sample(c("+","-"),N_H, replace=TRUE, prob=c(1-accuracy,accuracy))

table(outcome,test) #To create 2*2 matrix summary table (confusion matrix I think)

#So outcome is that only about 2% of those with a positive test have the disease, given how much 
#the population is dominated by people without the disease

#Replicating the above in a monte carlo shows the answer converge to 2%. Which is what 
#was calculated via Bayes Theorem


#6. ELECTION FORECASTING

mu <- 0 #assuming we don't have prior info on expected value of the spread
tau <- 0.035 #historical view that average historical spread is about 3.5%
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

#credible interval
posterior_mean + c(-1.96,1.96)*posterior_se   #interval between 1.6% and 4%

#Prob that spread is bigger than zero
1 - pnorm(0,posterior_mean,posterior_se) #almost 100% ... seems too high
#Doesn't account for general bias that appears to impact all polls in the same way
#In 2016, polls favoured democrats by 1-2%...
#Need to include a term in the model to account for the variability


#Example 1: Modelling 6 polls from 6 pollsters of 2000 people, with a spread of 2.1%
#Model here is just based on difference plus an error term to capture sampling error

J <- 6
N <- 2000
d <- .021
p<- (d+1)/2
X <- d + rnorm(J,0,2*sqrt(p*(1-p)/N))

X

#Example 2: Modelling 6 polls from 5 pollsters. If only assuming d and an error term (per below), the 
#simulated data doesn't capture the pollster to pollster variability we saw in the actual data
#Per the plot produced below

I <- 5
J <- 6
N <- 2000
d <- .021
p<- (d+1)/2
X <- sapply(1:I,function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p)/N))
})

library(dplyr)
test <- data.frame(X) %>% t() %>% data.frame() %>% mutate(pollster = 1:I) 
names(test) <- c("P1","P2","P3","P4","P5","P6","Pollster")
test %>% ggplot(aes(Pollster)) +
  geom_point(aes(y=P1)) + 
  geom_point(aes(y=P2)) + 
  geom_point(aes(y=P3)) + 
  geom_point(aes(y=P4)) + 
  geom_point(aes(y=P5)) + 
  geom_point(aes(y=P6)) +
  ylab("Spread")

#Example 3: Adding a term for the pollster effect h, assuming average effect of 0 and standard error of 0.025

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I,0,0.025)
X <- sapply(1:I,function(i){
  d + h[i]+ rnorm(J,0,2*sqrt(p*(1-p)/N))
})

test <- data.frame(X) %>% t() %>% data.frame() %>% mutate(pollster = 1:I) 
names(test) <- c("P1","P2","P3","P4","P5","P6","Pollster")
test %>% ggplot(aes(Pollster)) +
  geom_point(aes(y=P1)) + 
  geom_point(aes(y=P2)) + 
  geom_point(aes(y=P3)) + 
  geom_point(aes(y=P4)) + 
  geom_point(aes(y=P5)) + 
  geom_point(aes(y=P6)) +
  ylab("Spread")

#Simulated data now looks much more like the actual data

#Example 4: Adding another term in for general bias impacting all polls, b
#Assumed to have expected value 0, se of 0.025 based on historical data

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2) #So including the general bias variability
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)



#Predicting the electoral college
results <- polls_us_election_2016 %>%
  filter(state!="U.S." & 
           !grepl("CD",state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg=mean(spread),sd=sd(spread),n=n()) %>%
  mutate(state=as.character(state))

results <- left_join(results, results_us_election_2016, by = "state")

#Assign sd to states without a poll
results <- results %>%
  mutate(sd=ifelse(is.na(sd),median(results$sd,na.rm=TRUE),sd))

mu <- 0
tau <- 0.02
results %>% mutate(sigma=sd/sqrt(n),
                   B=sigma^2 / (sd^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean)) #sort results

#Monte Carlo assuming no general bias term...
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000,{
  results %>% mutate(sigma=sd/sqrt(n),
                     B=sigma^2 / (sd^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
                     clinton = ifelse(simulated_result>0,electoral_votes,0)) %>%
    summarize(clinton=sum(clinton)) %>%
    .$clinton + 7 ##7 for Rhode Island and DC, which are guaranteed Clinton (no polls in these states)
})
mean(clinton_EV >269) #Result is 0.997 of Clinton getting the 269 electoral college votes required..

data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = 269)

#Recomputing the Monte Carlo with general bias built in...

tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000,{
  results %>% mutate(sigma=sqrt(sd^2/n + bias_sd^2),
                     B=sigma^2 / (sd^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
                     clinton = ifelse(simulated_result>0,electoral_votes,0)) %>%
    summarize(clinton=sum(clinton)) %>%
    .$clinton + 7 ##7 for Rhode Island and DC, which are guaranteed Clinton (no polls in these states)
})
mean(clinton_EV_2 >269) #Result is 0.894 of Clinton getting the 269 electoral college votes required..


#Variability of poll results across time for a single pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#comparing se with theoretical se...
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

#NB: Is the theoretical right? Shouldn't it be p * (1-p) ... not d*(1-d)?
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth=0.01, color="black")

#Empirical results don't look normally distributed...

#Suggests p not consistent over time, which time series supports (below for several pollsters)

#Below time series looks across multiple pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)


polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))


#T distribution
z <- qt(0.975, nrow(one_poll_per_pollster)-1)
z #So T with 14 degrees of freedom (n-1) here gives a 2.14. vs the 1.96 produced in the normal distribution
#which you get from qnorm(0.975)

qt(0.975,14)
qnorm(0.975)

library(dplyr)
one_poll_per_pollster %>%
  summarize(avg=mean(spread),moe=z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

library(dslabs)
library(tidyverse)


#INFERENCE FOR BINARY, CATEGORICAL, ORDINAL DATA

data("research_funding_rates")
research_funding_rates

totals <- research_funding_rates %>%
  select(-discipline) %>% #removes discipline, keeps all else
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals

totals %>% summarize(percent_men = yes_men / (yes_men + no_men),
                    percent_women = yes_women / (yes_women + no_women))
#So about 18% for men, 15% for women.

#R.A. Fisher popularised hypothesis testing.
#Lady tasting tea test. Lady claimed she could tell if milk added before or after tea (she could as it turns out)

tab <- matrix(c(3,1,1,3),2,2)
rownames(tab) <- c("Poured Before","Poured After")
colnames(tab) <- c("Guessed Before","Guessed After")
tab

#p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")


#Chi Square test on the research funding example

funding_rate <- totals %>%
  summarize(percent_total = 
              (yes_men + yes_women) / 
              (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate #So total funding rate is between 16 and 17%

#Observed two by two table...
two_by_two <- tibble(awarded = c("no","yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

#expected two by two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

#Chi squared test tells us how likely to see the observed data if the expected table was true
#by random chance alone

two_by_two %>% 
  select(-awarded) %>%
  chisq.test()

two_by_two
two_by_two %>% 
  select(-awarded)

#0.05 ... so 5% chance of getting the results if at random

#Odds Ratio
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

odds_men / odds_women #So 1.2 times higher odds of getting funding if you're a man vs womean

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()




