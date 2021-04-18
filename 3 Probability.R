
#3. Probability Course

install.packages("gtools")


#DISCRETE PROBABILITY

#Random number generators

#Generate a 'bowl' to pick red or blue balls from. Two red and three blue balls.
beads <- rep( c("red","blue"), times = c(2,3))
beads

#Then to pick a ball from the 'bowl' at random
sample(beads,1)

#Monte Carlo simulation to basically do this experiment a large number of times
#replicate function helps with repeating the task

B <- 10000
events <- replicate(B, sample(beads,1))
 
#Then tabulate to get the distribution of counts
tab <- table(events)
tab

#prop.table to get proportions from a counts table
prop.table(tab)


#NB: don't have to use replicate. Sample function allows specification of number of balls
#to pick from the bowl.
#Note though that the default is to do this without replacement - have to specify that you 
#want to do it with replacement

sample(beads,5)
sample(beads,5)
sample(beads,5)

#This errors due to the default
sample(beads,6)

#To sample with replacement...
events <- sample(beads,B, replace = TRUE)
prop.table(table(events))


#Important line of code to set a random see
set.seed(1986)

?set.seed

#NB: Changed in R3.6 .. to go back to method used in 3.5 and before:
#set.seed(1, sample.kind="Rounding")  


#Mean function to get probabilities. Turns trues into 1s, falses into 0s and takes the average
beads <- rep( c("red","blue"), times = c(2,3))
beads
mean(beads=="blue")


#Probability Distributions for categorical data. Just like 44%, 44%, 10%, 2% ... adding up to 100%
#Probability Distributions for continuous vars often more useful in data science


#Independence - two events independent if the output of one does not impact the other
#classic example - coin toss. Dice throw etc.

#Often card game probabilities not independent. e.g. If I pick a king out of a deck first
#with a probability of 4 in 52, then the chance of picking a king next goes down
# to 3 in 51. Therefore these events are not independent.

x <- sample(beads,5)
x[2:5]


#When events are not independent, conditional probabilities are useful/necessary

#Assessment
balls <- rep( c("cyan","magenta","yellow"), times = c(3,5,7))
mean(balls=="cyan")
mean(!balls=="cyan")

p1 <- mean(balls=="cyan")

balls2 <- rep( c("cyan","magenta","yellow"), times = c(2,5,7))
p2 <- mean(!balls2=="cyan")

p1 * p2

p3 <- mean(!balls=="cyan")
p1 * p3


#Combinations and Permutations

#Going to use paste function (NB: paste used to create strings by 
#joining smaller strings)

number <- "Three"
suit <- "Hearts"

paste(number, suit)

#paste also works on pairs of vectors
#So to join first five letters with first five numbers (converted to character)
paste(letters[1:5], as.character(1:5))

#Also going to use expand.grid which gives us all the combinations of two lists

expand.grid(pants = c("blue","black"), shirt = c("white","grey","plaid"))

#So to generate a deck of cards:
suits <- c("Diamonds","Clubs","Hearts","Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")

#Create table of two cols for all cards
deck <- expand.grid(number = numbers, suit = suits)
#Join two cols together
deck <- paste(deck$number, deck$suit)

#Check probability of pulling a king

kings <- paste("King",suits)
mean(deck %in% kings) #So evaluates each element in vector deck to true or false (1 or 0), then gets means

#Conditional probability of pulling a 2nd king having pulled the first
#Going to do it using combinations() and permutations() functions available in gtools package

library(gtools)
permutations(5,2) #How many ways can we choose 2 numbers (r) from list 1,2,3,4,5 (i.e. n=5)

#NB: in permutations, 1,3 and 3,1 are different. 
#NB2: can't have 2,2 ... because once you've picked 2 it's not available from the list to pick again.


#Can add vector for this function (defaults to 1:n if not specified)
#Defaults: permutations(n,r,v=1:n,set=TRUE, repeats.allowed=FALSE)
all_phone_numbers <- permutations(10,7,v=0:9) #v=0:9 is defining vector of digits from 0-9 rather 
#than default 1-10

n <- nrow(all_phone_numbers) #to return number of rows in all phone numbers data frame (containing all
#the permutations of numbers 0 to 6 ... i.e. 7 digits (r), but 0-6 given vector definition)


#Pull out five sample numbers within the n permutations
sample(n,5)


index <- sample(n,5)
all_phone_numbers[index,] # ,to return the rows in the sample and all columns (just putting in
#all_phone_numbers[index] would only return the first column


#To compute all possible ways we can choose 2 cards, when the order matters

hands <- permutations(52,2,v=deck)

#Above creates a 2 column matrix, 1st col being the first card pick, 2nd col being the 2nd card pick
#To get the first and second columns into vectors
first_card <- hands[,1]
second_card <- hands[,2]

#So how many permutations have a king as the first card?
sum(first_card %in% kings)

#And how many of these 204 also have a king for the second card
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Given 1s/0s can also do it based on the proportion of both kings in card 1 and 2 vector
#out of those with king in the first/ This gives same answer:
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

?permutations

permutations(3,2)
combinations(3,2)

#Blackjack - order doesn't matter so use combinations.

#create vector of aces
aces <- paste("Ace",suits)
aces

#vector of facecards
facecard <- c("King","Queen","Jack","Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
facecard
class(facecard)

#All combinations of two cards
hands <- combinations(52,2,v=deck)
class(hands)

#Then how often in the hands 
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#NB: Assumption here is that aces come first, which they do in this case
#but to be safe can cover both options
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard)) 


#Monte Carlo of 21s.... NB: curly brackets used to create functions (and in for/do loops, if then logic etc)
#Here specifying the function (which is required as the 2nd element of replicate)
?replicate

#To get a sample of two cards
hand <- sample(deck,2)
hand

#Repeat sample and check if ace and a face card produced
B <- 10000
results <- replicate(B, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


#Class of 50 students. Chances two share a birthday. USing Monte carlo.
#For simplicity assume nobody born on Feb 29th.
#Represent birthdays as numbers between 1 and 365.

n <- 50
bdays <- sample(1:365,n,replace=TRUE)

#Can use duplicated function to see if value of a vector has already appeared in the vector
duplicated(c(1,2,3,1,4,3,5))

#any function then returns true if any of the elements in the vector are true
any(duplicated(bdays))

#To run this test 10,000 times and see proportion of times there's duplicate bdays...
B <- 10000
n <- 50
results <- replicate(B, {
  bdays <- sample(1:365,n,replace=TRUE)
  any(duplicated(bdays))
})
mean(results)

#NB: Gives about 97-98% likelihood of two people sharing birthdays


#Now, create a lookup tables of likelihoods vs number of people in the class
#Create a function to do it

compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365,n,replace=TRUE)
    any(duplicated(bdays))
})
  mean(same_day)
}

n <- seq(1,60)

#NB: A lot of functions in R work element wise. So could write something like the below and it would work
compute_prob(n)
#However, the function we just created is expecting a scalar for n, so will only produce one number
#not a vector

#In a case like this, sapply can be used to performn element-wise operations on any function.
#Example (although bad one(!), because sqrt does work with vectors)

x <- 1:10
sapply(x, sqrt)
sqrt(x)

#But for compute_prob is useful
prob <- sapply(n, compute_prob)

#And can plot
plot(n,prob)


#Compute exact probabilities now...
#Based on probabilities of each person having a unique birthday. 
#Can then get to probability of 50 having unique birthdays

exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365
  1 - prod(prob_unique)
}
eprob <- sapply(n,exact_prob)

#Plot the calculated lines on top of the monte carlo simulation

plot(n,prob)
lines(n, eprob, col="red")


#How many Monte Carlo experiments are enough?
#Theoretical stats to answer it
#But can also check stability of the estimate under different numbers of experiments 

#Create sequence between 10 and 100000
B <- 10^seq(1,5,len=100)
B

compute_prob <- function(B, n=22){
  same_day <- replicate(B, {
    bdays <- sample(1:365,n,replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B,compute_prob)
plot(log10(B), prob, type = "l")


#Monty Hall Problem - Monte Carlo, start looking at sticking strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3) #vector with 1,2,3
  prize <- sample(c("car","goat","goat")) #vector with prize and two goats
  prize_door <- doors[prize=="car"] #True/false vector to show which door has the prize
  my_pick <- sample(doors,1)  #Random selection of 1 of the three doors
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1) #randomly select one of the other doors with no prize
  stick <- my_pick  #stick with original door
  stick == prize_door #See if you won
})
mean(stick) #In first run got 0.325 as the probability of winning if sticking 


#Monty Hall Problem - Monte Carlo - switching strategy
B <- 10000
switch <- replicate(B, {
  doors <- as.character(1:3) #vector with 1,2,3
  prize <- sample(c("car","goat","goat")) #vector with prize and two goats
  prize_door <- doors[prize=="car"] #True/false vector to show which door has the prize
  my_pick <- sample(doors,1)  #Random selection of 1 of the three doors
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1) #randomly select one of the other doors with no prize
  switch <- doors[!doors %in% c(my_pick,show)] #Change doors
  switch == prize_door #See if you won
})
mean(switch) #In first run got 0.325 as the probability of winning if sticking 



#SECTION 2 : CONTINUOUS PROBABILITY

library(tidyverse)
library(dslabs)
data(heights)

#Create numeric vector of male heights (NB: remember can use .$height to create a vector or pull(height))
x <- heights %>% filter(sex=="Male") %>% .$height
class(x)
x

#To get proportion in vector x less than or equal to height a
F <- function(a) mean(x<=a)

#So to get probability of male being taller than 70.5 inches?
1 - F(70.5)

#To get probability in an interval a to b, it's F(b) - F(a)


#Theoretical distributions - e.g. normal distribution
?pnorm
#pnorm gives normal distribution function

#So probability of male under 70 inches based on normal distrib:
pnorm(70.5,mean(x),sd(x))

#... and prob over
1 - pnorm(70.5,mean(x),sd(x))

#NB: Above uses sample mean and standard deviation as approximations



# plot distribution of exact heights in data.Not hugely useful to treat every height as discrete
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

table(x)
prop.table(table(x))
head(table(x))


# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))



#dnorm used in R to get the probability density function for the normal distribution (d = density)

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#NB: just using dnorm(x) gives the standard normal prob density function by default (i.e centred on z=0,
# and z=1 meaning 1 standard deviation from the mean)
#For different mean, sds .... dnorm(z,mu,sigma)


#rnorm() to randomly generate normally distributed outcomes. Takes 3 arguments:
#1) size, 2) average (defaults to 0), 3) standard deviation (defaults to 1)

#VERY USEFUL FUNCTION

#To generate data that looks like reported heights (same number in sample, same mean and sd):

x<- heights %>% filter(sex=="Male") %>% .$height #create vector of male heights
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n,avg,s)

data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth=2)


#Using Monte Carlo and simulated data.
#How rare to get a 7 footer

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800,avg,s) #sample of 800
  max(simulated_data) # determine the tallest height
})
mean(tallest >=7*12) # proportion of times that tallest person exceeded 7 feet (84 inches)


#Other useful continuous distributions:
  # student-t
  # chi-squared
  # exponential
  # gamma
  # beta

# R standard to get quantiles, density functions etc

# d = density (NB: think this is the probability density function - so area under this (across a range of x) is probability)
# q = quantile
# p = probability (NB: think this is cumulative distribution function - returns actual probability less than x)
# r = random

#putting these in front of the distrib name gives us these useful functions.

#So dnorm, qnorm, pnorm, rnorm for the normal distribution
# dt, qt, pt, rt for the student's t distribution


# SECTION 3 - RANDOM VARIABLES, SAMPLING MODELS AND TEH CENTRAL LIMIT THEOREM

#Random variables. e.g. Random variable X : 1 if picking a blue ball from urn, 0 if not

beads <- rep( c("red","blue"), times = c(2,3)) #urn with two red and three blue balls
X <- ifelse(sample(beads,1) == "blue",1,0)
X


#SAMPLING MODELS 

#Looking at roulette. Assume $1 for a win; $1 lost if customer doesn't get their colour
# S = Casino's total winnings

colour <- rep(c("Black","Red","Green"),c(18,18,2))

#Replicate 1000 spins of the roulette wheel. Assuming customer chooses black.
n <- 1000
X <- sample(ifelse(colour=="Red",-1,1), n, replace=TRUE)
X[1:10] #have a look at outcomes from first ten spins

#Can generate with one line of code without specifying colours. Using a sampling model:
X <- sample(c(-1,1),n,replace=TRUE, prob=c(18/38,20/38))
X[1:10] #have a look at outcomes from first ten spins
S <- sum(X) #Total winnings/losses off 1000 spins
S

#S is a random variable.
#probability distribution of a random variable tells us the probability of the observed value
#falling in any given interval

#So the probability that we (thec casino) lose money from the roulette game... prob S < 0

#Can use monte carlo to generate distribution of S over many repetitions

n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1),n,replace=TRUE, prob=c(18/38,20/38))
  sum(X)
})

mean(S < 0) #probability of the casino losing money over 1000 spins

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#normal approx is v good...

#For sampling distributions, the mean is the expected value and the standard deviation is the standard error
#of the mean.

#NB: of these kind of 1-0 trials follows a binomial distribution. The sampling distribution of the mean 
#follows a normal distribution even though the underlying distribution is binomial. 
# This is the central limit theorem

#Can use understanding of this to skip the monte carlo 

a <- 1
b <- -1
p <- 20/38

mu <- n * ((p * a)+((1-p)*b))
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p)) #see formulae in notes
pnorm(0,mu,se) #probability of getting less than zero over 1000 roulette spins



#SECTION 4 - THE BIG SHORT
#Setting interest rates on loans!

#So set it up with probability of default of 2%, and loss given default of $200k
#Across 1000 loans what might the loss be?
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0,1), n, prob=c(1-p,p), replace=TRUE )
sum(defaults * loss_per_foreclosure)

#Above gives losses as a random variable. 
#Monte Carlo it to get distribution

B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob=c(1-p,p), replace=TRUE )
  sum(defaults * loss_per_foreclosure)
})

data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col="black")

#Or can use central limit theorem and associated formulae

n * (p*loss_per_foreclosure + (1-p)*0) # expected value of total losses from 1000 loans
sqrt(n)*abs(loss_per_foreclosure)+sqrt(p*(1-p)) #Standard Error of losses

#To break even on average, need to set an interest rate that gives enough income
#to cover the expected value of the losses
#i.e set x so loss per foreclosure * prob of loss + x * (1-prob of loss) = 0
x <- - loss_per_foreclosure * p/(1-p) # earnings on the ones that do repay
x/180000 #interest rate for 180000 loan (NB: no annualisation..!)

#But still 50% chance we'll lose money. So interest rate needs to be high enough that chances
#of losing money are manageably low. Will always be some risk though ... if pricing too high
#will lose all clients to other banks
 
#Say we want the probability of making a loss overall to be <1%.... what does x have to be now?
#Can turn into a z score, where we want the Z for a prob of <1%. Give a z is value - mean / SD, and
#the value is zero in this case, we can use mean and SD, and sub in previous formulae for the two
#Rearrange a bit to get the interest needed to reduce the probability of making a loss overall
#to under 1%


l <- loss_per_foreclosure
z <- qnorm(0.01) #the z value at which 1% of the standard normal distrib is less than
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Monte Carlo to get a similar answer to the central limit theorem based answer:
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money .. about 1%


#Going higher risk...

p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure * p + x * (1-p) #expected value of $640 a loan

#Calculating the n that gets the probability of making a loss less than 1%
#Will be an n at which this happens because the distribtution of the average return will
#get narrower around the expected value as n gets bigger

z <- qnorm(0.01)
n <- ceiling( (z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2 )
n  #So in this case 22163 loans required

#With this n we expect to earn:
n*(loss_per_foreclosure*p + x * (1-p)) #About 14m dollars

#Confirm with a Monte Carlo

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)


#Problem - must have independent draws
#i.e. one person defaulting must be independent of other people defaulting
#Not the case. Economic drivers impact all, meaning they're not independent events

#Resimulate assuming events that impact the probability of a loan going bad across all n loans
# from somewhere between 0.03 and 0.05, rather than just being 0.04

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money - now up to 35% likelihood
mean(profit < -10000000)    # probability of losing over $10 million -> 23%

#So way higher likelihood of losing money vs a static situation where the probability of default is known

#If plotting a histogram, it doesn't look normal, which shows why the central limit
#theorem shouldn't be applied to get the expected distribution of losses
data.frame(profit_in_millions = profit/10^6) %>%
  ggplot(aes(profit_in_millions)) +
  geom_histogram(binwidth = 4, col="black")






