
#7. Linear Regression

#Baseball example. Hits: Singles, doubles, triples, home run from a time at the plate
#At Bat: Number of times a batter gets a hit or an out (note, bases on balls are excluded)

#NB: Can also be walked to first base, if the pitcher pitches 3 balls, but this is not counted as a hit

#Batting Average is just the hits / at bats (i.e. bases on balls excluded from numerator and denominator). 
#And typically ranges from 20% to 38% depending on the player.
#25% typcally referred to as decimal with 3 digits, so 0.250


#Bill James first to consider base on balls (i.e. being walked to first base), as it's still 
#a 'success' and helps the team to get runs. So if higher than avg, but lower BA, these players may be undervalued

#Most stolen bases does get an award, but high numbers of stolen bases also tend to go hand in hand with a
#high number of outs. So question on whether that is helping to produce runs. 


#Q1: Do teams that hit more home runs, score more runs?

#Trying to replicate what the Oakland As were doing in 2002, so looking at data from 1961 (when 
#games changed to 162 per year) and 2001

#Plot a scatterplot of home runs vs overall runs...
library(Lahman)
library(ggthemes)
ds_theme_set()
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G,R_per_game = R/G) %>%
  ggplot(aes(HR_per_game,R_per_game)) +
  geom_point(alpha=0.5) +
  theme_economist()

#So pretty clear association between home runs per game and overall runs per game


#Q2: Do teams that steal more bases per game, score more runs?

str(Teams)
?Teams

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G,R_per_game = R/G) %>%
  ggplot(aes(SB_per_game,R_per_game)) +
  geom_point(alpha=0.5) +
  theme_economist()

#Not massively strong relationship, but is some correlation


#Q3: Do teams that have more base on balls per game, score more runs?

str(Teams)
?Teams

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G,R_per_game = R/G) %>%
  ggplot(aes(BB_per_game,R_per_game)) +
  geom_point(alpha=0.5) +
  theme_economist()

#Some positive correlation. Bigger slope than for stolen bases, less than for home runs... 
#Question raised as to whether the % of walks is also driven by the % of home run...
#Potential confounding. Linear regresision helps to try and parse these effects out


#Section 2: Correlation

#Galton came up with correlation and regression. Was studying heredity. 
#Had qs like: how much of a son's height can I predict with the parents' height

#HistData package contains Galton's family data. Start by creating a dataset with the heights of fathers
#and their first sons
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Data well approximated by normal distributions, so can start with two averages and two standard deviations as summaries
galton_heights %>%
  summarize(mean(father),sd(father),mean(son),sd(son))

#Doesn't capture the relationship between the two though, which can be seen in the scatterplot:
galton_heights %>% ggplot(aes(father,son)) +
  geom_point(alpha=0.5)

#Need the correlation coefficient (see notes). 
galton_heights %>% summarize(correlation_coeff = cor(father,son))

#So positive correlation (i.e. taller father correlated with taller son), and about half way from no correlation
#to perfect correlation (where all variation in son's height would be explained by father's height)

#NB: Sample correlation the most commonly used estimate of the population correlation
#Implies the correlation we use is a random variable

#e.g. if population was 179 pairs, but samples of 25. R here would be a random sample
set.seed(0)
R <- sample_n(galton_heights,25,replace=TRUE) %>%
  summarize(cor(father,son))

?sample
?sample_n
#NB: sample_n looks like it is the dplyr version of sample (the base R version)
#Also looks like it's going to be replace with slice_sample()

#Then can monte carlo simulate to see the distribution of the random variable
B <- 1000
N <- 25
R <- replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
    summarize(r=cor(father,son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) +
  geom_histogram(binwidth=0.05, color="black")

mean(R) #0.492, so ~the population correlation
sd(R) #0.152. So a decent size standard error


#Reminder that the central limit theoreom applies with enough samples, so expect R to be normally distributed

#25 doesn't appear to be enough though, looking at the QQ plot

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))



#Section 3: Stratification and Variance

#Correlation not always a good summary of the relationship between two variables.
#Famous example: Anscombe's quartet - shows v different relationships, all with a correlation of 0.82...

#In heights example, if we know father is 72 inches tall (1.14 SDs above the mean), we can 
#stratify the data to see the average of sons where the fathers are about 72 inches
#And see what the relationship between the two variables looks like
#This is a conditional average

conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarize(avg=mean(son)) %>% .$avg
conditional_avg

#Gives 71.8, which is 0.54 SDs higher than the mean ... less than the 1.14 for the father...
#Stratification and box plots good for seeing this.

galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata,son)) +
  geom_boxplot() +
  geom_point()


#To just get the averages... which follow approx. linear relationship
#Therefore linear relationship between two variables appears to be appropriate
galton_heights %>% 
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg=mean(son)) %>%
  ggplot(aes(father,son_conditional_avg)) +
  geom_point()


# Plotting standardised son heights vs standardised father
# Using correlation of 0.5 as gradient fits the data well.
# Is the regression line...


# calculate values to plot regression line on original data (m=slope, b=intercept, r = rho = correlation coeff)
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

m
b


# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)


#Checking bivariate normal distribution (i.e. y normally distributed at different levels of x (aka conditional distribution)
library(dplyr)
library(ggplot2)
galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)


# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

m_1
b_1

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

m_2
b_2



#Back to baseball. Confounding.
Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B - X3B)/G,BB=BB/G, HR=HR/G) %>%
  summarize(cor(BB,HR),cor(Singles, HR), cor(BB,Singles))

#More home run hitters on a team = more walks as changes pitching approach
# = more runs.

#i.e. is the home runs causing the runs in this case. 
#i.e. bases on balls are confounded with homeruns - regression can 
#help to separate the effects

#Without regression, could stratify home runs per game
#and look at impact of bases on balls on runs within each group

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

#Get regression slopes in each strata
dat %>%
  group_by(HR_strata) %>%
  summarize(slope=cor(BB_per_game,R_per_game)*sd(R_per_game)/sd(BB_per_game))
#Reduces the slope vs not considering home runs


# stratify by BB instead...
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, oR_per_game)*sd(R_per_game)/sd(HR_per_game)) 
#Less of a drop vs simple


#Multivariate regression to deal with the above
#Popular in fields where randomised experiments are hard to run

#"When we're not able to randomly assign each individual to a treament or control group,
#confounding is particularly prevalent"

#Now getting into more familiar notation for regression
#beta0 + beta1*x1 etc

#Errors assumed to be independent from each other
# Have expected value 0
# and standard deviation does not depend on i (i.e. index of the observation)


#NB: sometimes translate vars. e.g. subtracting mean from fathers' heights
#Then intercept in regression is more meaningful - gives forecast son's height at mean father's height

#Least squares estimates. 
#Minimisation of the Residual Sum of Squares (RSS)
#i.e. the sum of square deltas between each observed y and the predicted y

rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# Just an example show plot of RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#So can see there's a single value that minimises the RSS

#But simplification to state beta0 = 25
# Can use calculus though - take partial derivatives
#and solve for when they equal zero (i.e. at minima)


#For more complex stuff with lots of predictor vars, can use functions in R
#lm() function can be used to get least squares estimates

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)


#The least squares estimates are random variables
#Can run a Monte Carlo simulation to see this
#Sampling the height data and re-running regressions...

B <- 1000
N <- 50
lse <- replicate(B,{
  sample_n(galton_heights, N, replace=TRUE) %>%
    lm(son ~ father, data = .) %>% .$coef
})
lse2 <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

#CLT applies. For large enough N, the least squares estimates of bet0 and beta1 will be approx normally 
#distributed with expected value beta 0 and beta1
#SEs a bit more complicated to compute, but included in summary

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#Under assumption that epsilons are normally distributed..
#Ratio of parameter estimates to standard errors
#follow a t-distribution with N-p degrees of freedom

#Test null hypothesis that the betas are zero
#For large n, t-distrib comes v close to normal distributions

#To do hypothesis testing, the assumptions mentioned have to hold

#Least Squares coeff estimates can also be strongly correlated...
lse %>% summarize(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 

#NB: Guess this is starting to get into collinearity


#NB: yhat also a random variable and can construct CI of predictions assuming normal errors in yhat
#or large enough sample to use CLT

#In ggplot: geom_smooth(method="lm") shows confidence intervals around predictions
#for a linear regression model line
# Shows less confidence at the extremes, more confidence in the regression where the body of the historical data sits
#Presumably because a given error in slope has more impact at the extremes

#R function predict takes an lm (linear model) input and returns predictions from the linear model

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()



#Advanced dplyr: tibbles
#Tibble is a special kind of data frame

#Re-cap on baseball
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB= B/G,
         R=R/G) %>%
  select(HR,BB,R) %>%
  filter(HR>=0.4 & HR<=1.2)
dat %>%
  group_by(HR) %>%
  summarize(slope=cor(BB,R)*sd(R)/sd(BB))

#NB: if wanting to try to use lm to get CIs for each slope,
#can't group_by into lm, as lm not part of tidyverse
# outcome of group_by is a group tibble
#Summarize does know how to deal with it. How?

#inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#Note select, filter, mutate, arrange preserve the input class (where tibble or data frame)
#But group_by and summarize spit out tibbles always


#Differences

#1. Tibbles much more readable
Teams #raw dataframe - v hard to read in consolte
as_tibble(Teams) #output much more readable

#2. If subsetting a dataframe, may get back an object that is not a data frame
class(Teams[,20]) #e.g. subsetting to 20th column returns an integer

#With tibbles, a subset returns a tibble
#Useful in tidyverse since functions require data frames as inputs

class(as_tibble(Teams)$HR) #with tibble use $ to get back to acutal column

#Also tibbles warn if accessing a column that doesn't exist
#No warning in dataframes (just returns null)

#3. tibbles can have complext entries than data frames
#e.g. lists of functions

#NB: Can create tibbles with the tibble function
tibble(id = c(1,2,3), func = c(mean,median,sd))
#The above creates a tibble containing functions

#4. tibbles can be grouped
#group_by returns a grouped tibble
#summarize functions (tidyverse functions) are aware of the group information


#But functions like lm() on a grouped tibble will just treat the grouped tibble like an ungrouped data frame

#To help these functions outside the tidyverse better integrate with tidyverse, will use do() function.


#DO FUNCTION
#Do function understands group tibbles and always returns a data frame
#e.g. to fit regression line to each home run strata
dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

#NB: If don't define a column name, then do will return the output of lm() not a data frame
#which will result in an error as do() expecting a data frame
#output of function inside do must be a data frame as well


# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))



#Broom packages makes things easier.
#Helps to extract info from lm() and return it in tidyverse data frame


#Tidy() : returns estimates and related info as a data frame

library(broom)
fit <- lm(R~BB, data=dat)
tidy(fit)
#can add other important summaries such as confidence intervals:
tidy(fit, conf.int = TRUE)

#Because outcome a dasta frame, can immediately use it with do
#to string together the commands that produce the table we're after
dat %>%
  group_by(HR) %>%
  do(tidy(fit, conf.int = TRUE))

#and can then filter, select etc to get exactly what we want
dat %>%
  group_by(HR) %>%
  do(tidy(fit, conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

#and can easily visualise using ggplot...
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#Looks pretty flat suggesting estimate doesn't change
#with the home run strata

# inspect with glance
glance(fit)
#augment(): gives observation specific outcomes
augment(fit)  

?do


#2.4 - multiple predictor variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R=R/G) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

#More independent variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, 
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G, R=R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

tidy(fit, conf.int = TRUE)

#Predict 2002 runs, using our model
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G, 
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G, R=R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()


#So decent model for teams
#To get player level model, should be metrics per plate appearance
# average number of team plate appearances per game

pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game=sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

#Also need salaries, positions etc to pick a team for $40m in salary
#For position going to use the most populated position per player using top_n()

players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")


# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players for runs (most with high salaries)
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#Removing unavailable rookies
# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()



#Using linear programming to pick players

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)


my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))



#On base perecentage plus slugging percentage (OPS)
#New stat from the 1980s onwards
#Looks good - correlates closely with the regression predictions

#Sophomore slump - used a lot for second seasons that don't go as well as rookie yr
#Does the data support the theory of a sophomore slump

#player IDs, names and most played position
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#Rookie of the year award winners data with batting stats
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#Only keep players who played a sophomore yaer
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#Spread function to create separate var for rookie year and for sophomore year
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

#68% of the top rookies do have a worse batting average in year 2
mean(ROY$sophomore - ROY$rookie <= 0)

#Now looking at all players in 2013/14
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

#Same pattern when looking at all top performners - worse in 2nd year
#Not rookies, so can't be explained with sophomore slump

#Lookng at bottom performers, the opposite is true.
#Batting average tends to go up in the 2nd year...
arrange(two_years, `2013`)

#All to do with regression to the mean. Just due to change.


#Regression models so far have been predicting a random variable with other random variables
#Measurement error models... another major application of linear regression
#In these applications it is common to have nonrandom covariates such as time
#And randomness is introduced from measurement error rather than sampling of natural variability

library(dslabs)
falling_object <- rfalling_object()

falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

tidy(fit, conf.int = TRUE)




#SECTION 3: CONFOUNDING

#Spurious correlations

#Data dredging - e.g. going through reams of data
#to find correlations to support an argument

#Can produce spurious correlations using monte carlo
#from random processes

#e.g. this simulates random data. 1m groups of 25 obs
#Col 1 is the group, 2 and 3 just two random numbers
N <- 25
G <- 1000000
sim_data <- tibble(group=rep(1:G, each = N), X = rnorm(N*G),
                   Y=rnorm(N*G))

#So def not correlated data.
res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(X, Y)) %>%
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X, Y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")


# linear regression on group with maximum correlation
# Shows statistically significant relationship
# But is a form of data dredging known as p-hacking
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))

#Problem in scientific papers
#Incentive to report significant results, so often 
#only the significant tests that get in papers
#all the tests which returned insignificant don't get included


#Outliers - another way we get hight correlations when there's no causation

#e.g. if entries standardized apart from entry 23.

# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

#Spearmean Correlation is a way to produce correlation
#that is robust to outliers

# use ranks instead to generate correlation, not the values themselves
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")


#Cause and effect reversed...

#e.g. claiming lower test scores is a result of tutoring
# causation is other way around - kids with lower test scores are requiring tutoring

#Or, e.g. modelling father's height as a function of son's...
#Clearly not a causative relationship!


# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))



#Confounders. Variables Z that cause changes in both X and Y

#Example: data from UC Berkeley majors that showed more men
#were being admitted than women (44% of men, 30% of women)

library(dslabs)
data(admissions)
admissions

admissions %>% group_by(gender) %>%
  summarize(percentage = round(sum(admitted*applicants)/sum(applicants),1))

#Chi Sq test rejects hypothesis that gender and admissions are independent

admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

#Closer inspection shows that actually four out of six major favour women
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men )

#i.e. overall numbers suggest a dependence between admissiona and gender
#but when data is grouped by major the dependence seems to disappear

#Can happen if an uncounted confounder is driving most of the variability


# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

#Suggests women more likely to apply to the two hard majors

#And selectivity clearly impacts the probability of anybody entering a major

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()


#Simpson's paradox.
#When we see the sign of correlation flip when we compute it on the whole
#population vs correlation compued on specific strata



