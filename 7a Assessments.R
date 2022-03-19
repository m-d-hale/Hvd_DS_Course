
library(Lahman)

#Scatterplot At Bats vs Runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game,R_per_game)) +
  geom_point(alpha=0.5)

#Scatterplot, win rate versus fielding errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Wins_per_game = W/G, Errors_per_game = E/G) %>%
  ggplot(aes(Errors_per_game,Wins_per_game)) +
  geom_point(alpha=0.5)

#Scatterplot, triples vs doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Triples = X3B/G, Doubles = X2B/G) %>%
  ggplot(aes(Doubles,Triples)) +
  geom_point(alpha=0.5)


#correlation coefficient between at bats per game and runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(AB_per_game,R_per_game))

#correlation coefficient between win rate and fielding errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Wins_per_game = W/G, Errors_per_game = E/G) %>%
  summarize(r = cor(Wins_per_game,Errors_per_game))

#correlation coefficient between triples and doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Triples = X3B/G, Doubles = X2B/G) %>%
  summarize(r = cor(Triples,Doubles))


#Slope given rho of 0.5, sd father of2, sd son of 3
r <-0.5
sd_father_x <- 2 
sd_son_y <- 3

m <- r * (sd_son_y / sd_father_x)
m


set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


mu_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
mu_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)

r <- r <- cor(female_heights$mother, female_heights$daughter)

mu_mother
sd_mother
mu_daughter
sd_daughter
r

m <- r * (sd_daughter / sd_mother)
m
b <- mu_daughter - (m*mu_mother)
b

#% of variation explained by x
r*r

#If mother is 60 inches
daughter_height <- b + m*60
daughter_height



#Least squares plot...
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


library(Lahman)

str(Teams)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_G = R/G, BB_per_G = BB/G, HR_per_G = HR/G)
lm(R_per_G ~ BB_per_G + HR_per_G, data=Teams_small)




B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])



galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))





#Q8
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- lm(mother ~ daughter, data=female_heights)
predict_mother <- predict(model)
predict_mother[1]
female_heights[1,]


#Q9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) 

bat_99_01 %>% filter(mean_singles > 0.2) %>% summarize(n= n())
bat_99_01 %>% filter(mean_bb > 0.2) %>% summarize(n= n())

library(dplyr)
bat_combined <- bat_02 %>% inner_join(bat_99_01,by="playerID")

bat_combined %>% summarize(correlation = cor(mean_singles, singles))
bat_combined %>% summarize(correlation = cor(mean_bb, bb))

bat_combined %>%
  ggplot(aes(mean_singles,singles)) +
  geom_point()

bat_combined %>%
  ggplot(aes(mean_bb,bb)) +
  geom_point()

lm(singles ~ mean_singles, data=bat_combined)
lm(bb ~ mean_bb, data=bat_combined)


#2.3
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}




dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

tmp <- dat %>%
  group_by(lgID) %>%
  do(tidy(lm(R~HR, data=.)))



library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton


galton %>% group_by(pair) %>%
  summarize(n=n())

galton %>% group_by(pair) %>%
  summarize(n=n(), cor = cor(parentHeight,childHeight))

?cor

tmp <- galton %>% group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data=.),conf.int = TRUE)) %>%
  filter(term == "parentHeight")
  
  summarize(n=n(), cor = cor(parentHeight,childHeight))
  
  
  
#2.4
tmpdat <- data.frame(Team = c("TeamA","TeamB"), 
           BB = c(2,1),
           singles = c(4,6),
           doubles = c(1,2),
           triples = c(0,1),
           HR = c(1,0) ) %>% 
  mutate(R_hat = predict(fit, newdata= .))
tmpdat


  
  
fitNew <- Teams %>%
  filter(yearID == 1971) %>%
           lm(R ~ BB + HR, data = .)
tidy(fitNew, conf.int = TRUE)
  

#Same for every year 1961 to 2018
  
Teams %>%
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(yearID, estimate, conf.low, conf.high) %>%
  ggplot(aes(yearID, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  geom_smooth(method = "lm")


tmpdat <- Teams %>%
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>% 
  mutate(est = estimate) %>%
  select(yearID, est)

fit <- lm(est ~ yearID, data = tmpdat)
tidy(fit, conf.int = TRUE)



#Section 2 assessment
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)


fit <- lm(avg_attendance ~ R_per_game, data =Teams_small )
tidy(fit, conf.int = TRUE)


fit2 <- lm(avg_attendance ~ HR_per_game, data = Teams_small )
tidy(fit2, conf.int = TRUE)

fit3 <- lm(avg_attendance ~ W, data = Teams_small )
tidy(fit3, conf.int = TRUE)

fit4 <- lm(avg_attendance ~ yearID, data = Teams_small )
tidy(fit4, conf.int = TRUE)

Teams_small %>% summarize(Cor1 = cor(W,R_per_game),
                          Cor2 = cor(W,HR_per_game))

Teams_small_strat <- Teams_small %>%
  mutate(win_new = round(W/10)) %>%
  filter(win_new %in% 5:10) %>%
  group_by(win_new)

Teams_small_strat %>% summarize(n=n())


Teams_small_strat %>%
  do(tidy(lm(avg_attendance ~ R_per_game, data = .))) %>%
  filter(term == "R_per_game")

Teams_small_strat %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, data = .))) %>%
  filter(term == "HR_per_game")


fit <- lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = Teams_small )
tidy(fit, conf.int = TRUE)


#2002 attandance
TmpSet <- data.frame(R_per_game = c(5,5), 
                     HR_per_game = c(1.2,1.2) ,
                     W = c(80,80) ,
                     yearID = c(2002,1960))
TmpSet %>% mutate(Att_hat = predict(fit,newdata=.))


Teams_02 <- Teams %>% filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G ) %>%
  mutate(Att_hat = predict(fit,newdata=.))

Teams_02 %>% summarize(cor = cor(avg_attendance,Att_hat))


?admissions

#Confounders assessment
library(dslabs)
data("research_funding_rates")
research_funding_rates

fund_new <- research_funding_rates %>%
  gather(gender_apps,applications, c(applications_men, applications_women)) %>%
  gather(gender_awards,awards, c(awards_men, awards_women)) %>%
  filter((gender_apps  == "applications_men" & gender_awards == "awards_men") |
         (gender_apps  == "applications_women" & gender_awards == "awards_women")) %>%
  mutate(gender = ifelse(gender_apps == "applications_men","men","women")) %>%
  select(discipline, gender, applications, awards)

Summ_fund_new <- fund_new %>% group_by(gender) %>% summarize(TotApps = sum(applications),
                                            TotAwards = sum(awards)) %>%
  mutate(TotNotAwarded = TotApps - TotAwards, 
         PctAwarded = TotAwards / TotApps)

Summ_fund_2by2 <- Summ_fund_new %>% select(gender,TotAwards,TotNotAwarded)

Summ_fund_2by2 %>% select(-gender) %>% #get rid of gender var
  do(tidy(chisq.test(.)))


#Q4

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(discipline,success,color=gender,size=applications)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90,hjust=1))

dat2 <- dat %>% group_by(discipline) %>%
  summarize(TotApps = sum(applications),
            TotAwards = sum(awards)) %>%
  mutate(successrate = TotAwards/TotApps)