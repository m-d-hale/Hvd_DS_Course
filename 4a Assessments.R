
#INFERENCE AND MODELLING ASSESSMENT: BREXIT

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread


#Q1:

N <- 1500

#EV and SE for totals
Expected_Remain_Cnt <- N * p
Expected_Remain_Cnt

#EV and SE for proportions (avgs)
SE_exp_remain_cnt <- sqrt(N*p*(1-p))
SE_exp_remain_cnt

SE_p <- sqrt(p*(1-p)/N)
SE_p

#EV and SE for spread
d

SE_d <- 2*SE_p   #as d=2p-1  (and no error in the constant 1, so SE of d is 2* SE of p)
SE_d


#Q2

str(brexit_polls)
head(brexit_polls)

brexit_polls <- brexit_polls %>% mutate(x_hat = (spread + 1)/2)

brexit_summary <- brexit_polls %>% summarize(avg_spread = mean(spread),
                           sd_spread = sd(spread),
                           avg_Xhat = mean(x_hat),
                           sd_Xhat = sd(x_hat)
                           )

#Q3

#Using top poll - to pull first row, and add lower and upper CI bounds:
brexit_polls[1,] %>% mutate(se = sqrt(x_hat*(1-x_hat)/samplesize), lower= x_hat - qnorm(0.975)*se, 
                            upper = x_hat + qnorm(0.975)*se)


#Q4

june_polls <-  brexit_polls %>% filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2 * se_x_hat,
         lower= spread - qnorm(0.975)*se_spread, 
         upper = spread + qnorm(0.975)*se_spread,
         hit = ifelse(d>=lower & d<=upper,TRUE,FALSE),
         pred_TossUp = ifelse(0>=lower & 0<=upper,TRUE,FALSE),
         pred_remain = ifelse(lower > 0,TRUE,FALSE))

head(june_polls)

june_polls %>% summarize(n=n(), pred_TossUp = mean(pred_TossUp), 
                         pred_Remain = mean(pred_remain), pred_hit = mean(hit) )


#Q5

june_polls %>% group_by(pollster) %>% summarize(n=n(), pred_hit = mean(hit) ) %>%
  arrange(pred_hit)

#Q6

june_polls %>%
  ggplot(aes(x=poll_type, y=spread)) +
  geom_boxplot() +
  geom_point()

#Q7
#Q8

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2 
            )

combined_by_type %>% mutate(se_p = sqrt(p_hat*(1-p_hat)/N),
                            se_d = 2* se_p,
                            lower_d = spread - qnorm(0.975)*se_d,
                            upper_d = spread + qnorm(0.975)*se_d)


#Q9

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

str(brexit_hit)
head(brexit_hit)

brexit_2by2 <- brexit_hit %>% group_by(poll_type,hit) %>% summarize(N = n()) %>% spread(poll_type,N)

brexit_2by2 %>% select(-hit) %>% chisq.test()

#Q10 - Odds ratios

Odds_Online <- (brexit_2by2$Online[2]/sum(brexit_2by2$Online)) / 
  (brexit_2by2$Online[1]/sum(brexit_2by2$Online))

Odds_tel <- (brexit_2by2$Telephone[2]/sum(brexit_2by2$Telephone)) / 
  (brexit_2by2$Telephone[1]/sum(brexit_2by2$Telephone))

Odds_Online
Odds_tel

Odds_Online / Odds_tel


#Q11 - Time series plots

d

brexit_polls %>% ggplot(aes(enddate,spread,color=poll_type)) +
  geom_smooth(method="loess",span=0.4) + 
  geom_point() +
  geom_hline(yintercept = d)


#Q12

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

head(brexit_long)

brexit_long %>% ggplot(aes(enddate,proportion,color=vote)) +
  geom_smooth(method="loess",span=0.3) + 
  geom_point()

