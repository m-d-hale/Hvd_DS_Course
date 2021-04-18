

# Monte Carlo. Sampling distribution of the coefficient of a line between two probabilities.

Prop_in_test <- 0.5 #0.25
n_tot <- 200000
Ctrl_App_Rte <- 0.005
Ctrl_APR <- 0.1490
APR_Diff <- 0.3
Elasticity <- 1.5

Alpha <- Ctrl_App_Rte/(Ctrl_APR^-Elasticity)
Test_APR <- (1-APR_Diff)*Ctrl_APR
Test_App_Rte <- Alpha*(Test_APR^-Elasticity)

n_ctrl <- n_tot * (1-Prop_in_test)
n_test <-  n_tot * Prop_in_test

B <- 1000 #replicate the experiment 10,000 times

#Simulate random selection with probability per theorised take up rate, and get elasticity from samples
E <- replicate(B, {
  Apps_Ctrl <- sample(c(0,1),n_ctrl,replace=TRUE, prob=c(1-Ctrl_App_Rte,Ctrl_App_Rte))
  Apps_Test <- sample(c(0,1),n_test,replace=TRUE, prob=c(1-Test_App_Rte,Test_App_Rte))
  App_Rte_ctrl <- sum(Apps_Ctrl) / n_ctrl
  App_Rte_test <- sum(Apps_Test) / n_test
  -1 * (log(App_Rte_test/App_Rte_ctrl) / log(Test_APR/Ctrl_APR))
})

#Plot the sampling distribution
data.frame(E=E) %>%
  ggplot(aes(x=E)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  geom_density()


#Try broadening to allow entry of different numbers of displayed quotes
sssscdvsdaaa
NumQtes <- seq(100000,500000,100000)
Trial <- sapply(NumQtes,RunSim)
Trial_df <- data.frame(Trial=Trial)

Trial_df %>%
  ggplot() +
  xlab("Elasticity") +
  #theme(axis.text.x = "Elasticity") +
  geom_density(aes(x=Trial.1, color="0.1m quotes")) +
  geom_density(aes(x=Trial.2, color = "0.2m quotes")) + 
  geom_density(aes(x=Trial.3,  color = "0.3m quotes")) +
  geom_density(aes(x=Trial.4,  color = "0.4m quotes")) +
  geom_density(aes(x=Trial.5,  color = "0.5m quotes"))

#90% conf intervals for each of the trials
Trial_df %>% pull(Trial.1) %>% quantile(0.05)
Trial_df %>% pull(Trial.1) %>% quantile(0.95)
Trial_df %>% pull(Trial.2) %>% quantile(0.05)
Trial_df %>% pull(Trial.2) %>% quantile(0.95)
Trial_df %>% pull(Trial.3) %>% quantile(0.05)
Trial_df %>% pull(Trial.3) %>% quantile(0.95)
Trial_df %>% pull(Trial.4) %>% quantile(0.05)
Trial_df %>% pull(Trial.4) %>% quantile(0.95)
Trial_df %>% pull(Trial.5) %>% quantile(0.05)
Trial_df %>% pull(Trial.5) %>% quantile(0.95)




