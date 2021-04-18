
#ASSESSMENT 1

#Visualisation Exercise - Titanic Dataset

options(digits = 3)    # report 3 significant digits
install.packages("titanic")
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


#To find variable types
?titanic_train
head(titanic)
str(titanic)
class(titanic$Survived)


#Overall distribution
titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(x=Age)) +
  geom_density(alpha=0.2)


#Distributions for female and male
titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(x=Age)) +
  geom_density(alpha=0.2)+
  facet_grid(Sex~.)

titanic %>% filter(!is.na(Age) & !is.na(Sex)) %>% 
  ggplot(aes(x=Age,fill=Sex)) +
  geom_density(alpha=0.2)


titanic %>% filter(!is.na(Age) & !is.na(Sex)) %>% 
  ggplot(aes(x=Age,y=..count..,fill=Sex)) +
  geom_density(alpha=0.2)


#counts of males vs females
titanic %>% filter(!is.na(Sex)) %>% group_by(Sex) %>% summarize(CountPpl = n())

#counts of males vs females aged 40
titanic %>% filter(!is.na(Sex) & Age == 40) %>% group_by(Sex) %>% summarize(CountPpl = n())

#Proportion under 35 male vs female
titanictemp <- titanic %>% filter(!is.na(Sex) & !is.na(Age)) %>% 
  mutate(Btwn_18_35 = ifelse(Age >= 18 & Age <=35,1,0), 
         Under_17 = ifelse(Age < 17,1,0)) 

titanictemp %>% group_by(Sex) %>% summarize(Pct18to35 = sum(Btwn_18_35)/n())

#Proportions under 17
titanictemp %>% group_by(Sex) %>% summarize(PctU17 = sum(Under_17)/n())

#Oldest passenger
titanictemp %>% group_by(Sex) %>% summarize(Oldest = max(Age))


#QQ plot to test normality

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample=Age)) +
  geom_qq(dparams= params) +
  geom_abline()

?geom_qq

#Bar charts of survival rates
titanic %>% filter(!is.na(Survived) & !is.na(Sex)) %>%
  ggplot(aes(Survived, fill=Sex)) +
  geom_bar()


#Density of those who survived vs died by age
titanic %>% filter(!is.na(Age) & !is.na(Survived)) %>% 
  ggplot(aes(x=Age,y=..count..,fill=Survived)) +
  geom_density(alpha=0.2)


#Survival by fare
titanic %>% filter(!Fare==0 & !is.na(Fare) & !is.na(Survived)) %>% 
  mutate(logFare = log2(Fare)) %>%
  ggplot(aes(Survived, logFare)) +
  geom_boxplot() +
  geom_jitter(alpha=0.9, position=position_jitter(w=0.1))

titanic %>% filter(!Fare==0 & !is.na(Fare) & !is.na(Survived)) %>% 
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_jitter(alpha=0.9, position=position_jitter(w=0.1))


titanic %>% filter(Fare>7.5 & Fare<8.5 & !is.na(Survived)) %>%
  mutate(NumSurvived = as.numeric(as.character(Survived))) %>%
  summarize(PctSurvived = sum(NumSurvived)/n())


#Survival by Passenger Class
titanic %>% filter(!is.na(Pclass) & !is.na(Survived)) %>%
  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar()

   #Use position to get 100% bar chart
titanic %>% filter(!is.na(Pclass) & !is.na(Survived)) %>%
  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar(position= position_fill()) 

titanic %>% filter(!is.na(Pclass) & !is.na(Survived)) %>%
  ggplot(aes(Survived, fill=Pclass)) +
  geom_bar(position= position_fill()) 


#Survival by Age, Sex and Passenger Class

titanic %>% filter(!is.na(Age) & !is.na(Survived) & !is.na(Sex) & !is.na(Pclass)) %>% 
  ggplot(aes(x=Age,y=..count..,fill=Survived)) +
  geom_density(alpha=0.2)+
  facet_grid(Sex~Pclass)




#ASSESSMENT 2


library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

head(stars)

avg_mag <- mean(stars$magnitude)
avg_mag

sd_mag = sd(stars$magnitude)
sd_mag


stars %>%
  ggplot(aes(x=magnitude)) +
  geom_density()


stars %>%
  ggplot(aes(x=temp)) +
  geom_density()


stars %>%
  ggplot(aes(x=temp, y=magnitude)) +
  geom_point()

stars %>% mutate(logtemp = log10(temp)) %>%
  ggplot(aes(x=logtemp, y=magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse()

10^3.7

stars %>% mutate(logtemp = log10(temp)) %>% filter(temp > 5000) %>%
  ggplot(aes(x=logtemp, y=magnitude)) +
  geom_point() +
  scale_y_reverse() +
  #scale_x_continuous(trans = "log10") +
  scale_x_reverse() + 
  geom_text(aes(label=star), nudge_x = 0.04) 


stars %>% mutate(logtemp = log10(temp)) %>% 
  ggplot(aes(x=logtemp, y=magnitude)) +
  geom_point() +
  scale_y_reverse() +
  #scale_x_continuous(trans = "log10") +
  scale_x_reverse() + 
  geom_text(aes(label=star), nudge_x = 0.04) 


stars %>% mutate(logtemp = log10(temp)) %>% 
  ggplot(aes(x=logtemp, y=magnitude, color=type)) +
  geom_point() +
  scale_y_reverse() +
  #scale_x_continuous(trans = "log10") +
  scale_x_reverse() 


stars %>% mutate(logtemp = log10(temp)) %>% filter(type == "G") %>%
  ggplot(aes(x=logtemp, y=magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() 



#ASSESSMENT 3


#Climate Change exercise

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

head(temp_carbon)
str(temp_carbon)


#Ways of getting the max year with carbon emissions data
index <- !is.na(temp_carbon$carbon_emissions)
years <- temp_carbon$year[index]
max(years)

#Can't just do max on the data...this doesn't work
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

#... need to create numeric vector first
#Using dot or pull

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

#This does work though ... just can't do it via piping with the col in the max
t <- temp_carbon %>%
  filter(!is.na(carbon_emissions))

max(t$year)

#Another option, creating summary table using summarize
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarize(maximum=max(year))

#Minimum too
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarize(min = min(year), maximum=max(year))

#grouped by year
test <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  group_by(year) %>%
  summarize(sumemissions = sum(carbon_emissions))


temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarize(sumemissions_2014 = sum(ifelse(year=="2014",carbon_emissions,0)),
            sumemissions_1751 = sum(ifelse(year=="1751",carbon_emissions,0)),
            Pct_Emissions = sumemissions_2014 /sumemissions_1751)

#More flexible
Max_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

Min_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarize(sumemissions_Maxyr = sum(ifelse(year==Max_year,carbon_emissions,0)),
            sumemissions_Minyr = sum(ifelse(year==Min_year,carbon_emissions,0)),
            Pct_Emissions = sumemissions_Max /sumemissions_Min)


#temp_anomaly
Max_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  max()

Min_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()

Max_year
Min_year

Yrs <- c(Min_year,Max_year)

#NB: can't do an average with an ifelse like this.
#Basically keeps temp_anomaly for the year specified, but puts in zeroes
#for every other year. So the mean is way below what it should be....
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  summarize(Temp_Maxyr = sum(ifelse(year==Max_year,temp_anomaly,0)),
            Temp_Minyr = sum(ifelse(year==Min_year,temp_anomaly,0)),
            Pct_Tmp = Temp_Maxyr /Temp_Minyr)


temp_carbon %>%
  filter(year %in% Yrs,!is.na(temp_anomaly))


p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x=year, y=temp_anomaly)) + 
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0), col="blue")
p

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x=2000,y=0.05, label="20th century mean"), col="blue")


#Add line graphs of ocean temp anomalies and land temp anomalies.

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x=year)) + 
  geom_line(aes(y=temp_anomaly, color="Global anomaly"),size=1) +
  geom_line(aes(y=ocean_anomaly, color="Ocean anomaly"),size=1) +
  geom_line(aes(y=land_anomaly, color="Land anomaly"),size=1) +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  labs(color = "Legend")

p

?geom_line


#Second part of the assessment

head(greenhouse_gases)

greenhouse_gases %>%
  ggplot(aes(x=year, y=concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept=1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


temp_carbon %>%
  ggplot(aes(x=year, y=carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept=1850)) +
  ylab("Carbon emissions") +
  ggtitle("Carbon emissions by year")

co2_time <- historic_co2 %>%
  ggplot(aes(x=year, y=co2, color=source)) +
  geom_line() +
  ylab("Carbon emissions") +
  ggtitle("Carbon emissions by year")
co2_time

co2_time +
  xlim(-800000,-775000)

co2_time +
  xlim(-375000,-330000)

co2_time +
  xlim(-140000,-120000)

co2_time +
  xlim(-3000,2018)




