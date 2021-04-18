
###DATA VISUALISATION #####
###########################
###########################

library(dslabs)
data(murders)
head(murders)

#Data types - first two categorical, next two numerical.
#NOIR - Nominal, ordinal, integer (e.g counts), ratio (continuous, e.g. heights)

#Distributions

#Frequency table
prop.table(table(heights$sex))

#Cumulative Distribution Function - for numerical vars

a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function) #NB: Haven't done this yet, but looks to substitute for a for loop
                                      #to call the cdf_function function at each height in the a object

a
cdf_values

plot(a, cdf_values)


#Histograms normally more useful than CDF (even if some data loss by grouping)

#Smooth density plots - similar to histograms, but aesthetically more appealing. Smooths over
#some of the peaks. Also replaces counts with %s (i.e. area under chart = 1)
#Starting to get into samples vs populations

#Can select degree of smoothness in the ggplot package

#Normal Distrib related

#Average and Standard deviation calc (NB: Is Sdev estimate for whole population 
#I think - i.e. not sample used to infer population)
x <- c(230,249,102,302,302,201)
average <- sum(x)/length(x)
SD <- sqrt( sum((x-average)^2) / length(x))
average
SD

#Just return the male heights in vector x
index <- heights$sex == "Male"
x <- heights$height[index]
x

#In built R functions mean and SD. Note SD is the estimate of population from sample
#so divides by n-1 rather than n
average <- mean(x)
SD <- sd(x)
c(average=average,SD=SD) #bit confusing but is creating a vector with name and value in this.

#To get standard units (i.e. (x- avg)/SD)
z <- scale(x)
z

mean(abs(z)<2)

#So approx normal


#Using Pnorm to calculate probabilities (for normally distributed phenomena)

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#Prob a male is smaller than 70.5 inches
pnorm(70.5,mean(x),sd(x))

#i.e just need average heights and standard dev to get probabilities, not all data in a vector

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

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


#Quantiles
library(dslabs)
data(heights)

#Use summary to get min 1st quartile, median, 3rd quartile and max
summary(heights$height)

p <- seq(0.01,0.99,0.01)
quantile(heights$height,p)

#Qnorm - gives theoretical value of a quantile with prob p of observing
# a value equal to or less than tha quantile value given a normal distribution with mean
# mu and standard deviation sigma.

# Default is mu=0 and sigma =1 (i.e. standard normal)
qnorm(0.25)

#So to get theoretical 25th percentile height using normal distribution
qnorm(0.25, mean(heights$height), sd(heights$height))
#NB: So slightly different to data generated quartile

pnorm(-1.96)
qnorm(0.025)

#Quantile-quantile (or Q-Q) plots to test if normal distribution is a good approximation for 
#a variable (e.g. height, weight etc)

# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

p <- seq(0.05,0.95,0.05)

observed_quantiles <- quantile(x,p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


#Boxplots



#Creating the visualisation in R
#Can use standard R visualisation functions like plot, hist, and boxplot.
#Also other packages for creating grid and lattice

#GGPlot2 is used mainly here though, and is part of the tidyverse library
library(tidyverse)

#Or can load directly using command library(ggplot2) without loading all of tidyverse

#Quite intuitive etc, but downside is that you have to have data in 
#columns/rows format (each col a different variable)

#Use a cheat sheet to help with ggplot2!!


library(dslabs)
data(murders)

#1.Data component
#2."Geometry" component is the chart type - scatter plot, bar plot, histograms, smooth densities, q-q etc.
#3. Aesthetic Mapping component - what variables goes on y and x, colours for regions etc
#4. Scale component - how the x and y are scaled (log, z etc)
#5. Labels, Title, Legend etc

#1. Data component. First step to create a plot object
p <- ggplot(data=murders)
class(p)
p

#Or could pipe the data
murders %>% ggplot()

#In ggplot create graphs by adding layers using +
#So typical structure is: DATA %>% ggplot() + Layer 1 + Layer 2 + ... + Layer n

#Usually first layer defines geometry

?geom_point

#Requires arguments x and y. These are the aesthetic mappings

murders %>% ggplot() +
geom_point(aes(x=population/10^6, y=total))   

#Or can do via object (x= and y= are optional, as the first two expected arguments of aes):
p <- ggplot(data=murders)
p + geom_point(aes(population/10^6, total))   
#NB: don't need to state data set again i.e. murders$population... given data already assigned

#Add labels as well for each point - 3rd argument of aes
p + geom_point(aes(population/10^6, total)) + geom_text(aes(population/10^6, total, label=abb)) 


#To change/define point size
p + geom_point(aes(population/10^6, total), size=3) + geom_text(aes(population/10^6, total, label=abb)) 

#To move the text to the right of the points
p + geom_point(aes(population/10^6, total), size=3) + 
  geom_text(aes(population/10^6, total, label=abb), nudge_x = 1)

#More efficient way of writing this - define global aesthetic mapping
#Can check the arguments required for ggplot here:
args(ggplot)

#So can define overall aesthetic mapping inside the ggplot function
p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
p +geom_point(size=3) + geom_text(nudge_x = 1)

#But can still override the global mappings locally if required
p +geom_point(size=3) + geom_text(aes(x=10,y=800,label = "Hello there!"))

#To rescale the axes to log10 - need a scale layer
p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
p +geom_point(size=3) + geom_text(nudge_x = 0.05) +
scale_x_continuous(trans = "log10") +
scale_y_continuous(trans = "log10")

#Given log is such a standard transform, is a direct function to do it instead
p +geom_point(size=3) + geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10()

#Then to add labels to axes
p +geom_point(size=3) + geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010")

#Point colours using col argument in geom point function
p<- murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010")

#To change points to blue
p + geom_point(size=3, color = "blue")

#To associate colours with geographical region
# Assign categorical variable to color assignment differentiates the colour and adds a legend

p + geom_point(aes(color=region), size=3)

#To add a line reflecting the overall average US murder rate
r <- murders %>% summarize(rate = sum(total) / sum(population) * 10^6) %>%  pull(rate)

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

#To capitalise Region in legend
p <- p + scale_color_discrete(name="Region")

#Producing the whole lot
p<- murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name="Region")
p


#Add on packages from outside ggplot to augment capability
#need ggthemes and ggrepel packages

#Themes. For most part use a theme defined in the dslabs package
ds_theme_set()

#Adding themes in ggthemes can be added

library(dslabs)
ds_theme_set()
library(ggthemes)
p + theme_economist()


#Putting it all together to generate the plot

# load libraries
#install.packages("ggplot2")
library("ggplot2")

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


#Going back to histograms

heights %>% filter(sex=="Male")

#Use geom_histogram

?geom_histogram

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p + geom_histogram()

p + geom_histogram(binwidth = 1) #To define bin widths

p + geom_histogram(binwidth = 1, fill="blue", col="black") + #To add colours - bar fill and outline
  xlab("Male heights in inches") + #To add label to x axis
  ggtitle("Histogram") #To add chart title

#Smooth densities
p + geom_density(fill= "blue")

#QQ plots. Need to redefine p, as need to define data being used as 'sample' for QQ plot (see help file)

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
p + geom_qq()

#Defaults to standard normal for QQ plot. To change to use vals, need dparams argument (See help file)

params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd = sd(height))
p + geom_qq(dparams = params)

#Add identity line
p + geom_qq(dparams = params) + geom_abline()

#Or can scale variable to standard normal first, which saves having to compute mean and sd
heights %>% filter(sex=="Male") %>% ggplot(aes(sample=scale(height))) + geom_qq() + geom_abline()
                                           
#To put plots next to each other - can use gridExtra package, which has a function called grid.arrange
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth= 1, fill = "blue", col="black")
p2 <- p + geom_histogram(binwidth= 2, fill = "blue", col="black")
p3 <- p + geom_histogram(binwidth= 3, fill = "blue", col="black")


install.packages("gridExtra")

library(gridExtra)
grid.arrange(p1,p2,p3, ncol=3)

?geom_label


#SECTION 3: SUMMARISING WITH DPLYR

library(tidyverse)
data(heights)

#So to get mean and sd of male heights in a summary table
s <- heights %>% filter(sex=="Male") %>% summarize(average=mean(height), standard_deviation=sd(height))
s

#So can access values from data frame, e.g. 
s$average
s$standard_deviation

heights %>% filter(sex=="Male") %>% summarize(median=median(height), 
  minimum=min(height), maximum=max(height))

#Could get 3 using the below, 
quantile(heights$height, c(0,0.5,1))

#NB: Used to throw an error if using quantile within summarize function, but in later
#versions of dplyr it doesn't

#DOT PLACEHOLDER
#To return dplyr functions as vectors rather than data frames

data(murders)
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

#NB: Straight average of the states, not the avg for the US as a whole

us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

class(us_murder_rate)

#When requiring a numeric argument, won't be able to use this as a data frame.
#To convert the us_murder_rate into a numeric:
us_murder_rate %>% .$rate
class(us_murder_rate)
#NB: This didn't seem to work for me. Equivalent is pull function which does seem to work

us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate
class(us_murder_rate)

#Weirdly, this did work...

#GROUP BY 
heights %>% group_by(sex) #creates a group data frame
#dplyr functions, such as summarize, will behave differently when operating on a group data frame

heights %>% group_by(sex) %>% summarize(average=mean(height), standard_deviation=sd(height))

murders %>% group_by(region) %>% summarize(median_rate=median(murder_rate))


#SORTING

#Arrange function from dplyr package is v useful for sorting entire tables
murders %>% arrange(population) %>% head()

murders %>% arrange(murder_rate) %>% head()

#DESC to change to sort by descending order
murders %>% arrange(desc(murder_rate)) %>% head

#Adding 2nd, 3rd things to sort by
murders %>% arrange(region, murder_rate) %>% head()

#top_n function if want something other than first 6 rows (which head produces)
#So top 10 states with highest murder rates:
murders %>% top_n(10,murder_rate)

#Top n basically a filter. To order, use arrange function first, then top n
murders %>% arrange(desc(murder_rate)) %>% top_n(10,murder_rate)


#NB: to ignore NAs in summary functions put in na.rm=TRUE:
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)


#GAPMINDER DATASET USING SPREADSHEETS FROM GAPMINDER.ORG 
#Dataset is in dslabs
library(dslabs)
data(gapminder)
head(gapminder)

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>%
  select(country, infant_mortality)

#Looking at old date to start with
ds_theme_set()
filter(gapminder, year==1962) %>% 
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

#Coloring points for each continent
filter(gapminder, year==1962) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point()

#Faceting
#Putting in side by side plots
#Can achieve by faceting variables (creating strata)
#add facet_grid as a layer (can use up to two vars)

filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(continent~year) #so continent in the 'columns', continent in the 'rows' of the output charts

#To just facet by one variable, then use a dot for the other. e.g. 
filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(.~year)

#If you want to do lots of year, can use facet_wrap to wrap onto new rows (rather than keep 
#producing thinner and thinner columns). 

years <- c(1961,1980,1990,2000,2012)
continents <- c("Europe","Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility,life_expectancy, col=continent)) +
  geom_point() +
  facet_wrap(~year)

#NB: when using facet, the range of the axes is determined by data in all plots
#i.e. not different for each plot. Which makes sense in terms of comparisons


#TIME SERIES PLOTS

#geom_line instead of geom_point to produce line rather than scatter plot

gapminder %>% filter(country=="United States") %>%
  ggplot(aes(year,fertility)) +
  geom_line()

countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, group=country)) +
  geom_line()

#Use colour to distinguish the two countries ... then don't need group, as assumes what is wanted
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, col=country)) +
  geom_line()

#NB: They prefer labelling lines rather than legends... although a bit harder to produce...
#Per below, create data object first
labels <- data.frame(country=countries, x = c(1975,1965), y=c(60,72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,life_expectancy, col=country)) +
  geom_line() +
  geom_text(data=labels, aes(x,y,label=country), size=5) +
  theme(legend.position = "none")   #Tells plot not to print a legend 


#TRANSFORMATIONS

#Adding GDP in dollars per capita per day
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#Values already adjusted for inflation
past_year <- 1970
gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1,color="black")

#Log transformations to turn multiplicative changes into additive ones
gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth=1,color="black")

#Bumps in distribution sometimes referred to as local modes. 
#And multi-modal distribution if more than one mode

#Recommend log to the base 2 or log to the base 10, as more easy to compute 
#10 to the 1,2,3; or 2 to the 1,2,3, than e to the 1,2,3 etc.

#Two ways to use log transformations in plot:
  #1. log values before plotting them
  #2. Use log scales in the axis

#1st method makes it easier to intepret intermediate values. e.g. mid-point between 1 and 3
# will be 2.

#If the scale is logged then you'll have a non-linear scale - so say, 1, 10, 100, 1000. 
#And harder to conceive of where th point for say 500 is (won't be midway between 100 and 1000 on the
#x axis)

#But log scales does mean the original values are on the axis. Which is a benefit.
# To use log scale in the axis, use scale_x_continuous as we did before.
# For this to make sense, need to not transform the data. So code is per below.

gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1,color="black") +
  scale_x_continuous(trans="log2")


#STRATIFY AND BOXPLOT

#Large number of regions
length(levels(gapminder$region))

p <- gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(region,dollars_per_day)) 
p + geom_boxplot()
 
#Rotate the labels. Can do it by changing the theme
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle=90,hjust=1))

#Order by something other than alphabetical using reorder function
#to change order of levels of a factor variable based on a summary computed on a numeric vector

#Simple example. Start point with alphabetical sort
fac <- factor(c("Asia","Asia","West","West","West"))
levels(fac)

#Ordering by the mean value of another variable (ascending order)
value <- c(10,11,12,6,4)
fac <- reorder(fac,value,FUN=mean)
levels(fac)

#So to reorder by region, add a mutate in the data pipe

p <- gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region,dollars_per_day, FUN=median)) %>%
  ggplot(aes(region,dollars_per_day,fill=continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  xlab("")
p


#Then add layer to scale y by log base 2
p + scale_y_continuous(trans="log2")

#To overlay individual country on the plot...
p + scale_y_continuous(trans="log2") + geom_point(show.legend=FALSE)


#COMPARING DISTRIBUTIONS

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

#Comparing difference in distribution across time. 
#Start by a West vs Developing variable called group in the data (mutate)

gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans="log2") +
  facet_grid(.~ group)

#Facet by region and year
past_year <- 1970
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~ group)

#NB: So developing countries shifting to right

#Also note that more countries in 2010 vs 1970. More countries due to split of USSR etc
#Also more countries with data available


#So to get only countries available in both
#Filter data to the year, and pull out the country variable into a vector (.$ for dplyr)
country_list_1 <- gapminder %>%
  filter(year== past_year & !is.na(dollars_per_day)) %>%
  .$country
country_list_2 <- gapminder %>%
  filter(year== present_year & !is.na(dollars_per_day)) %>%
  .$country
country_list <- intersect(country_list_1, country_list_2)

gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~ group)

#Boxplot revisited
p <- gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(region = reorder(region,dollars_per_day, FUN=median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2")
p + geom_boxplot(aes(region,dollars_per_day, fill = continent)) +
  facet_grid(year~.)

#One above other- not ideal. Could facet to plot next to each other
#Or can try and put both on same chart with each year of box plot next to each other

#Turn year into factor, so we can use it as a dimension in the plot

p + geom_boxplot(aes(region,dollars_per_day, fill=factor(year)))


#DENSITY PLOTS

#By default, areas under smooth density plots equal 1. 
#So if dividing into West and Developing countries you lose the fact that there's way more countries
#classed as developing

#Some calculated variables produced in functions regardless, but not normally printed. Access
#these by surrounding the name of them with dot dot each side
#So...
#aes(x=dollars_per_day, y = ..count..)

past_year <- 1970
present_year <- 2010
country_list_1 <- gapminder %>%
  filter(year== past_year & !is.na(dollars_per_day)) %>%
  .$country
country_list_2 <- gapminder %>%
  filter(year== present_year & !is.na(dollars_per_day)) %>%
  .$country
country_list <- intersect(country_list_1, country_list_2)


p <- gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West","Developing")) %>%
  ggplot(aes(x=dollars_per_day, y = ..count.., fill=group)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  scale_x_continuous(trans="log2")
p + geom_density(alpha=0.2) +
  facet_grid(year~.)


#bw argument in geom_density can be used to change the smoothness
p + geom_density(alpha=0.2, bw=0.75) +
  facet_grid(year~.)

#Plot to show key regions separately using case_when

# add group as a factor, grouping up the regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#Reproduce density plots by newly defined group
p <- gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp) & country %in% country_list) %>%
  ggplot(aes(x=dollars_per_day, y = ..count.., fill=group)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  scale_x_continuous(trans="log2")
p + geom_density(alpha=0.2) +
  facet_grid(year~.)

#To stack densities for the defined group
p + geom_density(alpha=0.2, bw=0.75, position="stack") +
  facet_grid(year~.)

#To weight the densities by the population of each country, rather than the count of each country
#within each grouped region. Use the weight function and tell it what to weight by
gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill=group, weight=weight)) +
  scale_x_continuous(trans="log2") +
  geom_density(alpha=0.2,bw=0.75,position="stack") +
  facet_grid(year~.)


#ECOLOGICAL FALLACY

#Country child survival rates vs average income
#Redefine group to start with to get a slightly more granular grouping

# produce a more granular group variable in the gapminder data
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365, 
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)


#limit argument to change the range of the axes
#breaks argument lets the setting of the locations of the axis labels

#logistic - f(p) = log(p/(1-p))
#p/(1-p) is the odds
#log transformation makes quantity symmetric. If p is 0.5, the logged odds is zero
log(0.5/(1-0.5))

#NB: plot showing income and survival rates - v strong correlation
#BUT 'ecological fallacy' - only operates at regional level. 
#Breakdowns by country etc shows much more complexity

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 


#Slope Chart

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 


#Bland-Altman Plot

library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")


#Example using Vaccine data - adding different shapes to plot points, colours, intensity, size etc

data(us_contagious_diseases)
str(us_contagious_diseases)

the_disease <- "Measles"

dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii","Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state,rate))

#NB: Run this to get info on last error:
rlang::last_error()

dat %>% filter(state=="California") %>%
  ggplot(aes(year,rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col="blue")


#Sequential high to low colour palettes
library(RColorBrewer)
display.brewer.all(type="seq")

#Divergent example
display.brewer.all(type="div")

#geom_tile to tile the region with colours representing disease rates

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color="grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9,"Reds"),trans="sqrt") +
  geom_vline(xintercept=1963, col="blue") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")


#Good plot above, but using colour for rate can be tricky to ascertain exactly

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate=sum(count,na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


