
#SECTION 1
##########
##########


19.11*0.20

# installing packages
install.packages("dslabs")
install.packages("tidyverse")

#load packages
library(dslabs)
library(tidyverse)

#see installed packages
installed.packages()

#Test - produce first plot
murders %>%
  ggplot(aes(population,total,label=abb, color=region)) +
  geom_label()


# assigning values to variables
a <- 2
b <- -1
c <- -4

a
b
c


# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

#functions
log(a)
exp(a)

#Help files are like user manuals for the functions
#Two ways of getting the info
help("log")
?log

#args gives the arguments required for a function
args(log)

#following are equivalent
log(6,base = 2)
log(6,2)
log2(6)

#arithmetic operators info
help("+")

#vars in R have to start with a letter, and contain no spaces

#class function tells you the data type of an object:
class(a)
class(ls)

#Most common way of storing data are data frames 
#basically tables
#to load murders dataset from dslabs library
library(dslabs)
data("murders")
class(murders)

#NB data(murders) and data("murders") both work; normally leave them out for speed

#str shows structure (annoying, not string!)
str(murders)

#can also use names function to get the var names in dataframe
names(murders)

#head shows first 6 rows of dataset
head(murders)

#$ sign to access columns (called the accessor)
murders$population
#NB this preserves the ordering in the table

#vector. define it like follows
pop <- murders$population

#equivalent using square brackets would be
#pop <- murders[["population"]]

#then can get info on the vector
length(pop)

#Character vectors
# Need to use quotation marks for characters to differentiate them from variables
class(murders$state)

#Also can hagve logical vectors too (below will give false because 3 doesn't equal two)
z <- 3 == 2
z

#NB single equals tries to assign 3 to 2 so would throw an error, rather than a false,
#but this is explained down the line

#'Factor' data types too. e.g. regions column in murders dataset. 
#'Used in R for categorical data as more efficient data storage (stored as integers in the background)
class(murders$region) #returns the data type for the column
levels(murders$region) #gives the levels of the categorical variable region in dataset murders

#c() is used to concatenate
#table function then basically gives a summary 
#NB. table pretty much is like 'group by' and a count var by the look

x <- c("a","a","b","b","b","c")
table(x)



#ASSESSMENT

#Q1

# assigning values to variables
a <- 2
b <- -1
c <- -4

a
b
c


# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

#Q2

log(1024,4)


#Q3
library(dslabs)
data(movielens)
str(movielens)

class(movielens$title)

class(movielens$genres)

nlevels(movielens$genres)


#SECTION 2 - VECTORS
####################
####################

#Concatenate Function
#####################

#Create vectors using c

codes <- c(380,124,818)
country <- c("italy","canada","egypt")

#Can assign names to the codes ... but object codes is still numeric

codes

codes <- c(italy=380,canada=124,egypt=818)
codes <- c("italy"=380,"canada"=124,"egypt"=818)

codes

class(codes)

#Can also use names function to assign names to entries of a vector, using another vector
#So same end result as codes <- c(italy=380,canada=124,egypt=818)

names(codes) <- country
codes


#Sequence Function
##################

#seq function useful for creating sequences; e.g. numbers from 1-10
x <- seq(1,10)
class(x)

#stores as data type integer (just to save memory). Can define this using L
class(3L)


#Can create jumps in the sequence with 3rd argument
seq(1,10,2)

#shorthand for seq, for integers 1 to 10
1:10

#can also get sequences spread between a min and max, using length.out to define
#number of elements in the sequence
seq(1,21, length.out=4)


#Subsetting
###########

#Subsetting - allows access to specific parts of a vector, using square brackets

#For 2nd element in codes vector
codes[2]

#To get 1st and 3rd
codes[c(1,3)]

#to pull entries using the names

codes["canada"]

codes[c("egypt","italy")]


#Coercion - R attempt to be flexible with data types
# Trying to figure out what you meant before throwing an error

#e.g. combining data types- assumes you meant to enter 1 and 3 as characters
# given canada is included 

x <- c(1,"canada",3)
x
class(x)

#Can also use functions to force a coercion - known as typecasting
#e.g. numbers into characters with the as.character function
#back to numbers using as.numeric

x <- 1:5
y <- as.character(x)
z <- as.numeric(y)

x
class(x)
y
class(y)
z
class(z)


#Missing data - in R, NA is used for missing data
# Can be created by coercion, e.g.

x <- c("1","b","3")
as.numeric(x)


#SORT FUNCTION
##############

#Sorting
library(dslabs)
data(murders)
sort(murders$total)

#But sort function only returns for the specific variable, sorted. 
#What we want is the order function, which can be used to sort one variable (e.g. murder rate) and
#return another variable (e.g. state)

x <- c(31,4,15,92,65)
x
sort(x)

#ORDER FUNCTION
###############

#order(x) function gives the sort order; below assigned to the index object
#i.e. the second element of x is lowest, so the first element in order x is 2.
#So isn't like another col with a lookup; order x is sorted by x. Each element then 
#refers to the original position in x 
index <- order(x)
index

#Then returning the x object, but sorted by the index object is done per the below
x[index]

#So to get the list of states ordered by murder rate...
index <- order(murders$total)
murders$abb[index]

#If only interested in most or least murder states. To get abs max number of murders:
max(murders$total)

#To index on the row with the most murders
i_max <- which.max(murders$total)
i_max

#To lookup the state with the most murders, by using i_max index.
murders$state[i_max]

#To index on the row with the least murders
i_min <- which.min(murders$total)
i_min

#To lookup the state with the least murders, by using i_max index.
murders$state[i_min]


#RANK FUNCTION
##############

#This function, per my earlier note, doesn't do the ordering in the returned vector
#It instead returns in the same order as the original vector
#But returns the rank of the first entry in the vector, then the rank of the second etc etc.


x <- c(31,4,15,92,65)
x

#So.....
sort(x) #Values in vector x, sorted smallest to biggest
order(x) #First value tells you the position of the smallest value in x 
rank(x) #First value tells you the rank of the first value of x


#CREATE DATA FRAME OUT OF VECTORS
#################################

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)


#COUNT OF NA in  a vector
# Using new dataset 
library(dslabs)
data(na_example)

# Checking the structure 
str(na_example)

# Find out the mean of the entire dataset 
mean(na_example)

# Use is.na to create a logical index ind that tells which entries are NA
ind <- is.na(na_example)

# Determine how many NA ind has using the sum function
sum(ind)

#GET AN AVG of NON-NA data

# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])



#VECTOR ARITHMETIC
##################


murders$state[which.max(murders$population)]
max(murders$population)

#Arithmetic operations on vectors in R occur element-wise;
#e.g. to convert inches to cm
heights <- c(69,62,66,70,70,73,67,73,67,70)
heights * 2.54

#i.e doesn't do matrix style multiplication. Does it for each row of the vector (of same length)
murder_rate_pct <- (murders$total/murders$population)*100 #NB x100 to get pct in decimal (e.g. 1=1%)
murder_rate_pct

#order the state names by murders rate, from highest to lowest
ind <- order(murder_rate_pct, decreasing=TRUE)
murders$state[ind]


#Section 2 assessment

x <- c(2, 43, 27, 96, 18)

order(x)
sort(x)
rank(x)

min(x)
which.min(x)
max(x)
which.max(x)



name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_hrs <- time/60
time_hrs

distance / time_hrs



#SECTION 3 - Indexing, data wrangling, plots
############################################

#Get murder rate per 100,000 pop
murder_rate <- (murders$total/murders$population)*100000 

index <- murder_rate < 0.71
index

#To just return the state names where this is true:
murders$state[index]

#sum treats a true as 1 and false as 0
sum(index)

#Logical operators in R
# <. <=, >, >=    : all as you'd expect
# ==              : exactly equal to
# !=              : not equal to
# !               : NOT
# |               : OR
# &               : AND

#So to get states where the murder rate is less than or equal to 1, and that are in the west of the US
west <- murders$region == "West"
safe <- murder_rate <= 1

index <- safe & west
murders$state[index]

#USEFUL FUNCTIONS RELATED TO LOGICAL OPERATORS: which, match and %in%

x <- c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)

#which shows which elements in a logical vector are true
which(x)

#So to just get the index for Massachusetts
index <- which(murders$state == "Massachusetts")
index

#So murder rate for Massachusetts
murder_rate[index]

#To pull back the info for multiple states, can use match
index <- match(c("New York","Florida","Texas"),murders$state)
index

murder_state <- murders$state
murder_state[index]
murder_rate[index]

#If we want to know whether of not each element of a first vector is in a second vector...
#... we use the function %in%

x <- c("a","b","c","d","e")
y <- c("a","d","f")

y %in% x

c("Boston","Dakota","Washington") %in% murders$state
#returns false, false, true as washington is a state, but Boston and Dakota aren't


#BASIC DATA WRANGLING - dplyr to manipulate data tables

library(dplyr)

#to change data table by adding a column, or changing an existing one, use mutate
#to filter, use function filter
#to select specific columns, use select

#so, to add the murder rate as a column to the murders data, use mutate
# mutate uses data frame as first argument, and name and value of the variable in the second argument

murders <- mutate(murders,rate=total/population*100000) 
#NB, don't have to specify murders by var names, as defined in first argument
murders

#filter to those with murder rate less than 0.71
#filter function uses data frame as first argument, logical argument as 2nd

filter(murders,rate<=0.71)

#To just select cols you want to work with
#First argument is the data frame; subsequent arguments are the columns to keep

new_table <- select(murders,state,region,rate)
filter(new_table, rate <= 0.71)

#Can avoid creating intermediate new_table in dplyr. Using the 'pipe'
#NB: in this case, don't put in table name as first argument in filter and select
# as already defined in the first step of the pipe

murders %>%  select(state,region,rate) %>% filter(rate <= 0.71)


#CREATING DATA FRAMES
 #Use data.frame. Specify var name, then values.
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam_1 = c(95,80,90,85),
                     exam_2 = c(90,85,85,90))
grades 

#NB: on old R versions, characters used to be turned into factors by default
#But since R 4.0, that's not the case. Check below

class(grades$names)


#Note that if rank(x) gives you the ranks of x from lowest to highest...
#rank(-x) gives you the ranks from highest to lowest.


#BASIC PLOTS

#Scatterplot
population_in_millions <- murders$population / 10^6
total_gun_murders <- murders$total
plot(population_in_millions,total_gun_murders)

#histograms
hist(murders$rate)

murders$state[which.max(murders$rate)]

#Boxplots
boxplot(rate~region, data=murders)


#SECTION 3 ASSESSMENT

library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

avgheight <- mean(heights$height)
avgheight

ind <- heights$height > avgheight
sum(ind)

index <- (heights$height > avgheight) & heights$sex == "Female"
sum(index)

index_female <- heights$sex == "Female"

mean(index_female)


min(heights$height)
index <- match(min(heights$height),heights$height)

heights$sex[index]

max(heights$height)

x<- 50:82
x

y <- !x %in% heights$height
sum(y)


heights2 <- mutate(heights,ht_cm=height*2.54)
heights2$ht_cm[18]

mean(heights2$ht_cm)

females <- filter(heights2,sex == "Female")
str(females)

mean(females$ht_cm)


library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic~region, data=olive)


#SECTION 4 - Programming, for/do loops etc

a<- 2

if(a!=0){
  print(1/a)
} else {
  print("No reciprocal for 0.")
}

library(dslabs)
data(murders)
murder_rate <- murders$total/murders$population*100000

#Print min murder rate state if it has murder rate less than 0.5
ind <- which.min(murder_rate) #get index for smallest murder rate state
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}


#ifelse function (basically like an Excel if statement, that returns the else in the 2nd arguement)
#Function works on vectors

a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
result

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 

na_example
no_nas
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


#FUNCTIONS - creating your own where they don't exist already
#Example to produce a mean (obvs already done in reality in the mean function)

avg <- function(x){
  s<- sum(x)
  n<-length(x)
  s/n
}

MattVector <- c(91,22,34,1,23,45)
avg(MattVector)

#Can check the output is identical to the mean function
identical(mean(MattVector),avg(MattVector))

#NB: Vars defined in a function are not saved in the workspace. Only vars used within the function.


# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}

#NB: Can have multiple arguments to a function. Below allows user to choose between arithmetic
#and geometric mean (with default being arithmetic, if not specified)

avg <- function(x,arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg(MattVector)
avg(MattVector,TRUE)
avg(MattVector,FALSE)


#For Loops

#Calc sum of arithmetic series (1+2+3+4 etc)
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(6)
compute_s_n(100)

#If wanting to get this for all n from 1 to 25. Need a for loop.

#V simple for loop
for(i in 1:5){
  print(i)
}

#Note, i doesn't reset, so will be the last value in the for loop .. i.e. 5 in the above
i

m <- 25
#create empty vector
s_n <- vector(length=m)
#call compute_s_n function for each n up to 25 and store it in the vector created
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
s_n
n

n <- 1:m
plot(n,s_n)

#Then to add a line to the chart too, use lines function. Should get same result.
lines(n,n*(n+1)/2)


#OTHER FUNCTIONS
#For loops not actually used that often in R, as other functions often more powerful to 
#perform such repeated tasks. e.g. apply, sapply, tapply and mapply
#Other useful ones to look up : split, cut, quantile, reduce, identical, unique


#SECTION 4 ASSESSMENT

library(dslabs)
data(heights)

sum(ifelse(heights$sex=="Female",1,2))


newht <- ifelse(heights$height > 72, heights$height, 0)
mean(newht)

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

newheights <- mutate(heights,Height_ft = inches_to_ft(height))

sum(ifelse(newheights$Height_ft <5,1,0))

factorial(4)


# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n
