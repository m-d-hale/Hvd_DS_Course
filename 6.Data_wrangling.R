
#6. DATA WRANGLING

#Data import from spreadsheets; intro of tidyverse packages readr and readxl

#First step - importing data from files saved on laptop
#To understand where data is relative to working directory, need to know where working directory is.
#Use the following to check this.
getwd()
#"C:/Users/A2Hal/projects/Hvd_DS_Course"

#To change working directory use setwd()
setwd("~/projects")
getwd()

#NB: Had to reset home directory to C:/Users/A2Hal to match home directory in unix 
path.expand("~/")

#If a full path to files is specified, no problem
#It not, it is assumed the path is being given relative to the working directory

library(dslabs)
library(tidyverse)
library(readxl)


#Are some csvs in the dslabs package that can be used to practice importing
system.file("extdata",package="dslabs")

path <- system.file("extdata",package="dslabs")
list.files(path)

#To copy them over to the working directory
filename <- "murders.csv"
fullpath <- file.path(path,filename) #so path to directory plus file name...
fullpath
file.copy(fullpath, getwd())
file.exists(filename) #To check working directory for the file to make sure the copy worked ok

#readr - tidyverse package's library including functions to read text files into R. These include:
  #read_table - white space separated values - typically .txt files
  #read_csv - comma separated values - typically .csv
  #read_csv2 - semicoln separated values - typically csv
  #read_tsv - tab sdelimited separated values - typically tsv
  #read_delim - general text file format, must define delimiter - typically .txt

#readxl package - includes functions to read Excel files into R. These include:
  #read_excel - autodetects format - typically .xls or xlsx files
  #read_xls - original excel format - .xls files
  #read_xlsx - newer excel format - .xlsx files

#excel_sheets function gives the names of the sheets in an Excel files. 
#Sheet name(s) you want can then be passed into the sheet argument in the 3 excel import functions above

#No guarantee the file format matches the extension. Can open and take a look to check. Or can use
#functions such as read_lines() to see the first few lines of a file within R.
#Helpful to see if file has a header or not too

read_lines("murders.csv", n_max = 3) #to get first 3 lines of the murders csv in working directory

#This file is a csv with a header. So read_csv to import...

#Can use relative path to working directory...
dat <- read_csv(filename)
#... or full path to the file...
data <- read_csv(fullpath)

#Creates a tibble called dat. Can see first six lines using head() function:
head(dat)

#Can also use R base functions like read.csv if you want to create a data frame rather than a tibble...
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

str(dat)
class(dat)
str(dat1)
class(dat1)

#Note, it used to be the case that characters were auto converted to factors in e.g. read.csv
#But in R4.0 onwards (which this is using) they will remain as characters
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)


#Importing data from web. Can't download and import into R, or read directly from web

#To read from web can use read_csv with the url. e.g.:
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
head(dat)

#If want local copy of file, use download.file. First argument is the url to the file.
#Second argument is the destination file. NB, because only filename specified below, it saves to work directory

download.file(url, "murders.csv")
?download.file

#Two useful functions for downloading from internet:
# 1) tempdir()   - This creates a directory with a file name that is v likely to be unique
# 2) tempfile()   - creates a chatacter string that is v likely to be a unique file name

#Above useful to download to temporary location, then delete.
tempfile()  #to see the path this creates

tmp_filename <- tempfile() #sets temporary path on laptop
download.file(url,tmp_filename) #downloads the file from the internet to this temporary path
dat <- read_csv(tmp_filename) #reads the file into R
file.remove(tmp_filename) #removes the temporary file


#TIDY DATA

#In tidy data, each row represents an observation and each column represents a different variable.
#In wide data, each row includes several observations and one of the variables is stored in the header.
  #e.g. in the example below, the countries are rows, the years are columns and the data in the table
  #is fertility rates.

#To tidy this, need to turn year into a column as well as country. Then another column titled fertility rates...


#1) Re-shaping data

data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()


# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)


#tidyr pacakage contains several functions that are useful for tidying data. Included in tidyverse. tidyr includes:
    # gather() function  : converts wide data into tidy data. 
        #1st argument sets the name of the col to collapse the wide data columns into. 
            #e.g. year in the above example
        #2nd argument sets the name of the col that will hold the values currently in the wide data cols
            #e.g.fertility in the above example
        #3rd argument specifies the columns to be gathered. Default is to gather all the columns
    # spread() function  : converts tidy data to wide data (inverse of gather)
        #1st argument: which variable will be used as the column names
        #2nd arguement specifies which variables to use to fill out the cells.


# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year,fertility, '1960':'2015')

head(new_tidy_data)

#NB: in the above example of gather, everything except country was gathered, because it wasn't specified
#In this case, quicker to write code to tell gather function what not to gather...

new_tidy_data <- wide_data %>%
  gather(year,fertility, -country)
head(new_tidy_data)

#Year var is changed from integer to character. Gather function assumes column names are characters....
class(tidy_data$year)
class(new_tidy_data$year)

#... so need to convert it back to a number to work with it again in scatter plots etc
#either use as.numeric function, but can do it within the convert argument within the gather function
new_tidy_data <- wide_data %>%
  gather(year,fertility, -country, convert=TRUE)
head(new_tidy_data)
class(new_tidy_data$year)

new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#To convert back to wide data use spread function (see note above for arguments)
new_wide_data <- new_tidy_data %>% spread(year,fertility)
head(new_wide_data)

select(new_wide_data, country, '1960':'1967')


#Example using 
path <- system.file("extdata",package="dslabs")
filename <- file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

#Wide format raw data by year, but also two variables. e.g. 1960_fertility, 1960_life_expectancy
dat <- raw_dat %>% gather(key,value, -country)
head(dat)

#Better ... but still need to separate the value column into one col for fertility and one for life expectancy
#Use the separate function. Apart from the data name, it takes 3 arguments:
  #1) name of column to be separated
  #2) names to be used for the new columns
  #3) the character that separates the variables

dat %>% separate(key,c("year", "variable_name"),"_")
#Default is to separate underscores, so can actually leave that out
dat %>% separate(key,c("year", "variable_name"),"_")

#Problem as underscores not just used to separate year and the var name. Therefore can tell it to put
#an additional column in, and fill it with the 3rd piece of info (after 2nd underscore) or NAs. Use fill...

dat %>% separate(key,c("year", "first_variable_name","second_variable name"), fill="right")

#This is an even better way of dealing with it... tells it that anything after the first underscore 
#should just be merged into the variable name (2nd var in the list of year, variable_name)
dat %>% separate(key, c("year","variable_name"), sep="_", extra="merge")

#Now push the fertility and life expectancy into separate variables using spread function
dat %>% separate(key, c("year","variable_name"), sep="_", extra="merge") %>% spread(variable_name,value)

#Inverse of separate is unite() function. Unite function combines two columns and adds a separating character,

#Could have done this (although less efficient)

dat %>% separate(key,c("year", "first_variable_name","second_variable_name"), fill="right") %>%
  unite(variable_name,first_variable_name,second_variable_name, sep="_") %>%
  spread(variable_name,value) %>%
  rename(fertility=fertility_NA)


#COMBINING TABLES
#Can left_join(), right_join() etc. Based on SQL joins.

#left_join() only keeps rows that have information in the first table.
#right_join() only keeps rows that have information in the second table.
#inner_join() only keeps rows that have information in both tables.
#full_join() keeps all rows from both tables.
#semi_join() keeps the part of first table for which we have information in the second.
#anti_join() keeps the elements of the first table for which there is no information in the second.

library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

#Left join election data onto the murders data by state...
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)


# make two smaller tables to demonstrate joins. Slice to pull only certain rows back. Select to select certain columns.
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)  #NAs if it can't find a state in tab two...
tab1 %>% left_join(tab2)  #Same left join, but can receive first argument through the pipe
tab1 %>% right_join(tab2) 
inner_join(tab1, tab2)  #Only keeping where there is data in both tables
full_join(tab1,tab2)  #Keeping all the rows from left and right table whether matches or not.

#These aren't actually joins. Let you keep parts of one table depending on what's in the other
#Known as "filtering joins"...
semi_join(tab1, tab2)  #so this just filters down tab one to the rows with states in tab2

#Ant_join is the opposite of semi_join. Keeps elements of first table that aren't in the second table.
anti_join(tab1, tab2)


#BINDING

#Another way of joining tables, regardless of the row order. 
#Don't try to match a variable ... just try to combine datasets.

#dplyr function bind_cols() : binds columns and creates a tibble

bind_cols(a=1:3,b=4:6)

#Also an R base function, cbind(), that does the same thing, but creates objects other than tibbles (matrices, data frames etc)

#bind_cols can also bind data frames...  below splits data then binds together.

tab1 <- tab[,1:3] #pull first 3 columns from tab data frame
tab2 <- tab[,4:6]
tab3 <- tab[,7:9]

new_tab <- bind_cols(tab1,tab2,tab3)
head(new_tab)

#bind_rows similar but for rows...

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]

tab1
tab2

bind_rows(tab1,tab2)

#rbind R base function does something similar


#SET OPERATORS - union, intersect etc. If tidyverse and dplyr loaded, these can be used on data frames as well as vectors

#On vectors
intersect(c("a","b","c"),c("b","c","d"))   #delivers "b" "c"

#On data frames this will take the intersection of rows for tables having the same column names
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
tab1
tab2
intersect(tab1,tab2) #Just pulls back the rows that are in both tables (i.e. rows 3,4,5) of original tab

#Similar for union
#On vectors
union(c("a","b","c"),c("b","c","d"))   #delivers "a" b" "c" "d"

tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
tab1
tab2
union(tab1,tab2) #Pulls back rows from both tables (removing duplicates)

#Function setdiff()
#Unlike union/intersect it's not symmetric ... i.e. the below give different answers. 
setdiff(1:10,6:15) #Returns 1 to 5. The items in the first set of numbers that aren't in the second set.
setdiff(6:15,1:10) #Returns 11 to 15.

#With dplyr, again can used for data frames
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1,tab2) #Returns rows from tab1 that are not in tab2


#Function setequal(). This tests if two sets are the same regardless of order...
setequal(1:5,1:6) #Returns false
setequal(1:5,5:1) #Returns True

#Using on data frames with dplyr loaded
setequal(tab1,tab2)



#WEB SCRAPING / WEB HARVESTING

#Basically tables/data will be saved in the html code for a given webpage. Can download html and pull data from it

#Package to do this is part of tidyverse. Called: rvest

#Step 1 is to import the web page into R

# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

class(h)  #Says class is xml, not html. xml is general markup language ... so broader category than html.
h

#Need to look for table tag.  < and > in html are known as 'nodes'
#html_nodes() function extracts all nodes of a given type
#html_node() extracts the first node of a given type

tab <- h %>% html_nodes("table")
tab #3 table nodes listed
tab <- tab[[2]] #pulls 2nd table, the one we want
tab  #shows just the html code for the table

#Now need to put into data frame. Html_table() converts an html table in code into an R data frame
tab <- tab %>% html_table
class(tab)
tab

#Change variable names to make them simpler
tab <- tab %>% setNames(c("state","population","total","murders","gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(tab)


#NB: most websites today don't use basic html. Use CSS to make them more attractive to view/use.
#Installed selector gadget chrome extension. See magnifying glass to rhs of search bar
#Is helpful for scraping data other than from tables

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".recipe-text") %>% html_text()
ingredients <- h %>% html_nodes(".ingredient") %>% html_text()

guacamole <- list(recipe, ingredients)
guacamole


#Can then create a function to pull similar info from other food network pages which will use the same selectors
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".recipe-text") %>% html_text()
  ingredients <- h %>% html_nodes(".ingredient") %>% html_text()
  return(list(recipe = recipe, ingredients = ingredients))
} 

get_recipe("https://foodnetwork.co.uk/recipes/pancakes-4926/")
get_recipe("https://foodnetwork.co.uk/recipes/huckleberry-pancakes/")


#Other powerful tools in rvest to look into: html_form(), set_values(), submit_form(). Permit querying a webpage from R.


#SECTION 3 : STRING PROCESSING OVERVIEW

url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
murders_raw <- read_html(url) %>%    #Get all html from the url
  html_nodes("table") %>%  #Get the nodes for the tables in the html
  html_table()

murders_raw    #This just spits out all the tables

murders_raw[2]
murders_raw[[2]]   #Weird. Does look like single or double brackets generates the same, and pulls the 2nd table...

murders_raw <- murders_raw[[2]] %>%   #pull the 2nd table in the html. NBL 
  setNames(c("state","population","total","murders","gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
#change the column names in the table
head(murders_raw)    

class(murders_raw$population)
class(murders_raw$total)
#Above pulled as character vars, when we want numeric. Often happens where websites use commas in numbers etc.
#Quite common, so can use parse_number() function to pull the numbers out


#Dealing with Single and double quotes in strings
s <- '10"'
cat(s)  #Shows string. NB: just hitting s doesn't actually give the string. 
?cat  #cat - concatenate and print

s <- "10'"
cat(s)

#If wanting to have 5'10" in a string, then using single or double quotes won't work.
#For this we need to escape the quotes using backslash \

s <- '5\'10"'   #So, single quotes for character string. Gets to \ and escapes the singl quotes. Then starts on the 
#10 inches piece, which is definied in single quotes
cat(s)

s <- "5'10\""
cat(s)


#stringr package (part of tidyverse)

murders_raw$population[1:3]  #Characters due to commas in the numbers
#Coercion to numbers doesn't work for this.
as.numeric(murders_raw$population[1:3])  #Just produces NAs

#First thing we need to do is locate and remove the commas from the string
#Base R does have functions to do this. 
#Can also use stringr package which uses a more consistent approach vs base R. All start with str_ and include the string as first arg


#Going back to the murders example. Code to see which columns have commas
commas <- function(x) any(str_detect(x, ","))  #function to identify commas in a string, and return true/false
murders_raw %>% summarize_all(funs(commas))  #apply commas function to all cols

test_1 <- str_replace_all(murders_raw$population,",", "")  #Code to delete commas in a string
test_1 <- as.numeric(test_1)

#Can then use mutate_all to apply this operation to each column (won't impact those without commas)
#NB: Says this, but probably best to use mutate_at in general..

#Or instead of the above can use the function parse_number() in readr

test_2 <- parse_number(murders_raw$population)
identical(test_1,test_2)

#Instead of mutate_all can use mutate_at and define which columns to apply the function to
murders_new <- murders_raw %>% mutate_at(2:3,parse_number)   #So this applies parse_number to cols 2 and 3
murders_new %>% head()


#Heights examples
library(dslabs)
data(reported_heights)

class(reported_heights$height)   #People entering non numeric info on the webform, so stored as character, not numeric

x <- as.numeric(reported_heights$height)  #Generates lots of NAs
head(x)

sum(is.na(x))

#To have a look at the entries that triggered NAs...

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10)

#Shows some students reported height in feet and inches rather than inches, some used cm etc


#Code to try and deal with those. 
#First just getting to the cases with issues, and getting rid of extreme values etc

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

not_inches <- function(x, smallest=50, tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches<smallest | inches > tallest
  ind
}

problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)  #Shows there are 292 entries with issues. Either extremes, or NAs when converted to numeric

#3 common types of pattern appear...

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


#Regular expressions (regex) of characters in a string. Can be from one character (e.g. a comma) to many characters..
pattern <- ","
str_detect(murders_raw$total,pattern)

#... or cm
str_subset(reported_heights$height, "cm")

#Now trying to ask which of the strings include the pattern cm or the pattern inches
yes <- c("180 cm", "70 inches")  
no <- c("180", "70''")
s <- c(yes,no)
s

#Could call str_detect twice to find out...
str_detect(s,"cm") | str_detect(s,"inches")

#But don't need to do this. Regex language allow us to use special characters...
# | character used to mean 'or'

#So this does the same job as calling str_detect twice
str_detect(s,"cm|inches")

#\\d means any digit (0,1,2..9) ... (\d is actually any digit, but the first backslash escapes the backslash....)
#So this bit of code pulls out any elements of the string with numbers in
#To test the regex code, create test strings that match expected patterns and those that don't to check the right info
#is being identified by str_detect

yes <- c("5","6","5'10","5 feet", "4'11")
no <- c("",".","Five","six")
s <- c(yes,no)
pattern <- "\\d"
str_detect(s,pattern)   #Gives 5 trues followed by 4 falses

#str_view : useful function for troubleshooting. Shows first match for each element in a string
install.packages("htmlwidgets")
library(htmlwidgets)
str_view(s,pattern)

#str_view_all shows all matches in each element in a string
str_view_all(s,pattern)


#Character classes (defined with square brackets [] )
#e.g. to only pull the 5s and 6s, use [56]
str_view(s,"[56]")

#Or can use ranges to deine character class. Using [0-9] is equivalent to using \\d
str_view(s,"[0-9]")
str_view(s, "\\d")

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes,no)
str_detect(s,"[4-7]")
           
#Note, in regex, everything is a character - no numbers. So [1-30] is 1,2,3 (from the 1-3 part) and character 0.
#Although are ordered as 0,1,2,3
#[a-z] = all the lower case letters, abc...z
#[A-Z] = all the upper case letters, ABC ... Z
#[a-zA-Z] = all lower case and upper case letters


#Anchors ... define patterns that must start or end at specific places

#Two most common anchors are ^ and $  (carat and dollar). Represent the beginning and end of a string respectively.

#So ^\\d$  means start of the string, followed by one digit, followed by end of the string... e.g...

pattern <- "^\\d$"
yes <- c("1","5","9")
no <- c("12","123", " 1", "a4", "b")
s <- c(yes,no)

str_detect(s,pattern)
str_view(s,pattern)


#Quantifiers
#Inches part can have 1 or 2 digits. Can be specified in regex with quantifiers.

# \\d{1,2} ... this shows that the pattern (in this case \\d meaning any digit) can be repeated 1 or 2 times.
# So 1 will return true, 12 will return true, 123 will return false...

pattern <- "^\\d{1,2}$"
yes <- c("1","5","9","12")
no <- c("123", "a4", "b")
s <- c(yes,no)

str_detect(s,pattern)
str_view(s,pattern)

#So to look for feet and inches together...

pattern <- "^[4-7]'\\d{1,2}\"$"    
#So looking for numbers four to seven, followed by feet sign, then 1 or 2 digit nums followed by inches
#Note, have to do \" in regex to look for the inches quotes (otherwise will think it's the end of the string)

yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

#Back to the data...
pattern <- "^[4-7]'\\d{1,2}\"$" 
sum(str_detect(problems,pattern))    #So only 14 dealt with by the pattern defined


#Why only 14...
problems[c(2,10,11,12,15)] %>% str_view(pattern) #So shows some people wrote out feet and inches etc

#Can see these cases
str_subset(problems,"inches")

str_subset(problems,"''")  #Some used double single quotes instead of double quotes


#So trying to just use 5'y and not look for the inches symbol
pattern <- "^[4-7]'\\d{1,2}$" 

#Also replace any written feet, ft etc with the feet symbol '

problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>% 
  sum
#Increased number of matches to 48...

#Another issue we have is spaces in the elements of the character vector. R def doesn't ignore spaces...
identical("Hi","Hi ")

#In regex, spaces represented by \\s
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern_2)

#Can use quantifiers again, to permit spaces, but not to require them...
#In regex the * character means zero or more instances of the previous character
#Example below allows zero or more ones in the middle of the string

yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

#? character in regex means none or once.
#+ character in regex means one or more


# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))


#Back to the example
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"   #So the \\s* elements in here allow zero or more spaces in various places in the string
problems %>%
  str_replace("feet|ft|foot", "'") %>%      # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>%    # remove all inches symbols
  str_detect(pattern) %>% 
  sum   #Now get 53 that are sorted

#Can't just remove all spaces, because some entries of the form x y with a space separating feet and inches
#So removing all spaces would turn 6 1 into 61 ... whereas should be a 73 in inches...


#Groups with Regex
#Groups in regex defined using parentheses 


#Also others like 70.5 as well as 5.6 ... so can't just turn all x.y into x'y....


pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$"   #matching the parts we want to extract

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

str_detect(s,pattern_without_groups)
str_detect(s,pattern_with_groups) #This is the same as the pattern without groups .. detection/matching is the same

#But with groups can use str_match to extract the values the groups define

str_match(s, pattern_with_groups)
#Returns the first column with the full pattern matched, but the 2nd and 3rd cols reflect the two groups in the pattern that 
#were defined

str_extract(s, pattern_with_groups)   #... just returns the full pattern matched per col 1. Not values defined by groups.

#\\i   ... When finding replacing, can refer to the ith group using regex. 
#So  \\1 ... is the value extracted from the first group 


pattern_with_groups <- "^([4-7]),(\\d*)$"   #matching the parts we want to extract

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

str_replace(s,pattern_with_groups,"\\1'\\2")   #So pulls group 1, then puts in a ', then pulls group 2.
#Outputs [1] "5'9"   "5'11"  "6'"    "6'1"   "5'9"   ","     "2,8"   "6.1.1"

#Making this a bit more generic
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"     #[,\\.\\s+]   means comma or dot or at least one space

str_subset(problems, pattern_with_groups) %>% head   #So pulling out the records from problems that matches the pattern

#Replace with the standard feet and inches groups with ft symbol between...
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head 


#Final Tweaks

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"   #This is the pattern we're after...
index <- str_detect(converted, pattern)
mean(index)  #So over half of the problems now in format we're after...

converted[!index]    # show problems


#Separate with regex

s <- c("5'10","6'1")
tab <- data.frame(x=s)
tab

tab %>% separate(x,c("feet","inches"),sep="'")

#extract function from tidyr package lets us use regex groups to extract the desired values
#equivalent code to the separate code above

tab %>% extract(x,c("feet","inches"),regex="(\\d)'(\\d{1,2})" )
#groups in regex gives more flexibility than separate

#e.g.separate doesn't work on this ...

s <- c("5'10","6'1\"","5'8inches")
tab <- data.frame(x=s)
tab

tab %>% separate(x,c("feet","inches"),sep="'",fill="right")   #keeps the inches in words and the " signs.... can do better with regex...

tab %>% extract(x,c("feet","inches"),regex="(\\d)'(\\d{1,2})" )


#Sorting out instances where students just entered 6 for 6 ft or 5 for 5 ft
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")    #So looks for single digit from 4-7 at which point the string ends ($).
                                        #Then takes the result from group 1 in the regex (\\1) followed by '0


#Note, special characters need to be escaped. so . requires \\. in regex etc


#str_trim to remove leading and trailing blanks
str_trim("5 ' 9 ")

#to convert strings to lower case...
s <- c("Five feet eight inches")
str_to_lower(s)




convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#Putting it all together...

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)


new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)


#String splitting
#e.g.  if read csv function didn't exist...

filename <- system.file("extdata/murders.csv", package="dslabs")
lines <- readLines(filename)

lines %>% head()

#Above creates vector of strings... can use str_split() to extract values separated by commas (or any other delimiter)
#By default str_split creates a list. Use simplify=TRUE argument to return a matrix

x <- str_split(lines,",")  
x %>% head()

col_names <- x[[1]] #get top row of col names
x <- x[-1]  #update data to start from row 2?
col_names
x %>% head()

#To create data frame can use map function, which applies the same function to each element in a list
# map function is in the purrr package
# map function always returns a list

library(purrr)
map(x, function(y) y[1]) %>% head()

#If 2nd arg is just an integer, it assumes we want that entry, so this works too...
map(x,1) %>% head()

#To force map to return a character vector instead of a list can use map_chr()
#map_int() to return integers

#So to create data frame can use the following:

dat <- data.frame(map_chr(x,1),
                  map_chr(x,2),
                  map_chr(x,3),
                  map_chr(x,4),
                  map_chr(x,5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>% head()

#Using other purr package functions, is possible to do the above more efficiently...
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as.data.frame()
dat %>% head()

#Also, could have used an argument in str_split ... simplify=TRUE. This would return a matrix instead of a list...
x <- str_split(lines,",",simplify=TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

?parse_guess


#Case Study - extracting a table from a pdf
library(dslabs)
data("research_funding_rates")
research_funding_rates 

#Download the data
install.packages("pdftools")
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]   #Keeping page 2

#NB: above import didn't work - threw loads of errors in the pdf_text step. But raw data is in dslabs package:
library(dslabs)
data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head

tab <- str_split(raw_data_research_funding_rates, "\n")  #Can see each line in the string separated by \n.
                                                         #So use str_split to create elements in a list 

tab <- tab[[1]]
tab %>% head()

the_names_1 <- tab[3]     #Pull column names from 3rd and 4th entries
the_names_2 <- tab[4]

the_names_1
the_names_2


#Clean up the column names...
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

#Join to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names


#Pull the data itself...
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()



#RECODING THE NAMES OF CATEGORICAL VARIABLE
#Can use case_when() function for this, but tidyverse offers recode() function to do the same

library(dslabs)
data("gapminder")

gapminder %>%
  filter(region=="Caribbean") %>%
  ggplot(aes(year,life_expectancy,color=country)) +
  geom_line()

#Long country names... here's how to use recode to sort them out...
gapminder %>%
  filter(region=="Caribbean") %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda' = "Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year,life_expectancy,color=country)) +
  geom_line()


#Other similar functions include recode_factor() and fct_recoder() in the forcats package in the tidyverse. 
#The same result could be obtained using the case_when() function, but recode() is more efficient to write.



#DATES AND TIMES

#Computer languages typically use January 1 1970 as the reference date for all other dates. Jan 1 1970 known as the 'epoch'
#Often stored as number since epoch, but formatted as calendar dates

library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)  #Returns Date, not character

as.numeric(polls_us_election_2016$startdate) %>% head

#R can deal with treating dates as numeric, but putting the actual date format in the labels

polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate,rawpoll_trump)) +
  geom_line()

#lubridate package in tidyverse helps to deal with dates
library(lubridate)

