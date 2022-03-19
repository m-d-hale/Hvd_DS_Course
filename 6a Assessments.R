


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

read_lines(url, n_max = 3)

tstdat <- read_csv(url, col_names = FALSE)

length(tstdat) # number of columns
nrow(tstdat) # number of rows
str(tstdat)


?read_csv


dat <- read_csv(url)
head(dat)


#Tidy up times data
raw_dat <- read_csv("Times_Data.csv")
head(raw_dat)

#My answer..
raw_dat %>% gather(key,value,-age_group) %>% 
  separate(key,c("year", "variable_name"),"_") %>%
  spread(variable_name,value)

#Their answer...
raw_dat %>% gather(key = "key", value = "value", -age_group) %>%
  separate(col = key, into = c("year", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)



#Look at Co2 data
co2


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide,month,co2, -year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()



library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

dat %>% spread(gender,admitted)


tmp <- gather(admissions, key, value, admitted:applicants)
tmp

#My answer
tmp2 <- tmp %>% unite(column_name,key,gender, sep="_")

#Their version of the same
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

tmp2 %>% spread(column_name,value)


install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
head(top)

Master %>% as_tibble()

head(Master)

top_names <- top %>% left_join(Master,by="playerID") %>%
  select(playerID, nameFirst, nameLast, HR)

top_names



head(Salaries)
str(Salaries)

op_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names,by="playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary)

op_salary


str(AwardsPlayers)
head(AwardsPlayers)

Awards_2016 <- AwardsPlayers %>% filter(yearID==2016)
head(Awards_2016)
str(Awards_2016)

intersect

top_names %>% left_join(Awards_2016)
top_names

#Names in top_names that are also in the awards list
semi_join(top_names,Awards_2016)

#In awards list 2016, but not in top 10 HR hitters
anti_join(Awards_2016,top_names) %>% group_by(playerID) %>% summarize()


#Scraping
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])


tab1 <- html_table(nodes[[1]])
tab2 <- html_table(nodes[[2]])
tab3 <- html_table(nodes[[3]])
tab4 <- html_table(nodes[[4]])

head(tab1)
head(tab2)
head(tab3)
head(tab4)


tab19 <- html_table(nodes[[19]])
tab20 <- html_table(nodes[[20]])
tab21 <- html_table(nodes[[21]])

head(tab19)
head(tab20)
head(tab21)


tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])

#Clear up
tab_1_adj <- tab_1[2:31,2:4] %>% set_names(c("Team","Payroll","Average"))
tab_2_adj <- tab_2[2:31,] %>% set_names(c("Team","Payroll","Average"))

tab_join <- full_join(tab_1_adj,tab_2_adj,by="Team")


#Brexit Questions

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
tab <- html_nodes(h, "table")

html_table(tab[[1]],fill=TRUE)
html_table(tab[[2]],fill=TRUE)
html_table(tab[[3]],fill=TRUE)
html_table(tab[[4]],fill=TRUE)
html_table(tab[[5]],fill=TRUE)



#1
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

not_inches(c(150,70))


animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)


schools <- c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California","California State University")

schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")




problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")



yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

index <- str_detect(converted, pattern)

converted[!index] 


converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)


#Strings assessment

schedule <- data.frame(c("Monday","Tuesday"),c("Mandy, Chris and Laura","Steve, Ruth and Frank")) %>%
  setNames(c("day","staff"))

str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")


#3
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()


#Pt2

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

polls <- polls %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>%
  mutate(newremain = str_match(remain,"%")) %>% filter(!is.na(newremain)) %>% select(-newremain)
  
#polls_tmp <- polls[1:10,]
#polls_tmp2 <- polls_tmp %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>%
#  mutate(newremain = str_match(remain,"%")) %>% filter(!is.na(newremain)) %>% select(-newremain)
  

as.numeric(str_replace(polls$remain,"%",""))/100

str_replace(polls$undecided, "N/A", "0%")



#8
temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date


#SECTION 4

library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits


data(brexit_polls)
str(brexit_polls)

#Number of polls starting in April
brexit_polls %>% mutate(month = month(startdate)) %>% group_by(month) %>% summarize(n=n())

#Number of polls ending in week of 2016-06-12
summary_brex <- brexit_polls %>% mutate(endweek = round_date(enddate,'week')) %>% group_by(endweek) %>% summarize(n=n())

?round_date

#Most popular weekday
brexit_polls %>% mutate(weekday = weekdays(enddate)) %>% group_by(weekday) %>% summarize(n=n())



data(movielens)
str(movielens)

movielens %>% mutate(reviewdate = as_datetime(timestamp)) %>% mutate(reviewyr = year(reviewdate)) %>%
  group_by(reviewyr) %>% summarize(n=n()) %>% arrange(desc(n))

movielens %>% mutate(reviewdate = as_datetime(timestamp)) %>% mutate(reviewhr = hour(reviewdate)) %>%
  group_by(reviewhr) %>% summarize(n=n()) %>% arrange(desc(n))


?as_datetime
?year


#Part 2

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

pattern <- "Pride and Prejudice"
str_detect(gutenberg_metadata$title,pattern)

IDs <- gutenberg_metadata %>% mutate(Match = str_detect(title,pattern)) %>% filter(Match == TRUE) 

TmpIDs <- gutenberg_works() %>% mutate(Match = str_detect(title,pattern)) %>% filter(Match == TRUE) 

?gutenberg_works


words <- gutenberg_download(1342) %>% unnest_tokens(word,text)

#strip out standard words
words_small <- words %>% filter(!word %in% stop_words$word )

#strip out any containing a digit
pattern <- "\\d"
words_small2 <- words_small %>% mutate(Digit = str_detect(word, pattern)) %>% filter(Digit==FALSE)


words_summ <- words_small2 %>% group_by(word) %>% summarize(n=n()) %>% filter(n >= 100)

words_small2 %>% group_by(word) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head

#Q12
afinn <- get_sentiments("afinn")

words_small3 <- words_small2 %>% inner_join(afinn,by="word") %>% select(c("gutenberg_id","word","value"))

#positive sentiments
words_small3 %>% mutate(pos = ifelse(value>0 , 1 , 0 )) %>% summarize(pct = sum(pos)/n())
  

words_small3 %>% filter(value == 4) %>% summarize(n=n())



#Comprehensive Assessment - Puerto Rico Hurricane

library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
head(txt)

#3.
str(txt)

Pg_9 <- txt[9] 
x <- str_split(Pg_9,"\n")
head(x)

#Turn list of 1 into character string by selecting first entry of x
s <- x[[1]]
s

#Get rid of leading/trailing blanks
s <- str_trim(s)

#locate the index of the header row
header_index <- min(str_which(s,"2015"))

header <- s[header_index]
pattern <- "\\s+"  #looking for one or more spaces as the delimiter
split_head <- str_split(header,pattern,simplify=TRUE) #simplify translates from list to character vector


month <- split_head[,1]
month

header <-split_head[,2:5]
header

head_alt <- paste("Yr", header, sep = "_")
head_alt

tail_index <- str_which(s,"Total")
tail_index

pattern <- "\\d+"
n <- str_count(s,pattern)
n == 1

sum(n==1)

#Remove everything before head index, and everything after tail index

s %>% filter()

s1 <- s[(header_index + 1):(tail_index -1)] 
pattern <- "\\d+"
n1 <- str_count(s1,pattern)
s2 <- s1[!n1 == 1]

#Remove anything that isn't a digit or space. ^ inside square brackets means not like...
s3 <- str_remove_all(s2,"[^\\d\\s]")

#Turn into data frame and add col names
daynme <- "day"
ColNames <- c(daynme, head_alt)

s <- str_split_fixed(s3, "\\s+", n = 6)[,1:5]

s1 <- s %>% as.data.frame() %>% setNames(ColNames) %>% sapply(as.numeric)

newcol <- rep("SEP", nrow(s1))
s1 <- cbind(newcol,s1)

tab <- as.data.frame(s1) 

is.data.frame(tab)

tab %>% mutate(Yr_2015_num = as.numeric(Yr_2015)) %>% summarize(mean_2015 = mean(Yr_2015_num))
tab %>% mutate(Yr_2016_num = as.numeric(Yr_2016)) %>% summarize(mean_2016 = mean(Yr_2016_num))

tab2 <- tab %>% mutate(Yr_2017_num = as.numeric(Yr_2017), day_num = as.numeric(day)) %>% 
  mutate(Grp1 = ifelse(day_num>=1 & day_num <=19,1,0) )

tab2 %>% group_by(Grp1) %>% summarize(mean = mean(Yr_2017_num))


tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

library(ggplot2)
tab %>% filter(year != "newcol") %>% ggplot(aes(x=as.numeric(day),y=deaths,color=year)) +
  geom_line()






