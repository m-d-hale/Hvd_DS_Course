


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

