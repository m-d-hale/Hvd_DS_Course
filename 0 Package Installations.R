

#PACKAGE INSTALLATIONS
#ONLY NEED TO RUN WHEN UPDATING VERSION OF R (NOT RSTUDIO)

install.packages(c("ggthemes","dslabs","tidyverse","ggplot2","gridExtra","titanic","gtools","ggrepel","ggthemes",
                   "Lahman","htmlwidgets", "pdftools","tidytext","textdata","gutenbergr","HistData",
                   "reshape2","lpSolve","caret","e1071","randomForest","gam"))

#install.packages("gam")
#see installed packages
installed.packages()


#Update R version
install.packages("installr")
library(installr)
updateR()

#Load installed libraries
library(ggthemes)
library(tidyverse)
library(dplyr)
library(dslabs)
library(ggplot2)
library(gridExtra)
library(titanic)
library(gtools)
library(ggrepel)
library(ggthemes)
library(Lahman)
library(htmlwidgets)
library(pdftools)
library(tidytext)
library(textdata)
library(gutenbergr)
library(HistData)
library(reshape2)
library(lpSolve)
library(caret)
library(rpart)
library(randomForest)
library(gam)
