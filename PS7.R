#Problem Set 7! 

library(dplyr)
library(ggplot2)
setwd("C:/Users/isdav/Documents/GitHub/PS7")
rm(list = ls())
data<-read.csv("March2018.csv")
View(data)
strsplit(data$DateOccur[3])

table(data$DateOccur)
#1
table<-count(data, Description)
class(data$DateOccur)
data <- data %>% mutate_if(sapply(data, is.factor), as.character)
data$DateOccur<-strsplit(data$DateOccur, split=' ', fixed=TRUE)[1]
data<-filter(data, startsWith(data$DateOccur, "03"))
data<-filter(data, endsWith(data$DateOccur, "2018"))

