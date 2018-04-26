#Problem Set 7! 

#1: 
library(dplyr)
library(ggplot2)
setwd("C:/Users/isdav/Documents/GitHub/PS7")
rm(list = ls())
data<-read.csv("March2018.csv")
View(data)

#2:
#convert to a tibble so everything works
data<-as_tibble(data)

#create a new date variable that has a better date format and disregards time
data<-mutate(data, date=as.Date(DateOccur, "%m/%d/%Y"))
data<-mutate(data, Description=as.character(Description))
out <- data %>% 
  group_by(date) %>% 
  summarise(n = n())
dim(out) #165 rows implies 165 distinct days in this dataset. 

#subsetting data to only include march 2018
data<-mutate(data, dateSort=as.character(date))
out <- data %>% 
  group_by(dateSort) %>% 
  summarise(n = n())
data<-dplyr::filter(data, substr(dateSort, 1,7)=="2018-03")
out <- data %>% 
  group_by(date) %>% 
  summarise(n = n())
dim(out) #Now there are 31! 

#Leaving the scene of an accident is the most common crime, with 454 ocurrances and 14.6 per day. 
out <- data %>% group_by(Description) %>% summarise(n = n())
arrange(out, desc(n))
out<-mutate(out, typePerDay=n/31)
arrange(out, desc(typePerDay))

#This is the number of crimes by type per specific day, but there are so many times that this doesnt help us much. 
data<-mutate(data, dateSort=as.character(date))
out <- data %>% 
  group_by(date, Description) %>% 
  summarise(n = n())

#3:
#Neighborhood 35 is the most dangerous, with 294 crimes committed and 9.48 per day.  
out <- data %>% 
  group_by(Neighborhood) %>% 
  summarise(n = n())
arrange(out, desc(n))
out<-mutate(out, neighborhoodPerDay=n/31)
arrange(out, desc(neighborhoodPerDay))

#This is the number of crimes by neighborhood per specific day 
out <- data %>% 
  group_by(date, Neighborhood) %>% 
  summarise(n = n()) 
out

#4: 
data<-mutate(data, rob=grepl("ROBBERY", Description))
View(data)

out <- data %>% 
  group_by(District, rob) %>% 
  summarise(n = n())
out
#District 1: 
out[3,3]/(out[3,3]+out[2,3])
#District 2: 
out[5,3]/(out[5,3]+out[4,3])
#District 3: 
out[7,3]/(out[7,3]+out[6,3])
#District 4: 
out[9,3]/(out[9,3]+out[8,3])
#District 5: 
out[11,3]/(out[11,3]+out[10,3])
#District 6: 
out[13,3]/(out[13,3]+out[12,3])

#5
data<-read.csv("March2018.csv")
#create a new date variable that has a better date format and disregards time
data<-mutate(data, date=as.Date(DateOccur, "%m/%d/%Y"))
data<-mutate(data, Description=as.character(Description))
out <- data %>% 
  group_by(date) %>% 
  summarise(n = n())
out #165 rows implies 165 distinct days in this dataset. 

#We want to keep all dates this time, not just march 2018
data<-arrange(data, date)
library(ggplot2)
ggplot(data=out)+geom_line(mapping = aes(x = date, y=n))
#Never mind! We should subset, because the observations are concentrated very heavily in march 2018

#subsetting data to only include march 2018
data<-mutate(data, dateSort=as.character(date))
out <- data %>% 
  group_by(dateSort, District) %>% 
  summarise(n = n())
data<-dplyr::filter(data, substr(dateSort, 1,7)=="2018-03")
out <- data %>% 
  group_by(date) %>% 
  summarise(n = n())
dim(out) #Now there are 31! 
ggplot(data=out)+geom_line(mapping = aes(x = date, y=n))+labs(x="Date")+
  labs(y="Number of Crimes")+labs(title="Crime in March 2018")

#6 
#creating a separate column for every district 
d1<-filter(data, District==1)
out1 <- d1 %>% 
  group_by(date) %>% 
  summarise("District 1" = n())
d2<-filter(data, District==2)
out2 <- d2 %>% 
  group_by(date) %>% 
  summarise("District 2" = n())
d3<-filter(data, District==3)
out3 <- d3 %>% 
  group_by(date) %>% 
  summarise("District 3" = n())
d4<-filter(data, District==4)
out4 <- d4 %>% 
  group_by(date) %>% 
  summarise("District 4" = n())
d5<-filter(data, District==5)
out5 <- d5 %>% 
  group_by(date) %>% 
  summarise("District 5" = n())
d6<-filter(data, District==6)
out6 <- d6 %>% 
  group_by(date) %>% 
  summarise("District 6" = n())
#binding them all together 
outTotal<-bind_cols(out1, out2, out3, out4, out5, out6)
View(outTotal)
#Melt all varaibles into one to allow for a color-coded legend
dd_sub = outTotal[,c(1,2,4,6,8,10, 12)]
library(reshape2)
melter=melt(dd_sub, id=c("date"))
#Now, finally, we can plot with little code:
ggplot(melter) +
  geom_line(aes(x=date, y=value, color=variable)) + 
  scale_color_manual(values=c("red", "blue", "yellow", "green", "purple", "black"))+
  labs(x="Date", y="Number of Crimes", title="Crimes in March 2018 by District")
