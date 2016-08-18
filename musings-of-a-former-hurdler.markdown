---
title: Musings of a former hurdler
date: 2016-08-18 06:36:00 Z
---



# Load packages for downloading and preparing athlete data in R

library(XML)
library(RCurl)
library(dplyr)
library(stringr)
library(ggplot2)

# Read in csv file of web scraped data 

a = read.csv("~/Google Drive/Data Science/Olympics Data/Track110_2016.csv")

summary(a)

# Parse the data to get it into a workable format 

a$NAT <- substr(a$NAT,1,3)
a$NAT <- as.factor(a$NAT)

# Let's get the respective age of every potential competitor

#as.Date(medal$DOB, format="%d-%M-%y")

t <- substr(a$DOB, 1,2)
t <- str_replace_all(t, "-" , "")
t
t2 <- substr(a$DOB, 3,6)
t2 <- str_replace_all(t2, "-", "")
t2
t3 <- substr(a$DOB, 7,9)
t3 <- str_replace_all(t3,"-", "")
t3

fm  <- function(x) {
  a=x
  a[a=="Jan"] <- "01"
  a[a=="Feb"] <- "02"
  a[a=="Mar"] <- "03"
  a[a=="Apr"] <- "04"
  a[a=="May"] <- "05"
  a[a=="Jun"] <- "06"
  a[a=="Jul"] <- "07"
  a[a=="Aug"] <- "08"
  a[a=="Sep"] <- "09"
  a[a=="Oct"] <- "10"
  a[a=="Nov"] <- "11"
  a[a=="Dec"] <- "12"
  return(a)
}
fm(t2)
fy <- function(x){
  a=x
  if(a <20) a1 <- paste("20",a,sep="")
  if(a > 20) a1 <- paste("19",a,sep="")
  return(a1)
}
fy(t3)
fd <- function(x) {
  a = x
  a[(a=="1")] <- "01"
  a[(a=="2")] <- "02"
  a[(a=="3")] <- "03"
  a[(a=="4")] <- "04"
  a[(a=="5")] <- "05"
  a[(a=="6")] <- "06"
  a[(a=="7")] <- "07"
  a[(a=="8")] <- "08"
  a[(a=="9")] <- "09"
  return(a)
}

fd(t)

tty <- fy(t3)
ttm <- fm(t2)
ttd <- fd(t)

tt <- paste(tty, ttm, ttd, sep="-")

a$DOB <- as.Date(tt, format="%Y-%m-%d")
a$AGE <- as.numeric((Sys.Date()-a$DOB)/365)

# Calculate how many days it's been since each race so we can see if they are trending up or down over time

ta <- substr(a$DATE, 1,2)
ta <- str_replace_all(ta, "-" , "")
ta
tb <- substr(a$DATE, 3,6)
tb <- str_replace_all(tb, "-", "")
tb
tc <- substr(a$DATE, 7,9)
tc <- str_replace_all(tc,"-", "")
tc

tcy <- fy(tc)
tbm <- fm(tb)
tad <- fd(ta)

ttt <- paste(tcy, tbm, tad, sep="-")
a$DATE <- as.Date(ttt, format="%Y-%m-%d")
a$LastRace <- as.numeric((as.Date("2016-08-17",format="%Y-%m-%d")-a$DATE))


as.numeric((as.Date("2016-08-17",format="%Y-%m-%d")-(as.Date("2016-01-01",format="%Y-%m-%d"))))



head(a,20)

summary(a)

# Calculate the average time for each individual runner in this year's data

a1 <- a %>%
  group_by(COMPETITOR, NAT) %>%
  summarise(avgTime=mean(MARK),
            Age=mean(AGE),
            TopTimesRecorded=n(),
            DaysSinceLastRace=min(LastRace)) %>%
  arrange(avgTime)

head(a1,10)

USA = a1[a1$NAT=="USA",]

team1 <- a1 %>%
  group_by(NAT) %>%
  summarise(avgTime=mean(avgTime),
            AvgAge=mean(Age, na.rm=T),
            TopTimesRecorded=n(),
            AvgMinDaysLastRace=mean(DaysSinceLastRace)) %>%
  arrange(desc(TopTimesRecorded))

summary(team1)

# Filter our those who have not posted a top race time for the last 90 days

aa <- a[a$LastRace<228,]

b <- aa %>%
  group_by(COMPETITOR) %>%
  summarise(avgTime=mean(MARK),
            Age=mean(AGE),
            TopTimes=n()) %>%
  arrange(avgTime)

c <- a %>%
  group_by(COMPETITOR, NAT) %>%
  summarise(unique=n())

c <- c[-3]

d <- merge(b,c)

d <- arrange(d, avgTime)

# So let's consider what athletes are not allowed to compete in Rio 

"Hansle PARCHMENT"
"David OLIVER"

d <- d[d$COMPETITOR!="Hansle PARCHMENT",]
d <- d[d$COMPETITOR!="David OLIVER",]

#write.csv(d, "~/Google Drive/Data Science/Olympics Data/BestAthletes1.csv", row.names=F)

sd(d$avgTime)
remove(team)
team <- d %>%
  na.omit() %>%
  group_by(NAT) %>%
  summarise(avgTime=mean(avgTime),
            Athletes=n(),
            AvgAge=mean(Age, na.rm=T)) %>%
  arrange(desc(Athletes))

head(d,15)


# The elite runners look to fall in here

#	Devon ALLEN	USA	13.03000	1
#	Omar MCLEOD	JAM	13.03600	5
#	Orlando ORTEGA	ESP	13.12833	6
#	Pascal MARTINOT-LAGARDE	FRA	13.17000	1
#	Andrew POZZI	GBR	13.19000	1
#	Ronnie ASH	USA	13.19500	2
#	Dimitri BASCOU	FRA	13.20200	5
#	Deuce CARTER	JAM	13.20500	2
#	Gregor TRABER	GER	13.21000	1
#	Jeff PORTER	USA	13.21000	1

# Elite data set 

elites <- a[a$COMPETITOR=="Devon ALLEN" |a$COMPETITOR=="Omar MCLEOD" |
              a$COMPETITOR=="Orlando ORTEGA" |a$COMPETITOR=="Ronnie ASH" |
              a$COMPETITOR=="Pascal MARTINOT-LAGARDE" |a$COMPETITOR=="Dimitri BASCOU" |
              a$COMPETITOR=="Andrew POZZI" |a$COMPETITOR=="Deuce CARTER" |
              a$COMPETITOR=="Gregor TRABER" |a$COMPETITOR=="Jeff PORTER",]

summary(elites)




g <- ggplot(elites, aes(y=MARK, x=COMPETITOR, colour=COMPETITOR)) + geom_point(stat="identity")
g <- g + ggtitle("Elite Times: 110m Men's Hurdles")+labs(y="Time (In Seconds)", x="Athlete")

g

# You'll see in the initial elites chart that there is really a threshold at 13.1 seconds that only three/four athletes appear to cross in 2016

medal <- a[a$COMPETITOR=="Devon ALLEN" |a$COMPETITOR=="Omar MCLEOD" |
             a$COMPETITOR=="Orlando ORTEGA" | a$COMPETITOR=="Dimitri BASCOU",]

summary(medal)

g2 <- ggplot(medal, aes(y=MARK, x=COMPETITOR, colour=COMPETITOR)) + geom_point(stat="identity")
g2 <- g2 + ggtitle("Medal Contender Times: 110m Men's Hurdles")+labs(y="Time (In Seconds)", x="Athlete")

g2


g3 <- ggplot(elites, aes(y=MARK, x=LastRace, colour=COMPETITOR)) + geom_point(stat="identity")
g3 <- g3 + ggtitle("Medal Contender Trends: 110m Men's Hurdles") +labs(y="Time (In Seconds)", x="Days Since Last Race")
g3

# My prediction looking at this data is that 1-4 are:

# Omar McLeod (Jamaica)
# Orlando Ortega (Spain)
# Dimitri Bascou (France)
# Devon Allen (USA)
# Pascal Martinot-Lagarde (France)



