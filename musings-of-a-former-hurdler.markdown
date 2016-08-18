---
title: Musings of a former hurdler
date: 2016-08-18 06:36:00 Z
published: false
---

---
title: "2016 Olympics: Hurdles 110m Preview"
author: "Matthew Maher"
date: "Thursday, August 18, 2016"
output: html_document
---

Quick review of the available data prior to the Rio 2016 110 meter hurdles.


<div id="bg">
<img src="https://stillmed.olympic.org/media/Images/OlympicOrg/News/2016/08/16/2016-08-16-110m-hurdles-inside-01.jpg?interpolation=lanczos-none&resize=1060:*" alt="">
<figcaption> The final steps of Omar McLeod's (center) gold medal race in Rio de Janeiro, Brazil 2016. Dimitri Bascou (left) and Pascal Martinot-Lagarde (right).</figcaption>
</div>



Load packages for downloading and preparing athlete data in R:

```{r}
library(XML)
library(RCurl)
library(dplyr)
library(stringr)
library(ggplot2)
```

Read in csv file of web scraped data (scraped August 15th prior to the Rio Olympic Heats/Finals) :  <https://www.iaaf.org/records/toplists/hurdles/110-metres-hurdles/outdoor/men/senior/2016> 

```{r}
a = read.csv("C:/Users/MAHER/Google Drive/R Script Master File/Track110_2016.csv")
str(a)
summary(a)
```

Parse the data to get it into a workable format. This will include creating variables for age and days since last race. But first...coffee...nah, first we must use the stringr package to parse the variables for Nation and the dates.

```{r}
a$NAT <- substr(a$NAT,1,3)
a$NAT <- as.factor(a$NAT)

t <- substr(a$DOB, 1,2)
t <- str_replace_all(t, "-" , "")
t2 <- substr(a$DOB, 3,6)
t2 <- str_replace_all(t2, "-", "")
t3 <- substr(a$DOB, 7,9)
t3 <- str_replace_all(t3,"-", "")

# Produce functions to make this all easier 

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

fy <- function(x){
  a=x
  if(a <20) a1 <- paste("20",a,sep="")
  if(a > 20) a1 <- paste("19",a,sep="")
  return(a1)
}

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

tty <- fy(t3)
ttm <- fm(t2)
ttd <- fd(t)

tt <- paste(tty, ttm, ttd, sep="-")

# Below we calculate Age as of the time this script is run, however you may be more interested in age at the time of the race. If that is the case, you'll need to modify the script as I do with the "Last Race" variable below. 

a$DOB <- as.Date(tt, format="%Y-%m-%d")
a$AGE <- as.numeric((Sys.Date()-a$DOB)/365)

```

Calculate how many days it's been since each race so we can see if they are trending up or down over time (in terms of becoming faster/slower).


```{r}
ta <- substr(a$DATE, 1,2)
ta <- str_replace_all(ta, "-" , "")

tb <- substr(a$DATE, 3,6)
tb <- str_replace_all(tb, "-", "")

tc <- substr(a$DATE, 7,9)
tc <- str_replace_all(tc,"-", "")

tcy <- fy(tc)
tbm <- fm(tb)
tad <- fd(ta)

ttt <- paste(tcy, tbm, tad, sep="-")
a$DATE <- as.Date(ttt, format="%Y-%m-%d")
a$LastRace <- as.numeric((as.Date("2016-08-17",format="%Y-%m-%d")-a$DATE))

# To get a sense of how many days it has been since the beginning of the year....

as.numeric((as.Date("2016-08-17",format="%Y-%m-%d")-(as.Date("2016-01-01",format="%Y-%m-%d"))))
```

Calculate the average time for each individual runner in this year's data

```{r}
a1 <- a %>%
  group_by(COMPETITOR, NAT) %>%
  summarise(avgTime=mean(MARK),
            Age=mean(AGE),
            TopTimesRecorded=n(),
            DaysSinceLastRace=min(LastRace)) %>%
  arrange(avgTime)

head(a1,15)

```

Filter our those who have not posted a top race time for the last 228 days (aka, year-to-date prior to the Olympic Games in Rio de Janeiro, Brazil).

```{r}
aa <- a[a$LastRace<228,]

b <- aa %>%
  group_by(COMPETITOR) %>%
  summarise(avgTime=mean(MARK),
            Age=mean(AGE),
            TopTimes=n(),
            SinceLastRace=min(LastRace)) %>%
  arrange(avgTime)

c <- a %>%
  group_by(COMPETITOR, NAT) %>%
  summarise(unique=n())

c <- c[-3]

d <- merge(b,c)

d <- arrange(d, avgTime)

head(d,15)

```

Also, let's consider removing a couple top athletes who are not competing in Rio for various reasons (such as Jamaican Hansle Parchment and American David Oliver). 


```{r}
d <- d[d$COMPETITOR!="Hansle PARCHMENT",]
d <- d[d$COMPETITOR!="David OLIVER",]
```

What about a curiosity regarding each national team's pool of talent and descriptive statistics?

```{r}
team <- d %>%
  na.omit() %>%
  group_by(NAT) %>%
  summarise(avgTime=mean(avgTime),
            Athletes=n(),
            AvgAge=mean(Age, na.rm=T)) %>%
  arrange(desc(Athletes))

head(team,15)
```

If we look at the dataset "d", we get a sense of some of the Elite runners in this year's group. 

Competitor              NAT AvgTime   Number of Races

Devon ALLEN	            USA	13.03000	1

Omar MCLEOD	            JAM	13.03600	5

Orlando ORTEGA	        ESP	13.12833	6

Pascal MARTINOT-LAGARDE	FRA	13.17000	1

Andrew POZZI	          GBR	13.19000	1

Ronnie ASH	            USA	13.19500	2

Dimitri BASCOU	        FRA	13.20200	5

Deuce CARTER	          JAM	13.20500	2

Gregor TRABER	          GER	13.21000	1

Jeff PORTER	            USA	13.21000	1

Keep in mind that McLeod has truthfully run the fastest times this year, however Allen has only one data point in this sample and it's rather fast at 13.03 seconds.

```{r}
elites <- a[a$COMPETITOR=="Devon ALLEN" |a$COMPETITOR=="Omar MCLEOD" |
              a$COMPETITOR=="Orlando ORTEGA" |a$COMPETITOR=="Ronnie ASH" |
              a$COMPETITOR=="Pascal MARTINOT-LAGARDE" |a$COMPETITOR=="Dimitri BASCOU" |
              a$COMPETITOR=="Andrew POZZI" |a$COMPETITOR=="Deuce CARTER" |
              a$COMPETITOR=="Gregor TRABER" |a$COMPETITOR=="Jeff PORTER",]

summary(elites)
```





```{r}
g <- ggplot(elites, aes(y=MARK, x=COMPETITOR, colour=COMPETITOR)) + geom_point(stat="identity")
g <- g + ggtitle("Elite Times: 110m Men's Hurdles")+labs(y="Time (In Seconds)", x="Athlete")

```

```{r, echo=FALSE}
g
```

In this initial graphic we see that there seems to be a threshold at 13.1 seconds that only three or four athletes appear to cross in 2016. 


```{r}
medal <- a[a$COMPETITOR=="Devon ALLEN" |a$COMPETITOR=="Omar MCLEOD" |
             a$COMPETITOR=="Orlando ORTEGA" | a$COMPETITOR=="Dimitri BASCOU",]

summary(medal)
```


Now let's take a look at a couple of graphics, one looking at the larger elites field and the other at those we expect to medal.


```{r, echo=FALSE}
g3 <- ggplot(elites, aes(y=MARK, x=LastRace, colour=COMPETITOR)) + geom_point(stat="identity")
g3 <- g3 + ggtitle("Elite Contender Times: 110m Men's Hurdles") +labs(y="Time (In Seconds)", x="Days Since Last Race")
g3
```


```{r, echo=FALSE}
g2 <- ggplot(medal, aes(y=MARK, x=COMPETITOR, colour=COMPETITOR)) + geom_point(stat="identity")
g2 <- g2 + ggtitle("Medal Contender Times: 110m Men's Hurdles")+labs(y="Time (In Seconds)", x="Athlete")
g2
```


My prediction looking at this data is that 1-4 are:

GOLD    Omar McLeod (Jamaica)

SILVER  Orlando Ortega (Spain)

BRONZE  Dimitri Bascou (France)   | OR |  Devon Allen (USA)   |  OR |   Pascal Martinot-Lagarde (France)
        

My confidence in Gold and Silver is much higher than Bronze. Bronze I'd predict is a toss up. 

My reasoning for not putting a lot of confidence in Allen is that he's young, inexperienced on this stage, and only has one recorded time in this data set. McLeod and Ortega are consistently fast in this data. Lastly, Ortega and Pascal Martinot-Lagarde are physically gifted in their height and build for this event. I suspect they will have a better time running down the hurldes than the others. However, McLeod, Bascou and Allen are just blazing fast guys that can hit half the hurdles and still steal the race. 

Final results of the race can be found here: <http://results.nbcolympics.com/track-field/event/men-110m-hurdles/index.html> 


Thanks for reading!