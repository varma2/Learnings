#Lesson 2 Exploring 1 variable
#================================
## 
rm(list = ls())
library("ggplot2")
reddit <- read.csv("H:/reddit.csv")
head(reddit)
summary(reddit)
str(reddit)
levels(reddit$age.range)
qplot (reddit$age.range)
qplot (reddit$income.range)
levels(reddit$income.range)
reddit$age.range = ordered(reddit$age.range,levels= c("Under 18","18-24","25-34","35-44",
"45-54", "55-64", "65 or Above"))
reddit$income.range = factor(reddit$income.range, level = c("Under $20,000", "$20,000 - $29,999", 
                                                            "$30,000 - $39,999", "$40,000 - $49,999",
                                                            "$50,000 - $69,999", "$70,000 - $99,999", 
                                                            "$100,000 - $149,999","$150,000 or more" ),
                             ordered = T)

View(reddit)

#Lesson 3
#================================
  ## 
rm(list = ls())

pf <- read.csv("H:/pseudo_facebook.tsv", se = "\t")
View(pf)
names(pf)
sapply(pf, class)
class(pf$dob_day)
qplot(pf$dob_day)

qplot(pf$dob_day)+
  scale_x_continuous(breaks=1:31)

#alternative for above

hist(pf$dob_day, breaks=31)

ggplot(data = pf, aes(x = dob_day)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)
--------------------
ggplot(data=pf, aes(x = friend_count)) +
  geom_histogram()
#alternate code
qplot(pf$friend_count)

qplot(x = friend_count, data = pf, xlim = c(0,1000))
-------------------------
#alternate_code

qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth =25)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))

ggplot(data = pf[!is.na(pf$gender),], aes(x=friend_count))+
  geom_histogram(binwidth = 25)+
  scale_x_continuous(limit=c(0,1000), breaks = seq(0,1000,50))+
  facet_wrap(~gender)

table(pf$gender)

by(pf$friend_count, pf$gender, summary)

qplot(data=pf, x=tenure, binwidth = 30, color = I('black'), fill=I('#099dd9'))

qplot(data=pf, x=tenure/365, binwidth = 0.25, color = I('black'), fill=I('#099dd9'))

ggplot(data = pf, aes(x=tenure))+
  geom_histogram(binwidth = 30, color = I('black'), fill=I('#099dd9'))

ggplot(data = pf, aes(x=tenure/365))+
  geom_histogram(binwidth = 0.25, color = I('black'), fill=I('#099dd9'))+
  scale_x_continuous(breaks = seq(0,7,1), limits=c(0,7))+
  xlab('Number of years using facebook')+
  ylab('Number of users in sample')

ggplot(data =pf, aes(x=age))+
  geom_histogram(binwidth=1)+
  scale_x_continuous(breaks=seq(0,113,5))+
  xlab('Age')+
  ylab('Count')

ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') + 
  scale_x_continuous(breaks = seq(0, 113, 5))

summary(pf$age)

summary(pf$friend_count)
summary(log10(pf$friend_count+1))

p1<- qplot(pf$friend_count)
plog<- qplot(log(pf$friend_count+1))
p3<- qplot(log10(pf$friend_count+1))
p4<- qplot(sqrt(pf$friend_count+1))

install.packages('gridExtra')
library(gridExtra)

grid.arrange(p1,p2,p3,p4,ncol=1)

p1<- ggplot(data=pf, aes(x=friend_count))+geom_histogram()
plog10 = p1 + scale_x_log10()
p3 = p1 + scale_x_sqrt()

grid.arrange(plog,plog10,ncol=2)

#frequency line

ggplot(data = pf[!is.na(pf$gender),], aes(x=friend_count, y = ..count.. *100 / sum(..count..)))+ 
  geom_freqpoly(aes(color=gender), binwidth = 10) + 
  scale_x_continuous(limits=c(0,1000), breaks = seq(0,1000,50))+
  xlab("Friend Count")+
  ylab("Percentage of users with that Friend Count")


ggplot(data = pf[!is.na(pf$gender),], aes(x = www_likes, y = ..count.. / sum(..count..)))+
  geom_freqpoly(aes(color=gender), binwidth = 1)+
  scale_x_log10()

by(pf$www_likes, pf$gender, sum)
  

names(pf)

#==========================
  #boxplot
 # ======================
#

ggplot(data = subset(pf, !is.na(gender)), aes(x = gender, y = friend_count))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,1000))

#in the below method data points are not removed only y axis id modified, 
#where as in previous case data points get removed

ggplot(data = subset(pf, !is.na(gender)), aes(x = gender, y = friend_count))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,250))

by(pf$friendships_initiated, pf$gender, mean)

ggplot(data = subset(pf, !is.na(gender)), aes(x= gender, y = friendships_initiated))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,150))

#binary variable 

summary(pf$mobile_likes)

pf$mobile_checkins <- NA
pf$mobile_checkins <- ifelse(pf$mobile_likes>0,1,0)
summary(pf$mobile_checkins)
pf$mobile_checkins <- as.factor(pf$mobile_checkins)
100*summary(pf$mobile_checkins)/length(pf$mobile_checkins)
