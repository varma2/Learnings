pf<- read.csv("h:/pseudo_facebook.tsv", sep = '\t')
library(dplyr)
library(ggplot2)
age_groups <- group_by(pf, age)
pf.fc_by_age<- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())

head(pf.fc_by_age)
pf.fc_by_age<-arrange(pf.fc_by_age, age)
head(pf.fc_by_age)

pf.fc_by_new_age <- pf %>%
  group_by(age)%>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n())%>%
  arrange(age)

sapply(pf.fc_by_age, class)
pf.fc_by_age$friend_count_mean = as.numeric(pf.fc_by_age$friend_count_mean)
pf.fc_by_age$friend_count_median = as.numeric(pf.fc_by_age$friend_count_median)

ggplot(data=pf.fc_by_age, aes(x=age, y=friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,113,10))

##alternative to above
#qplot(data=pf.fc_by_age, x= age, y = friend_count_mean)

# ggplot(aes(x=age, y=friend_count), data = pf)+
#   ##coord_cartesian(xlim = c(13,90))+
#   coord_cartesian(xlim=c(13,70), ylim=c(0,1000))+
#   scale_x_continuous(breaks = seq(10,70,10))+
#   geom_point(alpha = 0.05, 
#              ##position = position_jitter(h=0),
#              color = 'orange')+
#   ##coord_trans(y='sqrt')+
#   geom_line(stat= 'summary', fun.y = mean)+
#   geom_line(stat= 'summary', fun.y = quantile,
#             fun.args = list(probs = 0.25 ,
#             linetype = 2,
#             color = 'blue'))+
#   geom_line(stat= 'summary', fun.y = quantile,
#             fun.args = list(probs = 0.5 ,
#                             linetype = 3,
#                             color = 'blue'))+
#   geom_line(stat= 'summary', fun.y = quantile,
#             fun.args = list(probs = 0.9 ,
#                             linetype = 4,
#                             color = 'blue'))

cor(pf$age, pf$friend_count)
cor.test(pf$age, pf$friend_count, method = 'pearson')

with(pf, cor.test(age, friend_count))
with(pf[pf$age<=70,], cor.test(age, friend_count))
  
with(subset(pf, age<=70), cor.test(age, friend_count))

ggplot(data = pf, aes(x=www_likes_received, y = likes_received))+
  geom_point()+
  #coord_trans(x="sqrt", y = "sqrt")+
  #coord_cartesian(x= c(0,20000), y = c(0,5000))
  xlim(0,quantile(pf$www_likes_received, 0.95))+
  ylim(0,quantile(pf$likes_received, 0.95))+
  geom_smooth(method = 'lm', color = 'red')

with(pf, cor(www_likes_received, likes_received))

ggplot(data =pf.fc_by_age, aes(x=age, y =friend_count_mean) )+
  geom_line()

pf$age_with_months <- pf$age + (1-pf$dob_month/12)

age_group_month <- group_by(pf, age_with_months) 
pf.fc_by_age_months <- summarise(age_group_month, 
                          friend_count_mean = mean(friend_count), 
                          friend_count_median = median(friend_count), 
                          n = n()) 
head(pf.fc_by_age_months)
pf.fc_by_age <- arrange(pf.fc_by_age_months, age_with_months) 

p1<- ggplot(data = subset(pf.fc_by_age, age<71), 
       aes(x=age, y=friend_count_mean))+
  geom_line()+
  geom_smooth()

p2<- ggplot(data = subset(pf.fc_by_age_months, age_with_months<71), 
       aes(x=age_with_months, y=friend_count_mean))+
  geom_line()+
  geom_smooth()

p3 <- ggplot(data = subset(pf, age<71), 
             aes(x=round(age/5)*5, y=friend_count))+
  geom_line(stat = "summary", fun.y = mean)
p3
library(gridExtra)
grid.arrange(p2,p1,p3,ncol = 1)
