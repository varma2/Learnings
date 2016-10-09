# rm(list = ls())
# install.packages("alr3")
# library(alr3)
# data("Mitchell")
# ?Mitchell
# 
# ggplot(data = Mitchell, aes(x=Month, y = Temp))+
#   geom_point()+
#   scale_x_continuous(breaks = seq(0,204,12))
# 
# cor(Mitchell$Month, Mitchell$Temp)
# 
# summary(Mitchell$Month)
# 
library(ggplot2)
data(diamonds)
ggplot(data = diamonds, aes(x= x, y = price))+
  geom_point()+
  geom_smooth(color = "red")
?diamonds

cor(diamonds$price, diamonds$z)

ggplot(data = diamonds, aes(x= z, y = price))+
  geom_point(aes(alpha = 1/20))+
  coord_cartesian(xlim=c(0,10))
?geom_point

summary(diamonds$depth)

plot(density(diamonds$depth))
density(diamonds$z)

cor(diamonds$depth, diamonds$price)

ggplot(data = diamonds, aes(x= (x*y*z), y = price))+
  geom_point()+
  coord_cartesian(xlim = c(0,500))+
  geom_smooth()
library(dplyr)

with(data =subset(diamonds, (x*y*z < 800) & (x*y*z > 0)), cor(price, x*y*z))

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
names(diamonds)

ggplot(data = diamonds, aes(x= volume, y = price))+
  geom_point()+
  coord_cartesian(xlim = c(1,799))+
  geom_smooth()

group_diamondsByClarity <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(group_diamondsByClarity,
                     mean_price = mean(price),
                     median_price = median(price),
                     min_price = min(price),
                     max_price = max(price),
                     n = n())
head(d.price)
tail(diamondsByClarity)
diamondsByClarity$volume <- NULL


diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
     
     
head(diamonds_mp_by_clarity)
head(diamonds_mp_by_color)

ggplot(data = diamonds_mp_by_color, aes(y= mean_price, x = color))+
  geom_boxplot()

plot(diamonds_mp_by_color$color, diamonds_mp_by_color$mean_price)
plot(diamonds_mp_by_clarity$clarity, diamonds_mp_by_clarity$mean_price)
?diamonds

###############################################
rm(list = ls())
pf<- read.csv("h:/pseudo_facebook.tsv", sep = '\t')
age_gender_groups <- group_by(pf[!is.na(pf$gender),], age, gender)
pf.fc_by_age_gender<- summarise(age_gender_groups,
                         mean_friend_count = mean(as.numeric(friend_count)),
                         median_friend_count = median(as.numeric(friend_count)),
                         n = n())
ungroup(pf.fc_by_age_gender)

head(pf.fc_by_age_gender)

ggplot(data = pf.fc_by_age_gender, aes(x = age, y = mean_friend_count))+
  geom_line(aes(color = gender), stat = "summary", fun.y  =median)
  
library(dplyr)
library(tidyr)
pf.fc_by_age_gender.wide <-subset(pf.fc_by_age_gender[,c('age', 'gender', 'median_friend_count')],
                                  !is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio = male / female) 

head(pf.fc_by_age_gender.wide)
 
ggplot(data= pf.fc_by_age_gender.wide, aes(x=age, y=ratio^-1))+
  geom_line()+
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)+
  scale_x_continuous(breaks = seq(0,113, 10))
?geom_abline

pf$year_joined <- floor(2014 - (pf$tenure / 365))
summary(pf$year_joined)
table(pf$year_joined)

#create custom bins,

pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014))


table(pf$year_joined.bucket)

ggplot(data = subset(pf, !is.na(gender)), aes(x=age, y=friend_count))+
  #geom_line(aes(color = gender), stat = "summary", fun.y = median)+
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = mean)+
  #geom_smooth()+
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

with(subset(pf,tenure>=1),cor(friend_count, tenure))
with(subset(pf,tenure>=1),max(friend_count / tenure))

#friendships initiated per tenure
ggplot(data = subset(pf, tenure>=1), aes(x=tenure, y=friendships_initiated/tenure))+
  #geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = mean)+
  geom_smooth(aes(color = year_joined.bucket))
  
  names(pf)
