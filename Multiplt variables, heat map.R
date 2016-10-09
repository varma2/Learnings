rm(list = ls())
library (ggplot2)
yo <- read.csv("H:/yogurt.csv")
str(yo)
summary(yo)
length(unique(yo$price))
table(yo$price)
View(yo)
yo <- transform(yo, all.purchases = strawberry + 
                  blueberry + pina.colada + plain + mixed.berry)
summary(yo$all.purchases)
# yo$id <- factor(yo$id)
# hist(yo$price)
ggplot(data=yo, aes(y=price, x = time))+
  geom_jitter(alpha=1/4, shape = 21, fill = I('#F79420'))

set.seed(12345)
sample.ids <- sample(yo$id,16)
sample.ids
ggplot(aes(x= time, y = price),
       data = subset(yo, id %in% sample.ids))+
  geom_line()+
  facet_wrap(~id, ncol=3)+
  geom_point(aes(size=all.purchases), pch = 1)

##Scatter Plot Matrices
install.packages("GGally")
library(GGally)
theme_set(theme_minimal(20))
pf<- read.csv("h:/pseudo_facebook.tsv", sep = '\t')
set.seed(1836)
pf_subset <- pf[,2:15]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),])
