library(ggplot2)
data("diamonds")
names(diamonds)
plot(diamonds$carat, diamonds$price)

ggplot(data = diamonds, aes(x = carat, y = price))+
  geom_point(alpha = 1/10, color = 'orange')+
  scale_x_continuous(lim = c(0,quantile(diamonds$carat, 0.99)))+
  scale_y_continuous(lim = c(0,quantile(diamonds$price, 0.99)))+
  stat_smooth(method = "lm")

# install these if necessary
# install.packages('GGally')
# install.packages('scales')
# install.packages('memisc')
# install.packages('lattice')
# install.packages('MASS')
# install.packages('car')
# install.packages('reshape')
# install.packages('plyr')

# load the ggplot graphics package and the others
library(ggplot2)
library(GGally)
library(scales)
library(memisc)


set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

###########


library(gridExtra)

plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = I('#099dd9')) + 
  ggtitle('Price')
plot1

plot2 <- qplot(data = diamonds , x = price, binwidth = .01, fill = I('#099dd9')) +
  ggtitle('Price (log10)')+
  scale_x_log10()
plot2
grid.arrange(plot1, plot2, ncol = 2)

#################

qplot(carat, price, data = diamonds)+
  scale_y_continuous(trans = log10_trans())+
  ggtitle("Price (log10) by Carat")

##############
#Using cube root trans function

cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(data = diamonds, aes(carat, price))+
  geom_point()+
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3),
                     breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans = log10_trans(), limits = c(350,15000), 
                     breaks = c(350,1000,5000,10000,15000))+
  ggtitle("price (log10) by cube root of carat")

#################
#overplotting revisited

ggplot(aes(carat, price), data = diamonds) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

#############

library(RColorBrewer)

ggplot(aes(x = carat, y = price, colour = cut), data = diamonds) + 
  geom_point(alpha = 1, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 3))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

#################

ggplot(aes(x = carat, y = price, colour = color), data = diamonds) + 
  geom_point(alpha = 1, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = F,
                                          override.aes = list(alpha = 1, size = 3))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and color')

#####################
#Creating LM Function

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

###################
#Using New dataset, downloaded fromt he recent files
# Create Lm on this new data set
rm(list=ls())
diamondsBigSample<- load('H:/Udacity_R/BigDiamonds.Rda')

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), 
         data = diamondsbig[diamondsbig$price< 10000 & diamondsbig$cert == "GIA",])
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)

mtable(m1, m2, m3, m4, m5)

thisDiamond <- data.frame( carat = 1, cut = "V.Good", color = 'I', clarity = 'VS1')

modelEstimate = predict(m5, newdata = thisDiamond, 
                        interval = "prediction", level = 0.95)
modelEstimate
exp(modelEstimate)
