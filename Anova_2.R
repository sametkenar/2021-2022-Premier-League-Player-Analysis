library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
data$Pos <- ordered(factor(data$Pos, levels=(c("DF","DF,FW", "DF,MF","FW",
                                               "FW,DF","FW,MF","MF","MF,DF","MF,FW"))))
str(data)


# Pozisyonlara göre nonpenalty expected goals + plus assists esitmidir?
# our k distincs groups are positions, categorical-nominal variable is
# position

# We are interested in comparing the mean nonpenalty expected goals+plus assists
# according to positions to see whether they're all equal.

# MEANS

players <- data %>% filter(Pos != "GK")

players <- na.omit(players)
pos.means <- tapply(players$npxG.xA, INDEX = players$Pos, FUN = mean)
pos.means

# BOXPLOT
boxplot(players$npxG.xA~players$Pos)
points(1:9,pos.means, pch= 4, cex=1.5)

ggplot(players,aes(x=players$Pos, y= players$npxG.xA, color = players$Pos))+
  geom_boxplot(width = 0.5, lwd=1)+
  theme_minimal()+
  stat_summary(fun="mean", color="red", shape=4)+
  labs(x= "Positions",
       y= "Non-Penalty Expected Goals plus assists ",
       title = "Boxplot for Each Position", color = "Pos")

# EXAMINING EQUALITY OF VARIANCES


pos.sds <- tapply(players$npxG.xA, INDEX = players$Pos, FUN = sd)
pos.sds

max(pos.sds)/min(pos.sds)

# NORMALITY

pos.meancen <- players$npxG.xA - pos.means[as.numeric(players$Pos)]
pos.meancen

qqnorm(pos.meancen, main="Normal QQ plot of residuals")
qqline(pos.meancen)

# WE CAN ASSUME THAT DATA IS NORMAL

# Building Anova Table

model <- lm(npxG.xA ~ Pos, data = players)
summary(model)
pos.anova <- aov(npxG.xA ~ Pos, data=players)
summary(pos.anova)
14.25/495

# We have a very small p value, so we can say that the 
# mean nonpenalty-expected goals + plus assists are not the same for the different positions.
