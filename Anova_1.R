library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
str(data)

data$Pos <- ordered(factor(data$Pos, levels=(c("DF","DF,FW", "DF,MF","FW",
                                               "FW,DF","FW,MF","MF","MF,DF","MF,FW"))))

# Which position players get the most YELLOW cards?
# our k distincs groups are positions, categorical-nominal variable is
# position

# We are interested in comparing the mean YCards according to positions
# to see whether they're all equal.

# MEANS

data <- na.omit(data)
pos.means <- tapply(data$CrdY, INDEX = data$Pos, FUN = mean)
pos.means
# BOXPLOT

boxplot(data$CrdY~data$Pos)
points(1:10,pos.means, pch= 4, cex=1.5)

ggplot(data,aes(x=data$Pos, y= data$CrdY, color = data$Pos))+
  geom_boxplot(width = 0.5, lwd=1)+
  theme_minimal()+
  stat_summary(fun="mean", color="red", shape=4)+
  labs(x= "Positions",
       y= "The Yellow Cards ",
       title = "Boxplot for Each Position", color = "Pos")

# EXAMINING EQUALITY OF VARIANCES

pos.sds <- tapply(data$CrdY, INDEX = data$Pos, FUN = sd)
pos.sds

max(pos.sds)/min(pos.sds)


# NORMALITY

pos.meancen <- data$CrdY - pos.means[as.numeric(data$Pos)]
pos.meancen

qqnorm(pos.meancen, main = "Normal QQ Plot of Residuals")
qqline(pos.meancen)
# Non parametric
# Building ANOVA Table

pos.anova <- aov(CrdY ~ Pos, data=data)
summary(pos.anova)
6.642/495

# We have a very small p value, so we can say that the 
# mean Yellow card are the same for the different positions.



# Perform one-way ANOVA
result <- aov(CrdY ~ Pos, data = data)
# Print the ANOVA table
summary(result)
# Get the means for each position
means <- aggregate(CrdY  ~ Pos, data = data, FUN = mean)
# Find the position with the highest mean card count
position_with_most_cards <- means$Pos[which.max(means$CrdY)]
# Print the position with the most cards
print(position_with_most_cards)
