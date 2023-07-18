library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)

data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
str(data)
data <- na.omit(data)

data1 <- subset(data, Pos != "GK")

# SLR about goals and expected goals 
ggplot(data1, aes(x = xG, y = Gls)) +
  geom_point() +
  stat_smooth() + 
  labs(title = "Goals Scored vs Expected Goals",
       subtitle = "y = -0.07757 + 1.02183x") +
  xlab(label = "Expected Goals (xG)")+
  ylab(label = "Goals Scored")+
  theme_minimal()

model <- lm(Gls ~ xG, data = data1)
model
par(mfrow=c(2,2))
plot(model)
cor(data1$Gls,data1$xG)
summary(model)
#MLR about matches played
# formula =>> MP ~ Non-penalty expected goals + expected assists + age

model1 <- lm(MP ~ npxG + xA + Age, data = data1)
model1
par(mfrow=c(2,2))
plot(model1)
summary(model1)
summary(model1)$coefficient
library(car)
avPlots(model1)

# MLR about Starts
# formula =>> Starts ~ Gls.1 + Ast.1

model2 <- lm(Starts ~ Gls.1 + Ast.1 + Age, data = data1)
model2
par(mfrow=c(2,2))
plot(model2)
summary(model2)

# SLR about Starts
# formula =>> Starts ~ Min, Starts ~ X90s 

model3 <- lm(Starts ~ X90s, data = data1)
model3
par(mfrow=c(2,2))
plot(model3)
summary(model3)


