library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
str(data)

# RESEARCH QUESTIONS 1 AND 2 (HYPOTHESES TESTINGS)

# Q1: We wish to test the claim that the average expected goal of champion team players is 
# greater than the rest of the team players.
# Is there enough evidence to support the claim at 0.05 significance level?
# Note: Although we are not given the population standard deviation, 
# Since our sample size is greater than 30, 
# We assume that the distribution of sample mean average expected goal of players 
# follows normal distribution. 
# Thus we can use the z-test.

# m = Champ team players mean of expected goal, T = rest of the team players mean of expected goal
# H0: m <  T 
# H1: m >  T

teams <- data %>% group_by(Team) 
teams$Team <- ordered(factor(teams$Team,
                             levels=(c("Arsenal","Aston Villa", "Brentford",
                                       "Brighton & Hove Albion",
                                       "Burnley","Chelsea","Crystal Palace",
                                       "Everton","Leeds United",
                                       "Leicester City","Liverpool","Manchester City",
                                       "Manchester United","Newcastle United","Norwich City",
                                       "Southampton","Tottenham Hotspur",
                                       "Watford", "West Ham United",
                                       "Wolverhampton Wanderers"))))
# Rest of the team players
rest <- teams %>% filter(Team != "Manchester City")
mean(na.omit(rest$xG))
# Champ team players
champ <- teams %>% filter(Team == "Manchester City")
mean(na.omit(champ$xG))
sample.mean.expg <- mean(na.omit(champ$xG))
sample.sd.expg <- sd(na.omit(champ$xG))
test.stat.1 <- (sample.mean.expg - mean(na.omit(rest$xG)))/(sample.sd.expg/sqrt(nrow(champ)))
test.stat.1
qnorm(0.025)

# Test Statistic = 2.3059, critical value 1.96, Since our test statistics is greater than
# critical value, we can reject our null hypothesis and say that there is enough evidence to
# support the claim that average expected goal of champ team players is greater than the 
# rest of the team players.


# Q2: We wish to test the claim the average expected assists of Manchester City (champteam), 
# the average expected assists of liverpool is not equal. 
# Note: Although we are not given the population standard deviation, 
# because our sample size is greater than 30, 
# we assume that the distribution of sample mean of expected assists follows normal distribution. 
# Thus we can use the z-test.

# m1 = the average expected assists of manchester city, m2 = the average expected assists of liverpool
# H0: m1 = m2
# H1: m1 != m2

teams <- data %>% group_by(Team) 
teams$Team <- ordered(factor(teams$Team,
                             levels=(c("Arsenal","Aston Villa", "Brentford",
                                       "Brighton & Hove Albion",
                                       "Burnley","Chelsea","Crystal Palace",
                                       "Everton","Leeds United",
                                       "Leicester City","Liverpool","Manchester City",
                                       "Manchester United","Newcastle United","Norwich City",
                                       "Southampton","Tottenham Hotspur",
                                       "Watford", "West Ham United",
                                       "Wolverhampton Wanderers"))))

# Champ team players
manch <- teams %>% filter(Team == "Manchester City")
# Liverpool team players
live <- teams %>% filter(Team == "Liverpool")

# Hypothesis testing

manch_mean <- mean(na.omit(manch$xA))
live_mean <- mean(na.omit(live$xA))

manch_var <- var(na.omit(manch$xA))
live_var <- var(na.omit(live$xA))

# manch_n = 33, live_n = 35

test_stat.2 <- ((manch_mean-live_mean)-0)/sqrt((manch_var/33)*(live_var/35))
test_stat.2
qnorm(0.025)

# Test Statistics = 0.3193601, Critical Value = -1.96 and 1.96. Since our test statistics is
# less than 1.96. We fail to reject the null hypothesis and say there is not enough evidence to say
# that the average expected assists of manchester city team players and liverpool team players are not different.

