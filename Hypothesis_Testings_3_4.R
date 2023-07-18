library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
str(data)

# RESEARCH QUESTIONS 3 AND 4 (HYPOTHESES TESTINGS) PROPORTIONS
mean_minplayed_90min <- mean(na.omit(data$Min))

# Q3: We wish to claim that more than 50% of european football league
# players' in 2021-2022 season their MIN played is greater than 1000.
# A sample of 691 players showed that 322 players MIN playes is above general average. At a=0.05, 
# is there enough evidence to support our claim? 

greater <- data %>% filter(data$Min > 1000)

p_bar <- 322/691
standard_error <- sqrt((0.50*0.50)/1000)

test_stat <- (p_bar-0.50)/standard_error
test_stat

city <- data %>% subset(Team == "Manchester City")
city_greater <- city %>% filter(city$Min > 1000)
p_bar <- 16/33
standard_error_city <- sqrt((0.50*0.50)/1000)  
test_stat <- (p_bar-0.50)/standard_error_city
test_stat

# Since our critical value is 1.64, and test statistic is -2.1509
# We reject the null hypothesis. There is enough evidence to accept the claim that
# %50 of european football league players' MIN played is greater than 1000 in the 2021-2022 season.

# Q4: We wish to claim that the top 10 and bottom 10 team's players 
# positions. 

# Yapılması gereken: takımlar top 10 team ve bottom 10 team olarak ikiye ayrılacak.
# Top 10 team ve bottom 10 teamin oyuncu sayısı bulunup pozisyonlardan biri seçilip
# (forvet olabilir, defans olabilir).
# genel oyuncu sayısına göre bölünerek proportionları hesaplanarak two sample population proportion
# test yapılacak.



top10_team <- c("Arsenal","Brighton & Hove Albion",
                "Chelsea","Leicester City","Liverpool",
                "Manchester City","Manchester United",
                "Tottenham Hotspur","West Ham United",
                "Wolverhampton Wanderers")
bot10_team <- c("Newcastle United", "Crystal Palace","Brentford",
                "Aston Villa","Southampton","Everton","Leeds United",
                "Burnley","Watford","Norwich City")

top_team <- subset(data, Team %in% top10_team)
bot_team <- subset(data, Team %in% bot10_team)

top_team_forvet <- subset(top_team,top_team$Pos == "FW")
bot_team_forvet <- subset(bot_team,bot_team$Pos == "FW")
count(subset(top_team, top_team$Pos == "DF"))
count(subset(bot_team, bot_team$Pos == "DF"))

p_top <- 122/343
p_bot <- 111/348  

p_bar <- 233/691
q_bar <- 1-p_bar
(p_top-p_bot)/(sqrt((p_bar*q_bar)*((1/343)+(1/348))))

# Fail to reject

