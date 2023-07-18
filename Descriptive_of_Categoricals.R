library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(paletteer)
library(ggthemes)
data <- read.csv("Football_Player_Stats.csv", header = T, sep=",")
str(data)
head(data, 10)



# HOW MANY PLAYERS DO TEAMS HAVE?

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

team_graph <- teams %>% 
  count(Team)

ggplot(team_graph,
       aes(x= reorder(Team, -n),
           y= n))+
  geom_bar(stat="identity",
           fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", n =20))+
  geom_text(aes(label=n),
            vjust=-0.3,
            size = 4)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle= 45,vjust=1, size=8))+
  labs(x = "Teams",
       y = "Count",
       title = "How many players do teams have?")

# HOW MANY PLAYERS INCLUDED ACCORDING TO NATIONS

nations <- data %>% group_by(Nation)

nation_graph <- nations %>% 
  count(Nation)

ggplot(nation_graph,
       aes(x= reorder(Nation, -n),
           y= n))+
  geom_bar(stat="identity",
           fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", n =67))+
  geom_text(aes(label=n),
            vjust=-0.3,
            size = 4)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle= 45,vjust=1, size=8))+
  labs(x = "Nations",
       y = "Count",
       title = "HOW MANY PLAYERS INCLUDED ACCORDING TO NATIONS")

# HOW MANY PLAYERS ACCORDING TO POSITION

positions <- data %>% group_by(Pos)

position_graph <- positions %>% 
  count(Pos)

ggplot(position_graph,
       aes(x= reorder(Pos, -n),
           y= n))+
  geom_bar(stat="identity",
           fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", n =10))+
  geom_text(aes(label=n),
            vjust=-0.3,
            size = 4)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle= 45,vjust=1, size=10))+
  labs(x = "Positions",
       y = "Count",
       title = "How many players the Premier League have according to positions ?")

                          