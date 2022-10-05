rm(list = ls())
library(tidyverse)
dat <- load(unzip("./Spain.zip",
                  "events_Spain.RData"))
dat <- events_Spain
rm(events_Spain)
gc()

# select only shoot events
shoot_id <- which(dat$eventId == 10)

# new dataset
start_action <- rep(NA, length(shoot_id))

# each action is a sequence of events that have the same teamId
# i.e. an action is a sequence of passes between players of the same team that ends with a shoot
m <- 1
for(i in shoot_id){
  j <- 1
  while(dat$teamId[i]==dat$teamId[i-j]) j <- j+1  
  start_action[m] <- i - j + 1
  m <- m + 1
}

# we consider only actions that have at least 4 passes
action_drop <- shoot_id - start_action + 1 > 4  
shoot_id <- shoot_id[action_drop]
start_action <- start_action[action_drop]

# response variable 
shoot_dat <- dat[shoot_id, ]
shoot_dat$outcome <- rep(NA, NROW(shoot_dat))
for(i in 1:NROW(shoot_dat)){
  shoot_dat[i, 30] <- ifelse(101 %in% shoot_dat$tags[[i]]$id, 1, 0)   
  cat(i, "\n")
}

# we consider only a few variables considered to be useful for the analysis
dat <- dat %>% select(id, matchId, x1, y1, x2, y2, eventSec)
dataset_shoot <- list()
for(i in 1:length(shoot_id)){
  dataset_shoot[[i]] <- dat[start_action[i]:shoot_id[i],]
  cat(i, "\n")
}

for(i in 1:length(start_action)){
  dataset_shoot[[i]]$t <- (dataset_shoot[[i]]$eventSec - min(dataset_shoot[[i]]$eventSec))/
    (max(dataset_shoot[[i]]$eventSec) - min(dataset_shoot[[i]]$eventSec))
}


# selection of the four teams 
teams <- c("Atlético Madrid", "Barcelona", "Real Madrid", "Sevilla")
labels <- which(shoot_dat$team_name %in% teams)
shoot_dat <- shoot_dat[labels,]
# dataset_shoot <- dataset_shoot[labels]

# side of the match (home or away)
load(unzip("./Spain.zip",
           "matches_Spain.RData"))
matches_unlist <- data.frame(NULL)
for(i in 1:38) matches_unlist <- rbind(matches_unlist, c(matches_Spain[[i]]$match.info))
home_away <- matches_unlist %>% filter(matchID %in% unique(shoot_dat$matchId) & teamName %in% 
                                         unique(shoot_dat$team_name)) %>% select(c(matchID, teamName, side))
for (i in 1:NROW(shoot_dat))  shoot_dat$side[i] <- home_away[which(shoot_dat$matchId[i] == home_away$matchID & shoot_dat$team_name[i] == home_away$teamName),"side"]

# number of passes at each action
n.pass <- rep(NA, NROW(shoot_dat))
for(i in 1:NROW(shoot_dat)) n.pass[i] <- NROW(dataset_shoot[[i]])

rm(matches_Spain)
rm(matches_unlist)
rm(home_away)
rm(dat)
rm(dataset_shoot)
gc()

shoot_dat$num_pass <- n.pass

shoot_dat <- shoot_dat %>% select(- tags)
shoot_dat <- shoot_dat %>% mutate(across(where(is.character), as.factor))
shoot_dat <- shoot_dat %>% rename(yn = outcome)

# variabili con una sola modalit?
to_drop <- rep(1, NCOL(shoot_dat))
for(i in 1:NCOL(shoot_dat)){
  to_drop[i] <- (length(unique(shoot_dat[,i])))
}
# elimino variabili con una modalit?
to_drop <- to_drop!=1
to_drop
shoot_dat <- shoot_dat[,to_drop]

# elimino variabili ridondanti o inutili
shoot_dat <- shoot_dat %>% select(-c(id, playerId, pl_passportArea_alpha3code,
                                       teamId, pl_currentTeamId, pl_currentNationalTeamId, team_city,
                                       team_officialName, pl_shortName, pl_birthArea_alpha3code))

# creazione variabili anno
shoot_dat$pl_year <- as.Date(shoot_dat$pl_birthDate)
shoot_dat$pl_year <- format(shoot_dat$pl_year, format="%Y")
shoot_dat$pl_year <- as.numeric(shoot_dat$pl_year)
shoot_dat$pl_year <- 2018 - shoot_dat$pl_year
table(shoot_dat$pl_year)
shoot_dat <- shoot_dat %>% select(-pl_birthDate)
colnames(shoot_dat)

# distance from the goal
distance <- function(x, y){
  xn <- x *105 / 100
  yn <- y * 68 / 100
  sqrt((xn - 105)^2 + (yn - 34)^2)
}

shoot_dat$sh_dist <- rep(NA, NROW(shoot_dat))
for(i in 1:NROW(shoot_dat)){
  shoot_dat$sh_dist[i] <- distance(shoot_dat$x1[i], shoot_dat$y1[i])
}

plot(shoot_dat$x1[1:100], shoot_dat$y1[1:100], col = shoot_dat$yn[1:100]+1)
for(i in 1:100){
  segments(shoot_dat$x1[i], shoot_dat$y1[i], 100, 50, col=shoot_dat$yn[i]+1)
}

dist_x <- function(x){
  105 - x*105/100
}

dist_y <- function(x){
  abs(34 - x*68/100)
}

# degree of the shoot from the goal
degree <- function(x, y) atan(dist_y(y)/dist_x(x))

shoot_dat$sh_degree <- rep(NA, NROW(shoot_dat))
for(i in 1:NROW(shoot_dat)){
  shoot_dat$sh_degree[i] <- degree(shoot_dat$x1[i], shoot_dat$y1[i])
}

shoot_dat$sh_degree <- shoot_dat$sh_degree * 360 / (2*pi)
boxplot(shoot_dat$sh_degree ~ shoot_dat$yn)
summary(shoot_dat$sh_degree)

shoot_dat <- shoot_dat %>% select(-c(x1, x2, y1, y2))

# qualitative answer ------------------------------------------------------
shoot_dat$y <- as.factor(shoot_dat$yn == 1) # goal = 1


