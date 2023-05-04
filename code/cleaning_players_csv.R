##################################################
# Script to clean the data from player dataset
##################################################
remove(list = ls())

# load the necessary pacages 

library(readr)

#load the dataset 

players <- read_csv("Noah/SCOLAIRE/BACHELOR STGALL/Bachelor/$kills programing/Group project/XCampRProjectAdvanced/data/players.csv")
View(players)

---------#cleanin the players dataset-----

----#active ----
# creating a dummy variable for the active players = 1
players$active <- ifelse(players$active == TRUE, 1, 0)

----# code CIO----

# convert the Code CIO into a factor

players$country <- as.factor(players$country)

----# turnedPro----
#convert turnedPro to numeric values
players$turnedPro <- as.numeric(players$turnedPro)

----# weight----

# rename the "weight" variable to "weightInKgs" and remove "kg" suffix 
players$weightInKg <- as.numeric(gsub("[^0-9.]+", "", players$weight))
players$weight <- NULL

----# height----

# rename the "height" variable to "heightInCm" and remove "cm" suffix 
players$heightInCm <- as.numeric(gsub("[^0-9.]+", "", players$height))
players$height <- NULL

----# lefty----

# creating a dummy variable for the dominant hand, left = 1

# replace "Ambidextrous" with NA 
players$lefty <- replace(players$lefty, players$lefty == "Ambidextrous", NA)

# convert the dominant hand into a factor
players$lefty <- as.factor(players$lefty)

#creat the dummy variable 
players$lefty <- ifelse(players$lefty == 'Left-Handed', 1, 0)
----# oneHandedBackhand----
# creating a dummy variable for the number of hands on the back hand , one hand = 1

# replace "Unknown Backhand" with NA 
players$oneHandedBackhand <- replace(players$oneHandedBackhand, players$oneHandedBackhand == "Unknown Backhand", NA)

# convert the oneHandedBackhand to a factor
players$oneHandedBackhand <- as.factor(players$oneHandedBackhand)

#creat the dummy variable 
players$oneHandedBackhand <- ifelse(players$oneHandedBackhand == 'One-Handed Backhand', 1, 0)
