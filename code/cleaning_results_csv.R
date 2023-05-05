##################################################
# Script to clean the data from player dataset
##################################################
remove(list = ls())

# load the necessary pacages 

library(readr)
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
install.packages("stringr")
install.packages("tidyr")

#load the dataset 

results <- read_csv("Noah/SCOLAIRE/BACHELOR STGALL/Bachelor/$kills programing/Group project/XCampRProjectAdvanced/data/results.csv")
View(results)


#Clean the results dataset

----#trounamentTour----
# convert the Type of tournament into a factor
results$tournamentType <- as.factor(results$tournamentType)

----#tournament----
# convert the tournament into a factor
results$tournament <- as.factor(results$tournament)

#correction for international games, rename them all as Davis cup

# naming every international game as "Davis CUP"

# use str_detect() to check if the column contains any of the patterns
patterns <- c(" vs ", " v ", " vs.", " V ")
results$tournament <- ifelse(str_detect(results$tournament, paste(patterns, collapse = "|")), "Davis CUP", results$tournament)


----#location----

# Split location into city and country variables
results$city <- ifelse(grepl(",", results$location), 
                    sub(",.*", "", results$location), 
                    sub(",.*,.+,", "", results$location))
results$country <- ifelse(grepl(",", results$location), 
                       sub(".*,", "", results$location), 
                       sub(".*,,", "", results$location))
results$location <- NULL

----#date----

# split the cell into two variables at the '-' character
results <- cbind(results, str_split(results$date, " - ", simplify = TRUE))
colnames(results)[19:20] <- c("startDate", "endDate")
results$date <- NULL 
  
----#outdoor----
#creat a dummy variable on the location of the field, if it is outside  = 1
results$outdoor <- ifelse(results$outdoor == "O", 1, 0)

----#surface----
# convert the Type of suface into a factor
results$surface <- as.factor(results$surface)

----#priceMoneyTotal----

#first getting rid of the commas 
results$priceMoneyTotal <- gsub(",", "", results$priceMoneyTotal)

#separating the numrical value from the character 

# convert priceMoneyTotal to a tibble
results <- as_tibble(results)

# separate currency and value columns
results <- results %>% 
  separate(priceMoneyTotal, into = c("currency", "value"), sep = "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", remove = FALSE)

#rename the variables
results <- results %>% rename(currencyMoneyPriceTotal = currency)
results <- results %>% rename(amountMoneyPriceTotal = value)
#deleting the old value 
results$priceMoneyTotal <- NULL

# convert the amount Money Price into a numeric factor
results$amountMoneyPriceTotal <- as.numeric(results$amountMoneyPriceTotal)

#all exchange values from 05.05.2023
results$MoneyPriceTotalInDollars <- ifelse(results$currencyMoneyPriceTotal == "£", as.numeric(results$amountMoneyPriceTotal) * 1.2631,
                                           ifelse(results$currencyMoneyPriceTotal == "CHF", as.numeric(results$amountMoneyPriceTotal) * 1.1287,
                                                  ifelse(results$currencyMoneyPriceTotal == "A$", as.numeric(results$amountMoneyPriceTotal) * 0.6742,
                                                         ifelse(results$currencyMoneyPriceTotal == "€", as.numeric(results$amountMoneyPriceTotal) * 1.1045,
                                                                ifelse(results$currencyMoneyPriceTotal == "A$", as.numeric(results$amountMoneyPriceTotal) * 1.3241,
                                                                       ifelse(results$currencyMoneyPriceTotal == "S$", as.numeric(results$amountMoneyPriceTotal) * 0.1446,
                                                                              ifelse(results$currencyMoneyPriceTotal == " ¥ ", as.numeric(results$amountMoneyPriceTotal) * 0.6742,
                                                                                     results$amountMoneyPriceTotal)))))))
----#round----
# convert the round of tournament into a factor
results$round <- as.factor(results$round)

----#rankOpponent----
#convert the rank of the opponent to numeric values
results$rankOpponent <- as.numeric(results$rankOpponent)

----#win----
# creating a dummy variable for the result of the game, if a win =1
results$win <- ifelse(results$win == "W", 1, 0)

----#score----
# split the data by creating a dummy variable for the Walkover occurs = 1
results$walkover <- ifelse(results$score == "(W/O)", 1, 0)

# deleting the the W/O from the score cheat
results$score <- ifelse(grepl("(W/O)", results$score), NA, results$score)

#creating a dummy variable for the retirements, game which did not end = 1
results$retirement <- ifelse(grepl("RET", results$score), 1, 0)

#deleting the characters (RET) from the cell 
results$score <- gsub("\\(RET\\)", "", results$score)


----#eventPoint----
# convert the even points into a factor
results$eventPoints <- as.factor(results$eventPoints)

----#rank----
# convert the round of tournament into a factor
results$rank <- as.factor(results$rank)

----#priceMoney----
#first getting rid of the commas 
results$priceMoney <- gsub(",", "", results$priceMoney)

#separating the numrical value from the character 

# convert priceMoneyTotal to a tibble
results <- as_tibble(results)

# separate currency and value columns
results <- results %>% 
  separate(priceMoney, into = c("currency", "value"), sep = "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", remove = FALSE)

#rename the variables
results <- results %>% rename(currencyMoneyPrice = currency)
results <- results %>% rename(amountMoneyPrice = value)

#deleting the old value 
results$priceMoney <- NULL


# convert the amount Money Price into a numeric factor
results$amountMoneyPrice <- as.numeric(results$amountMoneyPrice)


# converting all into a american dollars currency
results$MoneyPriceInDollars <- ifelse(results$currencyMoneyPrice == "£", results$amountMoneyPrice * 1.2631,
                             ifelse(results$currencyMoneyPrice == "CHF", results$amountMoneyPrice * 1.1287,
                                    ifelse(results$currencyMoneyPrice == "A$", results$amountMoneyPrice * 0.6742,
                                           ifelse(results$currencyMoneyPrice == "€", results$amountMoneyPrice * 1.1045,
                                                  ifelse(results$currencyMoneyPrice == "A$", results$amountMoneyPrice * 1.3241,
                                                         ifelse(results$currencyMoneyPrice == "S$", results$amountMoneyPrice * 0.1446,
                                                                ifelse(results$currencyMoneyPrice == " ¥ ", results$amountMoneyPrice * 0.6742,
                                                                      results$amountMoneyPrice)))))))







