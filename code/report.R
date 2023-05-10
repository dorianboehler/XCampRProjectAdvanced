##################################################
# Script to produce a report for a player of choice
##################################################

# SET UP ------------------------------------------------------------------
# Clean the environment
remove(list = ls())

# Load the necessary packages
library(countrycode)
library(rvest)
library(stringr)
library(tidyverse)

# Set fixed variables
australienDollarToUSDollar <- 0.6761 # 06.05.2023
euroToUSDollar <- 1.1210 # 06.05.2023
poundToUSDollar <- 1.2639 # 06.05.2023

# Import the cleaned list of players
load('data/playersCleaned.RData')

# CHOOSE A PLAYER FROM THE LIST -------------------------------------------
# Enter the player's name and birthday
playerName <- 'Roger Federer'
playerBirthday <- as.Date('1981-08-08')

# Extract the general information on the player
overview <- filter(players, name == playerName & birthday == playerBirthday)

# Check whether the player actually exists in the list
if(nrow(overview) != 1) {
  stop('The player does not exist in the list. Please make sure that you entered the name and birthday correctly.')
}

# SCRAPE THE RESULTS OF THE PLAYER ----------------------------------------
# Download the website
website <- read_html(paste0('https://www.atptour.com', overview$link, 'player-activity?year=all'))

# Extract all relevant information and put them into a new data frame
results <- data.frame(matrix(nrow = 0, ncol = 16))
colnames(results) <- c('tournamentType', 'tournament', 'location', 'date', 'outdoor',
                       'surface', 'priceMoneyTotal', 'round', 'opponentRank', 'opponent',
                       'opponentLink', 'win', 'score', 'eventPoints', 'rank',
                       'priceMoney')

numberOfTournaments <- length(html_elements(website, xpath = '//*[@id="currentTabContent"]/div[3]/div'))

for(i in 1:numberOfTournaments) {
  tournamentType <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[1]/img')) %>%
    html_attr('alt')
  if(tournamentType == "") {
    tournamentType <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[1]/img')) %>%
      html_attr('src')
    
    if(grepl('grandslam', tournamentType, fixed = TRUE)) {
      tournamentType <- 'Grand Slam'
    } else if(grepl('itf', tournamentType, fixed = TRUE)) {
      tournamentType <- 'ITF'
    }
  } # Some tournament types do not have an attribute alt so that we need to extract the information differently.
  
  if(length(html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/a'))) > 0) {
    tournament <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/a')) %>%
      html_text()
    tournament <- str_squish(gsub('\\r\\n', '', tournament))
    
    location <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span[1]')) %>%
      html_text()
    location <- str_squish(gsub('\\r\\n', '', location))
    
    date <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span[2]')) %>%
      html_text()
    date <- str_squish(gsub('\\r\\n', '', date))
  } else if(length(html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span'))) == 3) {
    tournament <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span[1]')) %>%
      html_text
    tournament <- str_squish(gsub('\\r\\n', '', tournament))
    
    location <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span[2]')) %>%
      html_text()
    location <- str_squish(gsub('\\r\\n', '', location))
    
    date <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[2]/span[3]')) %>%
      html_text()
    date <- str_squish(gsub('\\r\\n', '', date))
  } else {
    stop('Something unexpected happened')
  } # There are two different xpaths to the tournament, location, and date.
  
  temp <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[3]/table/tbody/tr/td[2]/div[2]/div')) %>%
    html_text()
  temp <- str_squish(gsub('\\r\\n', '', temp))
  outdoor <- substr(temp, 1, 1)
  
  surface <- substr(temp, 3, nchar(temp))
  
  priceMoneyTotal <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[3]/table/tbody/tr/td[3]/div[2]/div/span')) %>%
    html_text()
  priceMoneyTotal <- str_squish(gsub('\\r\\n', '', priceMoneyTotal))
  
  temp <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[2]/tbody')) %>%
    html_table()
  temp <- as.data.frame(temp)
  
  if(nrow(temp) > 0) {
    round <- temp$X1
    
    opponentRank <- temp$X2
    
    opponent <- temp$X3
    
    win <- temp$X4
    
    score <- temp$X5
    
    opponentLink <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[2]/tbody/tr/td[3]/div[2]/a')) %>%
      html_attr('href')
    opponentLink <- substr(opponentLink, 1, nchar(opponentLink) - 8)
    opponentLink <- append(opponentLink, rep(NA, sum(opponent == 'Bye')))
    
    temp <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/div')) %>%
      html_text()
    
    eventPoints <- str_extract(temp, '(?<=This Event Points: ).*?(?=,)')
    
    rank <- str_extract(temp, '(?<=ATP Ranking: ).*?(?=,)')
    
    priceMoney <- str_extract(temp, '(?<=Prize Money: ).*')
    
    temp <- data.frame(tournamentType = tournamentType,
                       tournament = tournament,
                       location = location,
                       date = date,
                       outdoor = outdoor,
                       surface = surface,
                       priceMoneyTotal = priceMoneyTotal,
                       round = round,
                       opponentRank = opponentRank,
                       opponent = opponent,
                       opponentLink = opponentLink,
                       win = win,
                       score = score,
                       eventPoints = eventPoints,
                       rank = rank,
                       priceMoney)
    
    results <- rbind(results, temp)
  } # Some tournaments do not display any match.
  
  # Display the progress
  print(paste('Tournament', i, 'out of', numberOfTournaments, 'done.'))
}

remove(temp, website, date, eventPoints, i, 
       location, numberOfTournaments, opponent, opponentLink, outdoor, 
       priceMoney, priceMoneyTotal, rank, opponentRank, round, 
       score, surface, tournament, tournamentType, win)

# CLEAN THE RESULTS OF THE PLAYER -----------------------------------------
# tournamentType: Change the data type to factor
results$tournamentType <- as.factor(results$tournamentType)

# tournament: If the player played explicitly for his country, it was in the Davis Cup
results$tournament[grepl(overview$country, results$tournament, fixed = TRUE)] <- 'Davis Cup'

# location: Separate the location into the city and country
results$city <- NA
results$country <- NA

for(i in 1:nrow(results)) {
  location <- results$location[i]
  
  if(grepl(',', location, fixed = TRUE)) {
    results$city[i] <- str_extract(location, '.*?(?=,)')
    results$country[i] <- gsub('.*, ', '', location)
  } else {
    if(is.na(countryname(location, warn = FALSE))) {
      results$city[i] <- location
    } else {
      results$country[i] <- location
    }
  }
}

remove(i, location)

# date: Separate the date into the start and end and change the data type to date
results <- separate(results, date, c('tournamentStart', 'tournamentEnd'), sep = '-')

for(col in c('tournamentStart', 'tournamentEnd')) {
  results[[col]] <- str_squish(results[[col]])
  
  results[[col]] <- as.Date(results[[col]], format = '%Y.%m.%d')
}

remove(col)

# outdoor: Create a dummy variable
results$outdoor <- ifelse(results$outdoor == 'O', 1, 0)

# surface: Change the data type to factor
results$surface <- as.factor(results$surface)

# priceMoneyTotal: Separate the total price money into the currency and value and change the data type to factor and numeric, respectively
results$priceMoneyTotal <- gsub(',', '', results$priceMoneyTotal, fixed = TRUE)

results$priceMoneyTotalValue <- str_extract(results$priceMoneyTotal, '[:digit:]+')
results$priceMoneyTotalValue <- as.numeric(results$priceMoneyTotalValue)

results$priceMoneyTotalCurrency <- str_extract(results$priceMoneyTotal, '.+?(?=[:digit:])')
results$priceMoneyTotalCurrency <- as.factor(results$priceMoneyTotalCurrency)

# priceMoneyTotal: Convert the total price money into US dollar
results$priceMoneyTotalUSDollar <- NA

for(i in 1:nrow(results)) {
  currency <- results$priceMoneyTotalCurrency[i]
  value <- results$priceMoneyTotalValue[i]
  
  if(currency == 'A$' & !is.na(currency)) {
    results$priceMoneyTotalUSDollar[i] <- value * australienDollarToUSDollar
  } else if(currency == '€' & !is.na(currency)) {
    results$priceMoneyTotalUSDollar[i] <- value * euroToUSDollar
  } else if(currency == '£' & !is.na(currency)) {
    results$priceMoneyTotalUSDollar[i] <- value * poundToUSDollar
  } else if(currency == '$' & !is.na(currency)) {
    results$priceMoneyTotalUSDollar[i] <- value
  } else {
    if(!is.na(currency)) {
      stop('There is a currency that we did not account for.')
    }
  }
}

remove(currency, i, value)

# round: Change the data type to factor
results$round <- as.factor(results$round)

# opponentRank: Change the data type to numeric
results$opponentRank[results$opponentRank == '-' & !is.na(results$opponentRank)] <- NA

results$opponentRank <- as.numeric(results$opponentRank)

# opponentLink: Change the link of the opponent in the same way as in the list of players (lines 96 to 100 in players.R)
results$opponentLink <- gsub('"', '', results$opponentLink, fixed = TRUE)

results$opponentLink <- gsub('./', '/', results$opponentLink, fixed = TRUE)

# win: Create a dummy variable
results$win[results$win == '' & !is.na(results$win)] <- NA

results$win <- ifelse(results$win == 'W', 1, 0)

# score: Replace empty strings with NA
results$score[results$score == '' & !is.na(results$score)] <- NA

# score: Create a dummy variable for (W/O) (walkover)
results$walkover <- NA

results$walkover[!is.na(results$score)] <- 0

results$walkover[results$score == '(W/O)' & !is.na(results$score)] <- 1

results$score[results$score == '(W/O)' & !is.na(results$score)] <- NA

# score: Create a dummy variable for (RET) (retirement)
results$retirement <- NA

results$retirement[!is.na(results$score)] <- 0

results$retirement[grepl('(RET)', results$score, fixed = TRUE)] <- 1

results$score <- gsub('(RET)', '', results$score, fixed = TRUE)
results$score <- trimws(results$score)

# score: Create a column for every set
results <- separate(results, score, c('set1', 'set2', 'set3', 'set4', 'set5'), sep = ' ', fill = 'right')

# score: Remove the number of points in a tiebreak from the score
for(col in c('set1', 'set2', 'set3', 'set4', 'set5')) {
  results[[col]][grepl('^(76|67|43|34|10|01)[0-9]+', results[[col]])] <- substr(results[[col]][grepl('^(76|67|43|34|10|01)[0-9]+', results[[col]])], 1, 2)
}

remove(col)

# score: Separate the score into the player's and opponent's number of games and change the data type to numeric
for(col in c('Set1', 'Set2', 'Set3', 'Set4', 'Set5')) {
  results[[paste0('gamesWon', col)]] <- NA
  results[[paste0('gamesLost', col)]] <- NA
}

for(i in 1:nrow(results)) {
  for(col in c('Set1', 'Set2', 'Set3', 'Set4', 'Set5')) {
    if(nchar(results[[tolower(col)]][i]) == 2 & !is.na(results[[tolower(col)]][i])) {
      results[[paste0('gamesWon', col)]][i] <- substr(results[[tolower(col)]][i], 1, 1)
      results[[paste0('gamesLost', col)]][i] <- substr(results[[tolower(col)]][i], 2, 2)
    } else if(!is.na(results[[tolower(col)]][i])) {
      if(grepl('-', results[[tolower(col)]][i], fixed = TRUE)) {
        results[[paste0('gamesWon', col)]][i] <- unlist(strsplit(results[[tolower(col)]][i], '-', fixed = TRUE))[1]
        results[[paste0('gamesLost', col)]][i] <- unlist(strsplit(results[[tolower(col)]][i], '-', fixed = TRUE))[2]
      } else {
        stop('There is a result pattern that we did not account for.')
      }
    }
  }
}
      
for(col in c('Set1', 'Set2', 'Set3', 'Set4', 'Set5')) {
  results[[paste0('gamesWon', col)]] <- as.numeric(results[[paste0('gamesWon', col)]])
  results[[paste0('gamesLost', col)]] <- as.numeric(results[[paste0('gamesLost', col)]])
}

remove(col, i)      

# score: Compute the number of sets
results$numberOfSets <- rowSums(!is.na(results[, c('set1', 'set2', 'set3', 'set4', 'set5')]))

results$numberOfSets[results$numberOfSets == 0] <- NA

# eventPoints: Change the data type to numeric
results$eventPoints <- as.numeric(results$eventPoints)

# rank: Change the data type to numeric
results$rank <- as.numeric(results$rank)

# priceMoney: Separate the price money into the currency and value and change the data type to factor and numeric, respectively
results$priceMoney <- gsub(',', '', results$priceMoney, fixed = TRUE)

results$priceMoneyValue <- str_extract(results$priceMoney, '[:digit:]+')
results$priceMoneyValue <- as.numeric(results$priceMoneyValue)

results$priceMoneyCurrency <- str_extract(results$priceMoney, '.+?(?=[:digit:])')
results$priceMoneyCurrency <- as.factor(results$priceMoneyCurrency)

# priceMoney: Convert the price money into US dollar
results$priceMoneyUSDollar <- NA

for(i in 1:nrow(results)) {
  currency <- results$priceMoneyCurrency[i]
  value <- results$priceMoneyValue[i]
  
  if(currency == 'A$' & !is.na(currency)) {
    results$priceMoneyUSDollar[i] <- value * australienDollarToUSDollar
  } else if(currency == '€' & !is.na(currency)) {
    results$priceMoneyUSDollar[i] <- value * euroToUSDollar
  } else if(currency == '£' & !is.na(currency)) {
    results$priceMoneyUSDollar[i] <- value * poundToUSDollar
  } else if(currency == '$' & !is.na(currency)) {
    results$priceMoneyUSDollar[i] <- value
  } else {
    if(!is.na(currency)) {
      stop('There is a currency that we did not account for.')
    }
  }
}

remove(australienDollarToUSDollar, currency, euroToUSDollar, i, poundToUSDollar, value)

# Add the general information on the opponents
results <- left_join(results, players, by = c('opponentLink' = 'link'))

# Harmonise the countries
results$country.x <- countryname(results$country.x, 'ioc')

results$country.x <- as.factor(results$country.x)

# Rearrange the columns
results <- results %>%
  select(tournament, tournamentType, tournamentCity = city, tournamentCountry = country.x, tournamentStart,
         tournamentEnd, outdoor, surface, priceMoneyTotalCurrency, priceMoneyTotalValue,
         priceMoneyTotalUSDollar, rank, eventPoints, priceMoneyCurrency, priceMoneyValue,
         priceMoneyUSDollar, round, opponent, opponentRank, opponentLink,
         opponentCountry = country.y, opponentBirthday = birthday, opponentTurnedPro = turnedPro, opponentWeight = weight, opponentHeight = height,
         opponentLefty = lefty, opponentOneHandedBackhand = oneHandedBackhand, win, walkover, retirement,
         numberOfSets, starts_with('games'))

# ANALYSE THE RESULTS OF THE PLAYER ---------------------------------------

#load the necessary packages 
library(dplyr)
library(ggplot2)
library(lubridate)

#general information about the player 


# Filter the dataset to include only rows where the 'name' variable matches the input player name
informationPlayer <- subset(players, name == playerName, select = -link)




# how much money the player has earned 

newTable <- results[, c("tournamentStart", "priceMoneyUSDollar", "tournament")]
newTable <- unique(newTable)
totalMoneyEarned <- sum(newTable$priceMoneyUSDollar)


# at which age the player earned the most 

newTable1 <- results[, c("tournamentStart", "priceMoneyUSDollar")]
newTable1 <- unique(newTable1)

# Convert playerBirthday to date format if it is not already
if (!is.Date(playerBirthday)) {
  playerBirthday <- as.Date(playerBirthday, format = "%m/%d/%Y")
}

# Create new variable agePlayer in newTable1

newTable1$agePlayer <- as.numeric(difftime(newTable1$tournamentStart, playerBirthday, units = "days")) / 365.25
newTable1$agePlayer <- floor(as.integer(newTable1$agePlayer))

#plot age against earnings
ggplot1 <- ggplot(newTable1, aes(x = agePlayer, y = priceMoneyUSDollar/10000)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = 35, y = 230, 
           label = paste0("Slope: ", round(coef(lm(priceMoneyUSDollar ~ agePlayer, data = newTable1))[2], 3),
                          "\nIntercept: ", round(coef(lm(priceMoneyUSDollar ~ agePlayer, data = newTable1))[1], 3))) +
  scale_y_continuous(labels = scales::comma_format(scale = 1), name = "Price Money (x10,000 USD)") +
  labs(x = "Age", tag = "") +
  ggtitle("Price Money vs. Age") +
  theme(plot.tag.position = c(0.7, 0.8))




# Aggregate prize money by age
ageEarnings <- aggregate(priceMoneyUSDollar ~ agePlayer, newTable1, sum)

# Find the age at which the player earned the most
maxAge <- ageEarnings[which.max(ageEarnings$priceMoneyUSDollar), "agePlayer"]
maxEarnings <- max(ageEarnings$priceMoneyUSDollar)


#in which country has the player played the most 

newTable2 <- results[, c("tournamentStart", "tournamentCountry")]
newTable2 <- unique(newTable2)
countryMostPlayed <- results %>%
  count(tournamentCountry) %>%
  top_n(1, n)
MostCountryPlayedIn <- countryMostPlayed$tournamentCountry

print(paste0('The country in which ', playerName, ' has played the most is:', countryMostPlayed$tournamentCountry))


# percentage win on outdoor and indoor 

newTable3 <- results[, c("tournamentStart", "win", "outdoor")]
newTable3 <- na.omit(newTable3)


#outdoor
percentage1 <- newTable3 %>% 
  filter(outdoor == 1) %>% 
  summarise(percentage = mean(win == 1)) %>% 
  pull(percentage)

percentage1 <- round(percentage1, 4)*100
percentage1
#indoor
percentage2 <- newTable3 %>% 
  filter(outdoor == 0) %>% 
  summarise(percentage = mean(win == 1)) %>% 
  pull(percentage)
percentage2 <- round(percentage2, 4)*100
percentage2



#histograms about the winning percent in each set played 
  for (Z in 1:5) {
    tableSetZ <- results[, c(paste0("gamesLostSet", Z), paste0("gamesWonSet", Z))]
    tableSetZ <- tableSetZ[complete.cases(tableSetZ), ]
    tableSetZ$setZWin <- ifelse(tableSetZ[[paste0("gamesWonSet", Z)]] > tableSetZ[[paste0("gamesLostSet", Z)]], 1, 0)
    
    assign(paste0("barSet", Z), ggplot(tableSetZ, aes(x = factor(setZWin))) +
             geom_bar(fill = "blue") +
             labs(title = paste0("Set ", Z, " Win Distribution"), x = paste0("Set ", Z, " Win"), y = "Count"))
    
    assign(paste0("set", Z, "WinPercentage"), round(mean(tableSetZ$setZWin == 1), 4) * 100)
  }


require(gridExtra)
grid.arrange(barSet1, barSet2, barSet3, barSet4, barSet5,  ncol=5)



# the highest rank achived and for how many weeks in total, and the maximum consequently  

newTable5 <- results[, c("tournamentStart", "rank")]
newTable5 <- na.omit(newTable5)


# Find the lowest rank and the number of times it appears
lowestRank <- min(newTable5$rank)
lowestRankCount <- sum(newTable5$rank == lowestRank)

# Find the maximum number of consecutive times the lowest rank appears
consecutiveCount <- 0
maxConsecutiveCount <- 0
for (r in newTable5$rank) {
  if (r == lowestRank) {
    consecutiveCount <- consecutiveCount + 1
    if (consecutiveCount > maxConsecutiveCount) {
      maxConsecutiveCount <- consecutiveCount
    }
  } else {
    consecutiveCount <- 0
  }
}
