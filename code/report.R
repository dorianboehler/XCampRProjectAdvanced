##################################################
# Script to produce a report for a player of choice
##################################################

# 1. SET UP ------------------------------------------------------------------
# Clean the environment
remove(list = ls())

# Load the necessary packages
library(broom)
library(countrycode)
library(knitr)
library(rmarkdown)
library(rvest)
library(scales)
library(stringr)
library(tidyverse)
library(zoo)

# Set fixed variables
australienDollarToUSDollar <- 0.6761 # 06.05.2023
euroToUSDollar <- 1.1210 # 06.05.2023
poundToUSDollar <- 1.2639 # 06.05.2023

# Create functions
beautifulPercentages <- function(x) {
  output <- rep(NA, length(x))
  
  for(i in 1:length(x)) {
    if(!is.na(x[i]) & !is.nan(x[i])) {
      output[i] <- paste0(format(x[i] * 100, digits = 2, nsmall = 2), '%')
    }
  }
  
  return(output)
}

# Import the cleaned list of players
load('data/playersCleaned.RData')
View(players)

# 2. CHOOSE A PLAYER FROM THE LIST -------------------------------------------
# Copy and paste the player's link
playerLink <- '/en/players/cyril-vandermeersch/v0bc/' # !!!

# Extract the general information on the player
overview <- filter(players, link == playerLink)

remove(playerLink)

# Extract the player's name for later
playerName <- overview$name

# Check whether the player actually exists in the list
if(nrow(overview) != 1) {
  stop('The player does not exist in the list. Please make sure that you entered the link correctly.')
}

# 3. SCRAPE THE RESULTS OF THE PLAYER ----------------------------------------
# Download the website
website <- read_html(paste0('https://www.atptour.com', overview$link, 'player-activity?year=all&matchType=Singles'))

# Extract all relevant information and put them into a new data frame
results <- data.frame(matrix(nrow = 0, ncol = 16))
colnames(results) <- c('tournamentType', 'tournament', 'location', 'date', 'outdoor',
                       'surface', 'priceMoneyTotal', 'round', 'opponentRank', 'opponent',
                       'opponentLink', 'win', 'score', 'eventPoints', 'rank',
                       'priceMoney')

numberOfTournaments <- length(html_elements(website, xpath = '//*[@id="currentTabContent"]/div[3]/div'))

if(numberOfTournaments == 0) { # If the player has actually never played, do not continue.
  stop('This player has actually never played a match.')
}

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
  
  if(nrow(temp) > 0) { # Some tournaments do not display any match.
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
  }
  
  # Display the progress
  print(paste('Tournament', i, 'out of', numberOfTournaments, 'done.'))
}

if(nrow(results) == 0) { # If the player has actually never played, do not continue.
  stop('This player has actually never played a match.')
}

remove(temp, website, date, eventPoints, i, 
       location, numberOfTournaments, opponent, opponentLink, outdoor, 
       priceMoney, priceMoneyTotal, rank, opponentRank, round, 
       score, surface, tournament, tournamentType, win)

# 4. CLEAN THE RESULTS OF THE PLAYER -----------------------------------------
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
results$tournamentStart <- NA
results$tournamentEnd <- NA

for(i in 1:nrow(results)) {
  if(grepl('-', results$date[i])) {
    temp <- unlist(strsplit(results$date[i], '-', fixed = TRUE))
  } else {
    temp <- rep(results$date[i], 2)
  }
  
  results$tournamentStart[i] <- temp[1]
  results$tournamentEnd[i] <- temp[2]
}

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

results$priceMoneyTotalCurrency <- str_extract(results$priceMoneyTotal, '.*?(?=[:digit:])')
results[results$priceMoneyTotalCurrency == '', c('priceMoneyTotalValue', 'priceMoneyTotalCurrency')] <- NA
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

# opponent: Correct some inconsistency in the raw data
results$opponent[grepl('Bye', results$opponent, fixed = TRUE)] <- 'Bye'

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

# score: Create a dummy variable for (DEF) (default)
results$default <- NA

results$default[!is.na(results$score)] <- 0

results$default[grepl('(DEF)', results$score, fixed = TRUE)] <- 1

results$score <- gsub('(DEF)', '', results$score, fixed = TRUE)
results$score <- trimws(results$score)

# score: Create a dummy variable for (RET) (retirement)
results$retirement <- NA

results$retirement[!is.na(results$score)] <- 0

results$retirement[grepl('(RET)', results$score, fixed = TRUE)] <- 1

results$score <- gsub('(RET)', '', results$score, fixed = TRUE)
results$score <- trimws(results$score)

# score: Create a column for every set
results$score[grepl('-[0-9]{3,}?', results$score)] <- gsub('(-[0-9]{1,2})([0-9]{2})', '\\1 \\2', results$score[grepl('-[0-9]{3,}?', results$score)])

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
      } else { # In this unlikely case, we assume that the third number is wrong.
        results[[paste0('gamesWon', col)]][i] <- substr(results[[tolower(col)]][i], 1, 1)
        results[[paste0('gamesLost', col)]][i] <- substr(results[[tolower(col)]][i], 2, 2)
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

# eventPoints: Change 0 to NA under certain conditions
results <- results %>%
  group_by(tournament, date) %>%
  mutate(firstRoundLost = if_else(sum(win, na.rm = TRUE) > 0, 0, 1)) %>%
  ungroup()

results$eventPoints[results$eventPoints == '0' & results$firstRoundLost == 0] <- NA

# eventPoints: Change the data type to numeric
results$eventPoints <- as.numeric(results$eventPoints)

# rank: Change 0 to NA
results$rank[results$rank == '0'] <- NA

# rank: Change the data type to numeric
results$rank <- as.numeric(results$rank)

# priceMoney: Change 0 to NA under certain conditions
results$priceMoney[results$priceMoney == '$0' & results$firstRoundLost == 0] <- NA

# priceMoney: Separate the price money into the currency and value and change the data type to factor and numeric, respectively
results$priceMoney <- gsub(',', '', results$priceMoney, fixed = TRUE)

results$priceMoneyValue <- str_extract(results$priceMoney, '[:digit:]+')
results$priceMoneyValue <- as.numeric(results$priceMoneyValue)

results$priceMoneyCurrency <- str_extract(results$priceMoney, '.*?(?=[:digit:])')
results[results$priceMoneyCurrency == '', c('priceMoneyValue', 'priceMoneyCurrency')] <- NA
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
results$country.x <- countryname(results$country.x, 'ioc', warn = FALSE) # If some country name does not make sense, the function simply assigns NA.

results$country.x <- as.factor(results$country.x)

# Rearrange the columns
results <- results %>%
  select(tournament, tournamentType, tournamentCity = city, tournamentCountry = country.x, tournamentStart,
         tournamentEnd, outdoor, surface, priceMoneyTotalCurrency, priceMoneyTotalValue,
         priceMoneyTotalUSDollar, rank, eventPoints, priceMoneyCurrency, priceMoneyValue,
         priceMoneyUSDollar, round, opponent, opponentRank, opponentLink,
         opponentCountry = country.y, opponentBirthday = birthday, opponentTurnedPro = turnedPro, opponentWeight = weight, opponentHeight = height,
         opponentLefty = lefty, opponentOneHandedBackhand = oneHandedBackhand, win, walkover, default,
         retirement, numberOfSets, starts_with('games'))

# 5. ANALYSE THE RESULTS OF THE PLAYER ---------------------------------------
# Compute the total number of matches
numberOfMatches <- results %>%
  filter(opponent != 'Bye' & walkover == 0) %>%
  summarise(n()) %>%
  pull(1)

# Compute the highest rank achieved
if(!all(is.na(results$rank))) {
  highestRank <- min(results$rank, na.rm = TRUE)
} else {
  highestRank <- NA
}

# Compute the overall match winning percentage
winningPercentage <- results %>%
  filter(opponent != 'Bye' & walkover == 0) %>%
  summarise(sum(win) / n()) %>%
  pull(1)

winningPercentage <- beautifulPercentages(winningPercentage)

# Compute the match winning percentage outdoor and indoor
for(i in c(1, 2)) {
  name <- c('Indoor', 'Outdoor')[i]
  
  temp <- results %>%
    filter(opponent != 'Bye' & walkover == 0 &
             outdoor == (i - 1)) %>%
    summarise(sum(win) / n()) %>%
    pull(1)
  
  assign(paste0('winningPercentage', name), beautifulPercentages(temp))
}

remove(temp, i, name)

# Compute the match winning percentage on hard, clay, and grass court
for(i in c('Hard', 'Clay', 'Grass')) {
  temp <- results %>%
    filter(opponent != 'Bye' & walkover == 0 &
             surface == i) %>%
    summarise(sum(win) / n()) %>%
    pull(1)
  
  assign(paste0('winningPercentage', i), beautifulPercentages(temp))
}

remove(i, temp)

# Compute the match winning percentage against players with a one-handed and two-handed backhand
for(i in c(1, 2)) {
  name <- c('TwoHandedBackhand', 'OneHandedBackhand')[i]
  
  temp <- results %>%
    filter(opponent != 'Bye' & walkover == 0 &
             opponentOneHandedBackhand == (i - 1)) %>%
    summarise(sum(win) / n()) %>%
    pull(1)
  
  assign(paste0('winningPercentage', name), beautifulPercentages(temp))
}

remove(temp, i, name)

# Compute the match winning percentage against top ten players
winningPercentageTop10 <- results %>%
  filter(opponent != 'Bye' & walkover == 0 &
           opponentRank <= 10) %>%
  summarise(sum(win) / n()) %>%
  pull(1)

winningPercentageTop10 <- beautifulPercentages(winningPercentageTop10)

# Compute the winning percentage of first, second, third, fourth, and fifth sets
for(i in 1:5) {
  results[[paste0('set', i, 'Won')]] <- NA
}

for(i in 1:nrow(results)) {
  if(!is.na(results$win[i]) & !is.na(results$walkover[i]) & results$walkover[i] == 0) {
    if(results$default[i] == 0 & results$retirement[i] == 0) {
      for(j in 1:results$numberOfSets[i]) {
        results[i, paste0('set', j, 'Won')] <- as.numeric(results[i, paste0('gamesWonSet', j)] > results[i, paste0('gamesLostSet', j)])
      }
    } else if(results$default[i] == 1 | results$retirement[i] == 1) {
      if(results$numberOfSets[i] > 1) {
        for(j in 1:(results$numberOfSets[i] - 1)) {
          results[i, paste0('set', j, 'Won')] <- as.numeric(results[i, paste0('gamesWonSet', j)] > results[i, paste0('gamesLostSet', j)])
        }
      }
      
      if(results[i, paste0('gamesWonSet', results$numberOfSets[i])] %in% c(6, 7) |
         results[i, paste0('gamesLostSet', results$numberOfSets[i])] %in% c(6, 7)) {
        results[i, paste0('set', results$numberOfSets[i], 'Won')] <- as.numeric(results[i, paste0('gamesWonSet', results$numberOfSets[i])] > results[i, paste0('gamesLostSet', results$numberOfSets[i])])
      }
    } else {
      stop('Something unexpected happened.')
    }
  }
}

winningPercentageSets <- results %>%
  select('1' = set1Won, '2' = set2Won, '3' = set3Won, '4' = set4Won, '5' = set5Won) %>%
  gather(key = 'set', value = 'won') %>%
  filter(!is.na(won)) %>%
  group_by(set) %>%
  summarise(numberOfSetsPlayed = n(),
            numberOfSetsWon = sum(won),
            winningPercentage = beautifulPercentages(sum(won) / n()))

plotWinningPercentageSets <- ggplot(winningPercentageSets, aes(x = set)) +
  geom_bar(aes(y = numberOfSetsPlayed), stat = 'identity', fill = 'grey') +
  geom_bar(aes(y = numberOfSetsWon), stat = 'identity', fill = 'darkgreen') +
  geom_text(aes(y = 0, label = winningPercentage), vjust = -4.5) +
  labs(title = 'Number of Sets Played and Won',
       x = 'Set',
       y = NULL) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.background = element_rect(colour = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5))

remove(winningPercentageSets, i, j)

# Assign a month and year to every match, using the starting and ending date of the tournament
results$monthYear <- as.yearmon(results$tournamentStart)
results$monthYear[ceiling_date(results$tournamentStart, unit = 'month') - results$tournamentStart <
                    results$tournamentEnd - floor_date(results$tournamentEnd, unit = 'month')] <- as.yearmon(results$tournamentEnd[ceiling_date(results$tournamentStart, unit = 'month') - results$tournamentStart <
                                                                                                                                     results$tournamentEnd - floor_date(results$tournamentEnd, unit = 'month')])
# Compute how the ratio between the number of wins and matches changed over time
matchesTime <- results %>%
  filter(opponent != 'Bye' & walkover == 0) %>%
  group_by(monthYear) %>%
  summarise(matches = n(),
            wins = sum(win))

monthYear <- as.yearmon(seq(as.Date(matchesTime$monthYear[1]), as.Date(matchesTime$monthYear[nrow(matchesTime)]), by = 'month'))
matchesTime <- left_join(tibble(monthYear), matchesTime, by = 'monthYear')
matchesTime$matches[is.na(matchesTime$matches)] <- 0
matchesTime$wins[is.na(matchesTime$wins)] <- 0

matchesTime <- matchesTime %>%
  mutate(matchesSum4 = rollsum(matches, k = 4, fill = NA, align = 'right'),
         winsSum4 = rollsum(wins, k = 4, fill = NA, align = 'right'),
         ratio4Matches = if_else(matchesSum4 > 0, winsSum4 / matchesSum4 * 100, NA))

# Compute how the ratio between the number of sets won and played changed over time
results[results$opponent != 'Bye' & !is.na(results$walkover) & results$walkover == 0, 'setsWon'] <- rowSums(results[results$opponent != 'Bye' & !is.na(results$walkover) & results$walkover == 0, paste0(rep('set', 5), 1:5, rep('Won', 5))], na.rm = TRUE)
results[results$opponent != 'Bye' & !is.na(results$walkover) & results$walkover == 0, 'setsLost'] <- rowSums(results[results$opponent != 'Bye' & !is.na(results$walkover) & results$walkover == 0, paste0(rep('set', 5), 1:5, rep('Won', 5))] - 1, na.rm = TRUE) * -1

setsTime <- results %>%
  filter(opponent != 'Bye' & walkover == 0) %>%
  group_by(monthYear) %>%
  summarise(setsPlayed = sum(setsWon + setsLost),
            setsWon = sum(setsWon))

setsTime <- left_join(tibble(monthYear), setsTime, by = 'monthYear')
setsTime$setsPlayed[is.na(setsTime$setsPlayed)] <- 0
setsTime$setsWon[is.na(setsTime$setsWon)] <- 0

setsTime <- setsTime %>%
  mutate(setsPlayedSum4 = rollsum(setsPlayed, k = 4, fill = NA, align = 'right'),
         setsWonSum4 = rollsum(setsWon, k = 4, fill = NA, align = 'right'),
         ratio4Sets = if_else(setsPlayedSum4 > 0, setsWonSum4 / setsPlayedSum4 * 100, NA))

# Compute how the ratio between the number of games won and played changed over time
results <- cbind(results, gamesWon = rowSums(results[, paste0(rep('gamesWonSet', 5), 1:5)], na.rm = TRUE))
results <- cbind(results, gamesLost = rowSums(results[, paste0(rep('gamesLostSet', 5), 1:5)], na.rm = TRUE))                                                     
for(i in 1:nrow(results)) {
  if(results$gamesWon[i] == 0 & results$gamesLost[i] == 0) {
    results$gamesWon[i] <- NA
    results$gamesLost[i] <- NA
  }
}

gamesTime <- results %>%
  filter(opponent != 'Bye' & walkover == 0) %>%
  group_by(monthYear) %>%
  summarise(gamesPlayed = sum(gamesWon + gamesLost),
            gamesWon = sum(gamesWon))

gamesTime <- left_join(tibble(monthYear), gamesTime, by = 'monthYear')
gamesTime$gamesPlayed[is.na(gamesTime$gamesPlayed)] <- 0
gamesTime$gamesWon[is.na(gamesTime$gamesWon)] <- 0

gamesTime <- gamesTime %>%
  mutate(gamesPlayedSum4 = rollsum(gamesPlayed, k = 4, fill = NA, align = 'right'),
         gamesWonSum4 = rollsum(gamesWon, k = 4, fill = NA, align = 'right'),
         ratio4Games = if_else(gamesPlayedSum4 > 0, gamesWonSum4 / gamesPlayedSum4 * 100, NA))

remove(i, monthYear)

# Create a plot showing these ratios
time <- reduce(list(matchesTime, setsTime, gamesTime), full_join, by = 'monthYear')

time <- time %>%
  select(monthYear, starts_with('ratio4')) %>%
  gather(key = 'scoreLevel', value = 'winningPercentage', starts_with('ratio4'))

for(level in c('Matches', 'Sets', 'Games')) {
  time$scoreLevel[time$scoreLevel == paste0('ratio4', level)] <- level
}

plotRatio4 <- ggplot(time, aes(x = monthYear, y = winningPercentage, colour = scoreLevel)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(na.rm = TRUE, se = FALSE) +
  scale_colour_brewer(palette = 'Set1') +
  ylim(0, 100) +
  labs(title = 'Percentage of Matches, Sets, and Games Won Over Time',
       x = NULL,
       y = 'Winning Percentage',
       colour = 'Score Level') +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = 'top',
        plot.background = element_rect(colour = 'black'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5))

remove(gamesTime, matchesTime, setsTime, time, level)

# Compute the winning percentage of best-of-three and best-of-five matches
results$bestOfFive <- NA

for(i in 1:nrow(results)) {
  if(results$opponent[i] != 'Bye' & !is.na(results$walkover[i]) & results$walkover[i] == 0 & results$retirement[i] == 0) {
    temp <- max(results$setsWon[i], results$setsLost[i])
    
    results$bestOfFive[i] <- ifelse(temp == 3, 1, 0)
  }
}

for(i in c(1, 2)) {
  name <- c('BestOfThree', 'BestOfFive')[i]
  
  temp <- results %>%
    filter(opponent != 'Bye' & walkover == 0 &
             bestOfFive == (i - 1)) %>%
    summarise(sum(win) / n()) %>%
    pull(1)
  
  assign(paste0('winningPercentage', name), beautifulPercentages(temp))
}

remove(temp, i, name)

# Compute the match winning percentages at the grand slams
winningPercentageGrandSlams <- results %>%
  filter(opponent != 'Bye' & walkover == 0 &
           tournamentType == 'Grand Slam') %>%
  group_by(tournament) %>%
  summarise(winningPercentage = sum(win) / n())

if(!all(c('Australian Open', 'Roland Garros', 'US Open', 'Wimbledon') %in% winningPercentageGrandSlams$tournament)) {
  missingGrandSlams <- setdiff(c('Australian Open', 'Roland Garros', 'US Open', 'Wimbledon'),
                               winningPercentageGrandSlams$tournament)
  
  winningPercentageGrandSlams <- rbind(winningPercentageGrandSlams,
                                       data.frame(tournament = missingGrandSlams, winningPercentage = NA))
  
  remove(missingGrandSlams)
}

winningPercentageGrandSlams$winningPercentage <- beautifulPercentages(winningPercentageGrandSlams$winningPercentage)

# Compute the total price money
priceMoneyUSDollarTotal <- results %>%
  select(tournament, tournamentStart, tournamentEnd, priceMoneyUSDollar) %>%
  distinct() %>%
  summarise(priceMoneyUSDollarTotal = sum(priceMoneyUSDollar, na.rm = TRUE))

priceMoneyUSDollarTotal <- paste0(format(priceMoneyUSDollarTotal$priceMoneyUSDollarTotal, big.mark = '\''), '$')

# Compute the total price money per year and produce a beautiful bar plot
priceMoneyUSDollarTotalYearly <- results %>%
  select(tournament, tournamentStart, tournamentEnd, priceMoneyUSDollar, monthYear) %>%
  mutate(year = year(monthYear)) %>%
  distinct() %>%
  group_by(year) %>%
  summarise(priceMoneyUSDollarTotalYearly = sum(priceMoneyUSDollar, na.rm = TRUE)) %>%
  arrange(year) %>%
  mutate(cumSum = cumsum(priceMoneyUSDollarTotalYearly),
         stack = cumsum(priceMoneyUSDollarTotalYearly) - priceMoneyUSDollarTotalYearly)

temp <- priceMoneyUSDollarTotalYearly %>%
  select(-cumSum) %>%
  gather('key', 'value', -year)
temp$key <- factor(temp$key, levels = c('priceMoneyUSDollarTotalYearly', 'stack'))

plotPriceMoneyUSDollarTotalYearly <- ggplot() +
  geom_bar(aes(x = year, y = value / 1000, alpha = key),
           temp,
           stat = 'identity',
           fill = 'yellow') +
  scale_alpha_discrete(range = c(1, 0)) +
  geom_text(aes(x = year, y = (stack + priceMoneyUSDollarTotalYearly / 2) / 1000, label = comma(priceMoneyUSDollarTotalYearly / 1000, big.mark = '\'')),
            priceMoneyUSDollarTotalYearly,
            size = 2.5,
            check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(min(priceMoneyUSDollarTotalYearly$year), max(priceMoneyUSDollarTotalYearly$year), 3)) +
  scale_y_continuous(labels = comma_format(big.mark = '\'')) +
  labs(title = 'Total Price Money per Year',
       x = 'Year',
       y = 'Total Price Money (K$)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = 'none',
        plot.background = element_rect(colour = 'black'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5))

remove(priceMoneyUSDollarTotalYearly, temp)

# If the player played more than 100 matches (otherwise the results do not make any sense), 
# let us estimate the effect of some variables on the winning probability, using a linear probability model
if(numberOfMatches > 100) {
  # First, let us create some additional variables
  results$tournamentCountryHome <- ifelse(as.character(results$tournamentCountry) == as.character(overview$country), 1, 0)
  results$priceMoneyTotalUSDollarM <- results$priceMoneyTotalUSDollar / 1000000
  results$opponentAge <- results$monthYear - as.yearmon(results$opponentBirthday)
  
  # Second, let us specify the base groups 
  i <- 1
  condition <- FALSE
  while(!condition) {
    baseGroup <- c('Hard', 'Clay', 'Grass', 'Carpet')[i]
    
    if(baseGroup %in% results$surface) {
      results$surface <- relevel(results$surface, ref = baseGroup)
      
      condition <- TRUE
    } else {
      i <- i + 1
    }
    
    if(i > 4) {
      break
    }
  }
  
  remove(baseGroup, condition, i)
  
  # Third, let us run the regression
  temp <- results %>%
    filter(opponent != 'Bye' & walkover == 0) %>%
    select(win, tournamentCountryHome, outdoor, surface, priceMoneyTotalUSDollarM,
           opponentRank, opponentAge, opponentHeight, opponentLefty, opponentOneHandedBackhand,
           bestOfFive)
  
  try(
    expr = {
      lpm <- lm(win ~ ., temp)
      
      enoughSurfaces <- TRUE},
    silent = TRUE
  )
  
  if(!exists('lpm')) {
    temp <- select(temp, -surface)
    
    lpm <- lm(win ~ ., temp)
    
    enoughSurfaces <- FALSE
  }
  
  remove(temp)
  
  # Finally, let us create a beautiful table with the results
  rSquared <- round(summary(lpm)$r.squared, digits = 2)
  
  lpm <- tidy(lpm)
  
  lpm$term[lpm$term == '(Intercept)'] <- 'Intercept'
  lpm$term[lpm$term == 'tournamentCountryHome'] <- 'Tournament at Home'
  lpm$term[lpm$term == 'outdoor'] <- 'Outdoor'
  lpm$term[grepl('surface', lpm$term, fixed = TRUE)] <- paste('Surface:', str_replace(lpm$term[grepl('surface', lpm$term, fixed = TRUE)], 'surface', ''))
  lpm$term[lpm$term == 'priceMoneyTotalUSDollarM'] <- 'Total Price Money (m$)'
  lpm$term[lpm$term == 'opponentRank'] <- 'Opponent: Rank'
  lpm$term[lpm$term == 'opponentAge'] <- 'Opponent: Age'
  lpm$term[lpm$term == 'opponentHeight'] <- 'Opponent: Height'
  lpm$term[lpm$term == 'opponentLefty'] <- 'Opponent: Lefty'
  lpm$term[lpm$term == 'opponentOneHandedBackhand'] <- 'Opponent: One Handed Backhand'
  lpm$term[lpm$term == 'bestOfFive'] <- 'Best of Five'
}

# Compute how many matches the player won with fewer games than the opponent
unfairWins <- results %>%
  filter(win == 1 & walkover == 0 & retirement == 0 & gamesWon < gamesLost) %>%
  select(Date = monthYear, Tournament = tournament, 'Tournament Country' = tournamentCountry, Surface = surface, Round = round, 
         Opponent = opponent, 'Opponent Country' = opponentCountry, starts_with('gamesWonSet'), starts_with('gamesLostSet'), 'Games Won' = gamesWon, 'Games Lost' = gamesLost) %>%
  mutate(Result = paste0(gamesWonSet1, ':', gamesLostSet1, ', ', 
                         gamesWonSet2, ':', gamesLostSet2, ', ',
                         gamesWonSet3, ':', gamesLostSet3, ', ',
                         gamesWonSet4, ':', gamesLostSet4, ', ',
                         gamesWonSet5, ':', gamesLostSet5), .keep = 'unused', .before = 'Games Won')

unfairWins$Result <- str_replace_all(unfairWins$Result, fixed(', NA:NA'), '')

numberOfUnfairWins <- nrow(unfairWins)

# Analyse the finals and tournament wins
finals <- results %>%
  select(tournamentEnd, round, win) %>%
  filter(round == 'Finals') %>%
  arrange(tournamentEnd)

finals$cumSum <- cumsum(finals$win)

numberOfFinals <- nrow(finals)

if(numberOfFinals > 0) {
  winningPercentageFinals <- beautifulPercentages(mean(finals$win, na.rm = TRUE))
  
  plotFinals <- ggplot(mapping = aes(x = tournamentEnd, y = cumSum)) +
    geom_line(data = finals, colour = 'blue') +
    geom_point(data = filter(finals, win == 0), colour = 'black', shape = 4, size = 3) +
    ylim(0, numberOfFinals) +
    labs(x = NULL,
         y = 'Cumulative Number of Tournament Wins',
         title = 'Tournament Wins',
         subtitle = paste('Winning Percentage in Finals:', winningPercentageFinals)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.background = element_rect(colour = 'black'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(size = 14, hjust = 0.5))
  
  remove(winningPercentageFinals)
}

remove(finals)

# 6. CREATE A REPORT ---------------------------------------------------------
# Define the name of the report
name <- paste0('report', gsub(' ', '', playerName, fixed = TRUE), gsub(' ', '', as.yearmon(Sys.Date()), fixed = TRUE))

# Finally create the report
render('code/report.Rmd', output_format = 'pdf_document', output_file = paste0('../reports/', name, '.pdf'))
