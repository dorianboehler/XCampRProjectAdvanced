##################################################
# Script to scrape the data from atptour.com
##################################################

# SET UP ------------------------------------------------------------------
# Clean the environment
remove(list = ls())

# Load the necessary packages
library(insight)
library(rvest)
library(stringr)
library(tidyverse)
library(xml2)

# CREATE A DATA FRAME WITH THE LINKS OF THE PLAYERS -----------------------
# If we have a data frame with the link of every singles player (since 1973),
# the user only needs to type in the name of the player that he is interested in and run the script.
# We first tried to use RSelenium (and directly search for the player) 
# but atptour.com successfully used bot detection.

# We need to download a list of players for every year. Unfortunately,
# we need to provide a date from the drop-down menu in the link to the list of players.
# For this reason, we first scrape these dates.

# Download the website
website <- read_html('https://www.atptour.com/en/rankings/singles')

# Loop through the drop-down menu and save the dates in a data frame
numberOfDates <- length(xml_find_all(website, xpath = '//*[@id="filterHolder"]/div/div/div[3]/div/ul/li'))

dates <- data.frame(matrix(ncol = 2, nrow = numberOfDates - 1))
colnames(dates) <- c('date', 'year')

for(i in 1:(numberOfDates - 1)) {
  dates$date[i] <- html_elements(website, xpath = paste0('//*[@id="filterHolder"]/div/div/div[3]/div/ul/li[', i + 1, ']')) %>%
    html_attr('data-value')
}

remove(website, i, numberOfDates)

# Extract the year
dates$year <- substr(dates$date, 1, 4)

# Keep only the latest date per year
dates <- dates %>%
  group_by(year) %>%
  slice(1)

# Remove the year
dates <- dates %>%
  ungroup() %>%
  select(-year)

# We can now create a list of links that we will use to download the lists of players.

# Create the links
dates$link <- paste0('https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate=', dates$date)

# We are now ready to download the lists of players.

# Loop through the list of links and save the names and links of the players
players <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(players) <- c('name', 'link')

for(link in dates$link) {
  website <- read_html(link)
  
  numberOfPlayers <- length(xml_find_all(website, xpath = '//*[@id="player-rank-detail-ajax"]/tbody/tr'))
  
  temp <- data.frame(matrix(ncol = 2, nrow = numberOfPlayers))
  colnames(temp) <- c('name', 'link')
  
  for(i in 1:numberOfPlayers) {
    temp$name[i] <- html_elements(website, xpath = paste0('//*[@id="player-rank-detail-ajax"]/tbody/tr[', i, ']/td[4]/span/a')) %>%
      html_attr('ga-label')
    
    temp$link[i] <- html_elements(website, xpath = paste0('//*[@id="player-rank-detail-ajax"]/tbody/tr[', i, ']/td[4]/span/a')) %>%
      html_attr('href')
  }
  
  players <- rbind(players, temp)
}

remove(dates, temp, website, i, link, numberOfPlayers)

# Remove duplicates (most players played for more than one year)
players <- distinct(players)

# Remove unknown players
players <- filter(players, name != 'Unknown Unknown')

# Remove the last part of the links so that we can use them for our purposes
players$link <- substr(players$link, 1, nchar(players$link) - 8)

# Remove quotation marks from links (as they do not work otherwise)
players$link <- gsub("\"", "", players$link, fixed = TRUE)

# Remove certain dots from links (as they do not work otherwise)
players$link <- gsub("./", "/", players$link, fixed = TRUE)

# There are some players with the same name. Therefore, we need additional information on the players
# in order to uniquely identify them. 
# Anyway, some additional information on the players may be useful in the analysis later.

# Create additional columns
for(col in c('active', 'country', 'birthday', 'turnedPro', 'weight', 'height', 'lefty', 'oneHandedBackhand')) {
  players[[col]] <- NA
}

remove(col)

# Loop through the players and download the information
errors <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(errors) <- c('name', 'link', 'error')

for(i in 1:length(players$link)) {
  tryCatch(expr = {
    website <- read_html(paste0('https://www.atptour.com', players$link[i], 'overview'))
    
    players$active[i] <- !is.na(str_extract(html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[1]/div[2]') %>%
                                              html_text(),
                                            '[:digit:]+'))
    
    country <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[2]/div[2]') %>%
      html_text()
    if(length(country) > 0) {
      players$country[i] <- country
    }
    
    temp <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[2]') %>%
      html_table()
    temp <- as.data.frame(temp)
    
    players$birthday[i] <- str_extract(temp[1, 1], '[:digit:]{4}.[:digit:]{2}.[:digit:]{2}')
    
    players$turnedPro[i] <- str_extract(temp[1, 2], '[:digit:]{4}')
    
    players$weight[i] <- str_extract(temp[1, 3], '[:digit:]{2,3}?kg')
    
    players$height[i] <- str_extract(temp[1, 4], '[:digit:]{3}cm')
    
    players$lefty[i] <- str_extract(temp[2, 2], '.*(?=,)')
    
    players$oneHandedBackhand[i] <- str_extract(temp[2, 2], '(?<=, ).*')
    
    print(paste0('Player ', i, ' out of ', length(players$link), ' done.'))
  }, error = function(e) {
    temp <- data.frame(name = players$name[i], link = players$link[i], error = as.character(e))
    
    errors <<- rbind(errors, temp) # Be careful with super assignment!
    
    print_colour(paste0('Error: ', as.character(e)), 'red')
  })
} # We can safely ignore the warning message.

remove(temp, website, country, i)

# Investigate the errors

# Only one error occurred. This is an HTTP error 404 because 
# this player does not have a profile (checked manually). 
# Let us keep the player in our list but with NAs.

remove(errors)

# Save the data frame with the players
write_csv(players, 'data/players.csv')

#players$birthday <- as.Date(players$birthday, format = '%Y.%m.%d')

# SCRAPE THE RESULTS OF THE PLAYER OF CHOICE ------------------------------
# In this section, we scrape all the results of the player that the user is interested in.

# playerName <- 'Roger Federer'
# playerBirthday <- as.Date('1981-08-08')

# overview <- filter(players, name == playerName & birthday == playerBirthday)
# if(nrow(overview) != 1) {
#   stop('This player does not exist in our list. Please check that you entered name and birthday correctly.')
# }

# Download the website
website <- read_html(paste0('https://www.atptour.com', overview$link, 'player-activity?year=all'))

# Extract all relevant information from the website and put them into a data frame
results <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(results) <- c('tournamentType', 'tournament', 'location', 'date', 'outdoor',
                       'surface', 'priceMoneyTotal', 'round', 'rankOpponent', 'opponent',
                       'opponentLink', 'win', 'score', 'eventPoints', 'rank',
                       'priceMoney')

numberOfTournaments <- length(html_elements(website, xpath = '//*[@id="currentTabContent"]/div[3]/div'))

for(i in 1:numberOfTournaments) {
  tournamentType <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[1]/img')) %>%
    html_attr('alt')
  if(tournamentType == "") {
    tournamentType <- html_elements(website, xpath = paste0('//*[@id="currentTabContent"]/div[3]/div[', i, ']/table[1]/tbody/tr/td[1]/img')) %>%
      html_attr('src')
    
    if(grepl('grandslam', tournamentType)) {
      tournamentType <- 'Grand Slam'
    } else if(grepl('itf', tournamentType)) {
      tournamentType <- 'ITF'
    }
  }
  
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
  }
  
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
    
    rankOpponent <- temp$X2
    
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
                       rankOpponent = rankOpponent,
                       opponent = opponent,
                       opponentLink = opponentLink,
                       win = win,
                       score = score,
                       eventPoints = eventPoints,
                       rank = rank,
                       priceMoney)
    
    results <- rbind(results, temp)
  }
  
  print(paste0('Tournament ', i, ' out of ', numberOfTournaments, ' done.'))
}

remove(temp, website, date, eventPoints, i, 
       location, numberOfTournaments, opponent, opponentLink, outdoor, 
       priceMoney, priceMoneyTotal, rank, rankOpponent, round, 
       score, surface, tournament, tournamentType, win)
