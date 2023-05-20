##################################################
# Script to scrape and clean the list of players from atptour.com (takes about 3 hours)
##################################################

# 1. SET UP ------------------------------------------------------------------
# Clean the environment
remove(list = ls())

# Load the necessary packages
library(insight)
library(rvest)
library(tidyverse)
library(xml2)

# 2. SCRAPE THE LIST OF PLAYERS FROM THE WEBSITE -----------------------------
# In order to have a complete list of players that have played since 1973, 
# we will download the year-end ranking for every year. Unfortunately,
# we need to provide dates from the drop-down menu in the links to the year-end rankings.
# For this reason, we will first scrape these dates.

# Download the website
website <- read_html('https://www.atptour.com/en/rankings/singles')

# Loop through the drop-down menu and save the dates in a data frame
numberOfDates <- length(xml_find_all(website, xpath = '//*[@id="filterHolder"]/div/div/div[3]/div/ul/li'))

dates <- data.frame(matrix(nrow = numberOfDates - 1, ncol = 2))
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

# We are now ready to download the year-end rankings.

# Create the links
dates$link <- paste0('https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate=', dates$date)

# Loop through the links and save the names and links of the players (takes about 35 minutes)
players <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(players) <- c('name', 'link')

for(link in dates$link) {
  # Download the website
  website <- read_html(link)
  
  # Count the number of players
  numberOfPlayers <- length(xml_find_all(website, xpath = '//*[@id="player-rank-detail-ajax"]/tbody/tr'))
  
  # Save the names and links of the players in a temporary data frame
  temp <- data.frame(matrix(nrow = numberOfPlayers, ncol = 2))
  colnames(temp) <- c('name', 'link')
  
  for(i in 1:numberOfPlayers) {
    temp$name[i] <- html_elements(website, xpath = paste0('//*[@id="player-rank-detail-ajax"]/tbody/tr[', i, ']/td[4]/span/a')) %>%
      html_attr('ga-label')
    
    temp$link[i] <- html_elements(website, xpath = paste0('//*[@id="player-rank-detail-ajax"]/tbody/tr[', i, ']/td[4]/span/a')) %>%
      html_attr('href')
  }
  
  # Add the temporary data frame to the final data frame
  players <- rbind(players, temp)
  
  # Display the progress
  print(paste('Year', substr(dates$date[which(dates$link == link)], 1, 4), 'done'))
}

remove(dates, temp, website, i, link, numberOfPlayers)

# Remove duplicates (most players played for more than one year)
players <- distinct(players)

# Remove the unknown players
players <- filter(players, name != 'Unknown Unknown')

# Remove the last part of the links so that we can use them generally
players$link <- substr(players$link, 1, nchar(players$link) - 8)

# Remove the quotation marks from the links (as they do not work otherwise)
players$link <- gsub('"', '', players$link, fixed = TRUE)

# Remove certain dots from the links (as they do not work otherwise)
players$link <- gsub('./', '/', players$link, fixed = TRUE)

# Since there are some players with the same name, 
# we need additional information on the players in order to uniquely identify them later.
# Besides, this information will be interesting in the analysis later.

# Create an empty csv file to which we will append the rows one by one (this is more efficient)
file.create('data/players.csv')
file <- file('data/players.csv')

writeLines('name\tlink\tactive\tcountry\tbirthday\tturnedPro\tweight\theight\tlefty\toneHandedBackhand', file)

close(file)

remove(file)

# Create an empty data frame for the errors
errors <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(errors) <- c('name', 'link', 'error')

# Loop through the players and download the information
for(i in 1:nrow(players)) {
  tryCatch(expr = {
    # Download the website
    website <- read_html(paste0('https://www.atptour.com', players$link[i], 'overview'))
    
    # Extract the information
    active <- !is.na(str_extract(html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[1]/div[2]') %>%
                                   html_text(),
                                 '[:digit:]+'))
    
    country <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[2]/div[2]') %>%
      html_text()
    if(length(country) == 0) {
      country <- NA
    }
    
    temp <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[2]') %>%
      html_table()
    temp <- as.data.frame(temp)
    
    birthday <- str_extract(temp[1, 1], '[:digit:]{4}.[:digit:]{2}.[:digit:]{2}')
    
    turnedPro <- str_extract(temp[1, 2], '[:digit:]{4}')
    
    weight <- str_extract(temp[1, 3], '[:digit:]{2,3}?kg')
    
    height <- str_extract(temp[1, 4], '[:digit:]{3}cm')
    
    lefty <- str_extract(temp[2, 2], '.*(?=,)')
    
    oneHandedBackhand <- str_extract(temp[2, 2], '(?<=, ).*')
    
    # Append the row to the csv file
    temp <- data.frame(name = players$name[i],
                       link = players$link[i],
                       active = active,
                       country = country,
                       birthday = birthday,
                       turnedPro = turnedPro,
                       weight = weight,
                       height = height,
                       lefty = lefty,
                       oneHandedBackhand = oneHandedBackhand)
    
    write.table(temp, file = 'data/players.csv', append = TRUE, sep = '\t', row.names = FALSE, col.names = FALSE, fileEncoding = 'UTF-8')
    
    # Display the progress
    print(paste('Player', i, 'out of', nrow(players), 'done.'))
  }, error = function(e) {
    temp <- data.frame(name = players$name[i],
                       link = players$link[i],
                       error = as.character(e))
    
    errors <<- rbind(errors, temp) # Be careful with super assignments!
    
    # Display the error in red
    print_colour(paste('Error:', as.character(e)), 'red')
  })
} # We can ignore the warning messages. They are in the errors.

remove(temp, website, active, birthday, country, 
       height, i, lefty, oneHandedBackhand, turnedPro,
       weight)

# Try the links that did not work again
tryAgain <- errors
errors <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(errors) <- c('name', 'link', 'error')

for(i in 1:nrow(tryAgain)) {
  tryCatch(expr = {
    # Download the website
    website <- read_html(paste0('https://www.atptour.com', tryAgain$link[i], 'overview'))
    
    # Extract the information
    active <- !is.na(str_extract(html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[1]/div[2]') %>%
                                   html_text(),
                                 '[:digit:]+'))
    
    country <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[1]/div/div[3]/div[2]/div[2]') %>%
      html_text()
    if(length(country) == 0) {
      country <- NA
    }
    
    temp <- html_elements(website, xpath = '//*[@id="playerProfileHero"]/div[2]/div[2]') %>%
      html_table()
    temp <- as.data.frame(temp)
    
    birthday <- str_extract(temp[1, 1], '[:digit:]{4}.[:digit:]{2}.[:digit:]{2}')
    
    turnedPro <- str_extract(temp[1, 2], '[:digit:]{4}')
    
    weight <- str_extract(temp[1, 3], '[:digit:]{2,3}?kg')
    
    height <- str_extract(temp[1, 4], '[:digit:]{3}cm')
    
    lefty <- str_extract(temp[2, 2], '.*(?=,)')
    
    oneHandedBackhand <- str_extract(temp[2, 2], '(?<=, ).*')
    
    # Append the row to the csv file
    temp <- data.frame(name = tryAgain$name[i],
                       link = tryAgain$link[i],
                       active = active,
                       country = country,
                       birthday = birthday,
                       turnedPro = turnedPro,
                       weight = weight,
                       height = height,
                       lefty = lefty,
                       oneHandedBackhand = oneHandedBackhand)
    
    write.table(temp, file = 'data/players.csv', append = TRUE, sep = '\t', row.names = FALSE, col.names = FALSE, fileEncoding = 'UTF-8')
  }, error = function(e) {
    temp <- data.frame(name = tryAgain$name[i],
                       link = tryAgain$link[i],
                       error = as.character(e))
    
    errors <<- rbind(errors, temp) # Again, be careful with super assignments!
  })
} # Again, we can ignore the warning messages. They are in the errors.

remove(temp, tryAgain, website, active, birthday,
       country, height, i, lefty, oneHandedBackhand,
       turnedPro, weight)

# Import the resulting list of players
players <- read.delim('data/players.csv')

# Save the errors and manually investigate them
write.csv(errors, 'data/errors.csv')

remove(errors)

# Only one error occurred. This error is an HTTP error 404
# because the player does not have a profile.
# Let us ignore this player.

# 3. CLEAN THE LIST OF PLAYERS -----------------------------------------------
# active: Change the data type to numeric
players$active <- as.numeric(players$active)

# country: Replace empty strings with NA
players$country[players$country == '' & !is.na(players$country)] <- NA

# country: Change the data type to factor
players$country <- as.factor(players$country)

# birthday: Change the data type to date
players$birthday <- as.Date(players$birthday, format = '%Y.%m.%d')

# turnedPro: Change the data type to numeric
players$turnedPro <- as.numeric(players$turnedPro)

# weight: Remove kg and change the data type to numeric
players$weight <- as.numeric(substr(players$weight, 1, nchar(players$weight) - 2))

# height: Remove cm and change the data type to numeric
players$height <- as.numeric(substr(players$height, 1, nchar(players$height) - 2))

# lefty: Create a dummy variable
players$lefty[players$lefty == 'Ambidextrous' & !is.na(players$lefty)] <- NA

players$lefty[players$lefty == 'Right-Handed' & !is.na(players$lefty)] <- 0

players$lefty[players$lefty == 'Left-Handed' & !is.na(players$lefty)] <- 1

players$lefty <- as.numeric(players$lefty)

# oneHandedBackhand: Create a dummy variable
players$oneHandedBackhand[players$oneHandedBackhand == 'Unknown Backhand' & !is.na(players$oneHandedBackhand)] <- NA

players$oneHandedBackhand[players$oneHandedBackhand == 'Two-Handed Backhand' & !is.na(players$oneHandedBackhand)] <- 0

players$oneHandedBackhand[players$oneHandedBackhand == 'One-Handed Backhand' & !is.na(players$oneHandedBackhand)] <- 1  

players$oneHandedBackhand <- as.numeric(players$oneHandedBackhand)  

# Save the cleaned list of players
save(players, file = 'data/playersCleaned.RData')

remove(players)
