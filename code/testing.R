##################################################
# Script to test report.R for a random sample of players
##################################################

# 1. SET UP ---------------------------------------------------------------
# Clean the environment
remove(list = ls())

# Load the necessary packages
library(insight)

# Set fixed variables
sampleSize <- 500

# Create functions
sourceCertainLines <- function(file, lines) {
  source(textConnection(readLines(file)[lines]))
} # Thank you, christophergandrud from GitHub.

# Source the set up from report.R
sourceCertainLines('code/report.R', c(9:40))

# 2. CREATE A RANDOM SAMPLE OF PLAYERS ------------------------------------
# Create the sample
playersSample <- players[sample(1:nrow(players), sampleSize), ]

# Add a column indicating success
playersSample$success <- NA

# Add a column for the errors
playersSample$error <- NA

# 3. TEST THE CODE --------------------------------------------------------
# Go through the sample players one-by-one
for(testingPlayer in 1:sampleSize) {
  tryCatch(expr = {
    # Extract the player's link
    playerLink <- playersSample$link[testingPlayer]
    
    # Try the code (without including the lines 442-443 because they remove the exchange rates)
    sourceCertainLines('code/report.R', c(45:441, 444:906))
    
    # Indicate the success
    playersSample$success[testingPlayer] <- 1
    
    # Remove the report just created
    file.remove(paste0('reports/', name, '.pdf'))
    
    # Display the progress
    print_colour(paste('Player', testingPlayer, 'out of', sampleSize, 'done.\n'), 'bg_green')
    
    # Clean the environment for the next player
    remove(list = setdiff(ls(), c('players', 'playersSample', 'australianDollarToUSDollar', 'euroToUSDollar', 'poundToUSDollar',
                                  'sampleSize', 'testingPlayer', 'beautifulPercentages', 'sourceCertainLines')))
  }, error = function(e) {
    # Indicate the error
    playersSample$success[testingPlayer] <<- 0 # SUPERASSIGNMENT!
    
    # Add the error
    playersSample$error[testingPlayer] <<- as.character(e) # SUPERASSIGNMENT!
    
    # Display the error in red
    print_colour(paste('Error:', as.character(e)), 'bg_red')
    
    # Clean the environment for the next player
    remove(list = setdiff(ls(), c('players', 'playersSample', 'australianDollarToUSDollar', 'euroToUSDollar', 'poundToUSDollar',
                                  'sampleSize', 'testingPlayer', 'beautifulPercentages', 'sourceCertainLines')))
  })
}

remove(australianDollarToUSDollar, euroToUSDollar, poundToUSDollar, sampleSize, testingPlayer)
