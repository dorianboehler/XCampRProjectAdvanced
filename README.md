# XCampRProjectAdvanced: Automatic Report on ATP Tennis Player of Choice

## About the Project
This project set out to create a programme that automatically compiles a report on a tennis player that the user can choose from a list. The list currently contains all players that have been listed in at least one of the year-end rankings from the [Association of Tennis Professionals (ATP)](https://www.atptour.com/en/) since 1973 (15,134 players).

The following steps were necessary to achieve this goal:

1. Scrape all year-end rankings and some general information on every player from [atptour.com](https://www.atptour.com/en/) in order to get the list of players.
2. Clean this list.
3. Write a programme that, regardless of the player that the user may choose,
  * scrapes all results of a player from [atptour.com](https://www.atptour.com/en/) (including information on the tournament, the opponent, and so on),
  * cleans the results,
  * analyses the results, and
  * produces a report on the player.
4. Iteratively test the programme.

## Folder Structure
This GitHub repository contains code, data, and a sample report.

The folder [code](code) contains:
  * [players.R](code/players.R): This R script scrapes and cleans the list of players (steps 1 and 2 above). It takes about three hours to run. The raw and cleaned list of players are saved in the folder [data](data).
  * [report.R](code/report.R): This R script scrapes, cleans, and analyses the results and produces the report (step 3 above).
  * [report.Rmd](code/report.Rmd): This R Markdown file is sourced by [report.R](code/report.R) at the very end. It dynamically puts the report together.
  * [testing.R](code/testing.R): This R script tests [report.R](code/report.R), using a random sample of players (step 4 above).

The folder [data](data) contains:
  * [errors.csv](data/errors.csv): This csv file contains the errors encountered when we scraped the list of players the last time (with [players.R](code/players.R)).
  * [players.csv](data/players.csv): This csv file contains the raw list of players.
  * [playersCleaned.RData](data/playersCleaned.RData): This RData file contains the cleaned list of players.

The folder [reports](reports) currently contains only [a sample report on Roger Federer](reports/sampleReportRogerFedererMay2023.pdf). The reports that the user may generate will be saved in this folder.

The RStudio project [XCampRProjectAdvanced.Rproj](XCampRProjectAdvanced.Rproj) is essential to correctly set the working directory.

## Basic Installation
No matter whether the user wants to run [players.R](code/players.R), [report.R](code/report.R), or [testing.R](code/testing.R), they should do the following:
1. **Download the GitHub repository**. We strongly advise the user to save the GitHub repository locally without changing the folder structure.
2. **Install the required packages**:
  * `broom`
  * `countrycode`
  * `insight`
  * `knitr`
  * `rmarkdown`
  * `rvest`
  * `scales`
  * `stringr`
  * `tidyverse`
  * `xml2`
  * `zoo`
3. **Open the RStudio project [XCampRProjectAdvanced.Rproj](XCampRProjectAdvanced.Rproj)**.
4. **Be connected to the internet**. Otherwise, scraping data is not possible.

## How to Choose a Player and Get a Report?
If the user wants to choose a player and get a report, they should do the following (after the basic installation):
1. Open [report.R](code/report.R).
2. Run the **first** section (*1. SET UP*). This loads the list of players, among other things. The list of players is called *players*.
3. View the list of players and manually copy the link of the player of interest.
4. In **line 43**, paste the link without changing anything else. Make sure that the quotation marks are still there and that there are no white spaces. The line should read: `playerLink <- 'LINK' # !!!`.
5. Run sections 2—6.
6. If the user entered the link correctly and the player actually played at least one match, **the report will be saved in the folder [reports](reports)**. Otherwise, the user will be informed.

## How to Update the List of Players?
If the user wants to update the list of players, they can simply run [players.R](code/players.R) (after the basic installation). However, they should be aware that it takes about three hours to run (which is why we provide the list of players ourselves).

## Testing
Using [testing.R](code/testing.R), we tested [report.R](code/report.R) with several samples of players (size: 500).

Although we do not get any error any more (except for the "error" that a player has actually never played a match), we cannot guarantee that the programme works for all 15,134 players because the raw data is very inconsistent.

## Sources
The data is scraped from [atptour.com](https://www.atptour.com/en/).

## Team
* Noah Baiguini ([XCamp](https://codingxcamp.com): Loloide)
* Dorian Böhler ([XCamp](https://codingxcamp.com): dorianboehler)

Please let us know if anything is unclear.


