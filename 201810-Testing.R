install.packages("devtools")
install.packages("git2r")
install.packages("ggplot2")
library(devtools)
install_github("cpsievert/pitchRx", force = T)

install.packages("pitchRx")
install.packages("dplyr")
install.packages("lubridate")
install.packages("RSQLite")


## load libraries
library(ggplot2)
library(graphics)
library(RColorBrewer)
library(pitchRx)    ## thank you Carson Sievert!!!
library(dplyr)      ## thank you Hadley Wickham
library(stringr)
library(lubridate)
library(RSQLite)

## Use dplyer to create SQLite database
#library(dplyr)
#my_db2016 <- src_sqlite("pitchRx2016.sqlite3", create = TRUE)
my_dbProd <- src_sqlite("pitchRxProd082018.sqlite3", create = TRUE)

Today <- Sys.Date()
ThirtyDaysAgo <- Today - 30
ThirtyDaysAhead = Today + months(1)

WhatMonth <- month(Today)

#confirm empty
#my_db2016
my_dbProd


## scrape game data and store in the database
## 2017 MLB season was from 02APR to 01NOV (inclusive of postseason)

#library(pitchRx)
dat1308 <- scrape(start = "2013-08-01", end = "2013-08-01", suffix = "inning/inning_all.xml")
dat160501 <- scrape(start = "2016-05-01", end = "2016-05-01", suffix = "inning/inning_all.xml")
dat1611 <- scrape(start = "2016-11-01", end = "2016-11-02", suffix = "inning/inning_all.xml")
dat1705 <- scrape(start = "2017-05-01", end = "2017-05-01", suffix = "inning/inning_all.xml")
dat170701 <- scrape(start = "2017-07-01", end = "2017-07-01", suffix = "inning/inning_all.xml")
dat1701 <- scrape(start = "2017-01-01", end = "2017-11-30", suffix = "inning/inning_all.xml")
dat1611 <- scrape(start = "2016-11-01", end = "2016-11-02", suffix = "inning/inning_all.xml")
dat180402 <- scrape(start = "2018-04-02", end = "2018-04-02", suffix = "inning/inning_all.xml")
dat1808 <- scrape(start = "2018-08-01", end = "2018-08-31", suffix = "inning/inning_all.xml")

