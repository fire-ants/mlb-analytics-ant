## load libraries
library(ggplot2)
library(graphics)
library(RColorBrewer)
library(pitchRx)    ## thank you Carson Sievert!!!
library(dplyr)      ## thank you Hadley Wickham
library(stringr)

## Use dplyer to create SQLite database
#library(dplyr)
#my_db2016 <- src_sqlite("pitchRx2016.sqlite3", create = TRUE)
my_dbProd <- src_sqlite("pitchRxProd.sqlite3", create = TRUE)

Today <- Sys.Date()
ThirtyDaysAgo <- Today - 30

#confirm empty
#my_db2016
my_dbProd


## scrape 2016 game data and store in the database
#library(pitchRx)
#scrape(start = "2016-04-03", end = "2016-11-02", suffix = "inning/inning_all.xml", connect = my_db1$con)
#scrape(start = "2016-04-01", end = "2016-10-31", suffix = "inning/inning_all.xml", connect = my_db2016$con)
scrape(start = ThirtyDaysAgo, end = Today, suffix = "inning/inning_all.xml", connect = my_dbProd$con)


# To speed up execution time, create an index on these three fields.
library("dbConnect", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

dbSendQuery(my_db2016$con, "CREATE INDEX url_atbat ON atbat(url)") 
dbSendQuery(my_db2016$con, "CREATE INDEX url_pitch ON pitch(url)")
dbSendQuery(my_db2016$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_db2016$con, "CREATE INDEX des_index ON pitch(des)")

dbSendQuery(my_db2017$con, "CREATE INDEX url_atbat ON atbat(url)") 
dbSendQuery(my_db2017$con, "CREATE INDEX url_pitch ON pitch(url)")
dbSendQuery(my_db2017$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_db2017$con, "CREATE INDEX des_index ON pitch(des)")

# load Quantitative and Qualitative Scoring Functions Functions
# Quant scored in terms of Out (-1) and Hit (1)
#get_quant_score <- function(des) {
#    score <- (
#        as.integer(str_detect(des, "Called Strike")) * -(1/3) +
#            as.integer(str_detect(des, "Foul")) * -(1/3) +
#            as.integer(str_detect(des, "In play, run")) * 1.0 +
#            as.integer(str_detect(des, "In play, out")) * -1.0 +
#            as.integer(str_detect(des, "In play, no out")) * 1.0 +
#            as.integer(str_detect(des, "^Ball$")) * 0.25 +
#            as.integer(str_detect(des, "Swinging Strike")) * -(1/2.5) +
#            as.integer(str_detect(des, "Hit By Pitch")) * 1.0 +
#            as.integer(str_detect(des, "Ball In Dirt")) * 0.25 +
#            as.integer(str_detect(des, "Missed Bunt")) * -(1/3) +
#            as.integer(str_detect(des, "Intent Ball")) * 0.25
#    )
#    return(score)
#}
#get_qual_score <- function(des) {
#    score <- (
#        as.integer(str_detect(des, "homer")) * 2 +
#            as.integer(str_detect(des, "line")) * 1.5 +
#            as.integer(str_detect(des, "sharp")) * 1.5 +
#            as.integer(str_detect(des, "grounds")) * -1 +
#            as.integer(str_detect(des, "flies")) * -1 +
#            as.integer(str_detect(des, "soft")) * -2 +
#            as.integer(str_detect(des, "pop")) * -2 +
#            as.integer(str_detect(des, "triples")) * 1.5 +
#            as.integer(str_detect(des, "doubles")) * 1.0 +
#            as.integer(str_detect(des, "error")) * 0.5
#    )
#    return(score)
#}

#fix_quant_score <- function(event) {
#    score <- (
#        as.integer(str_detect(event, "Groundout")) * -2 +
#            as.integer(str_detect(event, "Forceout")) * -2 +
#            as.integer(str_detect(event, "Field Error")) * -2 
#    )
#    return(score)
#}

# Load data from final month of World Series
#data.fin.month <- scrape(start = "2016-09-25", end = "2016-10-24", connect = my_db1$con)
#data.season 

#pitch16 <- select(tbl(my_db1, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, sz_top, sz_bot, px, pz, pitch_type, count, zone)
#atbat16 <- select(tbl(my_db1, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)
