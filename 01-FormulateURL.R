## Connect to JSON
##install packages
install.packages(rjson)
install.packages(jsonlite)
install.packages('curl')
library(rjson)
library(jsonlite)
library(curl)

#formulates URLs using mlbid and objectID from API
mlbid = "545361"
mlbid_URL <-paste0("http://67.205.147.49/player/mlbid/",mlbid)
json_data <-fromJSON(mlbid_URL)
objID =as.character(json_data[1])
object_URL <- paste0("http://67.205.147.49/atbat/batter/",objID)
json_data2 <- fromJSON(object_URL)
