## Connect to JSON

library(tidyjson)   # this library
library(dplyr)      # for %>% and other dplyr functions
library(httr)
json_raw <- GET("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16")
jsontxt <- content(json_raw, "text")

json_raw <-fromJSON("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16")

jsontxt %>%                  # %>% is the magrittr pipeline operator 
    gather_array %>%          # gather (stack) the array by index
    enter_object("pitcher") %>% gather_array %>% 
    enter_object("batter") %>% gather_array %>% 
    spread_values(atbat_des = jstring("description")) %>% 
    enter_object("pitches") %>% gather_array %>% 
    spread_values(            # spread (widen) values to widen the data.frame
        pitchType = jstring("pitchType"), # value of "name" becomes a character column
        endSpeed = jnumber("endSpeed")    # value of "age" becomes a numeric column
    )

purch_items <- purch_json %>%
    gather_array %>%                                     # stack the users 
    spread_values(person = jstring("name")) %>%          # extract the user name
    enter_object("purchases") %>% gather_array %>%       # stack the purchases
    spread_values(purchase.date = jstring("date")) %>%   # extract the purchase date
    enter_object("items") %>% gather_array %>%           # stack the items
    spread_values(                                       # extract item name and price
        item.name = jstring("name"),
        item.price = jnumber("price")
    ) %>%
    select(person, purchase.date, item.name, item.price) # select only what is needed



library(RJSONIO)
# from the website
json_raw <-fromJSON("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16")

library(plyr)
ldply(fromJSON(json_raw), stack)

json06 <-json_raw[['pitches']]

fmNames<-sapply(json_raw, function(x) x[[6]])
head(fmNames)

library(gdata) # for the trim function
grabInfo<-function(var){
    print(paste("Variable", var, sep=" "))  
    sapply(json_raw, function(x) returnData(x, var)) 
}

returnData<-function(x, var){
    if(!is.null( x[[var]])){
        return( trim(x[[var]]))
    }else{
        return(NA)
    }
}

# do the extraction and assembly
fmDataDF<-data.frame(sapply(1:22, grabInfo), stringsAsFactors=FALSE)


foodMarketsRaw<-fromJSON("retail_food_markets.json")


install.packages("rjson")
library("rjson")
library("jsonlite")install.packages("tidyjson")
library("tidyjson", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")


library(tidyjson)   # this library
library(dplyr)      # for %>% and other dplyr functions

json_url <- "http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16"
json_data <- fromJSON(json_file)
transform(stack(json_data), pitches=c(json_data, names))

json_data

json03 <- fromJSON(json_url, flatten = TRUE)
json04 <- toJSON(json_url, auto_unbox = TRUE)
json05 <- fromJSON(json04)




json_df <- data.frame(number = unlist(json_data))
json_df$rownames <- rownames(json_df) 
rownames(json_df) <- NULL
json_df$categories <- lapply(strsplit(as.character(json_df$rownames), "\\."), "[", 1)
json_df$pitches <- lapply(strsplit(as.character(json_df$rownames), "\\."), "[",2) 
json_df$rownames <- NULL




json.matchData <- fromJSON(json_url, flatten = TRUE)


matchData.i <- lapply(json04$pitches, function(x){ unlist(x)})

library(plyr)
matchData <- rbind.fill(lapply(matchData.i, 
    function(x) do.call("data.frame", as.list(x))
))

library(plyr)
ldply(fromJSON(json), stack)



raw_df <- jsonlite::fromJSON(json_file, simplifyDataFrame = TRUE)
raw_df
str(raw_df)


hAtBats <- raw_df %>%
    gather_array %>%                                     # stack the users 
    spread_values(batter = jstring("batter")) %>%             # extract the user name
    enter_object("pitches") %>% gather_array %>%       # stack the purchases
    spread_values(type = jstring("type")) %>%   # extract the purchase date
    ) %>%
    select(batter, type) # select only what is needed


purch_json <- '
[
    {
    "name": "bob", 
    "purchases": [
    {
    "date": "2014/09/13",
    "items": [
    {"name": "shoes", "price": 187},
    {"name": "belt", "price": 35}
    ]
    }
    ]
    },
    {
    "name": "susan", 
    "purchases": [
    {
    "date": "2014/10/01",
    "items": [
    {"name": "dress", "price": 58},
    {"name": "bag", "price": 118}
    ]
    },
    {
    "date": "2015/01/03",
    "items": [
    {"name": "shoes", "price": 115}
    ]
    }
    ]
    }
    ]'

    
    
purch_df <- jsonlite::fromJSON(purch_json, simplifyDataFrame = TRUE)
raw_df <- jsonlite::fromJSON(json_url, simplifyDataFrame = TRUE)
hv_df <- jsonlite::fromJSON("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16", simplifyDataFrame = TRUE)
purch_df 
str(purch_df)

hv_items <- json_data %>%  
    gather_array %>%
    spread_values(description = jstring("at_bat_des")) %>% 
    select(description)

library("rjson")
json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
json_data3 <- fromJSON(paste(readLines(json_file), collapse=""))



purch_items <- purch_json %>%
    gather_array %>%                                     # stack the users 
    spread_values(person = jstring("name")) %>%          # extract the user name
    enter_object("purchases") %>% gather_array %>%       # stack the purchases
    spread_values(purchase.date = jstring("date")) %>%   # extract the purchase date
    enter_object("items") %>% gather_array %>%           # stack the items
    spread_values(                                       # extract item name and price
        item.name = jstring("name"),
        item.price = jnumber("price")
    ) %>%
    select(person, purchase.date, item.name, item.price) # select only what is needed




toJSON(json_data, pretty=TRUE)
mymatrix <- fromJSON(json_file)
mymatrix

json <- '[
  [1, 2, 3, 4],
[5, 6, 7, 8],
[9, 10, 11, 12]
]'
mymatrix <- fromJSON(json)
mymatrix


m <- foreach(i=1:nrow(json_data)) %do%{lapply(
    json_data[[1]]$pitches, 
    function(x) c(x$des['des'], x$type['type'], x$tfs['tfs'])
)
}
    
des, type, tfs,

m <- do.call(rbind, m)


http://67.205.147.49/player/mlbid/545361
58b7a70a997e47000f72fa16
HitterAB <- jsonlite::
http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16

## load data frames
## Benita Here

