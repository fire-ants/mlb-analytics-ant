
library(tidyjson)   # this library
library(dplyr)      # for %>% and other dplyr functions
library(jsonlite)
library(httr)

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
    
# Parse the JSON into a data.frame
purch_df <- jsonlite::fromJSON(purch_json, simplifyDataFrame = TRUE)
# Examine results
purch_df

jsonlite::fromJSON(people, simplifyDataFrame = TRUE)

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



jsonBtr <- GET("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16")
jsontxt <- content(jsonBtr, "text")

hv_items <- jsontxt %>%
    gather_array %>%                                     # stack the users 
    #spread_values(id = jstring("_id")) %>% 
    #enter_object("pitcher") %>% gather_array %>%       # stack the purchases
    #spread_values(                                       # extract item name and price
    #    pitcher.id = jstring("_id"),
    #    pitcher.birth = jstring("birthDate"),
    #    pitcher.lname = jstring("lastName"),
    #    pitcher.fname = jstring("firstName"),
    #    pitcher.mlbid = jnumber("mlbid"),
    #    pitcher.v = jstring("__v")
    #) %>%  
    spread_values(atbat_des) = jstring("description") %>%
    #enter_object("items") %>% gather_array %>%           # stack the items
    #spread_values(                                       # extract item name and price
    #    item.name = jstring("name"),
    #    item.price = jnumber("price")
    #) %>%
    select(atbat_des) # select only what is needed


json_data <- fromJSON(json_file, flatten = TRUE)
as.list(json_data$pitches)
list <- json_data[1,]



json01 <- fromJSON("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16")
json02 <- stream_in(url("http://67.205.147.49/atbat/batter/58b7a70a997e47000f72fa16"))

json01_flat <- flatten(json01)
library(tibble)
json01_tbl <- as_data_frame(json01_flat)
json01_tbl

json01_tbl %>% mutate(pitches = as.character(pitches)) %>% select(pitches)

library(tidyr)

json01_tbl %>% 
    unnest(pitches) %>%
    select(pitcher.lastName, batter.lastName, description)


