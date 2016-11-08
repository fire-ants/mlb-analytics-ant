## Test R snippet to begin containerization activities.

##installing libraries
install.packages("XML2R")
install.packages("pitchRx")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("RColorBrewer")

## load libraries
library(ggplot2)
library(pitchRx)
library(dplyr)
library(stringr)
library(graphics)
library(RColorBrewer)

# load Quantitative and Qualitative Scoring Functions Functions
get_quant_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "Called Strike")) * -(1/3) +
            as.integer(str_detect(des, "Foul")) * -(1/3) +
            as.integer(str_detect(des, "In play, run")) * 1.0 +
            as.integer(str_detect(des, "In play, out")) * 0.0 +
            as.integer(str_detect(des, "In play, no out")) * 1.0 +
            as.integer(str_detect(des, "^Ball$")) * 0.25 +
            as.integer(str_detect(des, "Swinging Strike")) * -(1/3) +
            as.integer(str_detect(des, "Hit By Pitch")) * 1.0 +
            as.integer(str_detect(des, "Ball in Dirt")) * 0.25 +
            as.integer(str_detect(des, "Missed Bunt")) * -(1/3) +
            as.integer(str_detect(des, "Intent Ball")) * 0.25
    )
    return(score)
}
get_qual_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "homer")) * 2 +
            as.integer(str_detect(des, "line")) * 1 +
            as.integer(str_detect(des, "sharp")) * 1 +
            as.integer(str_detect(des, "grounds")) * -1 +
            as.integer(str_detect(des, "flies")) * -1 +
            as.integer(str_detect(des, "soft")) * -2 +
            as.integer(str_detect(des, "pop")) * -2 +
            as.integer(str_detect(des, "triples")) * 1.5 +
            as.integer(str_detect(des, "doubles")) * 1.0 +
            as.integer(str_detect(des, "error")) * 0.5
    )
    return(score)
}

## Get MLB data for a a week.
dat <- scrape(start = "2016-08-14", end = "2016-08-20")

# EDA: join pitch and at_bat data. dplyr uses table data frames so convert anyway.
pitch <- tbl_df(dat$pitch)
atbat <- tbl_df(dat$atbat)

# combine
joined <- pitch %>%
    select(gameday_link, num, des, type, tfs, tfs_zulu, 
           id, sz_top, sz_bot, px, pz, pitch_type, count) %>%
    inner_join(x = ., 
               y = atbat %>%
                   select(gameday_link, num, pitcher, batter, b_height, 
                          pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning), 
               by = c('gameday_link', 'num')) %>%
    mutate(quant_score = get_quant_score(des),
           qual_score = get_qual_score(atbat_des) * (type == 'X'),
           hitter_val = quant_score + qual_score)

## subset At Bats for Mike Trout At Bats
subTrout <- subset(joined, batter == "545361")

# subset for all successful hits
subHits <- subset(subTrout, type == "X" & des == "In play, no out" | des =="In play, run(s)")

# subset for all successful strikes
subStrikes <- subset(subTrout, type == "S")


## Graphing experiments

## another example with Color Brewer
library(RColorBrewer)
png("545361-All.png")
brewer.pal(11, "RdYlBu")
buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
smoothScatter(subTrout$pz~subTrout$px, nbin=1000, colramp = colorRampPalette(c(buylrd)), nrpoints=Inf, pch="", cex=.7, transformation = function(x) x^.6, col="black", main="Mike Trout - Pitch Locations", xlab="Horizontal Location", ylab="Vertical Location")
lines(c(0.708335, 0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
lines(c(-0.708335, -0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
lines(c(-0.708335, 0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_bot)), col="white", lty="dashed", lwd=2)
lines(c(-0.708335, 0.708335), c(mean(subTrout$sz_top), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
dev.off()

## Add Batter's strike zone
topKzone = mean(subTrout$sz_top)
botKzone = mean(subTrout$sz_bot) 
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# Battles - Heat Map Codebase
p0 <- ggplot() +
    geom_point(data=subTrout, aes(x=px, y=pz, shape=type, col=pitch_type)) +
    #facet_grid(. ~ p_throws) +
    coord_equal() +
    geom_path(aes(x, y), data = kZone, linetype = 2)
p0 = p0 +
    xlab("Horizontal Location\n(ft. from center of the plate)") +
    ylab("Vertical Location\n(ft. from ground)")
png("545361-HeatMap-YTD.png")
p0
dev.off()

##Dan's code
system("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key 545361-HeatMap-YTD.png --body 545361-HeatMap-YTD.png")
system("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key 545361-All.png --body 545361-All.png")