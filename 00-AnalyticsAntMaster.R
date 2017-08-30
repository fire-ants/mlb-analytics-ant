## load libraries
#INSTALL PRIOR TO RUNNING IN CLOUD
install.packages("tidyverse")
install.packages("DBI")
install.packages("RSQLite")
install.packages("XML2R",dependencies=TRUE)
install.packages("pitchRx")
install.packages("stringr")
install.packages("akima")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("graphics")

#LOADING in RSTUDIO
#library(tidyverse)
library(DBI)
library(RSQLite)
library(ggplot2)
library(akima)
library(pitchRx)
library(stringr)
library(dplyr)
library(dbplyr)
library(graphics)
library(RColorBrewer)
library(XML2R)
#library(devtools)
library(dbConnect)

## temporary while loading from SQL database
## if running via container
setwd("/DB")
#my_db1 <- src_sqlite("pitchRx.sqlite3", create = FALSE)

## Jason's macbook
#setwd("/Users/jrbattles/GitHub/mlb-analytics-ant")
#my_db1 <- src_sqlite("pitchRx.sqlite3", create = FALSE)
#my_db2016 <- src_sqlite("pitchRx2016.sqlite3", create = FALSE)
#my_db2017 <- src_sqlite("pitchRx2017.sqlite3", create = FALSE)

#my_db1 <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/cohend/Documents/code/mlb-analytics-ant/pitchRx.sqlite3")
#print(dbListTables(my_db1))

#system(sprintf("ls -lart"))
#system(sprintf("pwd"))


    
# load Quantitative and Qualitative Scoring Functions Functions
# Quant scored in terms of Out (-1) and Hit (1)
get_quant_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "Called Strike")) * -(1/3) +
            as.integer(str_detect(des, "Foul")) * -(1/3) +
            as.integer(str_detect(des, "In play, run")) * 1.0 +
            as.integer(str_detect(des, "In play, out")) * -1.0 +
            as.integer(str_detect(des, "In play, no out")) * 1.0 +
            as.integer(str_detect(des, "^Ball$")) * 0.25 +
            as.integer(str_detect(des, "Swinging Strike")) * -(1/2.5) +
            as.integer(str_detect(des, "Hit By Pitch")) * 1.0 +
            as.integer(str_detect(des, "Ball In Dirt")) * 0.25 +
            as.integer(str_detect(des, "Missed Bunt")) * -(1/3) +
            as.integer(str_detect(des, "Intent Ball")) * 0.25
    )
    return(score)
}
get_qual_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "homer")) * 2 +
            as.integer(str_detect(des, "line")) * 1.5 +
            as.integer(str_detect(des, "sharp")) * 1.5 +
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

fix_quant_score <- function(event) {
    score <- (
        as.integer(str_detect(event, "Groundout")) * -2 +
            as.integer(str_detect(event, "Forceout")) * -2 +
            as.integer(str_detect(event, "Field Error")) * -2 
    )
    return(score)
}



# Load data from final month of World Series
#data.fin.month <- scrape(start = "2016-09-25", end = "2016-10-24", connect = my_db1$con)
#data.season 

#pitch16 <- select(tbl(my_db1, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, sz_top, sz_bot, px, pz, pitch_type, count, zone)
#atbat16 <- select(tbl(my_db1, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)




#####################################################################
#####################################################################

create_hv_plots <- function(data, mlbID, ...) {
  
  ## Add Batter's strike zone
  topKzone = mean(data$sz_top, na.rm = TRUE)
  botKzone = mean(data$sz_bot, na.rm = TRUE) 
  inKzone = -.95
  outKzone = 0.95
  kZone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone), 
    y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  ## Filter out by Batter and Pitch Type
  sub.FF <- data %>% filter(pitch_type=="FF")
  sub.FF.RHP <- sub.FF %>% filter(p_throws=="R")
  sub.FF.LHP <- sub.FF %>% filter(p_throws=="L")
  
  sub.SL <- data %>% filter(pitch_type=="SL")
  sub.SL.RHP <- sub.SL %>% filter(p_throws=="R")
  sub.SL.LHP <- sub.SL %>% filter(p_throws=="L")
  
  sub.CH <- data %>% filter(pitch_type=="CH")
  sub.CH.RHP <- sub.CH %>% filter(p_throws=="R")
  sub.CH.LHP <- sub.CH %>% filter(p_throws=="L")
  
  sub.CU <- data %>% filter(pitch_type=="CU")
  sub.CU.RHP <- sub.CU %>% filter(p_throws=="R")
  sub.CU.LHP <- sub.CU %>% filter(p_throws=="L")
  
  sub.SI <- data %>% filter(pitch_type=="SI")
  sub.SI.RHP <- sub.SI %>% filter(p_throws=="R")
  sub.SI.LHP <- sub.SI %>% filter(p_throws=="L")
  
  ## FF Four-seam fastball - RHP
  hv.FF <- data.frame(x = sub.FF.RHP$px, y = sub.FF.RHP$pz, z = sub.FF.RHP$hitter_val)
  hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
  hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
  hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.FF.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("FF Hitter Value - RHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  
  filename = str_c(mlbID,"-rhp-hv-FF.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## FF Four-seam fastball - LHP
  hv.FF <- data.frame(x = sub.FF.LHP$px, y = sub.FF.LHP$pz, z = sub.FF.LHP$hitter_val)
  hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
  hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
  hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.FF.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("FF Hitter Value - LHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-lhp-hv-FF.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## SL Slider - RHP
  hv.SL <- data.frame(x = sub.SL.RHP$px, y = sub.SL.RHP$pz, z = sub.SL.RHP$hitter_val)
  hv.SL.grid <- interp(hv.SL$x, hv.SL$y, hv.SL$z)
  hv.SL.grid2 <- expand.grid(x=hv.SL.grid$x, y=hv.SL.grid$y)
  hv.SL.grid2$z <- as.vector(hv.SL.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.SL.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("SL Hitter Value - RHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-rhp-hv-SL.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## SL Slider - LHP
  hv.SL <- data.frame(x = sub.SL.LHP$px, y = sub.SL.LHP$pz, z = sub.SL.LHP$hitter_val)
  hv.SL.grid <- interp(hv.SL$x, hv.SL$y, hv.SL$z)
  hv.SL.grid2 <- expand.grid(x=hv.SL.grid$x, y=hv.SL.grid$y)
  hv.SL.grid2$z <- as.vector(hv.SL.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.SL.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("SL Hitter Value - LHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-lhp-hv-SL.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## CH Four-seam fastball - RHP
  hv.CH <- data.frame(x = sub.CH.RHP$px, y = sub.CH.RHP$pz, z = sub.CH.RHP$hitter_val)
  hv.CH.grid <- interp(hv.CH$x, hv.CH$y, hv.CH$z)
  hv.CH.grid2 <- expand.grid(x=hv.CH.grid$x, y=hv.CH.grid$y)
  hv.CH.grid2$z <- as.vector(hv.CH.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.CH.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("CH Hitter Value - RHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-rhp-hv-CH.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## CH Four-seam fastball - LHP
  hv.CH <- data.frame(x = sub.CH.LHP$px, y = sub.CH.LHP$pz, z = sub.CH.LHP$hitter_val)
  hv.CH.grid <- interp(hv.CH$x, hv.CH$y, hv.CH$z)
  hv.CH.grid2 <- expand.grid(x=hv.CH.grid$x, y=hv.CH.grid$y)
  hv.CH.grid2$z <- as.vector(hv.CH.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.CH.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("CH Hitter Value - LHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-lhp-hv-CH.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## CU Four-seam fastball - RHP
  hv.CU <- data.frame(x = sub.CU.RHP$px, y = sub.CU.RHP$pz, z = sub.CU.RHP$hitter_val)
  hv.CU.grid <- interp(hv.CU$x, hv.CU$y, hv.CU$z)
  hv.CU.grid2 <- expand.grid(x=hv.CU.grid$x, y=hv.CU.grid$y)
  hv.CU.grid2$z <- as.vector(hv.CU.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.CU.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("CU Hitter Value - RHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-rhp-hv-CU.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## CU Four-seam fastball - LHP
  hv.CU <- data.frame(x = sub.CU.LHP$px, y = sub.CU.LHP$pz, z = sub.CU.LHP$hitter_val)
  hv.CU.grid <- interp(hv.CU$x, hv.CU$y, hv.CU$z)
  hv.CU.grid2 <- expand.grid(x=hv.CU.grid$x, y=hv.CU.grid$y)
  hv.CU.grid2$z <- as.vector(hv.CU.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.CU.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("CU Hitter Value - LHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-lhp-hv-CU.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## SI Four-seam fastball - RHP
  hv.SI <- data.frame(x = sub.SI.RHP$px, y = sub.SI.RHP$pz, z = sub.SI.RHP$hitter_val)
  hv.SI.grid <- interp(hv.SI$x, hv.SI$y, hv.SI$z)
  hv.SI.grid2 <- expand.grid(x=hv.SI.grid$x, y=hv.SI.grid$y)
  hv.SI.grid2$z <- as.vector(hv.SI.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.SI.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("SI Hitter Value - RHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-rhp-hv-SI.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
  ## SI Four-seam fastball - LHP
  hv.SI <- data.frame(x = sub.SI.LHP$px, y = sub.SI.LHP$pz, z = sub.SI.LHP$hitter_val)
  hv.SI.grid <- interp(hv.SI$x, hv.SI$y, hv.SI$z)
  hv.SI.grid2 <- expand.grid(x=hv.SI.grid$x, y=hv.SI.grid$y)
  hv.SI.grid2$z <- as.vector(hv.SI.grid$z)
  
  b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
  re <- colorRampPalette(c("white","red2","darkred"))(200)
  
  ggplot(hv.SI.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + ggtitle("SI Hitter Value - LHP") + 
    geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
    geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
    scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
    geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  ## Save plot to working directory in the plots sub-folder
  filename = str_c(mlbID,"-lhp-hv-SI.png")
  ggsave(filename, device="png", path="plots", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body ./plots/%1$s", filename))
  
}

###############################################
####  MASTER CONROL 
###############################################

## Use dplyer to create SQLite database
#library(dplyr)
#my_db2016 <- src_sqlite("pitchRx2016.sqlite3", create = TRUE)
my_dbProd <- src_sqlite("pitchRxProd.sqlite3", create = TRUE)
Today <- Sys.Date()
ThirtyDaysAgo <- Today - 30
#confirm empty
#my_db2016
#my_dbProd

## scrape 2016 game data and store in the database
#library(pitchRx)
#scrape(start = "2016-04-03", end = "2016-11-02", suffix = "inning/inning_all.xml", connect = my_db1$con)
#scrape(start = "2016-04-01", end = "2016-10-31", suffix = "inning/inning_all.xml", connect = my_db2016$con)
scrape(start = ThirtyDaysAgo, end = Today, suffix = "inning/inning_all.xml", connect = my_dbProd$con)


# To speed up execution time, create an index on these three fields.
#library("dbConnect", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

dbSendQuery(my_dbProd$con, "CREATE INDEX url_atbat ON atbat(url)") 
dbSendQuery(my_dbProd$con, "CREATE INDEX url_pitch ON pitch(url)")
dbSendQuery(my_dbProd$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_dbProd$con, "CREATE INDEX des_index ON pitch(des)")

#dbSendQuery(my_db2017$con, "CREATE INDEX url_atbat ON atbat(url)") 
#dbSendQuery(my_db2017$con, "CREATE INDEX url_pitch ON pitch(url)")
#dbSendQuery(my_db2017$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
#dbSendQuery(my_db2017$con, "CREATE INDEX des_index ON pitch(des)")

pitch16 <- select(tbl(my_dbProd, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, sz_top, sz_bot, px, pz, pitch_type, count, zone)
atbat16 <- select(tbl(my_dbProd, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)

#src_tbls(my_dbProd)
#tbl(my_dbProd, "pitch")
#head(tbl(my_dbProd, "pitch"),3)


# doesn't work '467092'
# works for all other hitters
#hitters <- c('547180','457705','502671','518626','502517','518934','445988','471865','120074','514888')
#hitters <- c('502671','518626','502517','518934','445988','471865','120074','514888')
#mlbID <- '502671'

hitters <- c('514888','453568','457759','519317','458015','547180','592450','545361','457705','502671','518626','502517','518934','471865','592178','519346','460075')
#hitters <- c('514888','453568')

for (mlbID in hitters) {
    print(mlbID)
    # filter atbats by mlbID
    TargetedAtBats <- filter(atbat16, batter == mlbID)
    
    # join filtered atbats to all pitches
    pitchesJoin <- collect(inner_join(pitch16, TargetedAtBats))
    
    # score Qual and Quant mutate
    joined <- pitchesJoin %>% mutate(quant_score_des = get_quant_score(des),
                                      fix_quant_score = fix_quant_score(event) * (des == 'In play, run(s)'),
                                      quant_score = quant_score_des + fix_quant_score,
                                      qual_score = get_qual_score(atbat_des) * (type == 'X'),
                                      hitter_val = quant_score + qual_score)
    
    # convert to factor variables
    joined$pitch_type <- as.factor(joined$pitch_type) 
    joined$des <- as.factor(joined$des) 
    joined$type <- as.factor(joined$type)
    joined$count <- as.factor(joined$count) 
    joined$event <- as.factor(joined$event) 
    joined$p_throws <- as.factor(joined$p_throws)
    joined$zone <- as.factor(joined$zone)
    joined$stand <- as.factor(joined$stand)
    joined$inning <- as.factor(joined$inning)
    joined$inning_side <- as.factor(joined$inning_side)
    
    # convert FS and FT to SInkers 
    levels(joined$pitch_type)[levels(joined$pitch_type)=="FS"] <- "SI"
    levels(joined$pitch_type)[levels(joined$pitch_type)=="FT"] <- "SI"
    levels(joined$pitch_type)[levels(joined$pitch_type)=="FC"] <- "SL"
    levels(joined$pitch_type)[levels(joined$pitch_type)=="KC"] <- "KN"
    
    #subset for All Hits and AllBallsInPlay 
    #subAllHits <- subset(joined, type == "X" & des == "In play, no out" | des =="In play, run(s)")
    subAllBallsInPlay <- subset(joined, type == "X")
    
    # generate plots
    create_hv_plots(joined,mlbID)
    #create_HeatMap_plots(subAllBallsInPlay, mlbID)
}
