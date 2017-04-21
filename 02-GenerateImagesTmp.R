## load libraries
#INSTALL PRIOR TO RUNNING IN CLOUD
install.packages("RSQLite", type="binary")
install.packages("XML2R", type="binary")
install.packages("pitchRx", type="binary")
install.packages("dplyr", type="binary")
install.packages("stringr", type="binary")
install.packages("ggplot2", type="binary")
install.packages("RColorBrewer", type="binary")
install.packages("graphics", type="binary")
install.packages("akima", type="binary")

#LOADING in RSTUDIO
library(RSQLite)
library(ggplot2)
library(pitchRx)
library(dplyr)
library(stringr)
library(graphics)
library(RColorBrewer)
library(akima)


## TEMPORARY while loading from SQL database
my_db1 <- src_sqlite("pitchRx.sqlite3", create = TRUE)

pitch16 <- select(tbl(my_db1, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, sz_top, sz_bot, px, pz, pitch_type, count, zone)
atbat16 <- select(tbl(my_db1, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)



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



### Creates Traditional Heat Maps. Receive subAllBallsInPlay as subAPIB.   Also mlbID.
create_HeatMap_plots <- function(subABIP, mlbID, ...) {
    #Testing
    #subABIP <- subAllBallsInPlay
    
    # update labeler function for graphs if needed
    pitch_label <- c(
        L = "",
        R = ""
    )
    
    #subABIP <- subAllBallsInPlay
    
    # subset All Balls in Play by LHP, RHP
    subABIP.RHP <- subABIP %>% filter(p_throws=="R")
    subABIP.LHP <- subABIP %>% filter(p_throws=="L")
    
    # subset All Balls in Play by pitch type.
    subABIP.RHP.FF <- subABIP.RHP %>% filter(pitch_type=="FF")
    subABIP.RHP.SL <- subABIP.RHP %>% filter(pitch_type=="SL")
    subABIP.RHP.CU <- subABIP.RHP %>% filter(pitch_type=="CU")
    subABIP.RHP.SI <- subABIP.RHP %>% filter(pitch_type=="SI")
    subABIP.RHP.CH <- subABIP.RHP %>% filter(pitch_type=="CH")
#    subABIP.RHP.KN <- subABIP.RHP %>% filter(pitch_type=="KN")
    
    subABIP.LHP.FF <- subABIP.LHP %>% filter(pitch_type=="FF")
    subABIP.LHP.SL <- subABIP.LHP %>% filter(pitch_type=="SL")
    subABIP.LHP.CU <- subABIP.LHP %>% filter(pitch_type=="CU")
    subABIP.LHP.SI <- subABIP.LHP %>% filter(pitch_type=="SI")
    subABIP.LHP.CH <- subABIP.LHP %>% filter(pitch_type=="CH")
#    subABIP.LHP.KN <- subABIP.LHP %>% filter(pitch_type=="KN")    
    
    #strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
    #         density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    # generate Heat Map per p_throws and per p_type
    ## Save plot to working directory in the plots sub-folder
    
    filename = str_c(mlbID,"-rhp-hm-FF.png")
    print(filename)
    print(nrow(subABIP.RHP.FF))
    if (nrow(subABIP.RHP.FF) > 8 ) {
        strikeFX(subABIP.RHP.FF, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("FF Heat Map - RHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    } else { print(c('not generating a plot - too few observations ', nrow(subABIP.RHP.FF))) }
    
    filename = str_c(mlbID,"-rhp-hm-SL.png")
    print(filename)
    print(nrow(subABIP.RHP.SL))
    if (nrow(subABIP.RHP.SL) > 8 ) {
        strikeFX(subABIP.RHP.SL, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("SL Heat Map - RHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    } else { print(c('not generating a plot - too few observations ', nrow(subABIP.RHP.SL))) }
    
    filename = str_c(mlbID,"-rhp-hm-CU.png")
    print(filename)
    print(nrow(subABIP.RHP.CU))
    if (nrow(subABIP.RHP.CU) > 8 ) {
        strikeFX(subABIP.RHP.CU, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("CU Heat Map - RHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.RHP.CU))) }
    
    filename = str_c(mlbID,"-rhp-hm-SI.png")
    print(filename)
    print(nrow(subABIP.RHP.SI))
    if (nrow(subABIP.RHP.SI) > 8 ) {
        strikeFX(subABIP.RHP.SI, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("SI Heat Map - RHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.RHP.SI))) }
    
    filename = str_c(mlbID,"-rhp-hm-CH.png")
    print(filename)
    print(nrow(subABIP.RHP.CH))
    if (nrow(subABIP.RHP.CH) > 8 ) {
        strikeFX(subABIP.RHP.CH, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("CH Heat Map - RHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.RHP.CH))) }

## now for lefties
    filename = str_c(mlbID,"-lhp-hm-FF.png")
    print(filename)
    print(nrow(subABIP.LHP.FF))
    if (nrow(subABIP.LHP.FF) > 8) {
    strikeFX(subABIP.LHP.FF, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("FF Heat Map - LHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.LHP.FF))) }
    
    filename = str_c(mlbID,"-lhp-hm-SL.png")
    print(filename)
    print(nrow(subABIP.LHP.SL))
    if (nrow(subABIP.LHP.SL) > 8) {
    strikeFX(subABIP.LHP.SL, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("SL Heat Map - LHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.LHP.SL))) }

    filename = str_c(mlbID,"-lhp-hm-CU.png")
    print(filename)
    print(nrow(subABIP.LHP.CU))
    if (nrow(subABIP.LHP.CU) > 8) {
    strikeFX(subABIP.LHP.CU, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("CU Heat Map - LHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.LHP.CU))) }
    
    filename = str_c(mlbID,"-lhp-hm-SI.png")
    print(filename)
    print(nrow(subABIP.LHP.SI))
    if (nrow(subABIP.LHP.SI) > 8) {
    strikeFX(subABIP.LHP.SI, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("SI Heat Map - LHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.LHP.SI))) }
    
    filename = str_c(mlbID,"-lhp-hm-CH.png")
    print(filename)
    print(nrow(subABIP.LHP.CH))
    if (nrow(subABIP.LHP.CH) > 8 ) {
    strikeFX(subABIP.LHP.CH, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("CH Heat Map - LHP")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    # system(sprintf("/Library/Frameworks/Python.framework/Versions/3.5/bin/aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
    system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
	} else { print(c('not generating a plot - too few observations ', nrow(subABIP.LHP.CH))) }
}

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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
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
  ggsave(filename, device="png", path="plots/", width = 7, height = 7)
  
  system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s --body ./plots/%1$s", filename))
  
}

# doesn't work '467092'
# works for all other hitters
#hitters <- c('547180','457705','502671','518626','502517','518934','445988','471865','120074','514888')
#hitters <- c('502671','518626','502517','518934','445988','471865','120074','514888')
hitters <- c('120074')
#mlbID <- '445988'

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
    create_HeatMap_plots(subAllBallsInPlay, mlbID)
}