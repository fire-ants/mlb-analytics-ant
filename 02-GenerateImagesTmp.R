## load libraries
library(ggplot2)
library(pitchRx)
library(dplyr)
library(stringr)
library(graphics)
library(RColorBrewer)
library(akima)
#library(wesanderson)

create_heatmap_plots <- function(data, mlbID, ...) {
    # data <- joined
    
    #filename = str_c(mlbID,"-ALL.png")
    png(filename)
    brewer.pal(3, "RdBu")
    #wes_palette("Zissou")
    #pal <- wes_palette("Zissou", 100, type = "continuous")
    ## Add Batter's strike zone
    ## not required if loading workspace workspace_tables_with_pitch_type
    topKzone = mean(data$sz_top, na.rm = TRUE)
    botKzone = mean(data$sz_bot, na.rm = TRUE) 
    inKzone = -.95
    outKzone = 0.95
    kZone = data.frame(
        x = c(inKzone, inKzone, outKzone, outKzone, inKzone), 
        y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    
    ## Filter out by Batter and Pitch Type
    ## not required if loading workspace workspace_tables_with_pitch_type
    sub.FF <- joined %>% filter(pitch_type=="FF")
    sub.FF.RHP <- sub.FF %>% filter(p_throws=="R")
    sub.FF.LHP <- sub.FF %>% filter(p_throws=="L")
    
    sub.SL <- joined %>% filter(pitch_type=="SL")
    sub.SL.RHP <- sub.SL %>% filter(p_throws=="R")
    sub.SL.LHP <- sub.SL %>% filter(p_throws=="L")
    
    sub.CH <- joined %>% filter(pitch_type=="CH")
    sub.CH.RHP <- sub.CH %>% filter(p_throws=="R")
    sub.CH.LHP <- sub.CH %>% filter(p_throws=="L")
    
    
    sub.CU <- joined %>% filter(pitch_type=="CU")
    sub.CU.RHP <- sub.CU %>% filter(p_throws=="R")
    sub.CU.LHP <- sub.CU %>% filter(p_throws=="L")
    
    #sub.FC <- joined %>% filter(batter=="545361",pitch_type=="FC")
    #sub.FT <- joined %>% filter(batter=="545361",pitch_type=="FT")
    sub.SI <- joined %>% filter(pitch_type=="SI")
    sub.SI.RHP <- sub.SI %>% filter(p_throws=="R")
    sub.SI.LHP <- sub.SI %>% filter(p_throws=="L")
    
    #sub.FS <- joined %>% filter(batter=="545361",pitch_type=="FS")
    
    ## FF Four-seam fastball - RHP
    hv.FF <- data.frame(x = sub.FF.RHP$px, y = sub.FF.RHP$pz, z = sub.FF.RHP$hitter_val)
    hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
    hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
    hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.FF.grid2) + labs(x="x pos",y="z pos") + ggtitle("FF Hitter Value - RHP") + 
        geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
        geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
        scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
        geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
        theme_bw()
 
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-rhp-hm-FF.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    dev.off()
    
    ## FF Four-seam fastball - LHP
    hv.FF <- data.frame(x = sub.FF.LHP$px, y = sub.FF.LHP$pz, z = sub.FF.LHP$hitter_val)
    hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
    hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
    hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.FF.grid2) + labs(x="x pos",y="z pos") + ggtitle("FF Hitter Value - LHP") + 
        geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
        geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
        scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
        geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
        theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-lhp-hm-FF.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    dev.off()
    
    
    
    ## SL Slider - RHP
    hv.SL <- data.frame(x = sub.SL.RHP$px, y = sub.SL.RHP$pz, z = sub.SL.RHP$hitter_val)
    hv.SL.grid <- interp(hv.SL$x, hv.SL$y, hv.SL$z)
    hv.SL.grid2 <- expand.grid(x=hv.SL.grid$x, y=hv.SL.grid$y)
    hv.SL.grid2$z <- as.vector(hv.SL.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.SL.grid2) + labs(x="x pos",y="z pos") + ggtitle("SL Hitter Value - RHP") + 
        geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
        geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
        scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
        geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
        theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-rhp-hm-SL.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    dev.off()
    
    ## SL Slider - LHP
    hv.SL <- data.frame(x = sub.SL.LHP$px, y = sub.SL.LHP$pz, z = sub.SL.LHP$hitter_val)
    hv.SL.grid <- interp(hv.SL$x, hv.SL$y, hv.SL$z)
    hv.SL.grid2 <- expand.grid(x=hv.SL.grid$x, y=hv.SL.grid$y)
    hv.SL.grid2$z <- as.vector(hv.SL.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.SL.grid2) + labs(x="x pos",y="z pos") + ggtitle("SL Hitter Value - LHP") + 
        geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
        geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
        scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
        geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
        theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-lhp-hm-SL.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    dev.off()
    
    ## print(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
    #system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
}

hittersLst <- c('547180','457705','502671','518626','502517','518934','467092','445988','471865','120074','514888')
hitters <- c('518626')
#mlbdata <- pullMLBDataAndScore()
mlbID <- '518626'

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
    
    #subset for All Hits and AllBallsInPlay 
    #subAllHits <- subset(joined, type == "X" & des == "In play, no out" | des =="In play, run(s)")
    subAllBallsInPlay <- subset(joined, type == "X")
    
    pitch_label <- c(
        L = "LHP",
        R = "RHP"
    )
    
    # create Traditional Heat Map
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(type = "X"), layer = facet_grid(. ~ stand, labeller = labeller(p_throws = pitch_label)))
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(. ~ stand, labeller = labeller(p_throws = pitch_label)))
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    # generate plots
    create_heatmap_plots(joined,mlbID)
}