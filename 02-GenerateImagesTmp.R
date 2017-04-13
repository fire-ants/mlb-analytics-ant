## load libraries
library(ggplot2)
library(pitchRx)
library(dplyr)
library(stringr)
library(graphics)
library(RColorBrewer)
library(akima)
#library(wesanderson)


create_HeatMap_plots <- function(subAPIB, mlbID, ...) {

# update labeler function for graphs if needed
    pitch_label <- c(
        L = "LHP",
        R = "RHP"
    )

subs
    
    strikeFX(subABIP.FF.RHP, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws))

    
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    #png(filename = "bat-test.png")
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws))
    #dev.off()
    
    subABIP.FF <- subAllBallsInPlay %>% filter(pitch_type=="FF")
    subABIP.RHP <- subAllBallsInPlay %>% filter(p_throws=="R")
    
    subABIP.FF.RHP <- subABIP.FF %>% filter(p_throws=="R")
    subABIP.FF.LHP <- subABIP.FF %>% filter(p_throws=="L")
    
    strikeFX(subABIP.FF.RHP, geom = "hex", density1 = list(type = "X"),
             density2 = list(quant_score = 1))
    
    strikeFX(subABIP.FF.RHP, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws))
    
    strikeFX(subABIP.FF.LHP, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws))
    
    strikeFX(subABIP.FF.LHP, geom = "hex", density1 = list(type = "X"),
             density2 = list(quant_score = 1))
    
    strikeFX(subABIP.RHP, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    
}


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
    
    sub.SI <- joined %>% filter(pitch_type=="SI")
    sub.SI.RHP <- sub.SI %>% filter(p_throws=="R")
    sub.SI.LHP <- sub.SI %>% filter(p_throws=="L")
    
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
    filename = str_c(mlbID,"-rhp-hv-FF.png")
    ggsave(filename, device="png", path="plots/")
    
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
    filename = str_c(mlbID,"-lhp-hv-FF.png")
    ggsave(filename, device="png", path="plots/")

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
    filename = str_c(mlbID,"-rhp-hv-SL.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    #dev.off()
    
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
    filename = str_c(mlbID,"-lhp-hv-SL.png")
    #png(filename)
    ggsave(filename, device="png", path="plots/")
    #dev.off()
    
## CH Four-seam fastball - RHP
    hv.CH <- data.frame(x = sub.CH.RHP$px, y = sub.CH.RHP$pz, z = sub.CH.RHP$hitter_val)
    hv.CH.grid <- interp(hv.CH$x, hv.CH$y, hv.CH$z)
    hv.CH.grid2 <- expand.grid(x=hv.CH.grid$x, y=hv.CH.grid$y)
    hv.CH.grid2$z <- as.vector(hv.CH.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.CH.grid2) + labs(x="x pos",y="z pos") + ggtitle("CH Hitter Value - RHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-rhp-hv-CH.png")
    ggsave(filename, device="png", path="plots/")
    
    ## CH Four-seam fastball - LHP
    hv.CH <- data.frame(x = sub.CH.LHP$px, y = sub.CH.LHP$pz, z = sub.CH.LHP$hitter_val)
    hv.CH.grid <- interp(hv.CH$x, hv.CH$y, hv.CH$z)
    hv.CH.grid2 <- expand.grid(x=hv.CH.grid$x, y=hv.CH.grid$y)
    hv.CH.grid2$z <- as.vector(hv.CH.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.CH.grid2) + labs(x="x pos",y="z pos") + ggtitle("CH Hitter Value - LHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-lhp-hv-CH.png")
    ggsave(filename, device="png", path="plots/")
    
## CU Four-seam fastball - RHP
    hv.CU <- data.frame(x = sub.CU.RHP$px, y = sub.CU.RHP$pz, z = sub.CU.RHP$hitter_val)
    hv.CU.grid <- interp(hv.CU$x, hv.CU$y, hv.CU$z)
    hv.CU.grid2 <- expand.grid(x=hv.CU.grid$x, y=hv.CU.grid$y)
    hv.CU.grid2$z <- as.vector(hv.CU.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.CU.grid2) + labs(x="x pos",y="z pos") + ggtitle("CU Hitter Value - RHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-rhp-hv-CU.png")
    ggsave(filename, device="png", path="plots/")
    
    ## CU Four-seam fastball - LHP
    hv.CU <- data.frame(x = sub.CU.LHP$px, y = sub.CU.LHP$pz, z = sub.CU.LHP$hitter_val)
    hv.CU.grid <- interp(hv.CU$x, hv.CU$y, hv.CU$z)
    hv.CU.grid2 <- expand.grid(x=hv.CU.grid$x, y=hv.CU.grid$y)
    hv.CU.grid2$z <- as.vector(hv.CU.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.CU.grid2) + labs(x="x pos",y="z pos") + ggtitle("CU Hitter Value - LHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-lhp-hv-CU.png")
    ggsave(filename, device="png", path="plots/")
    
    
## SI Four-seam fastball - RHP
    hv.SI <- data.frame(x = sub.SI.RHP$px, y = sub.SI.RHP$pz, z = sub.SI.RHP$hitter_val)
    hv.SI.grid <- interp(hv.SI$x, hv.SI$y, hv.SI$z)
    hv.SI.grid2 <- expand.grid(x=hv.SI.grid$x, y=hv.SI.grid$y)
    hv.SI.grid2$z <- as.vector(hv.SI.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.SI.grid2) + labs(x="x pos",y="z pos") + ggtitle("SI Hitter Value - RHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-rhp-hv-SI.png")
    ggsave(filename, device="png", path="plots/")
    
    ## SI Four-seam fastball - LHP
    hv.SI <- data.frame(x = sub.SI.LHP$px, y = sub.SI.LHP$pz, z = sub.SI.LHP$hitter_val)
    hv.SI.grid <- interp(hv.SI$x, hv.SI$y, hv.SI$z)
    hv.SI.grid2 <- expand.grid(x=hv.SI.grid$x, y=hv.SI.grid$y)
    hv.SI.grid2$z <- as.vector(hv.SI.grid$z)
    
    b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
    re <- colorRampPalette(c("white","red2","darkred"))(200)
    
    ggplot(hv.SI.grid2) + labs(x="x pos",y="z pos") + ggtitle("SI Hitter Value - LHP") + 
      geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
      geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
      scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(data$hitter_val),max(data$hitter_val))) + 
      geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
      theme_bw()
    
    ## Save plot to working directory in the plots sub-folder
    filename = str_c(mlbID,"-lhp-hv-SI.png")
    ggsave(filename, device="png", path="plots/")
    
    
    ## print(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
    #system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
}


# hittersLst <- c('547180','457705','502671','518626','502517','518934','467092','445988','471865','120074','514888')
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
    
    
    # generate plots
    create_HeatMap_plots(subAllBallsInPlay, mlbID)
    create_HV_plots(joined,mlbID)
    
#   strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
#             density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws, labeller = labeller(p_throws = pitch_label)))
#    
#    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
#             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
#    
#    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
#             density2 = list(type = "X"), layer = facet_grid(. ~ stand, labeller = labeller(p_throws = pitch_label)))
#    
#    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
#             density2 = list(quant_score = 1), layer = facet_grid(. ~ stand, labeller = labeller(p_throws = pitch_label)))
#    
#    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
#             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    # generate plots
    create_hv_plots(joined,mlbID)

}