## load libraries
library(ggplot2)
library(pitchRx)
library(dplyr)
library(stringr)
library(graphics)
library(RColorBrewer)

create_plots <- function(data, mlbID, ...) {
    filename = str_c(mlbID,"-ALL.png")
    png(filename)
    brewer.pal(11, "RdYlBu")
    buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
    smoothScatter(data$pz~data$px, nbin=1000, colramp = colorRampPalette(c(buylrd)), nrpoints=Inf, pch="", cex=.7, transformation = function(x) x^.6, col="black", main="Pitch Locations", xlab="Horizontal Location", ylab="Vertical Location")
    lines(c(0.708335, 0.708335), c(mean(data$sz_bot), mean(data$sz_top)), col="white", lty="dashed", lwd=2)
    lines(c(-0.708335, -0.708335), c(mean(data$sz_bot), mean(data$sz_top)), col="white", lty="dashed", lwd=2)
    lines(c(-0.708335, 0.708335), c(mean(data$sz_bot), mean(data$sz_bot)), col="white", lty="dashed", lwd=2)
    lines(c(-0.708335, 0.708335), c(mean(data$sz_top), mean(data$sz_top)), col="white", lty="dashed", lwd=2)
    dev.off()
    
    ## print(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
    #system(sprintf("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key %1$s-All.png --body %1$s-All.png", mlbID))
}

hittersLst <- c('547180','457705','502671','518626','502517','518934','467092','445988','471865','120074','514888')
#mlbdata <- pullMLBDataAndScore()


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
    
    
    # generate plots
    create_plots(joined,mlbID)
}