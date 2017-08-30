## broken half graph
strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"), layer=facet_grid(.~stand))

# this is what we want but half the graph is gone
strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(quant_score = 1),
         density2 = list(type = "X"))

strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(quant_score = 1))


strikeFX(subABIP.LHP.SL, layer=facet_grid(.~stand))

strikeFX(pitchesJoin, geom="raster", density1=list(des="Called Strike"), density2=list(des="Ball"),
         draw_zones=FALSE, contour=TRUE, layer=facet_grid(.~stand))

strikeFX(subABIP.LHP.SL, geom = "raster", 
    density1 = list(subABIP.LHP.SL$type == "X"),
    density2 = list(subABIP.LHP.SL$quant_score == 1)
    
    , 
    layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label))) + 
    theme(legend.position="none") + 
    coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
    theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
    ggtitle("SL Heat Map - LHP")

density1 = list(subABIP.LHP.SL$type == "X")

###########
filename = str_c(mlbID,"-rhp-hm-FF.png")
print(filename)
strikeFX(subABIP.RHP.FF, geom = "raster", 
    density1 = list(type = "X"),
    density2 = list(quant_score = 1)) + 
    theme(legend.position="none") + 
    coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
    theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
    ggtitle("FF Heat Map - RHP")
ggsave(filename, device="png", path="plots/", width = 7, height = 7)

strikeFX(subABIP.LHP.SL, geom = "raster", 
         density1 = list(type = "X"),
         density2 = list(quant_score = 1), 
         layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label))) + 
    theme(legend.position="none")


subAllBallsInPlay <- subset(joined, type == "X")

pitch_label <- c(
    L = "LHP",
    R = "RHP"
)

strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))

#####################
heat_plot <- function(player, d, HR=FALSE){
    # inputs
    # player - name of player
    # d - pitchRX data frame with variables batter, event, and px, pz (location of pitch)
    # will output a ggplot2 object
    # need to use print function to display the plot
    require(dplyr)
    require(ggplot2)
    require(mgcv)
    # define the strike zone
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.95
    outKzone <- 0.95
    kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    
    # only consider events that are official at-bats
    TT <- table(d$event)
    To_Remove_indices <- c(1, 5, 8, 16, 18, 21, 22, 23, 24, 30)
    AB <- names(TT)[-To_Remove_indices]
    d_AB <- filter(d, event %in% AB )
    # define the 1/0 response variable
    if(HR == FALSE) d_AB <- mutate(d_AB, 
                                   Hit=ifelse(event %in% c("Single", "Double", "Triple", "Home Run"),
                                              1, 0)) else
                                                  d_AB <- mutate(d_AB, Hit=ifelse(event == "Home Run",
                                                                                  1, 0))
    
    # implement the GAM fit (logistic link)
    pdata <- filter(d_AB, batter==player)
    fit <- gam(Hit ~ s(X,Z), family=binomial, data=pdata)
    
    # find predicted probabilities over a 50 x 50 grid
    x <- seq(-1.5, 1.5, length.out=50)
    y <- seq(0.5, 5, length.out=50)
    data.predict <- data.frame(X = c(outer(x, y * 0 + 1)),
                               Z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))
    
    # construct the plot
    type <- ifelse(HR==TRUE, "HR", "HIT")
    ggplot(kZone, aes(x, y)) +
        geom_tile(data=data.predict, 
                  aes(x=X, y=Z, fill= Probability)) +
        scale_fill_distiller(palette = "Spectral") +
        geom_path(lwd=1.5, col="black") +
        coord_fixed() +
        ggtitle(paste(player, type))
}

heat_plot(mlbID, subAllBallsInPlay, HR=FALSE)



relabel <- function(variable, value) {
    value <- sub("^R$", "RHP", value)
    sub("^L$", "LHP", value)
}
strikes <- subset(decisions, strike == 1)
strikeFX(subAllBallsInPlay, geom = "raster", layer = facet_grid(. ~ p_throws, labeller = relabel))

# works but with half-graph    
strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(des = "In play, out(s)")) + facet_grid(. ~ stand, labeller = relabel)

strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(des = "In play, out(s)")) + facet_grid(. ~ stand, labeller = relabel)

# works but with half graph
strikeFX(subABIP.RHP.FF, geom = "raster", density1 = list(type = "X"),
    density2 = list(quant_score = 1),  
    labeller = labeller(p_throws = pitch_label)) + 
    theme(legend.position="none") + 
    coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
    theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
    ggtitle("FF Heat Map - RHP")


facet_layer <- facet_grid(. ~ stand)

strikeFX(subABIP.RHP.FF, geom = "raster", density1 = list(type = "X"),
    density2 = list(quant_score = 1), layer = facet_layer, 
    labeller = labeller(p_throws = pitch_label)) + 
    theme(legend.position="none") + 
    coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
    theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
    ggtitle("FF Heat Map - RHP")


strikeFX(pitchesJoin, geom = "raster", density1 = list(des = "Called Strike"),
         density2 = list(des = "Ball"), layer = facet_grid(. ~ stand, labeller = relabel))

