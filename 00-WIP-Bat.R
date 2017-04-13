# This function creates Traditional Heat Maps. Receive subAllBallsInPlay as subAPIB.   Also mlbID.
create_HeatMap_plots <- function(subABIP, mlbID, ...) {
    
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
    subABIP.RHP.KN <- subABIP.RHP %>% filter(pitch_type=="KN")
    subABIP.LHP.FF <- subABIP.LHP %>% filter(pitch_type=="FF")
    subABIP.LHP.SL <- subABIP.LHP %>% filter(pitch_type=="SL")
    subABIP.LHP.CU <- subABIP.LHP %>% filter(pitch_type=="CU")
    subABIP.LHP.SI <- subABIP.LHP %>% filter(pitch_type=="SI")
    subABIP.LHP.CH <- subABIP.LHP %>% filter(pitch_type=="CH")
    subABIP.LHP.KN <- subABIP.LHP %>% filter(pitch_type=="KN")    
    
    strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
             density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
    
    # generate Heat Map per p_throws and per p_type
    ## Save plot to working directory in the plots sub-folder
    
    filename = str_c(mlbID,"-rhp-hm-FF.png")
    ggsave(filename, device="png", path="plots/", width = 7, height = 7)
    strikeFX(subABIP.RHP.FF, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("FF Heat Map - RHP")
    #dev.off()
    

    strikeFX(subABIP.RHP.FF, geom = "raster", density1 = list(type = "X"),
        density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, 
        labeller = labeller(p_throws = pitch_label))) + 
        theme(legend.position="none") + 
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) +
        theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white")) +
        ggtitle("FF Heat Map - RHP")
    
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



subABIP.FF <- subAllBallsInPlay %>% filter(pitch_type=="FF")


