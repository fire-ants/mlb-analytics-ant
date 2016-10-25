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

pullMLBDataAndScore <- function() {
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
  
  return (joined)
}

filterMLBData <- function(mlbData, mlbID) {
  
  ## subset At Bats for Mike Trout At Bats
  subJoined <- subset(mlbData, batter == mlbID)
  
  return (subJoined)
}

create_plots <- function(data) {
  filename = str_c(mlbID,"-ALL.png")
  
  png(filename)
  brewer.pal(11, "RdYlBu")
  buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
  smoothScatter(data$pz~data$px, nbin=1000, colramp = colorRampPalette(c(buylrd)), nrpoints=Inf, pch="", cex=.7, transformation = function(x) x^.6, col="black", main="Pitch Locations", xlab="Horizontal Location", ylab="Vertical Location")
  lines(c(0.708335, 0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
  lines(c(-0.708335, -0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
  lines(c(-0.708335, 0.708335), c(mean(subTrout$sz_bot), mean(subTrout$sz_bot)), col="white", lty="dashed", lwd=2)
  lines(c(-0.708335, 0.708335), c(mean(subTrout$sz_top), mean(subTrout$sz_top)), col="white", lty="dashed", lwd=2)
  dev.off()
  
  
  
}

mlbdata <- pullMLBDataAndScore()

hitters <- c('545361','543606')

for (mlbID in hitters) {
  print(mlbID)
  data <- filterMLBData(mlbdata, '545361')
  
  create_plots(data,mlbID)
  
}




## FF Four-seam fastball
hv.FF <- data.frame(x = subTrout.FF$px, y = subTrout.FF$pz, z = subTrout.FF$hitter_val)
hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
ggplot(hv.FF.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout FF Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_FF.png", device="png", path="plots/")

system("aws s3api put-object --endpoint-url https://ecs2-us-central-1.emc.io/ --bucket fireants-dev --key 545361_hv_FF.png --body ./plots/545361_hv_FF.png")

