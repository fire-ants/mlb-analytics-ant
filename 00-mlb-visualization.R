library(RJSONIO)    # used for Pulling from CF API
library(RCurl)      # used for Pulling from CF API
library(dplyr)      # working with database | also includes %>% pipe notation
library(ggplot2)    # plotting library
library(akima)      # used for interpolation

hvPlot <- function(mlbid, start_date, end_date) {

    # open connection to database
    my_mlb_db <- DBI::dbConnect(RMySQL::MySQL(), 
                        host = Sys.getenv("mlb_db_hostname"),
                        dbname = Sys.getenv("mlb_db_dbname"),
                        user = Sys.getenv("mlb_db_username"),
                        password = Sys.getenv("mlb_db_password")
    )

    print(paste0("Generating Hitter-Val Strikezone plots for Batter with MLBID: ",mlbid))

    ## Pull Batter Data for Date Range
    sql_query = paste0("SELECT * FROM rawdata_joined where batter = '", mlbid, "' ", 
                        "AND date BETWEEN '", format(start_date,"%Y/%m/%d"), "'",
                        " and '", format(end_date,"%Y/%m/%d"), "'")
    
    working_data <- dbGetQuery(my_mlb_db, sql_query)
    
    ## Add Batter's strike zone
    topKzone = mean(working_data$sz_top, na.rm = TRUE)
    botKzone = mean(working_data$sz_bot, na.rm = TRUE) 
    inKzone = -.95
    outKzone = 0.95
    kZone = data.frame(
        x = c(inKzone, inKzone, outKzone, outKzone, inKzone), 
        y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    
    pitch_types <- c("FF","SL","CH","CU","SI")
    p_throws <- c("R","L")

    for(pitch_type_value in pitch_types) {
        for(p_throw_value in p_throws) {
            print(paste0("Generating Visualization for ", pitch_type_value," - ", p_throw_value, "HP"))
            
            ## Filter out by Batter and Pitch Type [PT] and pitcher throwing hand (ie. RHP, LHP) [HP]
            sub.PT <- working_data %>% filter(pitch_type==pitch_type_value)
            sub.PT.HP <- sub.PT %>% filter(p_throws==p_throw_value)
            
            hv.PT <- data.frame(x = sub.PT.HP$px, y = sub.PT.HP$pz, z = sub.PT.HP$hitter_val)
            hv.PT.grid <- interp(hv.PT$x, hv.PT$y, hv.PT$z)
            hv.PT.grid2 <- expand.grid(x=hv.PT.grid$x, y=hv.PT.grid$y)
            hv.PT.grid2$z <- as.vector(hv.PT.grid$z)
            
            b1 <- colorRampPalette(c("navy","royalblue","white"))(200)
            re <- colorRampPalette(c("white","red2","darkred"))(200)
            
            ggplot(hv.PT.grid2) + labs(x="Horizontal Pitch Location",y="Height from Ground") + 
                ggtitle(paste0(pitch_type_value," Hitter Value - ", p_throw_value,"HP")) + 
                geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + 
                geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + 
                scale_fill_gradientn(name="Hitter\nValue",colors=c(b1,"white",re), na.value="white", limits=c(min(working_data$hitter_val),max(working_data$hitter_val))) + 
                geom_path(aes(x, y), data = kZone, linetype = 1) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + 
                theme_bw() + theme(plot.title = element_text(hjust = 0.5))
                
            ## Save plot to working directory
            filename = paste0(mlbid,"-", p_throw_value,"hp-hv-", pitch_type_value,".png")
            ggsave(filename, device="png", path=".", width = 7, height = 7)
        
            ## Push the plot to S3 bucket
            ## will need to figure out how to configure access via environmental variables
            ## system(sprintf("aws s3api put-object --bucket mlb-pf --key %1$s --body /db/%1$s", filename))
        
            ## cleaning up
        
            rm(hv.PT.grid2)
            rm(hv.PT.grid)
            rm(hv.PT)
            rm(sub.PT.HP)
            rm(sub.PT)
        
            ## tested to work - remove local file (plot image) after pushing plot to S3 bucket
            # file.remove(filename)
        }
    }

    # clean up connection to the database
    rm(my_mlb_db)
}

# Pull batters from API
playerAPIList <- fromJSON(getURL("http://mlb-player-api.cfapps.io/player/"))

print(playerAPIList)

# create a null list
hitters <- vector("list", length(playerAPIList))

# populate list of batter mlbids
for (i in 1:length(playerAPIList)) {
    # print(playerAPIList[[i]][["mlbid"]])
    hitters[i] <- as.character(playerAPIList[[i]][["mlbid"]])
}

# actually need to determine start and end dates dynamically
# these static values are temporary
start_date <- as.Date("04-01-17",format="%m-%d-%y")
end_date   <- as.Date("06-01-17",format="%m-%d-%y")

# mlbid in local testing working data
# hitters <- 519317

for (mlbid in hitters) {
    hvPlot(mlbid, start_date, end_date)
}