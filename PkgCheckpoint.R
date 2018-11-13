## Install pitchRx directly from GitHub
install.packages("devtools")
library(devtools)
install_github("cpsievert/pitchRx", force = T)

3## This R command lists all the packages installed by the user
## (ignoring packages that come with R such as base and foreign) and the
## package versions.
ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)

## Create a checkpoint list of all installed packages
## https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
install.packages("checkpoint")
library(checkpoint)
checkpoint("2018-10-02")

## check which packages are installed in checkpoint library:
installed.packages(.libPaths()[1])[, "Package"]

## cleanup

unlink(example_project, recursive = TRUE)
unlink(file.path(tempdir(), "checkpoint_example_code.R"))
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)
options(repos = oldRepos)
unCheckpoint(oldLibPaths)
.libPaths()

## [1] "C:/tmp/Rtmp69rJVn/Rinst1e3c1470709e"
## [2] "C:/Users/richcala/Documents/R/win-library/3.5"
## [3] "C:/Program Files/R/R-3.5.1/library"



## Create a folder to contain the checkpoint
## This is optional - the default is to use ~/.checkpoint

dir.create(file.path(tempdir(), ".checkpoint"), recursive = TRUE, showWarnings = FALSE)

## Create a checkpoint by specifying a snapshot date

library(checkpoint)
checkpoint("2018-10-02", project = mlb-analytics-ant,
           checkpointLocation = tempdir())

#cleanup
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)


# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)

# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))

# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]

# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)

# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)

# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)



