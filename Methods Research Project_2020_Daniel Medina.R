#### METHODS RESEARCH PROJECT DANIEL I. MEDINA OVIEDO ####


#### DENSITY AND DISTRIBUTION MONGOLIAN LARGE MAMMALS CAMERA TRAPPING-BASED APPROACH ####

getwd()
setwd(":/.../.../...")      # adapt it at your convenience

rm()
rm(list = ls())


#### PREPARATION FOR THE ANALYSIS ####

# First, I need to install few packages to perform all the analysis:

install.packages(c("dsm", "Distance", "knitr", "captioner", "ggplot2", "rgdal", "maptools", "plyr", "tweedie", 
                   "RCurl", "suncalc", "dplyr", "DataCombine", "fasttime", "tidyr", "stringi", "splancs", 
                   "data.table"))

require(dsm)

require(Distance)

require(knitr)

require(captioner)

require(ggplot2)

require(rgdal)

require(maptools)

require(plyr)

require(tweedie)

require(RCurl)

require(suncalc)

require(dplyr)

require(DataCombine)

require(fasttime)

require(tidyr)

require(stringi)

require(splancs)

require(data.table)

options(stringsAsFactors=FALSE)


# Import my metadata file

metdat <- read.csv("metadata.csv", header = TRUE, stringsAsFactors = FALSE)


# Dataset with 669,989 rows (Camera Trap images) totalising 78 camera traps, each one with a different number of 
# images captured during their time of deployment for the study, which span between April and October of 2019 
# (almost seven months).


#### CORRECTION OF MISLABELLED CAMERA TRAP NAMES AND DATA ####


# First, I need to correct "metdat" rows that were mislabeled:

# The camera trap R83-ZSL60 was mislabeled as "R84-ZSL60", so its information rows were summed up to the camera 
# trap R84-ZSL72.


# Correcting camera trap ID in metadata_1$Directory column:

metdat$Directory[metdat$Directory == 
           "E:/GGA_Camere_Trap_survey/Random_place/R84-ZSL60"] <- "E:/GGA_Camere_Trap_survey/Random_place/R83-ZSL60"


# To check if it worked, let's count how many rows with R83-ZSL60 and R84-ZSL72 there are 
# in the metdat$Directory column:

sum(metdat$Directory == "E:/GGA_Camere_Trap_survey/Random_place/R83-ZSL60")
# [1] 8913 (checked with the Excel file)

sum(metdat$Directory == "E:/GGA_Camere_Trap_survey/Random_place/R84-ZSL72")
# [1] 9290 (checked with the Excel file)


# Correcting camera trap ID in metdat$SourceFile column:

metdat$SourceFile <- gsub("R84-ZSL60", "R83-ZSL60", metdat$SourceFile)


# Checking if it worked:

sum(grepl("R83-ZSL60", metdat$SourceFile))
# [1] 8913

sum(grepl("R84-ZSL60", metdat$SourceFile))
# [1] 0

sum(grepl("R84", metdat$SourceFile))
# [1] 9290


# The camera trap R65-ZSL54 was mislabeled as "R54-ZSL54", so its information rows were summed up to the camera 
# trap R54-ZSL102.


# Correcting camera trap ID in metdat$Directory column:

metdat$Directory[metdat$Directory == 
            "E:/GGA_Camere_Trap_survey/Random_place/R54-ZSL54"] <- "E:/GGA_Camere_Trap_survey/Random_place/R65-ZSL54"


# To check if it worked, let's count how many rows with R65-ZSL54 and R54-ZSL102 there are 
# in the metdat$Directory column:

sum(metdat$Directory == "E:/GGA_Camere_Trap_survey/Random_place/R65-ZSL54")
# [1] 8534 (checked with the Excel file)

sum(metdat$Directory == "E:/GGA_Camere_Trap_survey/Random_place/R54-ZSL102")
# [1] 8758 (checked with the Excel file)


# Correcting camera trap ID in metdat$SourceFile column:

metdat$SourceFile <- gsub("R54-ZSL54", "R65-ZSL54", metdat$SourceFile)


# Checking if it worked:

sum(grepl("R65-ZSL54", metdat$SourceFile))
# [1] 8534

sum(grepl("R54-ZSL54", metdat$SourceFile))
# [1] 0

sum(grepl("R54-ZSL102", metdat$SourceFile))
# [1] 8758


# The camera trap R17-W9 was mislabeled as "R19-W9", I need to correct it back:


# Correcting camera trap ID in metdat$SourceFile column:

metdat$SourceFile <- gsub("R19-W9", "R17-W9", metdat$SourceFile)


# Checking if it worked:

sum(grepl("R17-W9", metdat$SourceFile))
# [1] 8595 (checked with the Excel file)

sum(grepl("R19-W9", metdat$SourceFile))
# [1] 0


# Correcting camera trap ID in metdat$Directory column:

metdat$Directory <- gsub("R19-W9", "R17-W9", metdat$Directory)


# Checking if it worked:

sum(grepl("R17-W9", metdat$Directory))
# [1] 8595

sum(grepl("R19-W9", metdat$Directory))
# [1] 0


# After that, I need to eliminate all video typefiles, to just keep images in my Camera Trap dataset:

metdat <- grepl.sub(metdat, pattern = "JPEG", Var = "FileType")


# Checking if it worked:

sum(grepl("AVI", metdat$FileType))
# [1] 0


# Only Camera Trap R28-W16 was wrongly set in the year 2018; I need to correct it for the dates to be 
# in the year 2019:

str(metdat$CreateDate)

# It's a character string and not a date string, so I can use "gsub" function again

metdat$CreateDate <- gsub("2018", "2019", metdat$CreateDate)


# Checking if it worked:

sum(grepl("2018", metdat$CreateDate))
# [1] 0


#### "METDAT" FILE PROCESSING ####


# The Camera Trap deployment in Gobi desert was not exempt of field troubles. This novel approach on Distance 
# Sampling using time-lapse images from Camera Traps requires a set of images reliably taken in a  clear pattern of
# time-lapse, which in this case was every 20-10 minutes between the functioning hours, or even longer. 
# When Camera Traps took images triggered by movement of individuals in front of them, those images 
# have to be eliminated. The same will occur when Camera Traps incline, recline or simply fall on the ground, making
# these images unusable for the Distance Sampling (useless as sampling effort, and even for distance estimation when
# individuals were spot on them). The next lines will tackle this problem, leaving only time-lapse images on the
# datafile that will be used on Distance Sampling


# Loading some necessary function for the processing:

# Modal avaerage by frequency

modefrq <- function(x){
  tab <- sort(table(x), decr=TRUE)
  as.numeric(names(tab)[1])
}


# Correct date/time for a given point (pnt) in "dat" given time increments in seconds for the first image after a 
# reset (inc1), and the second (and subsequent) images after a reset (inc2).

# dat: dataframe of image metadata


correct.pnt <- function(pnt, dat, incs1, incs2){
  datetime <- subset(dat, point_id==pnt)$datetime.original
  inc1 <- incs1[pnt]
  inc2 <- incs2[pnt]
  
  tdiff <- diff(datetime)
  i <- which(tdiff < 0)
  n <- length(datetime)
  for(j in i){
    k <- (j+2):n
    datetime[k] <- datetime[k] + difftime(datetime[j], datetime[j+2], tz="UTC", units="secs") + inc2
  }
  datetime[i+1] <- datetime[i]+inc1
  datetime
}


# Correct date/times after resets for a given point (pnt), given data (dat; including at least columns point_id and 
# datetime.original), and vectors of increment values for the first and subsequent values after a reset 
# (incs1, incs2; must be named with point IDs).


correct.datetime <- function(dat, incs1, incs2){
  dtlist <- sapply(unique(metdat$point_id), correct.pnt, dat, incs1, incs2)
  dt <- do.call(c, dtlist)
  attr(dt, "tzone") <- "UTC"
  dt
}


# Flags whether an image from a given point on a given date is timelapse by setting out a regular sequence of times 
# and finding the images with times that most closely match these within a tolerance threshold.

# pd: character point_date identifier
# start.times: list of date-times of first image in each point_date combo
# tlap.incs: timelapse increments for each point
# tol: tolerance - seconds from a sequence time at which to accept an image as timelapse

is.tl <- function(pd, dat, start.times, tlap.incs, tol){
  f <- function(i){
    nbrsecs <- grd[nbrs[[i]], i]
    if(length(nbrs[[i]])>0) nbrs[[i]][nbrsecs==min(nbrsecs)] else integer(0)
  }
  p <- unlist(strsplit(pd, "_"))[1]
  tms <- subset(dat, point_date==pd)$datetime
  sq <- seq(start.times[[pd]], start.times[[pd]]+24*60^2, tlap.incs[p])
  grd <- expand.grid(tms, sq)
  grd <- abs(matrix(difftime(grd[,1], grd[,2], units="secs", tz="UTC"), nrow=length(tms)))
  nbrs <- apply(grd, 2, function(x) which(x<=tol))
  1:length(tms) %in% unlist(sapply(1:ncol(grd), f))
}


# Flags whether each record in a set of image metadata is timelapse

is.timelapse <- function(dat, tlap.incs, tol){
  dat$point_date <- with(dat, paste(point_id, date, sep="_"))
  start.times <- with(dat, tapply(datetime, point_date, min, simplify=F, na.rm=T))
  res <- pbapply::pbsapply(names(start.times), is.tl, dat, start.times, tlap.incs, tol)
  unlist(res)
}


# Then, I prepare the data to run these functions on:

# Adding some necessary columns to metdat: 

# point_id (point number, taken from the directory)
# datetime.original (character string CreateDate converted to POSIXct)
# date (date in date format)
# tdiff (stepwise time differences, -1 flags step to new point)

temp <- strsplit(metdat$Directory, "/", fixed=TRUE)

temp <- unlist(lapply(temp, function(x) x[4]))

temp <- strsplit(temp, "-", fixed=TRUE)

metdat$point_id <- unlist(lapply(temp, function(x) x[1]))

rm(temp)

metdat$datetime.original <- fastPOSIXct(metdat$CreateDate, tz="UTC")

# It is safest to stick to UTC (==GMT) time zone because daylight saving shifts might otherwise be applied 
# when I don't want them

td <- diff(metdat$datetime.original)
pd <- tail(metdat$point_id, -1) != head(metdat$point_id, -1)
td[pd] <- -1
metdat$tdiff <- c(-1, td)


# Get most common time increments for each point in seconds (timelapse and night time increments):

# tlap.incs: time between each timelapse image
# night.incs: time between last image of the night night and first image of the day

tlap.incs <- with(metdat, tapply(tdiff, point_id, modefrq))
thrshld <- tlap.incs[match(metdat$point_id, names(tlap.incs))]+60
night.incs <- with(subset(metdat, tdiff > thrshld), tapply(tdiff, point_id, modefrq))

# Creating incs1 and incs2 vectors for running correct.pnt. Most resets apear to happen once per point at the end 
# of a day, and the real-time delay is simply the usual night time pause. 

# Exceptions with multiple resets: 
# Point 39 appears to simply keep rolling without a gap until the clock time next reaches thenight pause; 
# Point 47 apparently has the usual time lapse interval for the first image of each reset, then a 5 h 20 m pause.

incs1 <- night.incs
incs2 <- night.incs+tlap.incs
incs1["R47"] <- tlap.incs["R47"]
incs2["R47"] <- 5*60^2+20*60
incs1["R39"] <- tlap.incs["R39"]
incs2["R39"] <- tlap.incs["R39"]*2


# Finally, I can add corrected datetime to metdat file:

metdat$datetime <- correct.datetime(metdat, incs1, incs2)

class(metdat$datetime)


# Then, I will strip out iamges from first and last days at each Camera Trap point, because the images taken on 
# them had not followed a time-lapse pattern, and also because there are a lot of images triggered due to the people
# installing/collecting the Camera Traps. Also, Camera Trap R71-ZSL95 completely malfunctioned during its deployment,
# taking no time-lapse images (with no useable datetime info)

# So, first I need to create a new column "date" on date format, from which eliminate the first and last days of 
# deployment for every Camera Trap. This column will serve me also when I want to calculate the sunlight times at 
# every Camera Trap location, in order to only select images taken on sunlight hours.


metdat$date <- as.Date(metdat$datetime)

class(metdat$date)


# Then I set the parameters to recognise first and last day records:

firstdays <- do.call(c, with(metdat, tapply(date, point_id, min, na.rm=TRUE, simplify=FALSE)))

lastdays <- do.call(c, with(metdat, tapply(date, point_id, max, na.rm=TRUE, simplify=FALSE)))

firstday <- firstdays[match(metdat$point_id, names(firstdays))]

lastday <- lastdays[match(metdat$point_id, names(lastdays))]

metdat <- subset(metdat, date>firstday & date<lastday & point_id!="R71" & !is.na(datetime.original))


# To check the elimination of images from R71-ZSL95 Camera Trap

sum(grepl("R71", metdat$point_id))
# [1] 0

# Then, I flag for timelapse images; from looking at the internal data, a tolerance of about 3 mins (180 secs) 
# seemed about right, and the result isn't too sensitive to variation around this value.
# This process had some intensive looping, so takes a few minutes.

metdat$istimelapse <- is.timelapse(metdat, tlap.incs, 180)


# To check if previous code worked:

sum(metdat$istimelapse == "FALSE")
# [1] 45712

sum(metdat$istimelapse == "TRUE")
# [1] 619059


#### ELIMINATE IMAGES FROM CT WITH SPECIAL CASES ####


## R27-ZSL40 ##

# From image PTDC0001 until PTDC8605 this CT worked normally; but then in the next image CT fell to its side. making
# these useless for distance calculation. So from image PTDC8606 until the last image PTDC1380, 
# between rows [140924:143697], these rows have to be deleted

metdat$SourceFile[140924]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R27-ZSL40/100MEDIA/PTDC8606.JPG" (first image to be eliminated)

metdat$SourceFile[143697] 

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R27-ZSL40/101MEDIA/PTDC1380.JPG" (last image of CT)

metdat <- metdat[-c(140924:143697), ]

metdat$SourceFile[140924] 

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R28-W16/100MEDIA/DSCF0019.JPG"


## R54-ZSL102 ##

# From image PTDC0001 until PTDC0253, CT worked normally. After that it started to incline, but images are still
# useful for distance calculation; until image PTDC0670 where they lost the horizon and distance calculation
# is not possible from these. CT finally dropped to the ground at PTDC1204, but keep working until the end of
# its deployment (PTDC8747). The rows that have to be eliminated are [388860:396937]

metdat$SourceFile[388860] 

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R54-ZSL102/PTDC0670.JPG" (first image to be eliminated)

metdat$SourceFile[396937] 

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R54-ZSL102/PTDC8747.JPG" (last image of CT)

metdat <- metdat[-c(388860:396937), ]

metdat$SourceFile[388860] 

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R65-ZSL54/PTDC0007.JPG"


## R74-ZSL83 ##

# From image PTDC0001 until PTDC3117 CT worked normally, but then CT fell to the ground, making images from 
# PTDC3118 to until the end of its deployment (PTDC8991) useless for distance calculation. 
# These images have to be deleted, between rows [522582:528455]

metdat$SourceFile[522582]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R74-ZSL83/PTDC3118.JPG" (first image to be eliminated)

metdat$SourceFile[528455]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R74-ZSL83/PTDC8991.JPG" (last image of CT)

metdat <- metdat[-c(522582:528455), ]

metdat$SourceFile[522582]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R75-ZSL51/PTDC0027.JPG"


## R88-ZSL37 ##

# From image PTDC0001 until PTDC8160 CT worked normally, but then CT fell to a side and took inclined images, 
# loosing the horizon, making these images from PTDC8161 until the end of its deployment (PTDC8570) useless 
# for distance calculation. These images have to be deleted, between rows [604150:604559]

metdat$SourceFile[604150]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R88-ZSL37/PTDC8161.JPG" (first image to be eliminated)

metdat$SourceFile[604559]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R88-ZSL37/PTDC8570.JPG" (last image of CT)

metdat <- metdat[-c(604150:604559), ]

metdat$SourceFile[604150]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R89-ZSL33/PTDC0002.JPG"


## RG87-ZSL23 ##

# From image PTDC0001 until PTDC0394 CT worked normally, but then CT fell to the ground and kept taking 
# images of it, making these images from PTDC0395 until the end of its deployment (PTDC8721) useless 
# for distance calculation. These images have to be deleted, between rows [630615:638941]

metdat$SourceFile[630615]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/RG87-ZSL23/PTDC0395.JPG" (first image to be eliminated)

metdat$SourceFile[638941]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/RG87-ZSL23/PTDC8721.JPG" (last image of CT)

metdat <- metdat[-c(630615:638941), ]

metdat$SourceFile[630615]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/RG90-ZSL25/PTDC0025.JPG"


## R67-W6 ##

# From image 100MEDIA/DSCF0001 until 101MEDIA/DSCF0213 CT worked normally, but then CT fell backwards and pointed 
# to the sky, and kept taking images like that, making these images from 100MEDIA/DSCF0214 until the end of its 
# deployment (108MEDIA/DSCF0775) useless for distance calculation. These images have to be deleted, 
# between rows [467845:476398]

metdat$SourceFile[467845]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R67-W6/100MEDIA/DSCF0214.JPG" (first image to be eliminated)

metdat$SourceFile[476398]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R67-W6/108MEDIA/DSCF0775.JPG" (last image of CT)

metdat <- metdat[-c(467845:476398), ]

metdat$SourceFile[467845]

# [1] "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC0016.JPG"



#################################################################################################################
# TO HAVE SHORTCUT AND DON'T NEED TO RUN ALL THE CODES AGAIN:

write.table(metdat, file = "metdat.csv", row.names = FALSE, sep = ",")

metdat <- read.csv("metdat.csv", header = TRUE, stringsAsFactors = FALSE)

#################################################################################################################



#### COORDINATES CONVERSION IN GGASPA DEPLOYMENT DATA AND SURVEY AREA ####


# Now I'm working with my corrected file metdat, and the Camera Trap deployment datasheet. I need to generate a 
# dataset with sunlight times for each date Camera Trap (each one with a specific latitude and longitude coordinates) 
# that will allow me to select only Camera Trap time-lapse images that were taken during sunlight hours in the
# "metdat" file:

depdat <- read.csv("GGASPA_deployment_data.csv", header = TRUE, stringsAsFactors = FALSE)


# As I know from the previous processing, "point_id" of each Camera Trap is just the first portion of their 
# ID, without any character value, just the number. So, R1-ZSL76 corresponds to the "point_id" = 1, and RG86-W5
# corresponds to "point_id" = 86.

# Now, I will need to add character values to each number of "point" column from "depdat", and also change the 
# column name to "point_id", because I will need to match this file with "metdat" to paste on it certain columns 

# An exploration of the nature of "depdat$point" values

class(depdat$point)
str(depdat$point)

# Adding up the letter "R" to each of the values in depdat$point

depdat$point <- sub("^", "R", depdat$point)


# Checking if it worked:

head(depdat$point)
tail(depdat$point)


# There are 3 Camera Traps that have another letter on their ID: RG86-W5, RG87-ZSL23, and RG90-ZSL25, so I have to 
# add "G" to their character string:

depdat$point <- sub("R86", "RG86", depdat$point)
depdat$point <- sub("R87", "RG87", depdat$point)
depdat$point <- sub("R90", "RG90", depdat$point)


# Checking if it worked:

tail(depdat$point)


# Now, I rename the "point" column in "depdat" file to "point_id"

names(depdat)[names(depdat) == "point"] <- "point_id"

names(depdat)


# I need to convert UTM coordinates on "depdat" file to longitude and latitude coordinates, in order to use the 
# "suncalc" package to calculate sunlight times and then select only Camera Trap time-lapse images that were taken 
# during sunlight hours in "metdat" file:


zones <- unique(depdat$zone)

utmdat <- lapply(zones, function(z){
  subdat <- subset(depdat, zone == z)[, c("long", "lat")]
  proj <- CRS(paste0("+proj=utm +zone=", z, "+datum=WGS84"))
  SpatialPoints(subdat, proj)
})

latlondat <- lapply(utmdat, spTransform, CRS("+proj=longlat +datum=WGS84"))

latlondat <- rbindlist(lapply(latlondat, as.data.frame))


# Here I backtransformed UTM coordinates on "depdat" file to long/lat coordinates by each point_id (CT) in 
# the corresponding rows of "latlondat"


# Now I need to rename "depdat" columns called "long" and "lat" to their real projection, which is UTM coordinates, 
# to not be confounded with the new longitude/latitude coordinates from "latlondat". I will name it "utmx" 
# (UTM long) and "utmy" (UTM lat).

names(depdat)[match(c("long", "lat"), names(depdat))] <- c("utmx", "utmy")

head(depdat)


# Then, I bind both vectors by columns into the "depdat" file: 

depdat <- cbind(as.data.frame(latlondat), depdat)


# I will load my GGASPA survey area shapefile (already in UTM Northing and Easting coordinates) into R 
# environment. I will transform its UTM coordinates to distance-based x,y coordinates in kilometres (km), using a 
# centroid coordinate to extrapolate all the remaining, using this coordinate format in the plotting stage


## After, I will have to correct the codes to load these files, just to a file name or a folder name, for everyone 
## can load it; something like this

GGASPA <- readOGR(dsn=":/.../.../.../GGASPA")     # adapt it at your convenience

GGASPA <- readOGR(dsn = "D:/MSc SILWOOD 2019/MAIN PROJECT/METHODS/GGASPA")

GGASPA <- data.frame(GGASPA@polygons[[1]]@Polygons[[1]]@coords)

names(GGASPA) <- c("utmx", "utmy")


# I need to backtransform first the UTM coordinates on GGASPA to Longitude and Latitude, to then transform it 
# to distance-based x,y coordinates:

GGASPA2 <- SpatialPoints(GGASPA)

proj4string(GGASPA2) <- CRS("+init=epsg:32647")    # project the dataframe on UTM 47N

GGASPA2 <- spTransform(GGASPA2, CRS("+proj=longlat +datum=WGS84"))  # project the dataframe on LongLat

GGASPA2 <- data.frame(long=GGASPA2@coords[, 1], lat=GGASPA2@coords[, 2]) 

GGASPA <- cbind(GGASPA, GGASPA2)

rm(GGASPA2)


# Adding the distance-based x,y coordinates in metres (m) to GGASPA file, which will use in the plotting stage

lon0 <- mean(GGASPA$long)
lat0 <- mean(GGASPA$lat)
sa.tmp <- latlong2km(GGASPA$long, GGASPA$lat, lon0 = lon0, lat0 = lat0)
GGASPA$xkm <- sa.tmp$km.e
GGASPA$ykm <- sa.tmp$km.n
GGASPA$x <- sa.tmp$km.e*1000
GGASPA$y <- sa.tmp$km.n*1000

rm(sa.tmp)


# Then, I will calculate and add a distance-based x,y coordinates in metres (m) for depdat file, using the 
# centroid of the GGASPA file as a projection, for then plotting CT locations on the GGASPA.


sa.tmp <- latlong2km(depdat$long, depdat$lat, lon0 = lon0, lat0 = lat0)
depdat$xkm <- sa.tmp$km.e
depdat$ykm <- sa.tmp$km.n
depdat$x <- sa.tmp$km.e*1000
depdat$y <- sa.tmp$km.n*1000

rm(sa.tmp)
rm(lon0)
rm(lat0)


# The code below generates a Figure which shows the GGASPA survey area with the CT locations overlaid

# plotting options

gg.opts <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.text = element_text(size = 14), 
                 axis.title = element_text(size = 14, face = "bold"))

p <- qplot(data=GGASPA, x = xkm, y = ykm, geom = "polygon",fill = I("lightblue"),
           ylab = "ykm", xlab = "xkm", alpha=I(0.7))
p <- p + coord_equal()
p <- p + geom_point(aes(xkm, ykm, group = point_id),data = depdat)  
p <- p + geom_text(aes(label = point_id), hjust=-0.4, vjust=-0.4, size=3, data = depdat)
p <- p + gg.opts
print(p)


# Now, I will match the long/lat and distance-based coordinates (xkm/ykm and x/y) columns of each point_id in 
# "depdat" file to their corresponding Camera Trap ID on the "metdat" file, using "point_id" column as match. 
# I will have six new columns in the "metdat" file. Columns "lon" and "lat" (case sensitive), will be used by the 
# "suncalc" package along with "date" column to generate a dataset with sunlight times for the whole deployment 
# dates of each Camera Trap ID, and with that I will after select time-lapse images taken during sunlight hours:


i <- match(metdat$point_id, depdat$point_id)
metdat$lon <- depdat$long[i]
metdat$lat <- depdat$lat[i]   
metdat$xkm <- depdat$xkm[i]
metdat$ykm <- depdat$ykm[i]
metdat$x <- depdat$x[i]
metdat$y <- depdat$y[i]

rm(i)

# Now, with the relevant columns "lat", "lon" and "date" in my "metdat" file (suncalc package requires these info) 
# I can create the sunlight times datafile for each Camera Trap location and date, and add these relevant 
# sunlightimes to my "metdat" file

suntimes <- getSunlightTimes(data = metdat, keep = c("sunrise", "sunset", "sunriseEnd", "sunsetStart"))


# There is a time difference between Mongolia time, where Camera Traps were deployed, and UTC time zone; suncalc 
# output time zone is UTC by default, so correction is needed:

gmtdiff <- 8*60^2

# This is the difference between Mongolia time and UTC time in seconds (eight hours to seconds)


# Then, I have to correct the difference between the calculated sunlightimes on the columns I need from "suntimes" 
# file

suntimes$sunrise <- suntimes$sunrise + gmtdiff

suntimes$sunriseEnd <- suntimes$sunriseEnd + gmtdiff

suntimes$sunsetStart <- suntimes$sunsetStart + gmtdiff

suntimes$sunset <- suntimes$sunset + gmtdiff


# Now, I am able to add the corrected sunlightimes from "suntimes" file to my "metdat" file:

metdat$sunrise <- suntimes$sunrise

metdat$sunriseEnd <- suntimes$sunriseEnd

metdat$sunsetStart <- suntimes$sunsetStart

metdat$sunset <- suntimes$sunset



#### TIMELAPSE AND SUNLIGHT HOURS IMAGES SELECTION ####


# For an appropriate calculation of distances between individuals of focal species and the Camera Traps in the 
# time-lapse images with captures, which then will be used to estimate abundance and density and also to construct 
# a density map of these species, I have to select from the "metdat" file only images that followed a time-lapse 
# pattern (taken every 20 or 10 minutes, or even longer) and were taken in sunlight. I will do these 
# subsetting the current "metdat" dataset on images taken after sunrise ends and before sunset starts for each date 
# and Camera Trap, and also images that fulfilled the time-lapse pattern.


# To do that, I am going to use the "istimelapse" column of the "metdat" file as a flag, together with the 
# sunrise>/<sunset criterion, and I will end with a number of timelapse images useable for distance estimation by
# each Camera Trap (which will let me calculate after the real sampling effort)

metdat_effort <- subset(metdat, istimelapse & datetime>sunriseEnd & datetime<sunsetStart)

sum(metdat_effort$istimelapse == "FALSE")
# [1] 0

length(metdat_effort$SourceFile)
# [1] 497087


# From a "metadata" dataset with 669,989 rows (Camera Trap images) in the beggining, I ended with a "metdat_effort"
# dataset of 497,087 rows of time-lapse and sunlight hours images from 77 Camera Traps (R71-ZSL95 images were all
# eliminated).


#################################################################################################################

# TO HAVE SHORTCUT AND DON'T NEED TO RUN ALL THE CODES AGAIN:

write.table(metdat_effort, file = "metdat_effort.csv", row.names = FALSE, sep = ",")

write.table(depdat, file = "depdat_fordistance.csv", row.names = FALSE, sep = ",")

# LOAD THEM

metdat_effort <- read.csv("metdat_effort.csv", header = TRUE, stringsAsFactors = FALSE)

depdat <- read.csv("depdat_fordistance.csv", header = TRUE, stringsAsFactors = FALSE)

#################################################################################################################



#### ESTIMATION OF DISTANCES BETWEEN CT AND DETECTED INDIVIDUALS OF FOCAL SPECIES IN TIMELAPSE IMAGES ####


# In a previous stage, I performed the digitisation of two points: height (from ground to shoulder) or total length 
# (from nose to base of tail) of every individuals detected in time-lapse images from both focal species 
# (Goitered gazelle and Mongolian Kulan), and exported those datafiles from "animaltracker" software in a 
# .csv format.


# For the estimation of distances between detected individuals and Camera Traps, I will need to load some functions 
# from a package called CTtracking from Github


giturl <- "https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r" 

script <- RCurl::getURL(giturl) 

eval(parse(text=script))


# # Then, I load the datafile with the digitisation of height or total length of detected individuals in 
# time-lapse images from both focal species 

distdata_gobi <- read.csv("digitisation_data.csv", header = TRUE, stringsAsFactors = FALSE)


# To be time saving, I labeled the digitisation of each spotted individuals with a short name. Now I have to replace
# the column "image_name" with a character string that reflects a unique identifier for each time-lapse image, in 
# order to correspond with the required features of this datafile in the spatial analysis stage.

str(distdata_gobi$image_name)

# It's a character string; so I will extract the image number from "image_name" column, and then create a dataframe 
# with the corresponding "SourceFile" column from "metdat_effort", on which I will extract the same image number to 
# match and paste on distdata_gobi.


image_num <- strsplit(distdata_gobi$image_name, ".", fixed=TRUE)

image_num <- unlist(lapply(image_num, function(x) x[1]))

image_num <- stri_sub(image_num, -4, -1)

distdata_gobi <- cbind(as.data.frame(image_num), distdata_gobi)

str(distdata_gobi)

rm(image_num)


# Now I will create the dataframe with the unique identifier for each timelapse image, and I will name the column as
# "Sample.Label"

sourcefile <- data.frame(Sample.Label = c("E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC0839.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC1555.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC1582.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC1950.JPG",
                "E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC2324.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R21-ZSL42/PTDC5729.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R45-ZSL84/PTDC2859.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R66-ZSL48/PTDC6723.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R66-ZSL48/PTDC7222.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC0504.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC0539.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC1825.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC1826.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC1827.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC1828.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC1952.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC3868.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC5776.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R69-ZSL30/PTDC6103.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC0278.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC0279.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC0280.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC0281.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC2174.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC2629.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC6962.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R82-ZSL36/PTDC4440.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R82-ZSL36/PTDC4444.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R88-ZSL37/PTDC7242.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R88-ZSL37/PTDC7264.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R88-ZSL37/PTDC7339.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R9-ZSL68/PTDC2631.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC0994.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC2962.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC3916.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC4795.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC4796.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC5037.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC5091.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC6491.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC8012.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC8013.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC8014.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R19-ZSL69/100MEDIA/PTDC8016.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC1269.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC1270.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC2735.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC3922.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC3923.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC3924.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R24-ZSL91/PTDC3925.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R27-ZSL40/100MEDIA/PTDC7808.JPG",
                "E:/GGA_Camere_Trap_survey/Random_place/R28-W16/110MEDIA/DSCF0914.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R28-W16/110MEDIA/DSCF0915.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R28-W16/110MEDIA/DSCF0916.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC1142.JPG", 
                "E:/GGA_Camere_Trap_survey/Random_place/R76-ZSL29/PTDC2410.JPG"))


count(unique(sourcefile))
# There are 57 rows, no duplicates

length(unique(distdata_gobi$image_name))

# There are 57 unique values, the same number as "sourcefile" dataframe. Now I will extract the image number 
# from sourcefile$Sample.Label


image_num <- strsplit(sourcefile$Sample.Label, ".", fixed=TRUE)

image_num <- unlist(lapply(image_num, function(x) x[1]))

image_num <- stri_sub(image_num, -4, -1)

sourcefile <- cbind(as.data.frame(image_num), sourcefile)

rm(image_num)

length(unique(sourcefile$image_num))


# Now, I am able to add the "sourcefile$Sample.Label" column to "distdata_gobi"

distdata_gobi$Sample.Label <- sourcefile$Sample.Label[match(distdata_gobi$image_num, sourcefile$image_num)]


# After that, I need to prepare the digitisation and the calibration datafiles for distance calculation:

# To load the CT calibration model:

calibpth <- "calibration.RData"

load(calibpth)


# To rename "name" column to "pair_id" in distdata_gobi

distdata_gobi <- dplyr::rename(distdata_gobi, pair_id = name)


# Then I have to pair up the points to give a single row per individual from both species detected in time-lapse 
# images in distdata_gobi

distdata_gobi <- make.pairdat(distdata_gobi)


# Finally, I do the distance calculation, adding a new column of estimated distances from CT to any single 
# individual from both focal species detected in time-lapse images in distdata_gobi:

distdata_gobi <- calc.distance(distdata_gobi, calibration)


# I obtained a new "distance" column in distdata_gobi file, in the same unit as the known body measurements of 
# both focal species (metres) for which I digitised points in time-lapse images


#################################################################################################################

# TO HAVE SHORTCUT AND DON'T NEED TO RUN ALL THE CODES AGAIN:

write.table(distdata_gobi, file = "distdata_gobi.csv", row.names = FALSE, sep = ",")

# LOAD IT

distdata_gobi <- read.csv("distdata_gobi.csv", header = TRUE, stringsAsFactors = FALSE)

#################################################################################################################


#### DENSITY SURFACE MODELLING (DSM) ANALYSIS ####


# Now, I will modify the datafiles that I have to use in the DSM Analysis, following the example provided by Miller
# et al. (2013) in their article.


# I need to calculate the sampling effort by each Camera Trap, and add it up as a new "Effort" (case-sensitive) 
# column to my "metdat_effort" file to correspond with the required fields of this datafile in the spatial analysis
# stage.
# Here, the sampling effort it is not just the number of time-lapse images taken during sunlight hours per Camera
# Trap. The definition of effort needs to take account of Camera Trap "field of view", as a proportion of a 
# full circular area.

circprop <- calibration$Forestcam$APratio / (2*pi)

# in this particular case for the used Camera Trap models, the sampling effort is about 24% of a circular area 
# of 1 (1 image)


# Then, I calculate the sampling effort and add it to my "depdat" file


images.per.point <- aggregate(istimelapse ~ point_id, metdat_effort, length)

depdat$Effort <- circprop * images.per.point$istimelapse[match(depdat$point_id, images.per.point$point_id)]

depdat <- subset(depdat, !is.na(Effort))

# My "depdat" file now is composed by 77 CT.


# Now, I will prepare the distdata_gobi file to adding up relevant columns on it, that will allow me to use it in 
# the spatial analysis stage:

class(distdata_gobi$image_name)


# "image_name" column it's a character string, so I will split the string and substract the "point_id" from
# "distdata_gobi$image_name" and add it up in a new column to paste on it certain columns from "depdat" file 
# required in the spatial analysis stage:

temp1 <- strsplit(distdata_gobi$Sample.Label, "/", fixed=TRUE)

temp1 <- unlist(lapply(temp1, function(x) x[4]))

temp1 <- strsplit(temp1, "-")

distdata_gobi$point_id <- unlist(lapply(temp1, function(x) x[1]))

rm(temp1)

head(distdata_gobi$point_id)
tail(distdata_gobi$point_id)


# Now, I can match the distdata_gobi file by "point_id" and paste on them lon/lat coordinates, distance-based 
# coordinates (xkm, ykm,x, y) from "depdat" file

i <- match(distdata_gobi$point_id, depdat$point_id)
distdata_gobi$lon <- depdat$long[i]
distdata_gobi$lat <- depdat$lat[i]
distdata_gobi$xkm <- depdat$xkm[i]
distdata_gobi$ykm <- depdat$ykm[i]
distdata_gobi$x <- depdat$x[i]
distdata_gobi$y <- depdat$y[i]

rm(i)

# Before to continue modifying the distdata_gobi file, I need to rename its "pair_id" column as "object" 
# (case-sensitive) and "number_of_points" as "size" (case-sensitive) for then extract the relevant columns for the 
# spatial analysis stage:

distdata_gobi <- rename(distdata_gobi, object=pair_id, size=number_of_points)


# And now, I am able to select the relevant columns on distdata_gobi file:

distdata_gobi <- dplyr::select(distdata_gobi, c(Sample.Label, object, size, distance, point_id, species, lon, lat, 
                                                xkm, ykm, x, y))

# here I used dplyr::select, just in case select() function were masked by raster package


# Finally, I am able to separate the distdata_gobi file in two different files, corresponding to each focal species, 
# and add a new column on depdat file about the count of individuals per CT for whom distance was estimated:


distdata_gazelle <- subset(distdata_gobi, species=="Goitered gazelle")

distdata_kulan <- subset(distdata_gobi, species=="Kulan")

gazelle.tab <- table(distdata_gazelle$point_id)

kulan.tab <- table(distdata_kulan$point_id)

depdat$gazelle_count <- depdat$kulan_count <- 0

depdat$gazelle_count[match(names(gazelle.tab), depdat$point_id)] <- gazelle.tab

depdat$kulan_count[match(names(kulan.tab), depdat$point_id)] <- kulan.tab



#### SELECTION OF THE DETECTION FUNCTION ####


#### Exploration of Distance data ####

# First, I will have a look at the frequency distributions of distances from CT to detected individuals of both
# focal species, and its relationship with the count numbers per CT.


# Histogram of frequency distribution of recorded distances

par(mfrow = c(1, 2))

hist(distdata_gazelle$distance, ylim = c(0, 25), xlab = "Distance of detection (metres)", 
     main = "Goitered gazelle", col = "white")

hist(distdata_kulan$distance, ylim = c(0, 15), xlab = "Distance of detection (metres)", 
     main = "Mongolian Kulan", col = "white")

dev.off()


# Also, I am able now to plot the count of individuals of both species detected in time-lapse images per CT 
# location, for whom the distances were calculated

# Plot of Count of animals per CT location for Goitered gazelle:

q <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
           ylab = "ykm", xlab = "xkm", alpha = I(0.7))
q <- q + gg.opts 
q <- q + coord_equal() 
q <- q + geom_point(aes(xkm, ykm, size = gazelle_count), data = depdat, colour = "red", alpha = I(0.7))
q <- q + labs(x= "xkm", y= "ykm", size = "Count")
q <- q + theme(legend.title = element_text(size = 23), legend.text = element_text(size = 19))
print(q)


# Plot of Count of animals per CT location for Kulan:

r <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
           ylab = "ykm", xlab = "xkm", alpha = I(0.7))
r <- r + gg.opts 
r <- r + coord_equal() 
r <- r + geom_point(aes(xkm, ykm, size = kulan_count), data = depdat, colour = "red", alpha = I(0.7))
r <- r + labs(x= "xkm", y= "ykm", size = "Count") 
r <- r + theme(legend.title = element_text(size = 23), legend.text = element_text(size = 19))
print(r)


#### Estimating the Detection Function in Conventional Distance Sampling (CDS) ####

# Now I will use the ds() function in the package Distance to fit the detection function to the estimated distances
# in CDS, that means only using coordinates to explain spatial distributions of focal species. I need to try 
# different existing keys (Half-normal and Hazard-rate with no adjustment terms, Uniform with adjustment terms) to 
# model the detection function that will allow me to estimate density and abundance of both focal species, and 
# comparing their AIC values I will be able to select the one that best fits their distance data:


# Looking for the detection function in CDS that best fits Goitered gazelle distance data.

# Half-normal key

hn.gazelle <- ds(distdata_gazelle, max(distdata_gazelle$distance), transect = "point", key = "hn", 
                 adjustment = NULL)

summary(hn.gazelle)

AIC(hn.gazelle)

plot(hn.gazelle, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")   # pdf: probability density function

# plotting option only available for point transects, shows how well fitted is the detection function key to the raw
# distance data

# With the following code I generate a plot of the fitted detection function and quantile-quantile plot:

par(mfrow = c(1, 2))
plot(hn.gazelle, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(hn.gazelle$ddf)
dev.off()


# Hazard-rate key

hr.gazelle <- ds(distdata_gazelle, max(distdata_gazelle$distance), transect = "point", key = "hr", 
                 adjustment = NULL)

summary(hr.gazelle)

AIC(hr.gazelle)

plot(hr.gazelle, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(hr.gazelle, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(hr.gazelle$ddf)
dev.off()


# Uniform key cosine adjustment

unif.gazelle.cos <- ds(distdata_gazelle, max(distdata_gazelle$distance), transect = "point", key = "unif", 
                   adjustment = "cos")

summary(unif.gazelle.cos)

AIC(unif.gazelle.cos)

plot(unif.gazelle.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.gazelle.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.gazelle.cos$ddf)
dev.off()


# Uniform key Hermite polynomial adjustment

unif.gazelle.herm <- ds(distdata_gazelle, max(distdata_gazelle$distance), transect = "point", key = "unif", 
                       adjustment = "herm")

# Have an error message, but the detection function ran anyway; I can access its summary. 

summary(unif.gazelle.herm)

AIC(unif.gazelle.herm)

plot(unif.gazelle.herm, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.gazelle.herm, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.gazelle.herm$ddf)
dev.off()


# Uniform key simple polynomial adjustment

unif.gazelle.poly <- ds(distdata_gazelle, max(distdata_gazelle$distance), transect = "point", key = "unif", 
                        adjustment = "poly")

summary(unif.gazelle.poly)

AIC(unif.gazelle.poly)

plot(unif.gazelle.poly, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.gazelle.poly, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.gazelle.poly$ddf)
dev.off()


# Finally, I am able to choose detection function keys fitted for recorded distances of Goitered gazelle based on 
# their AICs and their effective detection distance:

modelDF_gazelle <- summarize_ds_models(hn.gazelle, hr.gazelle, unif.gazelle.cos, unif.gazelle.herm, unif.gazelle.poly, 
                                        sort = "AIC", output = "plain", delta_only = FALSE)

modelDF_gazelle


# Based on its lowest AIC and delta AIC values, the detection function key that best fitted the Goitered
# gazelle distance data in CDS is Half-normal with no adjustment terms


# Looking for the detection function in CDS that best fits Kulan distance data.

# Half-normal key

hn.kulan <- ds(distdata_kulan, max(distdata_kulan$distance), transect = "point", key = "hn", adjustment = NULL)

summary(hn.kulan)

AIC(hn.kulan)

plot(hn.kulan,  pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)") 

# With this following code I generate a plot of the fitted detection function and quantile-quantile plot:

par(mfrow = c(1, 2))
plot(hn.kulan,  pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(hn.kulan$ddf)
dev.off()


# Hazard-rate key

hr.kulan <- ds(distdata_kulan, max(distdata_kulan$distance), transect = "point", key = "hr", adjustment = NULL)

summary(hr.kulan)

AIC(hr.kulan)

plot(hr.kulan, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(hr.kulan, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(hr.kulan$ddf)
dev.off()


# Uniform key cosine adjustment

unif.kulan.cos <- ds(distdata_kulan, max(distdata_kulan$distance), transect = "point", key = "unif", 
                   adjustment = "cos")

summary(unif.kulan.cos)

AIC(unif.kulan.cos)

plot(unif.kulan.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.kulan.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.kulan.cos$ddf)
dev.off()


# Uniform key Hermite polynomial adjustment

unif.kulan.herm <- ds(distdata_kulan, max(distdata_kulan$distance), transect = "point", key = "unif", 
                     adjustment = "herm")

# Have an error message, but the detection function ran anyway; I can access its summary. 

summary(unif.kulan.herm)

AIC(unif.kulan.herm)

plot(unif.kulan.herm, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.kulan.herm, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.kulan.herm$ddf)
dev.off()


# Uniform key single polynomial adjustment

unif.kulan.poly <- ds(distdata_kulan, max(distdata_kulan$distance), transect = "point", key = "unif", 
                      adjustment = "poly")

summary(unif.kulan.poly)

AIC(unif.kulan.poly)

plot(unif.kulan.poly, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")

par(mfrow = c(1, 2))
plot(unif.kulan.poly, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)")
ddf.gof(unif.kulan.poly$ddf)
dev.off()


# Finally, I am able to choose detection function keys fitted for recorded distances of Kulan based on their AICs 
# and their effective detection distance:

modelDF_kulan <- summarize_ds_models(hn.kulan, hr.kulan, unif.kulan.cos, unif.kulan.herm, unif.kulan.poly, 
                                      sort = "AIC", output = "plain", delta_only = FALSE)

modelDF_kulan


# Based on the lowest AIC and Delta AIC values, the detection function key that best fitted the Kulan
# distance data in CDS is Half-normal with no adjustment terms



#### FITTING A DENSITY SURFACE MODEL (DSM) ####

#### Conventional Distance Sampling (CDS) ####


# First of all, I need to change/add some columns on the files that will be used in this stage, which will link
# both files included in the anaylsis: the depdat file with the identifier, coordinates and sampling effort for each 
# sampling unit (CT location); and the distdata file, which also has an identifier per sampling unit along with
# their coordinates, and recorded distances by every individual detected in time-lapse images. The identifier, which
# links both files, is the Sample.Label (case-sensitive) column:

distdata_gazelle$Sample.Label <- distdata_gazelle$point_id

distdata_kulan$Sample.Label <- distdata_kulan$point_id

depdat$Sample.Label <- depdat$point_id


# I will start my DSM model analysis performing a CDS model, which assume that the number of individuals detected 
# in time-lapse images per each Camera Trap are quasi-Poisson distributed (the default in the model code), and that 
# these numbers (adjusted by the detection function) are a smooth function of their spatial coordinates.  As a 
# detection function, for both species I will use the one which resulted to have the lowest AIC value between all 
# key and adjustment terms combinations. I set "method = REML" to ensure that smooth terms are estimated reliably.


# Then, I will run the DSM with the chosen detection function in CDS for both focal species, but this time applying 
# two different error distributions to the count response: Tweedie and Negative Binomial distributions, testing if 
# them fit better these data, in case the quasi-Poisson distribution does not give them adequate flexibility and 
# does not capture their overdispersion. 


#### Goitered Gazelle DSM (CDS) ####

# Running DSM (CDS) with Half-normal Detection Function key and quasi-Poisson error distribution 
# for Goitered gazelle:

dsm.xy.qpois.gazelle <- dsm(N ~ s(x, y), hn.gazelle$ddf, depdat, distdata_gazelle, transect = "point", 
                            method = "REML")

summary(dsm.xy.qpois.gazelle)

# GAM check

par(mfrow=c(2,2))
gam.check(dsm.xy.qpois.gazelle)
dev.off()


# Fitting warning, gam.check plots and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson DSM model


# Running DSM (CDS) with a Half-normal Detection Function key and Tweedie error distribution for Goitered gazelle:

dsm.xy.tweedie.gazelle <- dsm(N ~ s(x, y), hn.gazelle$ddf, depdat, distdata_gazelle, family=tw(), 
                              transect = "point", method="REML")

summary(dsm.xy.tweedie.gazelle)

par(mfrow=c(2,2))
gam.check(dsm.xy.tweedie.gazelle)
dev.off()


# Running DSM (CDS) with a Half-normal Detection Function key and Negative Binomial error distribution for 
# Goitered gazelle :

dsm.xy.nb.gazelle <- dsm(N ~ s(x, y), hn.gazelle$ddf, depdat, distdata_gazelle, transect = "point", 
                         family=nb(), method = "REML")

summary(dsm.xy.nb.gazelle)

par(mfrow=c(2,2))
gam.check(dsm.xy.nb.gazelle)
dev.off()

# Both Negative Binomial and Tweedie distribution models have gam plots that looks OK. Even the Negative Binomial 
# has a lower REML than the Tweedie one, but its deviance explained is really low. So, for the Goitered gazelle 
# count data adjusted by the detection function, I will select the DSM in CDS with Tweedie error 
# distribution to estimate its density and abundance, and to map its distribution in the GGASPA.


#### Kulan DSM (CDS) ####

# Running DSM (CDS) with Half-normal Detection Function key and quasi-Poisson error distribution for Kulan:

dsm.xy.qpois.kulan <- dsm(N ~ s(x, y), hn.kulan$ddf, depdat, distdata_kulan, transect = "point", method = "REML")

summary(dsm.xy.qpois.kulan)

par(mfrow=c(2,2))
gam.check(dsm.xy.qpois.kulan)
dev.off()

# Fitting warning, gam.check plots and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson DSM model


# Running DSM (CDS) with a Half-normal detection function key and Tweedie error distribution for Kulan :

dsm.xy.tweedie.kulan <- dsm(N ~ s(x, y), hn.kulan$ddf, depdat, distdata_kulan, family=tw(), 
                            transect = "point", method="REML")

summary(dsm.xy.tweedie.kulan)

par(mfrow=c(2,2))
gam.check(dsm.xy.tweedie.kulan)
dev.off()


# Running DSM (CDS) with a Half-normal Detection Function key and Negative Binomial error distribution for Kulan:

dsm.xy.nb.kulan <- dsm(N ~ s(x, y), hn.kulan$ddf, depdat, distdata_kulan, family=nb(), 
                       transect = "point", method="REML")

summary(dsm.xy.nb.kulan)

par(mfrow=c(2,2))
gam.check(dsm.xy.nb.kulan)
dev.off()


# Both Negative Binomial and Tweedie distribution models have gam plots that looks OK. Even the Negative Binomial 
# has a higher Deviance explained than the Tweedie one, but its REML is higher than the one for Tweedie. So, for 
# the Kulan count data adjusted by the detection function, I will select the DSM in CDS with Tweedie error 
# distribution to estimate its density and abundance, and to map its distribution in the GGASPA.



#### Spatial Analysis in DSM (CDS) ####

# In this stage, I need to make a grid of spatial coordinates along the whole survey area, over which be able to 
# predict the spatial distribution and expected abundance and density of both species, according to the selected 
# DSM (CDS) model.

# First step is to construct the prediction grid for the GGASPA, so I will select from the survey area the columns 
# of distance-based x,y coordinates (in metres):

GGASPA2 <- dplyr::select(GGASPA, c("x", "y"))  # inout() function seems sensitive to these column names


# Then, I construct the square grid of distance-based coordinates ("1000" value is the dimension along each axis for
# each cell in the grid):

grd <- with(GGASPA2, expand.grid(x=seq(min(x), max(x), 1000), y=seq(min(y), max(y), 1000)))   


# After, I need to select from the square prediction grid the area corresponding to the GGASPA, so I shaped the grid 
# as the survey area with inout() function (needs columns "x" and "y", case-sensitive)

grd.in <- grd[inout(grd, GGASPA2),]      

plot(grd.in)


# Now, I am able to make predictions over the grid and estimate density and abundance for both focal species, using
# for the prediction the DSM (CDS) selected for each one:

# Density and Abundance estimation for Goitered gazelle, using DSM with Half-normal detection function key and
# Tweedie error distribution:

off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance

predn_gazelle <- predict(dsm.xy.tweedie.gazelle, grd.in, off.set)  


# I need to bind on my predictions file with the data from the prediction grid used to create them:

ss <- cbind(grd.in, predn_gazelle)


# And then, add a column on this prediction grid file of abundances with the dimension for both axis of each cell, 
# from which calculate density:

ss$dim <- 1000


# Now that I have the predictions, I will plot the density for Goitered gazelle over the GGASPA:

s <- ggplot(ss) + gg.opts
s <- s + geom_tile(aes(x = x, y = y, fill = predn_gazelle, width = dim, height = dim))
s <- s + coord_equal()
s <- s + geom_path(aes(x = x, y = y), data = GGASPA)
s <- s + labs(fill = "Density / km2")
s <- s + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18))
print(s)

# Plot with density + CT location for Goitered gazelle over the GGASPA: (not included in the report, just 
# exploration of which look more informative)

s2 <- ggplot(ss) + gg.opts
s2 <- s2 + geom_tile(aes(x = x, y = y, fill = predn_gazelle, width = dim, height = dim))
s2 <- s2 + coord_equal()
s2 <- s2 + geom_path(aes(x = x, y = y), data = GGASPA)
s2 <- s2 + geom_point(aes(x, y, size = gazelle_count), data = depdat, colour = "red", alpha = I(0.7))
s2 <- s2 + labs(fill = "Density / km2", size="Count")
s2 <- s2 + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18))
print(s2)

# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Goitered gazelle 
# in the GGASPA:

est.abun.gazelle <- sum(predn_gazelle)

est.abun.gazelle
# [1] 2071.026


# Also, with the estimated total abundance I can calculate the estimated total density for the species in the GGASPA.
# I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) to calculate total area which 
# will divide total estimated abundance to obtain total estimated density:

est.dens.gazelle.predgrid <- est.abun.gazelle / length(predn_gazelle)

est.dens.gazelle.predgrid
# [1] 0.0451558

# Transforming density into individuals/100km2

est.dens.gazelle.predgrid*100
# [1] 4.51558 individuals/100 km2


# Density and Abundance estimation for Mongolian Kulan, using DSM (CDS) with Half-normal detection function key and
# Tweedie error distribution:

predn_kulan <- predict(dsm.xy.tweedie.kulan, grd.in, off.set)  


# I need to bind on my predictions file with the data from the prediction grid used to create them:

tt <- cbind(grd.in, predn_kulan)


# And then, add a column on this prediction grid file of abundances with the dimension of each cell, from which
# calculate density:

tt$dim <- 1000


# Now that I have the predictions, I will plot the density for Mongolian Kulan over the GGASPA:

t <- ggplot(tt) + gg.opts
t <- t + geom_tile(aes(x = x, y = y, fill = predn_kulan, width = dim, height = dim))
t <- t + coord_equal()
t <- t + geom_path(aes(x = x, y = y), data = GGASPA)
t <- t + labs(fill = "Density / km2")
t <- t + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18))
print(t)


# Plot with density + CT location for Goitered gazelle over the GGASPA: (not included in the report, just 
# exploration of which look more informative)

t2 <- ggplot(tt) + gg.opts
t2 <- t2 + geom_tile(aes(x = x, y = y, fill = predn_kulan, width = dim, height = dim))
t2 <- t2 + coord_equal()
t2 <- t2 + geom_path(aes(x = x, y = y), data = GGASPA)
t2 <- t2 + geom_point(aes(x, y, size = kulan_count), data = depdat, colour = "red", alpha = I(0.7))
t2 <- t2 + labs(fill = "Density / km2", size="Count")
print(t2)


# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Mongolian Kulan 
# in the GGASPA:

est.abun.kulan <- sum(predn_kulan)

est.abun.kulan
# [1] 166.7125


# Also, with the estimated total abundance I can calculate the estimated total density for the species in the GGASPA.
# I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) to calculate total area which 
# will divide total estimated abundance to obtain total estimated density:

est.dens.kulan.predgrid <- est.abun.kulan / length(predn_kulan)

est.dens.kulan.predgrid
# 1] 0.003634931


# Transforming density into individuals/100km2

est.dens.kulan.predgrid*100
# [1] 0.3634931 individuals/100 km2


#### Variance of Estimated Density and Abundance in DSM (CDS) ####


# Here I will calculate the variance of the density and abundance estimates for both species using the estimation of 
# their DSM uncertainty (uncertainty of GAM models) and combining it with the uncertainty of the detection 
# function. This approach assumes independency between the detection function and spatial modelling of density and 
# abundance


# Variance calculation of Density and Abundance estimates for Goitered gazelle using a DSM model 

varprop_gazelle <- dsm.var.gam(dsm.xy.tweedie.gazelle, grd.in, off.set)

summary(varprop_gazelle)

# CV of detection function       : 0.1261167
# CV from GAM                    : 0.6328
# Total coefficient of variation : 0.6453 



# Variance calculation of Density and Abundance estimates for Mongolian Kulan using a DSM (CDS) model

varprop_kulan <- dsm.var.gam(dsm.xy.tweedie.kulan, grd.in, off.set)

summary(varprop_kulan)

# CV of detection function       : 0.146196 
# CV from GAM                    : 0.5366
# Total coefficient of variation : 0.5562




