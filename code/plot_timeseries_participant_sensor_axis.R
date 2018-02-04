#
#  Displaying the time series data
#  from Razor 9DOF IMU
#
#   > Miguel Xochicale <perez.xochicale AT gmail.com>
#   > Doctoral Researcher in Human-Robot Interaction
#   > University of Birmigham, U.K. (2014-2017)
#   > http://mxochicale.github.io/
#
#  June 2016
#
#  NOTE: pattern 3 and 4 are swapped because one participant have a different sequence (as far as I remember it was sara)
#

# > source('~/mxochicale/phd/manuscripts/WearableRobotics/2016/results/r-scripts/plot_timeseries_participant_sensor_axis.R', echo=TRUE)


setwd("../")
MAINPATH <- getwd()


#LOAD FUNCTIONS
source("~/mxochicale/phd/r-code/functions/functions_inertial_sensors.R")

data_path <- "rawdata"
scripts_path <- "r-scripts"



# ##########################
# class <- "novices"
# username <- "weitong_yan"
# pattern_number <- "pattern2"

# ##########################
# class <- "intermediate"
# username <- "cristian"
# pattern_number <- "pattern2"

###########################
class <- "expert"
username <- "jose"
pattern_number <- "pattern0"



setwd(paste(MAINPATH,"/",data_path,"/",class,"/",username,sep="")); # set and get the current working directory
details = file.info(list.files(pattern="*.csv"))
files = rownames(details)


if (pattern_number == "pattern0" )
{
  print("Pattern 0 <--> mambo ")
  # ##pattern0
  filename_imu0 <- files[1]
  filename_imu1 <- files[8]
  filename_imu2 <- files[15]
  filename_imu3 <- files[22]
} else if (pattern_number == "pattern1" ){
  print("Pattern 1 <--> side step")
  #pattern1
  filename_imu0 <- files[2]
  filename_imu1 <- files[9]
  filename_imu2 <- files[16]
  filename_imu3 <- files[23]
} else if (pattern_number == "pattern2" ){
  print("2  <--> side crossover")
  filename_imu0 <- files[3]
  filename_imu1 <- files[10]
  filename_imu2 <- files[17]
  filename_imu3 <- files[24]
} else if (pattern_number == "pattern3" ){
  #######################PATTERNS 3 AND 4 ARE SWAPED
  # #pattern3  <--> front double cross
  filename_imu0 <- files[5]
  filename_imu1 <- files[12]
  filename_imu2 <- files[19]
  filename_imu3 <- files[26]  ###############SOMETHING IS WRONG WITH THIS TIME SERIE
} else if (pattern_number == "pattern4" ){
  #######################PATTERNS 3 AND 4 ARE SWAPED
  #pattern4  <--> backstep
  filename_imu0 <- files[4]
  filename_imu1 <- files[11] ###############SOMETHING IS WRONG WITH THIS TIME SERIE
  filename_imu2 <- files[18]
  filename_imu3 <- files[25]
} else if (pattern_number == "pattern5" ){
  # # #pattern5  <--> back step swing
  filename_imu0 <- files[6]
  filename_imu1 <- files[13]
  filename_imu2 <- files[20]
  filename_imu3 <- files[27] ###############SOMETHING IS WRONG WITH THIS TIME SERIE
} else if (pattern_number == "pattern6" ){
  #pattern6 <--> back peddal
  filename_imu0 <- files[7]
  filename_imu1 <- files[14]
  filename_imu2 <- files[21]
  filename_imu3 <- files[28]
} else {
  print("no valid vulue for the value patterns")
}



timeseries <- get_ACCMAGGYRdata(filename_imu0)


star <- 000
max_length <- dim(timeseries)[1]

windowframe <- star:max_length;



plot_mag_razor(timeseries,windowframe)







# for(file_name_number_k in c(1:6) ) {
#
#
# setwd(paste(MAINPATH,"/data/",main_experiment_name,"/",type,"/",name_of_the_experiment,"/",participant_number,"/",sensor_type,"/",sep="")); # set and get the current working directory
#
#
# details = file.info(list.files(pattern="*.csv"))
# details = details[with(details, order(as.POSIXct(mtime))), ]
# files = rownames(details)
#
#
# lastfile <- length(files)
# datafilename <- files[ file_name_number_k ]
# data <- get_ACCMAGGYRdata(datafilename)
#
# pathfilename <- substr(datafilename, 1, 20)
#
#
# mainDir <- getwd()
# subDir <- paste( "plots_razor_", pathfilename, sep=""  )
#
# if (file.exists(subDir)){
#   setwd(file.path(mainDir, subDir))
# } else {
#   dir.create(file.path(mainDir, subDir))
#   setwd(file.path(mainDir, subDir))
#
# }
#
#
# default_min_sample <- 0
# default_max_sample <- dim(data)[1]
# default_windowframe <- default_min_sample:default_max_sample;
#
# #default_windowframe <- 3000:5000;
# #default_windowframe <- 1500:3500;
#
#
#
# ############################################################
#
# png(file = paste( "plot_acc_razor_", pathfilename, ".png", sep=""), width = 1500, height = 900, units = "px", bg = "white")
# plot_acc_razor(data,default_windowframe)
# dev.off()
#
# # png(file = paste( "plot_mag_razor_", pathfilename, ".png", sep=""), width = 1500, height = 900, units = "px", bg = "white")
# # plot_mag_razor(data,default_windowframe)
# # dev.off()
#
# png(file = paste( "plot_gyr_razor_", pathfilename, ".png", sep=""), width = 1500, height = 900, units = "px", bg = "white")
# plot_gyr_razor(data,default_windowframe)
# dev.off()
#
# }




#back to R sourcecode path
setwd(paste(MAINPATH,"/",scripts_path,"/",sep="")); # set and get the current working directory
