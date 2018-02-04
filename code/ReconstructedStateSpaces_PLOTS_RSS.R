
##
##    Reconstructed State Spaces
##

#   > Miguel Xochicale <perez.xochicale AT gmail.com>
#   > Doctoral Researcher in Human-Robot Interaction
#   > University of Birmigham, U.K. (2014-2017)
#   > http://mxochicale.github.io/
#
#  June 2016


setwd("../")
MAINPATH <- getwd()


#LOAD FUNCTIONS
source("~/mxochicale/phd/r-code/functions/ollin_cencah.R")
source("~/mxochicale/phd/r-code/functions/functions_inertial_sensors.R")

main_data_path <- "rawdata"



#NOTE
## FOR PATTERN 0 use timeseries <- timeserie_n_mag_z
## FOR PATTERN 1 use timeseries <- timeserie_n_mag_y

############################
#class <- "novices"
#username <- "chunkit"
#pattern_number <- "pattern2"

##########################
#class <- "intermediate"
#username <- "cristian"
#pattern_number <- "pattern2"


###########################
class <- "expert"
username <- "jose"
pattern_number <- "pattern2"


star <- 400
end <- 900
windowframe <- star:end;


setwd(paste(MAINPATH,"/",main_data_path,"/",class,"/",username,sep="")); # set and get the current working directory
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



dataMatrix <- get_ACCMAGGYRdata(filename_imu0)


#plot_mag_razor(dataMatrix,windowframe)
#plot_n_mag_razor(dataMatrix,windowframe)
#plot_razor_one_axis(dataMatrix,17,windowframe)






timeserie_mag_x <- dataMatrix[windowframe,6]
timeserie_mag_y <- dataMatrix[windowframe,7]
timeserie_mag_z <- dataMatrix[windowframe,8]


timeserie_n_mag_x <- dataMatrix[windowframe,15]
timeserie_n_mag_y <- dataMatrix[windowframe,16]
timeserie_n_mag_z <- dataMatrix[windowframe,17]


#timeseries <- timeserie_n_mag_z   ## pattern 0
timeseries <- timeserie_n_mag_y  ## pattern 2



#save the graphical plots in the otuput directory
setwd(paste(MAINPATH,"/outputs/",sep=""));




max_state_space_lenght <- 4

   for (dim_i in (1:200)[c(10)]){ #>>for (dim_i in (1:200)[c(10,20,30,40)]){
     for (tau_j in (1:100)[c(6)]){ #>> for (tau_j in (1:10)[c(1,2,3,4,5)]){

        #############
        ## ACC X
        embeddedmatrix <- Takens_Theorem(timeseries,dim_i,tau_j,1) #Takens_Theorem(timeserie,dim,tau,print_flag)
        pcamatrix <- PCA(embeddedmatrix,0) #PCA(Embedded_Matrix, print_flag)
        #        ¦  ([1] ,[2]             ,[3]         ,[4]          ,[5] ,[6]     ,[7]   )
        #        ¦  (P   ,singular_values ,rotateddata ,Eigen$values ,POV ,cumEigv ,twoPC )


        filenameimage <- paste("2drss_","m",formatC(dim_i,digits=2,flag="0"),"t",formatC(tau_j,digits=1,flag="0"),
        "_",class,"_",username,"_",pattern_number,"_",".png",sep="")
        png(filenameimage, width=2000, height=2000, units="px", res=400,bg = "transparent")
        Plot_2D_State_Space_testing(pcamatrix[[3]],dim_i,tau_j, "blue", max_state_space_lenght) # function(PCAMatrix, colour, maxplotlenght){
        dev.off()

        filenameimage <- paste("pv_","m",formatC(dim_i,digits=2,flag="0"),"t",formatC(tau_j,digits=1,flag="0"),
        "_",class,"_",username,"_",pattern_number,"_",".png",sep="")
        png(filenameimage, width=2500, height=2500, units="px", res=400 ,bg = "transparent")
        Plot_PV_testing(pcamatrix[[5]],dim_i,tau_j)
        dev.off()


    }##tau_j
  }##dim_i





#back to R sourcecode path
setwd(paste(MAINPATH,"/r-scripts/",sep="")); # set and get the current working directory
