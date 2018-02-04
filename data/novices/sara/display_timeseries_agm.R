#
#  Displaying the time series data
#  from Razor 9DOF IMU
# 
#  Miguel Perez-Xochicale <perez.xochicale AT gmail.com>
#  University of Birmingham, UK
#  Wednesday, the 28th of January 2015
#

library(lattice) ##xyplot

rawdata <- NULL;
data <- NULL;


#If you want to change your working directory to a subdirectory,
#then you can simply do this:
setwd(paste(getwd(),"",sep=""))  ; # set and get the current working directory


##list all files with a specific extension
files <-list.files(pattern = "\\.csv")

#pattern0 
#filename_imu0 <- files[1]
#filename_imu1 <- files[10]
#filename_imu2 <- files[19]
#filename_imu3 <- files[28]

#pattern6
filename_imu0 <- files[7]
filename_imu1 <- files[16]
filename_imu2 <- files[25]
filename_imu3 <- files[34]


plot_timeseries_imus<- function(filename_imuN)
{
rawdata<-read.csv(paste(filename_imuN,"",sep=""), sep=',');
#If you want to move back up to the parent directory, the type
# setwd("../")

N <- nrow(rawdata); #get the number of rows of the data file
window <- 0:N;
#window <- 400:800;

data$sample <- rawdata$Sample[window];
data$ax <- rawdata$ACCX[window];
data$ay <- rawdata$ACCY[window];
data$az <- rawdata$ACCZ[window];
data$am <- sqrt(data$ax^2 + data$ay^2 + data$az^2 );
data$mx <- rawdata$MAGX[window];
data$my <- rawdata$MAGY[window];
data$mz <- rawdata$MAGZ[window];
data$mm <- sqrt(data$mx^2 + data$my^2 + data$mz^2 );
data$gx <- rawdata$GYRX[window];
data$gy <- rawdata$GYRY[window];
data$gz <- rawdata$GYRZ[window];
data$gm <- sqrt(data$gx^2 + data$gy^2 + data$gz^2 );


# ############################################################
# #### Low-Pass Filter to remove the short-term fluctuations
# alpha <- 0.5
# data$eylp[1] <- 0
# data$erlp[1] <- 0
# for(i in 1:N) {
#    #print(data$ey[i])
#   #data$eylp[i+1] = data$ey[i]*alpha + ( (data$eylp[i])* (1-alpha)  )
#   data$erlp[i+1] = data$er[i]*alpha + ( (data$erlp[i])* (1-alpha)  )
# }
# ############################################################


###ACC
plotacc = xyplot( data$ax + data$ay + data$az + data$am ~ data$sample, data=rawdata, 
#xyplot( data$erlp ~ data$sample, data=rawdata, 
        #xyplot( data$accx ~ data$sample, data=rawdata, 
        pch=16, col.line = c('red', 'blue', 'green', 'black'), type = c("l","g"), lwd=3,
        main=list(label="ACC", cex=2.5),
        xlab=list(label="Samples", cex=2),
        ylab=list(label="Raw Data", cex=2),

## LABELS
key=list(
  border= "grey", 
  text = list(c("X", "Y", "Z", "Mag")), 
  lines = list(pch=c(1,2,3,4), col= c('red','blue','green','black')), type="l", lwd=3,
  cex=2, # control the character expansion  of the symbols
  corner=c(0,0) # position
),

        
        grid = TRUE
)

###MAG
plotmag = xyplot( data$mx + data$my + data$mz + data$mm ~ data$sample, data=rawdata, 
        #xyplot( data$erlp ~ data$sample, data=rawdata, 
        #xyplot( data$accx ~ data$sample, data=rawdata, 
        pch=16, col.line = c('red', 'blue', 'green', 'black'), type = c("l","g"), lwd=3,
        main=list(label="MAG", cex=2.5),
        xlab=list(label="Samples", cex=2),
        ylab=list(label="Raw Data", cex=2),
        
        ## LABELS
        key=list(
          border= "grey", 
          text = list(c("X", "Y", "Z", "Mag")), 
          lines = list(pch=c(1,2,3,4), col= c('red','blue','green','black')), type="l", lwd=3,
          cex=2, # control the character expansion  of the symbols
          corner=c(0,0) # position
        ),
        
        grid = TRUE
)


###GYRO
plotgyr = xyplot( data$gx + data$gy + data$gz + data$gm ~ data$sample, data=rawdata, 
        pch=16, col.line = c('red', 'blue', 'green', 'black'), type = c("l","g"), lwd=3,
        main=list(label="GYRO", cex=2.5),
        xlab=list(label="Samples", cex=2),
        ylab=list(label="Raw Data", cex=2),
        
        ## LABELS
        key=list(
          border= "grey", 
          text = list(c("X", "Y", "Z", "Mag")), 
          lines = list(pch=c(1,2,3,4), col= c('red','blue','green','black')), type="l", lwd=3,
          cex=2, # control the character expansion  of the symbols
          corner=c(0,0) # position
        ),
        
        grid = TRUE        
        
)


# print(pa, position=c(0,0.6,1,1), more=TRUE)
# print(pm, position=c(0,0,1,.4))
png(filename=paste("ACC_PLOT_",filename_imuN,".png",sep=""), height=600, width=1000,bg="white")
print(plotacc)
dev.off() # Turn off device driver (to flush output to PNG)

png(filename=paste("MAG_PLOT_",filename_imuN,".png",sep=""), height=600, width=1000,bg="white")
print(plotmag)
dev.off() # Turn off device driver (to flush output to PNG)

png(filename=paste("GYRO_PLOT_",filename_imuN,".png",sep=""), height=600, width=1000,bg="white")
print(plotgyr)
dev.off() # Turn off device driver (to flush output to PNG)



print(plotacc)
print(plotmag)
print(plotgyr)

}  

plot_timeseries_imus(filename_imu0)
plot_timeseries_imus(filename_imu1)
plot_timeseries_imus(filename_imu2)
plot_timeseries_imus(filename_imu3)

  


