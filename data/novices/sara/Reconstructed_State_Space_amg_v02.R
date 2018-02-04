#
#  Displaying the time series data
#  from Razor 9DOF IMU
# 
#  Miguel Perez-Xochicale <perez.xochicale AT gmail.com>
#  University of Birmingham, UK
#  Wednesday, the 28th of January 2015
#

#library(scatterplot3d)
require(tseriesChaos)
require(rgl)
library(lattice) ##xyplot

rawdata <- NULL;
data <- NULL;
sdev_pc <- NULL;

#If you want to change your working directory to a subdirectory,
#then you can simply do this:
setwd(paste(getwd(),"",sep=""))  ; # set and get the current working directory


##list all files with a specific extension
files <-list.files(pattern = "\\.csv")


##pattern0 
filename_imu0 <- files[1]
filename_imu1 <- files[8]
filename_imu2 <- files[15]
filename_imu3 <- files[22]

# #pattern1
# filename_imu0 <- files[2]
# filename_imu1 <- files[9]
# filename_imu2 <- files[16]
# filename_imu3 <- files[23]


# #pattern2
# filename_imu0 <- files[3]
# filename_imu1 <- files[10]
# filename_imu2 <- files[17]
# filename_imu3 <- files[24]

# #pattern3
# filename_imu0 <- files[4]
# filename_imu1 <- files[11]
# filename_imu2 <- files[18]
# filename_imu3 <- files[25]

# # # #pattern4
#   filename_imu0 <- files[6]
#   filename_imu1 <- files[13]
#   filename_imu2 <- files[20]
#   filename_imu3 <- files[27]

# #pattern5
# filename_imu0 <- files[7]
# filename_imu1 <- files[14]
# filename_imu2 <- files[21]
# filename_imu3 <- files[28]

# 
# # #pattern6
#  filename_imu0 <- files[5]
#  filename_imu1 <- files[12]
#  filename_imu2 <- files[19]
#  filename_imu3 <- files[26]






# =====================================
# Plot Time Series
plot_timeseries <- function(timeseries, colour)
{
  
  xyplot( timeseries ~ data$sample, data=rawdata,
          pch=16, col.line = c(colour), type = c("l","g"), lwd=3,
          main=list(label="", cex=2.5),
          xlab=list(label="Samples",font=2, cex=2),
          ylab=list(label="Raw",font=2, cex=2),
          scales=list(font=2, cex=1.5),# size of the number labels from the x-y axes
          ## LABELS
          #key=list(
          #border= "grey", 
          #text = list(c("X", "Y", "Z", "Mag")), 
          #lines = list(pch=c(1,2,3,4), col= c('red','blue','green','black')), type="l", lwd=3,
          #cex=2, # control the character expansion  of the symbols
          #corner=c(0,0) # position
          #),
  )
  
}




# =====================================
# Takens' Theorem
Takens_Theorem <- function(timeserie,dim,tau,print_flag)
{
  timedelayembedded <- embedd(timeserie,  m=dim, d=tau)
  
  if (print_flag == 1){
    print("--------------")
    print(paste("Embedded Parameters:         " ,"m=",dim," t=",tau,sep="" ))
    print(paste("Embedded Matrix dimension:  ",dim(timedelayembedded)[1], 'x' ,dim(timedelayembedded)[2] ) )
  }
  
  return (timedelayembedded)
}





# =====================================
# Principal Component Analysis
PCA <- function(Embedded_Matrix, print_flag)
{
  # Center the data so that the mean of each row is 0
  rm=rowMeans(t(Embedded_Matrix));
  X= t(Embedded_Matrix  - t((matrix(rep(rm,dim(Embedded_Matrix)[1]),nrow=dim(Embedded_Matrix)[2]))))
  
  # Covariance Matrix
  E=X %*% t(X)
  Eigen=eigen(E,TRUE)
  
  P=t(Eigen$vectors) # Principal Components
  
  #sdev_method1= sqrt(Eigen$values)
  #the standard deviations of the principal components 
  singular_values =  sqrt(diag(( 1 /(dim(X)[2]-1)*P%*% E %*% t(P))))
  #(i.e., the square roots of the eigenvalues of the covariance/correlation matrix).
  
  
  # Find the new data ##Rotated data
  rotateddata = P %*% X
  
  if (print_flag == 1){
    print(singular_values)
  }
  
  output<-list(P,singular_values,rotateddata)
  return(output)
  
}




# =====================================
# Plot_State Space
Plot_State_Space <- function(PCAMatrix,dim,tau, colour){
  
  rgl.open()
  rgl.clear()
  bg3d("white") # background color
  light3d()
  rgl.linestrips(PCAMatrix[1,],PCAMatrix[2,],PCAMatrix[3,],color=c(colour), alpha=0.8, lwd =3)
  #rgl.linestrips(PCAMatrix[1,],PCAMatrix[3,],PCAMatrix[5,],color=c(colour), alpha=0.9, lwd =2)
  rgl.viewpoint( theta = 0, phi = 15, fov = 60, zoom = .8, scale = par3d("scale"), interactive = TRUE)
  
  #axis labels
  rgl.material(
    color = c("black")  
  )
  
  axes3d()
  #   #axis3d('x',pos=c(NA, 0, 0))
  
  
  title3d(paste("m=",dim,"t=",tau,sep=""),'','PC1','PC2','PC3')
  rgl.bringtotop()
}



#########################################
##  Main


datasensornumber <- filename_imu2


rawdata<-read.csv(paste(datasensornumber,"",sep=""), sep=',');
N <- nrow(rawdata); #get the number of rows of the data file
#window <- 0:N;
#window <- 200:600;
window <- 200:1000;

wL <- length(window); #get the number of rows of the data file
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
# write(data$ey, file = paste("yaw_wL",wL,"_",filename,".dat",sep=""), ncolumns=1)



State_Space_Iteration<- function(timeserie, colour) {
  plot_timeseries(timeserie, colour)
  # for (dim_i in (1:100)[c(20,30,40,50,60,70,80,90)]){
  #   for (tau_j in (1:10)[c(1,3,5)]){
  for (dim_i in (1:200)[c(50,75,100)]){
    for (tau_j in (1:10)[c(1,3,5)]){
      
      embeddedmatrix <- Takens_Theorem(timeserie,dim_i,tau_j,1)
      pcamatrix <- PCA(embeddedmatrix,0)
      #    barplot(pcamatrix[[2]] / pcamatrix[[2]][1], xlab="k", ylab="Normalized Singular Values")
      #    title(main = list(paste("Singular Spectrum (m=",dim_i," t=",tau_j,")",sep=""), font = 2))
      Plot_State_Space(pcamatrix[[3]],dim_i,tau_j, colour)
      
    }##tau_k
  }##dim_j
  
}


######################
#IMU0 
#timeserie <- data$mx
#timeserie <- data$ax
#timeserie <- data$gy #80
#colour <- 'red'


# State_Space_Iteration(data$ax, 'red')
# State_Space_Iteration(data$ay, 'blue')
# State_Space_Iteration(data$az, 'green')
# 
# State_Space_Iteration(data$mx, 'red')
# State_Space_Iteration(data$my, 'blue')
# State_Space_Iteration(data$mz, 'green')

State_Space_Iteration(data$gx, 'red')
State_Space_Iteration(data$gy, 'blue')
State_Space_Iteration(data$gz, 'green')
