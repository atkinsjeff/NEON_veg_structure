####################################
###PCL Workflow
#Franklin Wagner & Liz LaRue
#12/17/2017
####################################

##############################################################
#workflow to measure max canopy height and mean canopy height in a single AOP 40 x 40 m tower plot
##############################################################


#Load package, set directory
library(lidR) #Loads package
setwd("C:/Users/Franklin/Documents")


#output data file info
out.path<-"C:/Users/Franklin/Documents"
site <- "GRSM"
plot <- 058


#STEP 1) open single .laz file and use filter = to drop 190 > z (m) < 0
data <- readLAS("d:/D07/GRSM/2015/GRSM_L1/GRSM_Lidar/Classified_point_cloud/2015_GRSM_1_273000_3952000_colorized.laz")


#STEP 2) error check
summary(data) #summary of meta data of las file, include min & max Z
extent(data) #spatial extent of LAS tile in easting and northing 
plot(data) # check to see if we need to clean up outlier points NEED TO FIX THIS


#STEP 3) cut out a 200 x 200 m buffer plot to create DTM with kriging method
#center of 40 m x 40 m plot SERC_048 is 38.89318, -76.566967 
easting <- 273293.1
northing <- 3952675
#add 20 m to easting and northing coordinates (x,y)
data.200m <- lasclipRectangle(data, xleft = (easting - 100), ybottom = (northing - 100), 
                             xright = (easting + 100), ytop = (northing + 100))
plot(data.200m)

########################################################################################################
#STEP 4) correct for ground height
#make a digital terrain model by splitting ground (4th return) and non-ground points (1, 2, 3rd returns)
dtm <- grid_terrain(data.200m, method = "kriging", k = 10L, keep_lowest = FALSE)
plot(dtm)
lasnormalize(data.200m, dtm)
plot(data.200m) #the data.200m is updated and now corrected for elevation
#We are still getting negative values when this code is run
#7952 points below zero are found
########################################################################################################

#STEP 5) cut out 40 x 40 m plot
#GPS point given by NEON is at center of plot, so clip 20 m out from each side of this point
#plot SERC_048 is 38.89318, -76.566967 
# easting <- 364106.214
# northing <- 4306089.7
#add 20 m to easting and northing coordinates (x,y)
data.40m <- lasclipRectangle(data.200m, xleft = (easting - 20), ybottom = (northing - 20), 
        xright = (easting + 20), ytop = (northing + 20))
#check a couple things
summary(data.40m) #should be below 0, but its not - ERROR HERE
plot(data.40m) #you can see a couple weird negative points that were added by the DTM
extent(data.40m) #correct extent 
sum(data.40m@data$Z<0)  #this command returns z vals less than 0

data.40m@data$Z[data.40m@data$Z <= 0] <- NA


#STEP 6) Calculate mean maximum canopy height of points in 40 x 40 m pot 
#create a CHM to measure max canopy height in 1 x 1 m grids
chm <- grid_canopy(data.40m, res = 1, subcircle = 0)
mean.max.canopy.ht <- sum(chm[,3])/nrow(chm[,3])
mean.max.canopy.ht


#STEP 7) Calculate maximum canopy height of points in 40 x 40 m plot 
max.canopy.ht <- max(chm[,3])
max.canopy.ht
#lasmetrics(data.40m, max(Z)) (this command accomplishes the same thing)


#STEP 8) calculate rumple of the 40 x 40 m block
rumple <- rumple_index(chm) #use canopy height model
rumple
#data.40m.sp <- as.spatial(data.40m) #creates a SpatialPointsDataFrame w/ 14 columns
#data.40m.df <- as.data.frame(mat) #transform sp into a dataframe
#data.40m.xyz <- cbind(data.40m.df[,13:14], data.40m.df[,1:2]) #extra xyz
#data.40m.xyz <- data.40m.xyz[,-4]
#rumple <- rumple_index(data.40m.xyz[,1], data.40m.xyz[,2], data.40m.xyz[,3])
#huge difference between rumple by point cloud and from CHM, but rumple by CHM is closer to forestR


#STEP 9) Output data as a .csv (give row and columns appropriate names) 
output <- data.frame(matrix(c(site, plot, mean.max.canopy.ht, max.canopy.ht, rumple), ncol = 5))
colnames(output) <- c("site", "plot", "mean.max.canopy.ht", "max.canopy.ht", "rumple")
output
write.csv(output, file=paste(out.path, "NEON.AOP.height.csv"), row.names = FALSE) 

x <- data.40m@data$X
y <- data.40m@data$Y
z <- data.40m@data$Z

plot()

