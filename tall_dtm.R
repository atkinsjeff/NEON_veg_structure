#lidar data for talladega
require(rgdal)
require(maps)
require(raster)
require(rasterVis)
require(gdalUtils)

# read the individual rasters into a list of RasterLayer objects
setwd("d:/D08/TALL/2015/TALL_L3/TALL_Lidar/DTM")
file.list <- list.files(pattern = "\\.tif$")

input.rasters <- lapply(list.files(pattern = "\\.tif$"), raster)

# create an empty output raster that spans the full extent of all input
# rasters, and uses the same coordinate reference system; in this case
# we know that all input rasters share the same CRS, so we can
# arbitrarily extract CRS information from the first one
full.extent <- extent( c(451000,468000, 3630000, 3650000))
bounding.raster <- raster(full.extent,
                          crs=projection(input.rasters[[1]]))


# set the output resolution to match the center tile (somewhat
# arbitrarily); this can also be specified manually if preferred
res(bounding.raster) <- res(input.rasters[[5]])



# # for each input raster, extract the corresponding sub-extent of the
# # empty output raster, and use this as the basis for resampling
# # !! note that this may take several minutes to run !!
# resampled.rasters <- lapply(input.rasters, function(input.raster) {
#   target.raster <- crop(bounding.raster, input.raster)
#   resample(input.raster, target.raster, method="bilinear")
# })

# mosaic all 9 resampled rasters, taking the maximum pixel value in
# cases where there is overlap
input.rasters$fun <- mean 
raster.mosaic <- do.call(mosaic, input.rasters)

writeRaster( filename="d:/tall_full_dtm.tif")