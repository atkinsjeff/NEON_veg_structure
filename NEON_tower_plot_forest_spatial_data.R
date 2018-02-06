# Making this script to focus on spatializing tree data for Bob
require(maps)
require(maptools)
require(mapproj)
require(rgdal)
require(plyr)
require(dplyr)
require(ggrepel)

#setwd("C:/github/NEON_veg_structure/")

# First we bring in the reference points from NEON reference data
ref.points <- read.csv("C:/github/NEON_veg_structure//data/pointID_spatialData_towerPlots.csv")

str(ref.points)


# Then we bring in all of the tree marker data. This is every tree, by tag, in a plot
# with a spatial ID tag and azimuth
tree.mapping.data <- read.csv("C:/github/NEON_veg_structure//data/vst_mappingTagging2015_goughSites.csv")

sum(is.na(tree.mapping.data$pointID))

df.tree <- join(tree.mapping.data, ref.points, type = "left", match = "all")

sum(is.na(tree.mapping.data$pointID))

# # notes from Atticus
# x <- distance * cos(theta)
# y <- distance * sin(theta)

# the theta is stemAzimuth
# distance is stemDistance

# Now we use Trig, hopefully
df.tree$x <- df.tree$stemDistance * cos(df.tree$stemAzimuth)
df.tree$y <- df.tree$stemDistance * sin(df.tree$stemAzimuth)

# Then we add our x and y values
df.tree$tree.easting <- df.tree$easting + df.tree$x
df.tree$tree.northing <- df.tree$northing + df.tree$y

tree.info <- read.csv("C:/github/NEON_veg_structure//data/vst_apparentIndiv2015_goughSites.csv")

# Now let's look at table data

# Table 7. Growth forms into which woody vegetation is classified, and their definitions. Parentheses following the terms in the list below indicate the 
# lia - liana
# sbt - single bole tree
# mbt - multi bole tree
# smt - small tree
# sap - sapling
# sis - single shrub, self-supporting

# sms - small shrub, multi stem
library("plyr")
data_frame <- ddply(data_frame, .(v), transform, n = length(v))
#writing to disk
#write.csv(df, file = "NEON_veg_mapping_data_merged.CSV")
df <- join( tree.info, df.tree, type = "left", match = "all")

df$growthForm <- tolower(df$growthForm)

#look at only trees
df <- subset(df, df$growthForm == "sbt" | df$growthForm == "mbt" | df$growthForm == "smt") 
#organize the names
#df$scientificName <- tolower(df$scientificName)
df$growthForm <- tolower(df$growthForm)
df$taxonID <- toupper(df$taxonID)
df$taxonID <- trimws(df$taxonID, which = "both")
unique(df$scientificName)

#based on work below, I am going to replace the following
df$taxonID[df$taxonID == "QUERCUS"] <- "QUERC"

#
taxon <- data.frame(unique(df$taxonID))
taxon <- taxon[order(taxon$unique.df.taxonID.),]
data.frame(taxon)


sp.table <- table(df$taxonID, df$plotID)
sp.table <-as.data.frame(sp.table)

#Now we sort to the plot level
df %>%
  group_by(plotID) %>% summarize(n_distinct(taxonID)) -> df.plot
#write.csv(df.plot, "NEON_species_richness_by_plot.CSV")
data.frame(df.plot)

#Now the site level
df %>%
  group_by(siteID) %>% summarize(n_distinct(scientificName)) -> df.site

tree.mapping.data %>%
     group_by(siteID) %>% summarize(n_distinct(taxonID)) -> taxon.site


data_frame <- data.frame(v = rep(c(1, 2, 2, 2), 25))

library("plyr")
data_frame <- ddply(data_frame, .(v), transform, n = length(v))
#writing to disk
#write.csv(df, file = "NEON_veg_mapping_data_merged.CSV")

harv.41 <- subset(df, df$plotID == "HARV_041")
grsm <- subset(df, df$siteID == "GRSM")


#grsm.47 <- subset(harv, harv$measurementHeight.1 == 130)

require(ggplot2 )
ggplot()+
  geom_point(data = grsm, aes(x = tree.easting, y = tree.northing, size = stemDiameter))+
  coord_fixed(1.3)+
  ggtitle("GRSM")


eastcoast <- subset(states, region %in% c("maine","new hampshire", "massachusetts","vermont", "new york", "connecticut", "rhode island", "pennsylvania", "delaware", "new jersey","maryland", "virginia", "ohio", "north carolina", "south carolina", "georgia", "florida", "alabama", "indiana", "illinois", "wisconsin", "michigan", "tennessee", "kentucky", "west virginia", "missouri", "arkansas", "mississippi", "iowa", "minnesota", "lousiana"))


x11()
usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = eastcoast, aes(x=long, y = lat, group = group), fill = "light grey") + 
  coord_fixed(1.3)+
  scale_fill_manual(values=cbPalette)+
  theme_bw()+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  geom_point(data = df, aes(x = long, y = lat,  fill = Affiliation), color = "black", pch = 21, size = 3)+
  geom_text_repel(data = df, aes(label = paste(" ", as.character(site_code), sep = ""), x = long, y = lat), angle = 0, hjust = 0)+
  xlab("Longitude")+
  ylab("Latitude")