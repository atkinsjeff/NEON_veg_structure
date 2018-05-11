# Making this script to focus on spatializing tree data for Bob
require(maps)
require(maptools)
require(mapproj)
require(rgdal)
require(plyr)
require(dplyr)
require(tidyr)
require(ggrepel)

#setwd("C:/github/NEON_veg_structure/")

# First we bring in the reference points from NEON reference data
ref.points <- read.csv("C:/github/NEON_veg_structure//data/pointID_spatialData_towerPlots.csv")

str(ref.points)


# Then we bring in all of the tree marker data. This is every tree, by tag, in a plot
# with a spatial ID tag and azimuth
tree.mapping.data <- read.csv("C:/github/NEON_veg_structure//data/vst_mappingTagging2015_goughSites.csv")
tree.mapping.data.16 <- read.csv("./data/vst_mappingTagging2016_goughSites.csv")

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

# make unique ID for trees
df.tree$treeID <- paste(df.tree$plotID, df.tree$tagID, sep = "_")

tree.info.15 <- read.csv("C:/github/NEON_veg_structure//data/vst_apparentIndiv2015_goughSites.csv")
tree.info.16 <- read.csv("C:/github/NEON_veg_structure/data/vst_apparentIndividuals2016_goughSites.csv")

# tree.info.16 has leading goddamn zeroes
tree.info.16$tagid <- sub("^[0]+", "", tree.info.16$tagid)
tree.info.16$tagid <- as.factor(tree.info.16$tagid)

tree.15 <- tree.info.15
tree.16 <- tree.info.16
# tree.15 <- subset(tree.info.15, tree.info.15$growthForm == "sbt" | tree.info.15$growthForm == "mbt" | tree.info.15$growthForm == "smt")
# 
# tree.16 <- subset(tree.info.16, tree.info.16$growthform == "sbt" | tree.info.16$growthform == "mbt" | tree.info.16$growthform == "smt")
# some tree tags are repated so we need to make a unique id

tree.15$treeID <- paste(tree.15$plotID, tree.15$tagID, sep = "_")
tree.16$treeID <- paste(tree.16$plotid, tree.16$tagid, sep = "_")

harv39 <- subset(tree.15, tree.15$plotID == "HARV_039")


#### selecting 2015
tree.15 %>% select(siteID, plotID, subplotID, nestedSubplotID, tagID, treeID, stemDiameter, measurementHeight, stemStatus, growthForm) %>% 
  filter(measurementHeight >= 100) -> sort.15

# colnames(sort.15)[colnames(sort.15) == "siteID.x"] <- "siteID"
# colnames(sort.15)[colnames(sort.15) == "plotID.x"] <- "plotID"
# colnames(sort.15)[colnames(sort.15) == "subplotID.x"] <- "subplotID"
# colnames(sort.15)[colnames(sort.15) == "tagID.x"] <- "tagID"
colnames(sort.15)[colnames(sort.15) == "measurementHeight"] <- "measurementHeight.15"
colnames(sort.15)[colnames(sort.15) == "growthForm.x"] <- "growthForm"
colnames(sort.15)[colnames(sort.15) == "stemDiameter"] <- "dbh.15"
colnames(sort.15)[colnames(sort.15) == "stemStatus"] <- "stemStatus.15"

tree.16 %>% select(siteid, plotid, subplotid, nestedsubplotid, tagid, treeID, stemdiameter, measurementheight, plantstatus, growthform) %>%
  filter(measurementheight >= 100) -> sort.16

colnames(sort.16)[colnames(sort.16) == "siteid"] <- "siteID"
colnames(sort.16)[colnames(sort.16) == "plotid"] <- "plotID"
colnames(sort.16)[colnames(sort.16) == "subplotid"] <- "subplotID"
colnames(sort.16)[colnames(sort.16) == "tagid"] <- "tagID"
colnames(sort.16)[colnames(sort.16) == "measurementheight"] <- "measurementHeight.16"
colnames(sort.16)[colnames(sort.16) == "growthform"] <- "growthForm"
colnames(sort.16)[colnames(sort.16) == "stemdiameter"] <- "dbh.16"
colnames(sort.16)[colnames(sort.16) == "plantstatus"] <- "stemStatus.16"

#check GRSM
grsm.15 <- subset(sort.15, sort.15$siteID == "GRSM")
grsm.16 <- subset(sort.16, sort.16$siteID == "GRSM")
#write.csv(grsm.15, "grsm2015.csv")
#write.csv(grsm.16, "grsm2016.csv")
grsm <- subset(combo.meal, combo.meal$siteID == "GRSM")
grsm <- subset(super.size, super.size$siteID == "GRSM")
#### merging all them dirty bois

table(match(sort.15$treeID, sort.16$treeID))
combo.meal <- merge(x = sort.15,y =  sort.16[, c("treeID", "dbh.16")], by = "treeID", all.x = TRUE)
combo.meal$flag <- ifelse(combo.meal$dbh.15 > combo.meal$dbh.16, TRUE, FALSE)

super.size <- merge(combo.meal, df.tree[, c("treeID", "taxonID", "tree.easting", "tree.northing")], by = "treeID", all.x = TRUE)

### Bring in the jenkins with the data frame jenkins_plus

colnames(jenkins_plus)[colnames(jenkins_plus) == "Symbol"] <- "taxonID"

big.time <- merge(super.size, jenkins_plus[, c("jenkins_model", "taxonID", "beta_one", "beta_two")], by = "taxonID", all.x = TRUE)


#check
write.csv(big.time, "neon_match_allometry_data_frame_check.csv")

# check
### make biomass
# equation  is biomass = e^(beta_one + beta_two* ln(dbh))
# GIVES BIOMASS IN KG
#given the NEON plot design, this is KG per 800 m^2
big.time$bm.15 <- exp(big.time$beta_one + (big.time$beta_two * log(big.time$dbh.15)))
big.time$bm.16 <- exp(big.time$beta_one + (big.time$beta_two * log(big.time$dbh.16)))

big.time$biomass.change <- ifelse(big.time$bm.15 < big.time$bm.16, big.time$bm.16 - big.time$bm.15, 0)

#write.csv(big.time, "neon_match_allometry_data_frame_check.csv")
#### take it to the plot scale
big.time %>% 
  group_by(plotID, siteID) %>%
  summarize(npp = sum(biomass.change, na.rm = TRUE)) -> plot.biomass
big.time %>%
  group_by(plotID, siteID) %>%
  summarize(no.subplot = n_distinct(subplotID)) -> subplot.count

#count the no of species
big.time %>%
  group_by(plotID, siteID) %>%
  summarize(no.species = n_distinct(taxonID)) -> plot.species

# no. of species in data frame
plot.species <- data.frame(plot.species)

# just a check, all is good.
subplot.count <- data.frame(subplot.count)

# let's do this thing with npp
plot.biomass <- data.frame(plot.biomass)

# converts to megagrams per hectare
plot.biomass$biomass.16 <- (plot.biomass$biomass.16 / 800) * 10   #1) /800 to get to sq. m, then 2) * 0.001 to get to Mg, then by 0.0001to per hectare

### bring in CSC
neon.csc <- read.csv("./data/laserquest_pcl_master_transects_2018_clean.csv")
#isolate rugosity
neon.csc %>%
  group_by(plotID, siteID) %>%
  summarize(rugosity = mean(rugosity)) -> plot.rc

plot.rc <- data.frame(plot.rc)

#adds that 0
require(stringi)
stri_sub(plot.rc$plotID, 6, 5) <- 0

plot.csc <- merge(plot.biomass, plot.rc, by = "plotID")
plot.csc <- merge(plot.csc, plot.species, by = "plotID")

colnames(plot.csc)[colnames(plot.csc) == "siteID.x"] <- "siteID"

plot.csc <- subset(plot.csc, plot.csc$npp > 0)
plot(plot.csc$rugosity, plot.csc$npp)

require(ggplot2)

ggplot(plot.csc, aes(x = rugosity, y = (npp / 800) * 10, color = siteID))+
  geom_point(size = 5)+
  ylab("NPP (Mg Ha-1 yr-1)")+
  xlab("Rugosity")+
  ylim(c(0, 60))

ggplot(plot.csc, aes(x = rugosity, y = (npp / 800) * 10, color = siteID))+
  geom_point(size = 5)+
  ylab("NPP (Mg Ha-1 yr-1)")+
  xlab("Rugosity")+
  ylim(c(0, 10))

ggplot(plot.csc, aes(x = rugosity, y = no.species, color = siteID))+
  geom_point(size = 5)+
  ylab("Species Richness")+
  xlab("Rugosity")


##### Let's do it with a larger data set
tree.mapping.data.16 %>%
  group_by(plotid, siteid) %>%
  summarize(no.species = n_distinct(taxonid)) -> plot.species.16

plot.species.16 <- data.frame(plot.species.16)
colnames(plot.species.16)[colnames(plot.species.16) == "siteid"] <- "siteID"
colnames(plot.species.16)[colnames(plot.species.16) == "plotid"] <- "plotID"

plot.full <- merge(plot.species.16, plot.rc)

ggplot(plot.full, aes(x = rugosity, y = no.species, color = siteID))+
  geom_point(size = 5)+
  ylab("Species Richness")+
  xlab("Rugosity")



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
#writing to disk
#write.csv(df, file = "NEON_veg_mapping_data_merged.CSV")
df <- merge(tree.info.15, df.tree, by = c("plotID", "tagID", "growthForm"), keep_all = FALSE)
df.1 <- merge(tree.info, df.tree)
df$growthForm <- tolower(df$growthForm)

#look at only trees
df <- subset(df, df$growthForm == "sbt" | df$growthForm == "mbt" | df$growthForm == "smt") 
#organize the names
#df$scientificName <- tolower(df$scientificName)
#df$growthForm <- tolower(df$growthForm)
df$taxonID <- toupper(df$taxonID)
df$taxonID <- trimws(df$taxonID, which = "both")

#based on work below, I am going to replace the following
df$taxonID[df$taxonID == "QUERCUS"] <- "QUERC"

#

taxon <- data.frame(unique(df$taxonID))
taxon <- taxon[order(taxon$unique.df.taxonID.),]
data.frame(taxon)
write.csv(taxon, "neon_taxon.csv")

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