# NEON data cleaning scripts

# These data are for sites:   BART HARV SERC SCBI OSBS TREE UNDE ORNL GRSM DELA LENO TALL
# not sure the years yet, but were listed as 2016, some 2017 measures are in there.
neon.one <- read.csv("./data/vst_apparentIndividuals2016_goughSites.csv")

###
neon.one %>%
  select(date, domainid, siteid, plotid, subplotid,	plotType,	subtype, tagid, growthform,	stemdiameter,	
         basalstemdiametermeasurementheight, plantstatus,	measurementheight,	remarks) -> neon.one.clean			

#remove leading zeros
neon.one.clean$tagid <- as.factor(gsub("^0+", "\\1", neon.one.clean$tagid , perl = TRUE))

# add the year
neon.one.clean$year <- as.factor(substr(neon.one.clean$date, 0, 4))

# filter to only trees
neon.one.clean %>%
  filter(growthform %in% c("sbt", "mbt", "smt")) %>%
  select(siteid, plotid, tagid, year, stemdiameter) -> df.1
  
colnames(df.1)[colnames(df.1) == "siteid"] <- "siteID"
colnames(df.1)[colnames(df.1) == "plotid"] <- "plotID"
colnames(df.1)[colnames(df.1) == "tagid"] <- "tagID"
colnames(df.1)[colnames(df.1) == "stemdiameter"] <- "stemDiameter"


##############################################################
# These data are for sites:   HARV SCBI SERC OSBS GRSM ORNL DELA LENO TALL
# not sure the years yet, but were listed as 2015
# some of the names are different too and need to be changed
#  MISSING domainid
neon.two <- read.csv("./data/vst_apparentIndiv2015_goughSites.csv")

neon.two %>%
  select(date, siteID, plotID, subplotID,	plotType,	subtype, tagID, growthForm,	stemDiameter,
         stemStatus,	measurementHeight,	remarks) -> neon.two.clean	

# remove leading zeros
neon.two.clean$tagID <- as.factor(gsub("^0+", "\\1", neon.two.clean$tagID , perl = TRUE))

# add the year
neon.two.clean$year <- as.factor(substr(neon.two.clean$date, 0, 4))

# filter to only trees
neon.two.clean %>%
  filter(growthForm %in% c("sbt", "mbt", "smt")) %>%
  select(siteID, plotID, tagID, year, stemDiameter) -> df.2


##############################################################
# MLBS 15
neon.mlbs15 <- read.csv("./data/apparentindividual_L0_2015_MLBS.csv")

neon.mlbs15 %>%
  select(endDate, siteID, plotID, subplotID,	tagID, growthForm,	stemDiameter,	
         plantStatus,	measurementHeight,	remarks) -> neon.mlbs15.clean	

#order
neon.mlbs15.clean <- neon.mlbs15.clean[order(neon.mlbs15.clean$tagID),]

# clean the leading zeros
neon.mlbs15.clean$tagID <- as.factor(gsub("^0+", "\\1", neon.mlbs15.clean$tagID , perl = TRUE))
neon.mlbs15.clean$year <- as.factor(substr(neon.mlbs15.clean$endDate, 0, 4))

neon.mlbs15.clean %>%
  filter(growthForm %in% c("small tree", " single bole tree", "multi-bole tree")) %>%
  select(siteID, plotID, tagID, year, stemDiameter) -> df.3

##############################################################
# MLBS
neon.mlbs <- read.csv("./data/apparentindividual_fulcrum_2017_MLBS.csv")

neon.mlbs %>%
  select(enddate, siteid, plotid, subplotid, tagid,	stemdiameter,	
         measurementheight, plantstatus,	remarks) -> neon.mlbs.clean

#changing names
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "enddate"] <- "endDate"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "siteid"] <- "siteID"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "plotid"] <- "plotID"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "subplotid"] <- "subplotID"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "tagid"] <- "tagID"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "stemdiameter"] <- "stemDiameter"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "measurementheight"] <- "measurementHeight"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "plantstatus"] <- "plantStatus"
colnames(neon.mlbs.clean)[colnames(neon.mlbs.clean) == "stemdiameter"] <- "stemDiameter"

#order the dataset
neon.mlbs.clean <- neon.mlbs.clean[order(neon.mlbs.clean$tagID),]

#this cleans up leadings 0's
neon.mlbs.clean$tagID <- as.factor(gsub("^0+", "\\1", neon.mlbs.clean$tagID , perl = TRUE))

d <- merge(neon.mlbs.clean, neon.mlbs15.clean[, c("tagID", "growthForm")], by = "tagID")

d$year <- as.factor(substr(d$endDate, 0, 4))

d %>%
  filter(growthForm %in% c("small tree", " single bole tree", "multi-bole tree")) %>%
  select(siteID, plotID, tagID, year, stemDiameter) -> df.4

##############################################################
# BART 2015
neon.bart <- read.csv("./data/apparentindividual_L0_2015_BART.csv")

neon.bart %>%
  select(date, siteID, plotID, subplotID,	tagID,	stemDiameter,	
         stemStatus,	measurementHeight,	remarks) -> neon.bart15.clean


# remove leading zeros
neon.bart15.clean$tagID <- as.factor(gsub("^0+", "\\1", neon.bart15.clean$tagID , perl = TRUE))

# add the year
neon.bart15.clean$year <- as.factor(substr(neon.bart15.clean$date, 0, 4))

### making just the BART stuff from neon.one
neon.one.clean %>%
  filter(siteid == "BART") -> sub.bart
 
colnames(sub.bart)[colnames(sub.bart) == "tagid"] <- "tagID"
colnames(sub.bart)[colnames(sub.bart) == "growthform"] <- "growthForm"
#now adding correct growthform to 
e <- merge(neon.bart15.clean, sub.bart[, c("tagID", "growthForm")], by = "tagID")

e %>%
  filter(growthForm %in% c("sbt", "mbt", "smt")) %>%
  select(siteID, plotID, tagID, year, stemDiameter) -> df.5




##############################################################
# TREE
neon.tree15 <- read.csv("./data/apparentindividual_L0_2015_TREE.csv")

neon.tree15 %>%
  select(endDate, siteID, plotID, subplotID,	tagID, growthForm,	stemDiameter,	
         plantStatus,	measurementHeight, remarks) -> neon.tree15.clean	


#order the dataset
neon.tree15.clean <- neon.tree15.clean[order(neon.tree15.clean$tagID),]

#this cleans up leadings 0's
neon.tree15.clean$tagID <- as.factor(gsub("^0+", "\\1", neon.tree15.clean$tagID , perl = TRUE))

neon.tree15.clean$year <- as.factor(substr(neon.tree15.clean$endDate, 0, 4))

neon.tree15.clean %>%  filter(growthForm %in% c("small tree", " single bole tree", "multi-bole tree")) %>%
  select(siteID, plotID, tagID, year, stemDiameter) -> df.6


###################################
###################################
###################################
###################################
###################################

# now we bind

neon.dbh <- rbind(df.1, df.2, df.3, df.4, df.5, df.6)

colnames(neon.dbh)[colnames(neon.dbh) == "stemDiameter"] <- "dbh"

# fix errant plots
neon.dbh$plotID <- substr(neon.dbh$plotID, start = 1, stop = 8)

write.csv(neon.dbh, "./data/neon_dbh.csv")


#### I don't know what this one is
#### 
# # BART but n
# neon.bart15 <- read.csv("./data/apparentindividual_L0_BART_alphaTags.csv")
# 
# neon.bart15 %>%
#   select(endDate, sitID, plotID, subplotID,	 tagID, growthForm,	stemDiameter,	
#          plantStatus,	measurementHeight, remarks) -> neon.bart15.clean	
# 
# colnames(neon.bart15.clean)[colnames(neon.bart15.clean) == "sitID"] <- "siteID"
########

set.x <- rbind(neon.mlbs15.clean, neon.tree15.clean, neon.mlbs.clean)






### EXTRAS
neon.jerc15 <- read.csv("./data/apparentindividual_fulcrum_2016_JERC.csv")
neon.jerc16 <- read.csv("./data/apparentindividual_L0_2015_JERC.csv")

