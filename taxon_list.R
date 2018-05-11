d2016 <- read.csv("./data/vst_mappingTagging2016_goughSites.CSV")

taxon_list <- unique(d2016$taxonid)

taxon_list <- data.frame(taxon_list)

#write.csv(taxon_list, "taxon_list.CSV")

colnames(taxon_list)[1] <- "Symbol"
#####
usda <- read.csv("usda_checklist.csv")

df2 <-  merge(taxon_list, usda)

#### This one is great in that it shows the differences.
setdiff(taxon_list, df2)

#write.csv(df2, "neon_taxons_expanded.csv ")


#### To make allometric

jenkins_model <- c("aspen/alder/cottonwood/willow", "soft maple/birch", "mixed hardwood", "hard maple/oak/hickory/beech", "cedar/larch", "doug fir", "fir/hemlock", "pine", "spruce", "juniper/oak/mesquite")

model_name <- c("hw1", "hw2", "hw3", "hw4", "sw1", "sw2", "sw3", "sw4", "sw5", "wl")

beta_one <- c(-2.20294, -1.9123, -2.4800, -2.0127, -2.0336, -2.2304, -2.5384, -2.5356, -2.0773, -0.7152)

beta_two <- c(2.3867, 2.3651, 2.4835, 2.4342, 2.2592, 2.4435, 2.4814, 2.4349, 2.3323, 1.7029)

jenkins <- data.frame(jenkins_model, model_name, beta_one, beta_two)

tax.jenk <- read.csv("neon_taxon_jenkins.csv")

jenkins_plus <- merge(tax.jenk, jenkins)

# Table 4. Parameters and equations* for estimating total aboveground biomass for all hardwood and softwood
# species in the United States.
# Parameters
# Species group β0 β1
# Data
# points†
# Max††dbh
# cm
# RMSE§
# log units R2
# Hardwood Aspen/alder/cottonwood/willow –2.2094 2.3867 230 70 0.507441 0.953
# Soft maple/birch –1.9123 2.3651 316 66 0.491685 0.958
# Mixed hardwood –2.4800 2.4835 289 56 0.360458 0.980
# Hard maple/oak/hickory/beech –2.0127 2.4342 485 73 0.236483 0.988
# Softwood Cedar/larch –2.0336 2.2592 196 250 0.294574 0.981
# Douglas-fir –2.2304 2.4435 165 210 0.218712 0.992
# True fir/hemlock –2.5384 2.4814 395 230 0.182329 0.992
# Pine –2.5356 2.4349 331 180 0.253781 0.987
# Spruce –2.0773 2.3323 212 250 0.250424 0.988
# Woodland|| Juniper/oak/mesquite –0.7152 1.7029 61 78 0.384331 0.938