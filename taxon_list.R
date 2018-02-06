d2016 <- read.csv("./data/vst_mappingTagging2016_goughSites.CSV")

taxon_list <- unique(d2016$taxonid)

write.csv(taxon_list, "taxon_list.CSV")