# Community ecology work
require(plyr)
require(dplyr)
require(tidyr)
require(vegan)
require(ggplot2)

df <- read.csv("./data/vst_mappingTagging2016_goughSites.csv")
mlbs <- read.csv("./data/mapping_fulcrum_MLBS.csv")
#jerc <- 

head(df)

##### Adding taxon data
tax.jenk <- read.csv("neon_taxon_jenkins.csv")

tax.jenk$taxonid <- tax.jenk$Symbol

df <- merge(df, tax.jenk[, c("taxonid", "Genus", "Family")], by="taxonid", all.x = TRUE)

# merge in with mlbs data too
mlbs.combo <- merge(mlbs, tax.jenk[, c("taxonid", "Genus", "Family")], by="taxonid", all.x = TRUE)

mlbs.combo %>%
  select(siteid, plotid, taxonid, Genus, Family) -> mlbs.species
# sort to just a df of the plotID and taxonid


df %>%
  select(siteid, plotid, taxonid, Genus, Family) -> df.species

df.species <- rbind(df.species, mlbs.species)

# check the dim  should be ~29993
dim(df.species)

# species by plot
df.species %>%
  group_by(plotid, taxonid) %>%
  count(taxonid) -> df.count

# Genus by plot
df.species %>%
  group_by(plotid, siteid) %>%
  count(genus.rich = n_distinct(Genus)) -> df.genus.richness

# Genus by plot
df.species %>%
  group_by(plotid, siteid) %>%
  count(family.rich = n_distinct(Family)) -> df.family.richness

# richness
df.species %>%
  group_by(plotid, siteid) %>%
  summarize(richness = n_distinct(taxonid)) -> plot.diversity


# OK now we need make wide for vegan
df.count %>%
  spread(taxonid, n) -> df.sp.wide
df.sp.wide <- data.frame(df.sp.wide)

#check the dimensions
dim(df.sp.wide)

df.sp.wide[is.na(df.sp.wide)] <- 0

df2 <- df.sp.wide[-1,]

#vegan time
 plot.diversity$simpsons <- diversity(df.sp.wide[,2:246], index = "simpson")
 plot.diversity$shannon <- diversity(df.sp.wide[,2:246], index = "shannon")

 #plot.diversity$simpsons <- diversity(df2, index = "simpson")
 #plot.diversity$shannon <- diversity(df2, index = "shannon")
 
 # recombine
 plot.diversity <- data.frame(plot.diversity)
 plot.diversity <- merge(plot.diversity, df.genus.richness)
 plot.diversity <- merge(plot.diversity, df.family.richness)
 
 
 ### bring in CSC
 neon.csc <- read.csv("./data/laserquest_pcl_master_transects_2018_clean.csv")
 neon.csc2 <- neon.csc
   colnames(neon.csc2)[colnames(neon.csc2) == "siteID"] <- "siteid"
   colnames(neon.csc2)[colnames(neon.csc2) == "plotID"] <- "plotid"
 #isolate rugosity
 neon.csc2 %>%
   group_by(plotid, siteid) %>%
   summarize(rugosity = mean(rugosity), moch = mean(mean.max.ht)) -> plot.rc2

 
 
 plot.rc2 <- data.frame(plot.rc2)
 
 #adds that 0
 require(stringi)
 plot.rc2 %>%
   filter(siteid != "BART") -> split.rc
 plot.rc2 %>%
   filter(siteid == "BART") -> just.bart
 #fix zeros
   stri_sub(split.rc$plotid, 6, 5) <- 0
   split.rc <- data.frame(split.rc)
   just.bart <- data.frame(just.bart)
   
  plot.rc3 <- rbind(split.rc, just.bart) 

 
 plot.diversity <- merge(plot.diversity, plot.rc3, by = "plotid")
 colnames(plot.diversity)[colnames(plot.diversity) == "siteid.x"] <- "siteid"
 
 # normalized CSC
 
 plot.diversity$normal <- plot.diversity$rugosity / plot.diversity$moch
 
 ggplot(plot.diversity, aes(y = normal, x = simpsons))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
     panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Simpson's Index")+
   ylab("Canopy Rugosity/MOCH")
 
 
 ggplot(plot.diversity, aes(y = rugosity, x = shannon))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Shannon-Weiner Index")+
   ylab("Canopy Rugosity")
 
 ggplot(plot.diversity, aes(y = normal, x = genus.rich))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Genus Richness")+
   ylab("Canopy Rugosity/moch")
 
 ggplot(plot.diversity, aes(y = rugosity, x = richness))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Species Richness")+
   ylab("Canopy Rugosity")
 
 ggplot(plot.diversity, aes(y = rugosity, x = family.rich))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Family Richness")+
   ylab("Canopy Rugosity")
 
 ggplot(plot.diversity, aes(y = rugosity, x = n))+
   geom_point(aes(fill = siteid), color = "black", pch = 21, size = 4)+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("No. of Individuals")+
   ylab("Canopy Rugosity")
 
 
 
 ### do site level
 plot.diversity %>%
   group_by(siteid) %>%
   summarize_each(funs(mean, sd)) -> site.diversity
 
 site.diversity <- data.frame(site.diversity)
 
 ggplot(site.diversity, aes(x = simpsons_mean, y = rugosity_mean, color = siteid))+
   geom_point(size = 4)+
   geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                      ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
   geom_errorbarh( aes(xmin = simpsons_mean - (simpsons_sd / (sqrt(7))), 
                      xmax = simpsons_mean + (simpsons_sd / (sqrt(7)))))+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Simpson's Index")+
   ylab("Rugosity")
 
 ggplot(site.diversity, aes(x = shannon_mean, y = rugosity_mean, color = siteid))+
   geom_point(size = 4)+
   geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                      ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
   geom_errorbarh( aes(xmin = shannon_mean - (shannon_sd / (sqrt(7))), 
                       xmax = shannon_mean + (shannon_sd / (sqrt(7)))))+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Shannon-Weiner Index")+
   ylab("Rugosity")
 
 ggplot(site.diversity, aes(x = genus.rich_mean, y = rugosity_mean, color = siteid))+
   geom_point(size = 4)+
   geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                      ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
   geom_errorbarh( aes(xmin = genus.rich_mean - (genus.rich_sd / (sqrt(7))), 
                       xmax = genus.rich_mean + (genus.rich_sd / (sqrt(7)))))+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Genus Richness")+
   ylab("Rugosity")
 
 ggplot(site.diversity, aes(x = richness_mean, y = rugosity_mean, color = siteid))+
   geom_point(size = 4)+
   geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                      ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
   geom_errorbarh( aes(xmin = richness_mean - (richness_sd / (sqrt(7))), 
                       xmax = richness_mean + (richness_sd / (sqrt(7)))))+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("Species Richness")+
   ylab("Rugosity")
 
 
 ggplot(site.diversity, aes(x = n_mean, y = rugosity_mean, color = siteid))+
   geom_point(size = 4)+
   geom_errorbar( aes(ymin = rugosity_mean - (rugosity_sd / (sqrt(7))), 
                      ymax = rugosity_mean + (rugosity_sd / (sqrt(7)))))+
   geom_errorbarh( aes(xmin = n_mean - (n_sd / (sqrt(7))), 
                       xmax = n_mean + (n_sd / (sqrt(7)))))+
   theme( panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
   theme(axis.title = element_text(face="bold", colour="black", size=20),
         axis.text  = element_text(color = "black", size = 16),
         axis.line.x=element_line(color = "black"),
         axis.line.y=element_line(color = "black"),
         legend.key = element_rect(fill = "transparent", color = "transparent"),
         legend.background = element_rect(fill = "transparent"),
         legend.text = element_text(colour="black", size=10, 
                                    face="bold"),
         legend.title = element_blank(),
         legend.position = "right")+
   xlab("No. of Individuals")+
   ylab("Rugosity")
 +
   geom_smooth(method = "lm", se = FALSE)
 
 ggplot(site.diversity, aes(x = genus.rich, y = rugosity, color = siteid))+
   geom_point(size = 4)+
   ylab("Genus Richness")+
   xlab("Rugosity")+
   stat_smooth(method = "lm", se = FALSE)
 
 #site level
 summary(lm(genus.rich_mean ~ rugosity_mean, data = site.diversity))
 
 # plot level
 summary(lm(rugosity ~ genus.rich, data = plot.diversity))
 summary(lm(rugosity ~ simpsons, data = plot.diversity))
 summary(lm(rugosity ~ shannon, data = plot.diversity))
 summary(lm(rugosity ~ richness, data = plot.diversity))
 summary(lm(rugosity ~ n, data = plot.diversity))
 
 summary(lm(rugosity_mean ~ genus.rich_mean, data = site.diversity))
 summary(lm(rugosity_mean ~ simpsons_mean, data = site.diversity))
 summary(lm(rugosity_mean ~ shannon_mean, data = site.diversity))
 summary(lm(rugosity_mean ~ richness_mean, data = site.diversity))
 summary(lm(rugosity_mean ~ n_mean, data = site.diversity))
 
 # We correlated measures of ecosystem physical structure
 # with conventional indices of plant species and genera diversity, including richness,
 # Shannon’s Index, and Simpson’s Index derived from NEON plant inventories including
 # Jeff, these look great! A couple of preliminary thoughts:
 #   
 #   I’d flip the axes on the richness-rugosity plot. Explaining rugosity as a function of richness is more defensible (at least until we get the RoFL EAGER!)
 # You could do a broken y-axis on the NPP-rug plot (not sure I’m actually advocating for this though, since it can be very misleading for anyone not paying close attention). Maybe plot on a log scale or plot the normalized (or standardized?) values?
 # Are you using the Jenkins et al 2003 allometries? You mention that some of the trees are pretty big, so we should be careful about applying those allometries to anything that is outside the parameterized range. Big trees will be disproportionately sensitive to this. Table 4 gives the max dbh for each eqn (56cm for mixed hardwoods).
 # How are you handling trees that were not re-censused in 2016? Omitting from the 2015 NPP value?
 # Are these figures for your next paper? :)
 
 
 ##### Adding taxon data
 tax.jenk <- read.csv("neon_taxon_jenkins.csv")
 
 
 