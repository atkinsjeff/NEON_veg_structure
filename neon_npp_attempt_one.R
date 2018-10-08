# NEON NPP

require(plyr)
require(dplyr)
require(tidyverse)

x16 <- read.csv("./data/vst_apparentIndividuals2016_goughSites.csv")

x16 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> n.16

# add year
n.16$year <- 2016


n.17pt1 <- read.csv("./data/apparentindividual_fulcrum_2017_1st7sites.csv")
n.17pt2 <- read.csv("./data/apparentindividual_fulcrum_2017_2nd7sites.csv")


n.17pt1 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> x17.1

n.17pt2 %>%
  filter(measurementheight == 130) %>%
  select(domainid, siteid, plotid, tagid, individualid, plantstatus, stemdiameter) -> x17.2



n.17 <- rbind(x17.1, x17.2)

# add a year
n.17$year <- 2017

df <- rbind(n.16, n.17)
#### ids
tree.id <- read.csv("./data/vst_mappingTagging2016_goughSites.csv")

df$taxonid <- tree.id$taxonid[match(df$individualid, tree.id$individualid)]

# make wide for npp

#check for dups
df <- df[!duplicated(df[, c(5,8)]), ]


npp <- spread(df, year, stemdiameter)

#bring in jenkins model

jenkins_model <- c("aspen/alder/cottonwood/willow", "soft maple/birch", "mixed hardwood", "hard maple/oak/hickory/beech", "cedar/larch", "doug fir", "fir/hemlock", "pine", "spruce", "juniper/oak/mesquite")

model_name <- c("hw1", "hw2", "hw3", "hw4", "sw1", "sw2", "sw3", "sw4", "sw5", "wl")

beta_one <- c(-2.20294, -1.9123, -2.4800, -2.0127, -2.0336, -2.2304, -2.5384, -2.5356, -2.0773, -0.7152)

beta_two <- c(2.3867, 2.3651, 2.4835, 2.4342, 2.2592, 2.4435, 2.4814, 2.4349, 2.3323, 1.7029)

jenkins <- data.frame(jenkins_model, model_name, beta_one, beta_two)

tax.jenk <- read.csv("neon_taxon_jenkins.csv")

jenkins_plus <- merge(tax.jenk, jenkins)

df2 <- merge(npp, jenkins_plus, by = "taxonid", all = TRUE)


df2 %>%
  filter(plantstatus == 1 | plantstatus == 4 | plantstatus == 5 | plantstatus == 7 | plantstatus == 9 ) -> df3
# rename columns
colnames(df3)[8] <- "dbh16"
colnames(df3)[9] <- "dbh17"

 
#biomass

df3$b2016 <- exp(df3$beta_one + (df3$beta_two * log(df3$dbh16)))
df3$b2017 <- exp(df3$beta_one + (df3$beta_two * log(df3$dbh17)))
df3$growth <- df3$b2017 - df3$b2016

# remove negatives

df3$growth[df3$growth < 0] <- 0
# get plot sum
df3 %>% 
  group_by(plotid) %>%
  dplyr::summarise(npp = sum(growth, na.rm = TRUE)) -> plot.npp

plot.npp <- data.frame(plot.npp)

head(plot.npp)

# change to per hectare
plot.npp$npp <- plot.npp$npp * 12.5

# change to Mg per hectare
plot.npp$npp <- plot.npp$npp * 0.001



### bring in CSC
neon.csc <- read.csv("./data/laserquest_pcl_master_transects_2018_clean.csv")
neon.csc2 <- neon.csc
colnames(neon.csc2)[colnames(neon.csc2) == "siteID"] <- "siteid"
colnames(neon.csc2)[colnames(neon.csc2) == "plotID"] <- "plotid"


#isolate rugosity
neon.csc2 %>%
  group_by(plotid, siteid) %>%
  summarize(rugosity = mean(rugosity), moch = mean(mean.max.ht)) -> plot.rc2

neon.csc2 %>%
  group_by(plotid, siteid) %>%
  summarize_all(mean)-> plot.csc

plot.csc <- data.frame(plot.csc)

require(stringi)

#fix zeros
stri_sub(plot.csc$plotid, 6, 5) <- 0
stri_sub(plot.csc2$plotid, 6, 5) <- 0


#combine
big.boi <- merge(plot.npp, plot.csc)

big.boi %>%
  filter(!npp == 0 & !npp > 20) -> big.boi

big.boi %>%
  filter(!rugosity >50) -> big.boi

ggplot(big.boi, aes(x = rugosity, y = npp))+
  geom_point(aes(color=siteid), size = 4)+
  geom_point(shape = 1, size = 4, color = "black")+
  ylab("Wood NPP (Mg per Ha)")+
  xlab("Canopy Rugosity (m)")+
  theme_classic()+
  stat_smooth(method = "lm")

lm.rc <- lm(npp ~ rugosity, data = big.boi)
lm.rump <- lm(npp ~ rumple, data = big.boi)
lm.mst <- lm(npp ~ mean.std, data = big.boi)

#### correlation matrix
x <- big.boi[c(2, 6:24, 26:34)]

m <- cor(x)
corrplot(m)
#######


big.boi %>%
  group_by(siteid) %>%
  summarise_all(funs(mean, sd)) -> site.means

 site.means <- data.frame(site.means)
 
 head(site.means)
 
 write.csv(site.means, "neon_csc_npp_means.csv")

#get a count
 big.boi %>%
   group_by(siteid) %>%
   summarize(no = n()) -> no.site
 
 npp.label <- expression(paste("NPP"[Wood]~"( Mg Ha"^-1~")"))
 
 # get a SE for rugosity
 site.means$rugosity_se <- site.means$rugosity_sd / sqrt(site.means$no)
 site.means$npp_se <- site.means$npp_sd / sqrt(site.means$no)
 site.means$vai_se <- site.means$mean.vai_sd / sqrt(site.means$no)

 
 site.means <- cbind(site.means, no.site) 
 # colors
 # The palette with black:
 cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
 # To use for fills, add
 scale_fill_manual(values=cbPalette)
 
 # To use for line and point colors, add
 scale_colour_manual(values=cbPalette)

 ggplot(site.means, aes(x = rugosity_mean, y = npp_mean))+
   geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
   geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
   geom_point(aes(color=siteid), size = 5)+
   geom_point(shape = 1, size = 5, color = "black")+
   scale_colour_manual(values=cbPalette)+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12))+
   theme(legend.title=element_blank())+
   ylab(npp.label)+
   xlab("Canopy Rugosity (m)")+
   stat_smooth(method = "lm", se = FALSE)
 
 
 ggplot(site.means, aes(x = mean.vai_mean, y = npp_mean))+
   geom_errorbarh(aes(xmin = mean.vai_mean - vai_se, xmax = mean.vai_mean + vai_se))+
   geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
   geom_point(aes(color=siteid), size = 5)+
   geom_point(shape = 1, size = 5, color = "black")+
   scale_colour_manual(values=cbPalette)+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12))+
   theme(legend.title=element_blank())+
   ylab(npp.label)+
   xlab("VAI")
 
 ggsave("npp_rugosity.png", units="in", width=5, height= 5, dpi=300)
 png('npp_rugosity.png', units="in", width=5, height=5, res=300)
 #insert ggplot code
 dev.off()
 
 lm.rc.site <- lm(npp_mean ~ rugosity_mean, data = site.means)
 lm.vai.site <- lm(npp_mean ~ mean.vai_mean, data = site.means)
 
 summary(lm.rc.site)
 m <- 
 nls.rc.site <- nls(npp_mean ~ rugosity_mean, data = site.means)
 
 
 ####
 div.lite <- site.diversity[,c(1, 3:12)]
 
 div.lite %>%
   filter(!siteid == "MLBS") -> div.lite2
 site.all <- cbind(site.means, div.lite2)

 ###
 ggplot(site.all, aes(x = shannon.genus_mean, y = npp_mean))+
   # geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
   # geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
   geom_point(aes(color=siteid), size = 5)+
   geom_point(shape = 1, size = 5, color = "black")+
   scale_colour_manual(values=cbPalette)+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12))+
   theme(legend.title=element_blank())+
   ylab(npp.label)+
   xlab("Shannon Diversity Index")+
   stat_smooth(method = "lm", se = FALSE)
 
 lm.shannon <- lm(npp_mean ~ shannon.genus_mean, data = site.all)
 summary(lm.shannon)
 
 ggplot(site.all, aes(y = rugosity_mean, x = shannon.genus_mean))+
   # geom_errorbarh(aes(xmin = rugosity_mean - rugosity_se, xmax = rugosity_mean + rugosity_se))+
   # geom_errorbar(aes(ymin = npp_mean - npp_se, ymax = npp_mean + npp_se))+
   geom_point(aes(color=siteid), size = 5)+
   geom_point(shape = 1, size = 5, color = "black")+
   scale_colour_manual(values=cbPalette)+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12))+
   theme(legend.title=element_blank())+
   xlab("Shannon Diversity Index")+
   ylab("Canopy Rugosity (m)")+
   stat_smooth(method = "lm", se = FALSE)
 
 lm.shan.rc <- lm(rugosity_mean ~ shannon.genus_mean, data = site.all)
 summary(lm.shan.rc)
 
 write.csv(site.all, "neon_csc_npp_means.csv")
 
 