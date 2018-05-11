# working with FERNOW watershed data
require(plyr)
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(maps)
require(ggplot2)
require(cluster)
require(wesanderson)

fern.pcl <- read.csv("./data/fernow_pcl_transects_2016.CSV")

# make a plotID column

fern.pcl$plotID <-  substr(fern.pcl$plot, 0, 10)

#drop watershed 15
fern <- subset(fern.pcl, fern.pcl$watershedID != "W15")
#fern <- fern.pcl
#### classes
# 2 = W3
#  4 = W7
#  5 = W4
#### CLASSIFCATION

require(randomForest)

fern$watershedID <- factor(fern$watershedID)

set.seed(400)

fern.fit <- randomForest(as.factor(watershedID) ~  porosity + rugosity + rumple,
                      data = fern,
                      importance = TRUE,
                      ntree = 2000)
# View the forest results.
print(fern.fit) 

# Importance of each predictor.
print(importance(fern.fit)) 

varImpPlot(fern.fit)

# mean.height + mean.max.ht + 
fern %>%
  group_by(watershedID) %>%
  summarize_all(funs(mean)) -> fern.means

fern.means <-data.frame(fern.means)
##################
# covariance matrix

drops <- c("X","plot", "siteID", "watershedID", "transect.length", "deep.gaps", "deep.gap.fraction", "plotID")
fern2 <- fern[ , !(names(fern) %in% drops)]
m <- cor(fern2, method = "pearson")

require(corrplot)

corrplot(m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#####
gg <- ggplot(fern, aes(x = watershedID, y = porosity, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Porosity")+
  xlab("")

ggsave("fernow_porosity.png", gg, width = 6, height = 4, units = "in", bg = "transparent")


hh <- ggplot(fern, aes(x = watershedID, y = rugosity, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Rugosity")+
  xlab("")

ggsave("fernow_rug.png", hh, width = 6, height = 4, units = "in", bg = "transparent")

ii <- ggplot(fern, aes(x = watershedID, y = rumple, fill = watershedID))+ 
  geom_boxplot(color = "white")+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Rumple")+
  xlab("")

ggsave("fernow_rump.png", ii, width = 6, height = 4, units = "in", bg = "transparent")

