
require(plyr)
require(dplyr)
require(tidyverse)

blk <- read.table("e:/test_tls.pts", skip = 1, 
            col.names = c("x", "y", "z", "intensity", "r", "g", "b"))

plot3d(blk$x, blk$y, blk$z, col = blk$intensity)


blk %>% select(x,y,z,intensity) -> blk.lite

write.csv(blk.lite, "blk_lite.csv")