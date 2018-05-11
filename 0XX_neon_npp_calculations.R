require(tidyverse)
require(reshape2)
# First import data
neon.dbh <- read.csv("./data/neon_dbh.csv")
neon.dbh$year <- as.factor(neon.dbh$year)
df <- neon.dbh[,2:6]
# 
# df.dup <- df[duplicated(df),]
# write.csv(df.dup, "duplicate_to_rm.")


#neon.dbh$treeID <- paste(neon.dbh$plotID, neon.dbh$tagID, sep = "_")

# we are going to try and take the first 
df$treeID <- paste(df$plotID, df$tagID, sep = "_")

# df <- df[order(df$treeID, -abs(df$dbh) ), ]
# df <- df[!duplicated(df$treeID), ]

df$year <- paste0("dbh.", df$year)

df %>%
  select(treeID, year, dbh) %>%
  spread(year, dbh) -> dbh.wide
#this data set sucks
df %>%
  group_by_at(vars(-dbh)) %>%
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index  
  spread(key = year, value = dbh) %>%
  select(-row_id) -> dbh.wide

colnames(dbh.wide)[colnames(dbh.wide) == "2015"] <- "dbh.15"
colnames(dbh.wide)[colnames(dbh.wide) == "2016"] <- "dbh.16"
colnames(dbh.wide)[colnames(dbh.wide) == "2017"] <- "dbh.17"

dbh.wide %>%
  tibble::rowid_to_column() %>%
  gather( year, dbh, -siteID, -plotID, -tagID, -treeID)

#make it wide.....
df%>%
  select(treeID, year, dbh) %>%
  spread(key = "year",
         value = "dbh")  -> dbh.wide

neon.dbh[30830:30835,]


str(dbh.wide)

colnames(dbh.wide)[colnames(dbh.wide) == "2015"] <- "dbh.15"
colnames(dbh.wide)[colnames(dbh.wide) == "2016"] <- "dbh.16"
colnames(dbh.wide)[colnames(dbh.wide) == "2017"] <- "dbh.17"

         


dbh.wide %>%
  complete(plotID, tagID, dbh.15, dbh.16, dbh.17) -> dbh.test



jj <- data.frame(month=rep(1:3,4),
                 student=rep(c("Amy", "Bob"), each=6),
                 A=c(9, 7, 6, 8, 6, 9, 3, 2, 1, 5, 6, 5),
                 B=c(6, 7, 8, 5, 6, 7, 5, 4, 6, 3, 1, 5))

jj %>% 
  gather(variable, value, -(month:student)) %>% 
  unite(temp, student, variable) %>% 
  group_by(temp) %>% 
  mutate(id=1:n()) %>% 
  spread(temp, value) 