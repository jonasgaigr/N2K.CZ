# SET WD ----
setwd("~/N2K.CZ-main")
# LOAD PACKAGES ----
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

habanalysis <- read.csv("C:/Users/jonas.gaigr/N2K.CZ/results/habitaty_vyhodnoceni_20220930_UTF-8.csv", 
                        encoding = "UTF-8")
habanalysis$RE
td_repre_rb <- ggplot(data = habanalysis, aes(x = TYPICKE_DRUHY, y = REPRE_RB)) +
  geom_point() +
  theme_bw()
ggsave(td_repre_rb, filename = "typicke_druhy_repre_vrstva.png",
       height = 5, width = 8) 

td_repre_sdf <- ggplot(data = habanalysis, aes(x = TYPICKE_DRUHY, y = REPRE_SDF)) +
  geom_point() +
  theme_bw()
ggsave(td_repre_sdf, filename = "typicke_druhy_repre_sdf.png",
       height = 5, width = 8) 

kvalita_repre_sdf <- ggplot(data = habanalysis, aes(x = KVALITA, y = REPRE_SDF)) +
  geom_point() +
  theme_bw()
ggsave(kvalita_repre_sdf, filename = "kvalita_druhy_repre_sdf.png",
       height = 5, width = 8) 

kvalita_repre_rb <- ggplot(data = habanalysis, aes(x = KVALITA, y = REPRE_RB)) +
  geom_point() +
  theme_bw()
ggsave(kvalita_repre_rb, filename = "kvalita_druhy_repre_vrstva.png",
       height = 5, width = 8) 

kvalita_repre_rb <- ggplot(data = habanalysis, aes(x = KVALITA, y = REPRE_RB)) +
  geom_point() +
  theme_bw()
ggsave(kvalita_repre_rb, filename = "kvalita_druhy_repre_vrstva.png",
       height = 5, width = 8) 
