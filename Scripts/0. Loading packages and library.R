rm(list=ls())
start_time <- Sys.time()
#Loading Packages and themes
library(quantmod)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(visdat)
library(ggrepel)
library(gtools)
#Set theme
theme_set(theme_minimal()+
            theme(axis.title = element_text(size=12),
                  title = element_text(size = 14)))

scripts <- data.frame(script = list.files(path = "./Scripts/",
                                          full.names = T)) %>% 
  filter(script != "./Scripts/funktioner",
         script != "./Scripts/funktioner.R",
         script != "./Scripts/0. Loading packages and library.R") %>% 
  pull(script) %>% 
  mixedsort()


funktioner <- list.files(path = "./Scripts/funktioner/",
                         full.names = T)
#Sourcing funcitions
sapply(funktioner, source)
#Sourcing scripts
  print("==============================================")#ASCII kunst
sapply(scripts, function(x){war
  print(x)
  source(x, echo = F)
  print("Done")
  print("==============================================")
})

print(paste("Complete in ", Sys.time()-start_time))
