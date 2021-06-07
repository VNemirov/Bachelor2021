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
library(ftplottools)
# You might have to run loadfonts() to get all fonts for plots
library(extrafont)
#Set theme
theme_set(ft_theme()+
            theme(axis.title = element_text(size=10,
                                            family = "Helvetica"),
                  title = element_text(size = 11,
                                       family = "Helvetica"),
                  text = element_text(size = 10,
                                      family = "Helvetica"),
                  plot.margin=unit(c(.2,.5,.2,.2),"cm"),
                  plot.background = element_rect(fill = "#f1dfcb")))

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
sapply(scripts, function(x){
  start_script <- Sys.time()
  print(x)
  source(x, echo = F)
  print(Sys.time()-start_script)
  print("==============================================")
})

print(paste("Complete in ", Sys.time()-start_time))
