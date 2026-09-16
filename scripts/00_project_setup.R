# 00_project_setup.R

# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidycensus,sf,glue,here,purr)

