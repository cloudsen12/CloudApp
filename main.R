library(googledrive)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(outliers)
library(plotly)
library(raster)
library(mmand)
library(Orcs)
library(rgee)
library(zip)
library(sf)

source("https://gist.githubusercontent.com/csaybar/daa1a877f3d1703b61846603e986b14c/raw/222c23488951eb837623ac091debede408d60351/demo.R")

ee_Initialize()

drive_auth("s1078735@stud.sbg.ac.at")


# 1. Display S2 images ----------------------------------------------------
id <- "20190212T142031_20190212T143214_T19FDF"
map_results <- s2_comparison(point = "point_0001", id, max = c(4000, 2000))
map_results$rgb
map_results$cirrus

# 2. Display App ----------------------------------------------------
coordx <- " lon: -69.79300 | lat: -49.00847 | zoom: 14 "
display_app(coordx, id, cc = 5)
