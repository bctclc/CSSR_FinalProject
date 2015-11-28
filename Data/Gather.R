#################################
# Gather.R
# Written by Christopher Cosler and Lisa Katharina Schmid
# Created 21.10.2015
# c.cosler@mpp.hertie-school.org
# lisa.schmid@mpp.hertie-school.org
#################################

##############################
### Structure of this script
##############################

# 0. Preparations

# 1. Dependent Variable "Number of Refugee Initiatives per district in Germany"
## 1.1 Gather coordinates of refugee initiatives from ProAsyl Google Map
## 1.2 Gather shapefile data of German counties

# 2. Independent Variables
## 2.1 Gender and Age
## 2.2 Education
## 2.3 Unemployment and GDP per Capita
## 2.4 Population Density
## 2.5 Refugee Numbers
## 2.6 Voter Turnout


##############################
### 0. Preparations
##############################

### Install packages, if necessary
packages <- c('Hmisc', 'plyr', 'httr', 'dplyr', 'XML', 'maptools', 'sp', 
              'rgdal', 'gsubfn', 'RColorBrewer', 'prevR', 'repmis', 
              'sandwich', 'msm', 'ggplot2', 'lmtest', 'MASS', 'pscl', 'stargazer',
              'AER', 'shiny', 'leaflet', 'stats', 'knitr', 'ggplot2', 'ggthemes')

for (p in packages) {
  if (p %in% installed.packages()[,1]) require(p, character.only=T)
  else {
    install.packages(p)
    require(p, character.only=T)
  }
}

### Create bib file for all the packages used in this script
repmis::LoadandCite(packages, file = 'Rpackages.bib')

### Load libraries
library(ggthemes)
library(AER)
library(Hmisc)
library(plyr)
library(sandwich)
library(msm)
library(ggplot2)
library(httr)
library(dplyr)
library(XML)
library(maptools)
library(sp)
library(rgdal)
library(gsubfn)
library(RColorBrewer)
library(prevR)
library(lmtest)
library(MASS)
library(pscl)
library(stargazer)
library(knitr)

### Set working directory
try(setwd("C:/Users/Christopher/Google Drive/GitHub/CSSR_FinalProject/"), silent = TRUE)
try(setwd("C:/Users/Lisa/Documents/GitHub/CSSR_FinalProject"), silent = TRUE)

##############################
### 1. Dependent Variable "Number of Refugee Initiatives per district in Germany"
##############################

### 1.1 Gather coordinates of refugee initiatives from ProAsyl Google Map
##############################

### Get data from Google Map of refugee initiatives, provided by ProAsyl 
### Original map see here: https://www.google.com/maps/d/viewer?mid=zc6TdvfelKuY.kUvriXoSREXw

Initiativen <- GET("https://mapsengine.google.com/map/kml?mid=zc6TdvfelKuY.kUvriXoSREXw&forcekml=1") # Download data
Initiativen <- as.character(Initiativen) # Convert to character

### Extract names, links, and location of refugee initiatives
Names <- strapplyc(Initiativen, "<name>(.*?)</name>", simplify = c) # Extract names
Names <- Names[-(1:2)] # Delete headers
Names <- Names[-(15:16)] # Delete list of overview pages of refugee initiatives

Coordinates <- strapplyc(Initiativen, "<coordinates>(.*?)</coordinates>", simplify = c) # Extract coordinates
Coordinates <- strsplit(Coordinates, split = ",") # Split into two rows
Coordinates <- data.frame(matrix(unlist(Coordinates), ncol=3, byrow=T)) # Convert to dataframe
Coordinates <- plyr::rename(Coordinates, c("X1"="Longitude", "X2"="Latitude", "X3" = "Heigt")) # Rename

### 1.2 Gather shapefile data of German counties
##############################

# The data has to be downloaded manually from
# http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=19&gdz_user_id=0
# and stored in the subfolder Data/Shapefiles.
# It is the UTM32 coordinate system (file size 1.8mb).
# No changes have been made to the file before loading it into R.

unzip("Data/Shapefiles/vg2500.utm32s.shape.zip", file = NULL, exdir = "Data/Shapefiles") # Unzip files and store

# Import the map and transform it to the coordinate system that matches 
# with the coordinates (to WGS84, epsg code 4326).
Shapes_krs <- readOGR(dsn = "Data/Shapefiles/vg2500.utm32s.shape/vg2500", layer = "vg2500_krs") # Load Kreise shapefile
Shapes_krs<-spTransform(Shapes_krs, CRS("+init=epsg:4326")) # Transform to different coordinates format (to WGS84)


#############################
# 2. Independent Variables
#############################

# Data has to be downloaded manually, as the federal statistical offices
# (DeStatis/Genesis) does not provide an API for free. Two sources are used: 
# https://www-genesis.destatis.de/genesis/online and
# https://www.regionalstatistik.de/genesis/online (specified for each variable).
# All data sets on those platforms are attributed a certain table number,
# which is provided in the sections below. Finding the files works best when
# inserting the table number into the search box at the beginning of the
# respective pages.
# For some data sets a certain time frame has to be specified. If we have done so,
# it is described in the sections below.
# All downloaded data is downloaded as a csv file and saved in the folder Data/Indepdendent Variables.
# No changes have been made to the files before loading them into R.
# Note: the table number and the file name are identical.

# 2.1 Gender and Age
#############################

# Table 12411-0017 from https://www-genesis.destatis.de/genesis/online
# Downloaded on 11/7/2015
# Specification before downloading: "Zeit auswählen" (select time) = 12/31/2014

GenderAgeRaw <- read.csv("Data/Independent Variables/12411-0017.csv",
                      header = FALSE, 
                      sep = ";", 
                      na.strings = c("-"),
                      skip = 6,
                      nrows = 475
                      ) # loads data frame

# 2.2 Education
#############################

# Table AI003-2 from https://www.regionalstatistik.de/genesis/online
# Downloaded on 11/8/2015
# Specification: "Zeit auswählen" (select time) = 2013

EducationRaw <- read.csv("Data/Independent Variables/AI003-2.csv",
                         header = FALSE, 
                         sep = ";", 
                         na.strings = c("-","."),
                         col.names = c("year", "district.ID", "district.name", "abitur.per", "nodegree.per"),
                         skip = 6,
                         nrows = 525,
                         dec=",") # loads data frame

# 2.3 Unemployment and GDP per Capita
#############################

### Unemployment

# Table 659-71-4 from https://www.regionalstatistik.de/genesis/online
# Downloaded 11/8/2015
# Specification: "Zeit auswählen" (select time) = 2014

JobsRaw <- read.csv("Data/Independent Variables/659-71-4.csv",
                         header = FALSE, 
                         sep = ";", 
                         na.strings = c("-","."),
                         nrows = 533,
                         dec = ",") # loads data frame

### GDP per capita

# Table AI-N-10 from https://www.regionalstatistik.de/genesis/online
# Downloaded 11/8/2015
# Specification: "Zeit auswählen" (select time) = 2012

GDPRaw <- read.csv("Data/Independent Variables/AI-N-10.csv",
                   header = FALSE, 
                   sep = ";", 
                   na.strings = c("-","."),
                   col.names = c("year", "district.ID", "district.name", "GDP.cap"),
                   skip = 7,
                   nrows = 525
                   ) # loads data frame

# 2.4 Population density
#############################

# Table AI002-1 from https://www.regionalstatistik.de/genesis/online
# Downloaded 11/8/2015
# Specification: "Zeit auswählen" (select time) = 2014

PopRaw <- read.csv("Data/Independent Variables/AI002-1.csv",
                   header = FALSE, 
                   sep = ";", 
                   na.strings = c("-","."),
                   nrows = 531,
                   dec = ",") # loads data frame

# 2.5 Refugee numbers
#############################

# Table 661-31-4 from https://www.regionalstatistik.de/genesis/online
# Downloaded 11/8/2015
# Specification: "Zeit auswählen" (select time) = 12/31/2013

RefRaw <- read.csv("Data/Independent Variables/661-31-4.csv",
                   header = FALSE, 
                   sep = ";", 
                   na.strings = c("-","."),
                   nrows = 534,
                   dec = ",") # loads data frame


# 2.6 Voter turnout
#############################

# Table 252-01-4 from https://www.regionalstatistik.de/genesis/online
# Downloaded 11/13/2015
# Specification: "Zeit auswählen" (select time) = 9/22/2013

TurnoutRaw <- read.csv("Data/Independent Variables/252-01-4.csv",
                   header = FALSE, 
                   sep = ";", 
                   na.strings = c("-","."),
                   nrows = 534,
                   dec = ",") # loads data frame
