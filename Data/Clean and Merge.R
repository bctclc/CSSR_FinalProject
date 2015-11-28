#################################
# Clean and Merge.R
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

# 2. Independent Variables
## 2.1 Age and gender
## 2.2 Education
## 2.3 Unemployment
## 2.4 Population density
## 2.5 Refugee numbers
## 2.6 East-west dummy

# 3. Merging all datasets

# 4. Creation of some more variables

# 5. Final data set for analysis

##############################
### 0. Preparations
##############################

### Setting working directory, please change if necessary! 
try(setwd("C:/Users/Christopher/Google Drive/GitHub/CSSR_Dataanalysis/"), silent = TRUE)
try(setwd("C:/Users/Lisa/Documents/GitHub/CSSR_DataAnalysis"), silent = TRUE)

### Source preceding R code "Gather.R"
source("Data/Gather.R")

##############################
### 1. Dependent Variable "Number of Refugee Initiatives per district in Germany"
##############################

### Convert coordinates to numeric
Coordinates$Longitude <- as.numeric(as.character(Coordinates$Longitude))
Coordinates$Latitude <- as.numeric(as.character(Coordinates$Latitude))

### In which district are the initiatves? Bind the district name to the coordinates.
Coordinates_transformed <- Coordinates[c("Longitude", "Latitude")] # Make new df
coordinates(Coordinates_transformed) <- ~Longitude+Latitude # Convert to spatial object
proj4string(Coordinates_transformed) <- proj4string(Shapes_krs) # Assign CSR coordinates
over(Coordinates_transformed, Shapes_krs)$GEN # Find respective district
Coordinates <- cbind.data.frame(Coordinates_transformed, Kreis=over(Coordinates_transformed, Shapes_krs)$RS) # Cbind into df

### Calculate number of initiatives per district
data <- data.frame(Shapes_krs$RS) # Create dataframe with one row per district
data <- rename(data, Kreis= Shapes_krs.RS) # Rename
Initiatives <- Coordinates %>% count(Kreis) # Calculate number of initiatives per district
Initiatives <- rename(Initiatives, Initiatives = n) # Rename
data <- full_join(data, Initiatives) # Join to data
data$Initiatives[is.na(data$Initiatives)] <- 0 # Replace NA with 0

### Match DESTATIS data to dataframe "data" using id
data$id <- Shapes_krs$GEN

### Rename variables for matching
names(data) <- c("district.ID", "numb.ini", "district.name")

### Transform variable classes
sapply(data, class) # view classes of all variables
data$district.ID <- as.numeric(as.character(data$district.ID))
data[, 3] <- sapply(data[, 3], as.character) # change district.name to character
sapply(data, class) # have a look at the changed classes

### Rename data set for clarity
Inis <- data # "Inis" = "initiatives"

#############################
# 2. Independent Variables
#############################

# 2.1 Sex and Gender
#############################

# Rename variables

GenderAge <- GenderAgeRaw

names(GenderAge) <- c("date", "district.ID", "district.name", "m1", "m2", "m3", "m4", 
                      "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14",
                      "m15", "m16", "m17", "f1", "f2", "f3", "f4", "f5", "f6", 
                      "f7", "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15", "f16", 
                      "f17") 

# Transform factors to numeric or character
sapply(GenderAge, class) # view classes of all variables
GenderAge[, c(2,4:37)] <- sapply(GenderAge[, c(2,4:37)], as.numeric) # change all age variables and the district.ID to numeric
GenderAge[, 3] <- sapply(GenderAge[, 3], as.character) # change district.name to character
sapply(GenderAge, class) # have a look at the changed classes

# Drop counties with NA (as they are referring to counties that do not longer exist)
GenderAge <- na.omit(GenderAge)

# Built new, more useful variables for gender
## Generate variables for total female and male population per district
GenderAge$males <- rowSums(GenderAge[, c(4:20)]) # total male population
GenderAge$females <- rowSums(GenderAge[, c(21:37)]) # total female population
## Generate variable for total population per district
GenderAge$total <- GenderAge$males + GenderAge$females
## Ratio of males and females per district
GenderAge$males.per <- GenderAge$males / GenderAge$total
GenderAge$females.per <- GenderAge$females / GenderAge$total
## Gender ratios per district
GenderAge$gender.ratio <- GenderAge$males.per / GenderAge$females.per

# Built new, more useful variable for age: Old Age Dependency Ratio
## Compute total young (15<65) and old (>=65) population
GenderAge$young <- rowSums(GenderAge[, c(8:18, 25:35)]) # persons aged 15<65 (both males, and females)
GenderAge$old <- rowSums(GenderAge[, c(19:20, 36:37)]) # persons aged >=65 (both males and females)
## Old Age Dependency Ratio (number of persons aged 65 and older by persons aged 15<65)
GenderAge$oldage.dependency <- GenderAge$old / GenderAge$young # Old Age Dependency Ratio

# 2.2 Education
#############################

Education <- EducationRaw

# Drop Germany
Education <- Education[-1,]

# Transform factors to numeric or character
sapply(Education, class) # view classes of all variables
Education[, c(1:3)] <- sapply(Education[, c(1:3)], as.character) # change non-numeric variables to character
Education[, c(1:2)] <- sapply(Education[, c(1:2)], as.numeric) # change year and district.ID to numeric
sapply(Education, class) # have a look at the changed classes

# 2.3 Unemployment and GDP per capita
#############################

### Unemployment
Jobs <- JobsRaw

# Deleting columns with unnecessary information
Jobs <- subset(Jobs, select = c(V1, V2, V3, V12))

# Deleting rows with unnecessary information (including Germany)
Jobs <- Jobs[-c(1:9), ]

# Rename variables
names(Jobs) <- c("year", "district.ID", "district.name", "unemployment")

# Transform to numeric
sapply(Jobs, class) # view classes of all variables
Jobs[, ] <- sapply(Jobs[, ], as.character) # transform all variables to character
Jobs[,4] <- as.numeric(gsub(",", ".", as.matrix(Jobs[,4]))) # replace commas with periods in unemployment rate
Jobs[, c(1:2, 4)] <- sapply(Jobs[, c(1:2, 4)], as.numeric) # change year and district.ID to numeric
sapply(Jobs, class) # have a look at the changed classes

### GDP per capita
GDP <- GDPRaw

# Transform to numeric
sapply(GDP, class) # view classes of all variables
GDP[,4] <- as.numeric(gsub(",", ".", as.matrix(GDP[,4]))) # replace commas with periods in unemployment rate
GDP[, ] <- sapply(GDP[, ], as.character) # transform all variables to character
GDP[, c(1:2, 4)] <- sapply(GDP[, c(1:2, 4)], as.numeric) # change year, district.ID and GDP.cap to numeric
sapply(Jobs, class) # have a look at the changed classes

# Recode to GDP per capita in 1000 Euros
GDP$GDP.cap <- GDP$GDP.cap / 1000

# 2.4 Population density
#############################

Pop <- PopRaw

# Deleting columns with unnecessary information
Pop <- subset(Pop, select = c(V1, V2, V3, V4))

# Deleting rows with unnecessary information (including Germany)
Pop <- Pop[-c(1:7), ]

# Rename variables
names(Pop) <- c("year", "district.ID", "district.name", "pop.dens")

# Add label
label(Pop$pop.dens) <- "Inhabitants per square kilometer"

# Transform to numeric
sapply(Pop, class) # view classes of all variables
Pop[, ] <- sapply(Pop[, ], as.character) # transform all variables to character
Pop[,4] <- as.numeric(gsub(",", ".", as.matrix(Pop[,4]))) # replace commas with periods in unemployment rate
Pop[, c(1:2, 4)] <- sapply(Pop[, c(1:2, 4)], as.numeric) # change year and district.ID to numeric
sapply(Pop, class) # have a look at the changed classes

# 2.5 Refugee numbers
#############################

Ref <- RefRaw

# Deleting columns with unnecessary information
Ref <- subset(Ref, select = c(V1, V2, V3, V10))

# Rename variables
names(Ref) <- c("year", "district.ID", "district.name", "refugees")

# Deleting rows with unnecessary information (including Germany)
Ref <- Ref[-c(1:10), ]

# Transfer to numeric/character
sapply(Ref, class) # view classes of all variables
Ref[, ] <- sapply(Ref[, ], as.character) # transform all variables to character
Ref[, c(1:2, 4)] <- sapply(Ref[, c(1:2, 4)], as.numeric) # change year, district.ID and refugee numbers to numeric
sapply(Ref, class) # have a look at the changed classes


# 2.6 Voter turnout
#############################

Turnout <- TurnoutRaw

# Deleting columns with unnecessary information
Turnout <- subset(Turnout, select = c(V1, V2, V3, V5))

# Rename variables
names(Turnout) <- c("year", "district.ID", "district.name", "turnout")

# Deleting rows with unnecessary information (including Germany)
Turnout <- Turnout[-c(1:10), ]

# Transfer to numeric/character
sapply(Turnout, class) # view classes of all variables
Turnout[, ] <- sapply(Turnout[, ], as.character) # transform all variables to character
Turnout[,4] <- as.numeric(gsub(",", ".", as.matrix(Turnout[,4]))) # replace commas with periods in voter turnout
Turnout[, c(1:2, 4)] <- sapply(Turnout[, c(1:2, 4)], as.numeric) # change year, district.ID and voter turnout to numeric
sapply(Turnout, class) # have a look at the changed classes

#############################
# 3. Merging all data sets 
#############################

### Recode Berlin (from 11 to 11000) and Hamburg (from 2 to 2000), to make 
### ensure coherent coding

GenderAge$district.ID[GenderAge$district.ID == 2] <- 2000
GenderAge$district.ID[GenderAge$district.ID == 11] <- 11000

Education$district.ID[Education$district.ID == 2] <- 2000
Education$district.ID[Education$district.ID == 11] <- 11000

Jobs$district.ID[Jobs$district.ID == 2] <- 2000
Jobs$district.ID[Jobs$district.ID == 11] <- 11000

GDP$district.ID[GDP$district.ID == 2] <- 2000
GDP$district.ID[GDP$district.ID == 11] <- 11000

Pop$district.ID[Pop$district.ID == 2] <- 2000
Pop$district.ID[Pop$district.ID == 11] <- 11000

Ref$district.ID[Ref$district.ID == 2] <- 2000
Ref$district.ID[Ref$district.ID == 11] <- 11000

Turnout$district.ID[Turnout$district.ID == 2] <- 2000
Turnout$district.ID[Turnout$district.ID == 11] <- 11000

### Keep only necessary vectors from data sets before merging: district.ID and final variables
a <- subset(Inis[, 1:2]) # district.ID + number of initiatives per district
b <- subset(GenderAge[,  c(2:3, 40, 43, 46)]) # district.ID + district.name + total population + gender.ratio + oldage.dependency
c <- subset(Education[, c(2, 4:5)]) # district.ID + abitur.per + nodegree.per
d <- subset(Jobs[, c(2,4)]) # district.ID + unemployment rate
e <- subset(GDP[, c(2,4)]) # district.ID + GDP per capita
f <- subset(Pop[, c(2,4)]) # district.ID + population density
g <- subset(Ref[, c(2,4)]) # district.ID + number of refugees per district
h <- subset(Turnout[, c(2,4)]) # district.ID + voter turnout

### Merging all data sets, one by one
Data <- merge(a, b, by = "district.ID", all = TRUE)
Data <- merge(Data, c, by = "district.ID", all = TRUE)
Data <- merge(Data, d, by = "district.ID", all = TRUE)
Data <- merge(Data, e, by = "district.ID", all = TRUE)
Data <- merge(Data, f, by = "district.ID", all = TRUE)
Data <- merge(Data, g, by = "district.ID", all = TRUE)
Data <- merge(Data, h, by = "district.ID", all = TRUE)

### Merge to Initiatives
Data <- left_join(Inis, Data, by = "district.ID") # Merge both dataframes
Data$district.name.x <- NULL # Delete column 
Data$numb.ini.x <- NULL # Delete column
Data <- rename(Data,  numb.ini = numb.ini.y, district.name = district.name.y) # Rename


#############################
# 4. Creation of some more variables
#############################

# 4.1 East-West dummy
#############################

Data$east <- ifelse(Data$district.ID >= 11000, 1, 0)

# Explanation: district.ID is comprised of 5 figures: 00-000. The first two describe the
# Land, the three last numbers describe the district itself. Länder codes 1 to 10 are 
# West German districts, Länder codes 11 to 16 are East German Länder codes.
### Building a more useful variable for refugees: refugees per citizen/refugee ratio

# 4.2 Refugee ratio
#############################

Data$refugee.ratio <- Data$refugees / Data$total # computes refugees per citizen in a district
Data$refugee.ratio <- Data$refugee.ratio * 1000 # computes refugees per 1000 citizens in a district


#############################
# 5. Final data set for analysis
#############################

Data.fin <- subset(Data, select = c(district.ID, district.name, numb.ini, oldage.dependency, gender.ratio,
                                    abitur.per, GDP.cap, unemployment, pop.dens, refugee.ratio, east, 
                                    turnout))