#################################
# Analyse.R
# Written by Christopher Cosler and Lisa Katharina Schmid
# Created 21.10.2015
# c.cosler@mpp.hertie-school.org
# lisa.schmid@mpp.hertie-school.org
#################################

##############################
### Structure of this script
##############################

# 0. Preparations
# 1. Descriptive Statistics
## 1.1 Districts and Initiatives
## 1.2 Independent Variables
# 2. Inferential Statistics

##############################
### 0. Preparations
##############################

### Set working directory
try(setwd("C:/Users/Christopher/Google Drive/GitHub/CSSR_FinalProject/"), silent = TRUE)
try(setwd("C:/Users/Lisa/Documents/GitHub/CSSR_FinalProject"), silent = TRUE)

### Calling preceding R script "Clean and Merge"
source("Data/Clean and Merge.R")

##############################
### 1. Descriptive Statistics
##############################

### 1.1 Districts and Initiatives
##############################

###
histogram <- ggplot(Data.fin, aes(x=numb.ini)) + 
  geom_histogram(binwidth = 1) +
  theme_light() +
  ylab("Count") +
  xlab("Number of Initiatives per district")

histogram

### Plot shapefile and dots
plot(Shapes_krs) # creates map of Germany with district borders
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19, cex = 0.5) # plots initiatives on the map

### List counties
Shapes_krs$GEN

### Plot only specific districts
Berlin_shapes <- Shapes_krs[Shapes_krs$GEN == "Berlin",]
plot(Berlin_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

Konstanz_shapes <- Shapes_krs[Shapes_krs$GEN == "Konstanz",]
plot(Konstanz_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

Oberberg_shapes <- Shapes_krs[Shapes_krs$GEN == "Oberbergischer Kreis",]
plot(Oberberg_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

Aachen_shapes <- Shapes_krs[Shapes_krs$GEN == "StÃ¤dteregion Aachen",]
plot(Aachen_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

München_shapes <- Shapes_krs[Shapes_krs$RS == "09162",]
plot(München_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

München_shapes <- Shapes_krs[Shapes_krs$RS == "09184",]
plot(München_shapes)
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)


### Plotting Number of Initiatives per District

# Cut data into classes
classes <- cut(Data.fin$numb.ini, c(0,1,2,3,5,7,9,40), right = FALSE)
levels(classes) <- c("0", "1", "2", "3-4", "5-6", "7-8", "9 or higher")

# Assign colors
colours <- brewer.pal(8,"Greys") # Pick color palette

# Plot the shapefiles colored
plot(Shapes_krs,border = "darkgrey", col = colours[classes])

# Add legend
levels <- c("0", "1", "2", "3-4", "5-6", "7-8", "9 or higher")
legend("right", fill = colours, legend = levels)

# Add Title
title("Number of Initiatives per District")

# Add initiatives
points(Coordinates$Longitude, Coordinates$Latitude, col = adjustcolor("red",0.4),pch = 19)

# Label (not really useful here)
# text(coordinates(Shapes_krs), labels =Shapes_krs$GEN, col = "white")


### 1.2 Independent Variables
##############################

### Age
# Cut data into classes
classes <- cut(Data.fin$oldage.dependency, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5), right = FALSE)
levels(classes) <- c("0<0.25", "0.25<0.3", "0.3<0.4", "0.4<0.45", "0.45<0.5")
# Assign colors
colours <- brewer.pal(6,"Greys") # Pick color palette
# Plot shapefile colored
plot(Shapes_krs,border = "darkgrey", col = colours[classes])

# Gender
# Education
# GDP per capita
# Unemployment
# Population density
# Refugee ratio
# Turnout
# (East West)

############################################
# 3. Inferential Statistics
############################################

# 3.1 Regressions
############################################

# Make a nice clean subset with dependent and independent variables only and remove NAs
temp1 <- Data.fin
drops <- c("district.ID","district.name")
temp1 <- temp1[,!(names(temp1) %in% drops)]
temp1 <- na.omit(temp1) # Omit row if missing value
############# Drop Berlin, do we really want this?
#############
#############
#############
#############
temp1 <- dplyr::filter(temp1, numb.ini != 40)
# Second subset for comparison reasons if you want to
temp2 <- Data.fin[c("numb.ini", "unemployment", "pop.dens", "GDP.cap")] # Copy columns in new dataframe
temp2 <- na.omit(temp2) # Omit row if missing value

# Poisson regression
pois <- glm(numb.ini ~ ., data = temp1, family = poisson) # Poisson regression using all variables in temp1
summary(pois)
overdispersion <- dispersiontest(pois,trafo=1) # From Cameron and Trived (1990)
overdispersion_alpha <- round(unlist(overdispersion[3]), digits = 2) # Shows test statistics
overdispersion_alpha
overdispersion_p <- round(unlist(overdispersion[2]), digits = 4) # Shows p value
overdispersion_p
# All are highly significant. But this might be to optimistic due to
# a misspecficiation of the likelihood. The dispersion test based on
# Cameron and Trivedi (1990) tests whether the variance and the mean are equal.
# c < 0 means underdispersion, c > 0 means overdispersion. Dispersion shown at alpha.

# A quasi-poisson takes overdispersion into account
qpois <- glm(numb.ini ~ ., data = temp1, family = quasipoisson)
summary(qpois)
# The model leads to an estimated dispersion of 2.2, which is clearly
# larger than 1 and another hint that overdispersion could be present

# A more formal way to accommodate over-dispersion in a count data regression
# model is a negative binomal model
nbin <- glm.nb(numb.ini ~., data = temp1)
summary(nbin) 

#### Predicted probabilities
# First create a dataset with the variables as you want them

# Here, everything but East/West is at its mean
predicted <- data.frame(oldage.dependency = mean(temp1$oldage.dependency),
                        gender.ratio = mean(temp1$gender.ratio),
                        abitur.per = mean(temp1$abitur.per),
                        GDP.cap = mean(temp1$GDP.cap),
                        unemployment = mean(temp1$unemployment),
                        pop.dens = mean(temp1$pop.dens),
                        refugee.ratio = mean(temp1$refugee.ratio),
                        east = unique(temp1$east),
                        turnout = mean(temp1$turnout))

predicted <- cbind(predicted, predict(nbin, predicted, type = "link", se.fit = TRUE))
predicted <- within(predicted, {
  NumberInitiatives <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

# East West as linechart
plot_east_line <- ggplot(predicted, aes(east, NumberInitiatives)) + 
  geom_line(colour = "firebrick", size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "firebrick", alpha = .25) +
  ggtitle("Prediction of number of refugee initatives \n in East and West") +
  theme(plot.title = element_text(size=20, face="bold", vjust=1, lineheight=1)) +
  theme(panel.background = element_rect(fill = 'grey75')) +
  theme(panel.grid.major = element_line(colour = "white", size=1)) +
  theme(panel.grid.minor = element_line(colour = "white", size=1)) +
    labs(x= "Region (0 = West, 1 = East)", y = "Predicted number of refugee initiatives")
plot_east_line


predicted$east <- factor(predicted$east)
levels(predicted$east)
plot_eastwest <- ggplot(predicted, aes(east, NumberInitiatives)) + 
  geom_point(colour = "green", size = 4) +
  geom_point(aes(east, LL), size = 4, colour = "red") +
  geom_point(aes(east, UL), size = 4, colour = "red") +
  theme_light() +
  ylab("Predicted Number of Initiatives") +
  xlab("0 = West Germany , 1 = East Germany")
plot_eastwest


# The same thing for old age dependency, splitted for east west
remove(predicted)

predicted <- data.frame(oldage.dependency = rep(seq(from = min(temp1$oldage.dependency), to = max(temp1$oldage.dependency), length.out = 100), 2),
                        gender.ratio = mean(temp1$gender.ratio),
                        abitur.per = mean(temp1$abitur.per),
                        GDP.cap = mean(temp1$GDP.cap),
                        unemployment = mean(temp1$unemployment),
                        pop.dens = mean(temp1$pop.dens),
                        refugee.ratio = mean(temp1$refugee.ratio),
                        east = rep(0:1, each = 100, len = 200),
                        turnout = mean(temp1$turnout))

predicted <- cbind(predicted, predict(nbin, predicted, type = "link", se.fit = TRUE))
predicted <- within(predicted, {
  NumberInitiatives <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

predicted$east <- factor(predicted$east, labels = c("West Germany", "East Germany"))
levels(predicted$east)



plot_age_eastwest <- ggplot(predicted, aes(oldage.dependency, NumberInitiatives)) +
  geom_line(aes(colour = east), size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = east), alpha = .25) +
  labs(x = "Old Age Dependency", y = "Predicted Number of Initiatives") +
  theme_light() +
  theme(legend.title=element_blank())
plot_age_eastwest

predicted <- predicted[1:100,]
ggplot(predicted, aes(oldage.dependency, NumberInitiatives)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  labs(x = "Old Age Dependency", y = "Predicted Number of Initiatives")


### Same thing for Turnout
remove(predicted)

predicted <- data.frame(oldage.dependency = mean(temp1$oldage.dependency),
                        gender.ratio = mean(temp1$gender.ratio),
                        abitur.per = mean(temp1$abitur.per),
                        GDP.cap = mean(temp1$GDP.cap),
                        unemployment = mean(temp1$unemployment),
                        pop.dens = mean(temp1$pop.dens),
                        refugee.ratio = mean(temp1$refugee.ratio),
                        east = rep(0:1, each = 100, len = 200),
                        turnout = rep(seq(from = min(temp1$turnout), to = max(temp1$turnout), length.out = 100), 2))

predicted <- cbind(predicted, predict(nbin, predicted, type = "link", se.fit = TRUE))
predicted <- within(predicted, {
  NumberInitiatives <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

predicted$east <- factor(predicted$east, labels = c("West Germany", "East Germany"))
levels(predicted$east)

plot_turnout_eastwest <- ggplot(predicted, aes(turnout, NumberInitiatives)) +
  geom_line(aes(colour = east), size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = east), alpha = .25) +
  labs(x = "Turnout", y = "Predicted Number of Initiatives") +
  theme_light() +
  theme(legend.title=element_blank())
plot_turnout_eastwest

predicted <- predicted[1:100,]
plot_turnout <- ggplot(predicted, aes(turnout, NumberInitiatives)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  labs(x = "Turnout", y = "Predicted Number of Initiatives")

plot_turnout

### Same thing for GDP
remove(predicted)

predicted <- data.frame(oldage.dependency = mean(temp1$oldage.dependency),
                        gender.ratio = mean(temp1$gender.ratio),
                        abitur.per = mean(temp1$abitur.per),
                        GDP.cap = rep(seq(from = min(temp1$GDP.cap), to = max(temp1$GDP.cap), length.out = 100), 2),
                        unemployment = mean(temp1$unemployment),
                        pop.dens = mean(temp1$pop.dens),
                        refugee.ratio = mean(temp1$refugee.ratio),
                        east = rep(0:1, each = 100, len = 200),
                        turnout = mean(temp1$turnout))

predicted <- cbind(predicted, predict(nbin, predicted, type = "link", se.fit = TRUE))
predicted <- within(predicted, {
  NumberInitiatives <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

predicted$east <- factor(predicted$east, labels = c("West Germany", "East Germany"))
levels(predicted$east)

plot_GDP_eastwest <- ggplot(predicted, aes(GDP.cap, NumberInitiatives)) +
  geom_line(aes(colour = east), size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = east), alpha = .25) +
  labs(x = "GDP per capita (1000)", y = "Predicted Number of Initiatives") +
  theme(legend.title=element_blank()) +
  theme_light()

plot_GDP_eastwest

predicted <- predicted[1:100,]
plot_GDP <- ggplot(predicted, aes(GDP.cap, NumberInitiatives)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  labs(x = "Turnout", y = "Predicted Number of Initiatives")

plot_GDP

### Same thing for Gender
remove(predicted)

predicted <- data.frame(oldage.dependency = mean(temp1$oldage.dependency),
                        gender.ratio = rep(seq(from = min(temp1$gender.ratio), to = max(temp1$gender.ratio), length.out = 100), 2),
                        abitur.per = mean(temp1$abitur.per),
                        GDP.cap = mean(temp1$GDP.cap),
                        unemployment = mean(temp1$unemployment),
                        pop.dens = mean(temp1$pop.dens),
                        refugee.ratio = mean(temp1$refugee.ratio),
                        east = rep(0:1, each = 100, len = 200),
                        turnout = mean(temp1$turnout))

predicted <- cbind(predicted, predict(nbin, predicted, type = "link", se.fit = TRUE))
predicted <- within(predicted, {
  NumberInitiatives <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

predicted$east <- factor(predicted$east, labels = c("West Germany", "East Germany"))
levels(predicted$east)

plot_gender_eastwest <- ggplot(predicted, aes(gender.ratio, NumberInitiatives)) +
  geom_line(aes(colour = east), size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = east), alpha = .25) +
  labs(x = "Gender Ratio", y = "Predicted Number of Initiatives") +
  theme_light() +
  theme(legend.title=element_blank())
plot_gender_eastwest

predicted <- predicted[1:100,]
plot_gender <- ggplot(predicted, aes(gender.ratio, NumberInitiatives)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  labs(x = "Turnout", y = "Predicted Number of Initiatives")

plot_gender
