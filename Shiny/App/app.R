### Load libraries
library(shiny)
library(leaflet)
library(httr)
library(dplyr)
library(XML)
library(maptools)
library(sp)
library(rgdal)
library(gsubfn)
library(proj4)

#setwd("C:/Users/Christopher/Google Drive/GitHub/CSSr_Dataanalysis/Shiny/App")

### Prepare Coordinates
Initiativen <- GET("https://mapsengine.google.com/map/kml?mid=zc6TdvfelKuY.kUvriXoSREXw&forcekml=1") # Download data
Initiativen <- as.character(Initiativen) # Concert to character
Names <- strapplyc(Initiativen, "<name>(.*?)</name>", simplify = c) # Extract names
Coordinates <- strapplyc(Initiativen, "<coordinates>(.*?)</coordinates>", simplify = c) # Extract coordinates
Coordinates <- strsplit(Coordinates, split = ",") # Split into two rows
Coordinates <- data.frame(matrix(unlist(Coordinates), ncol=3, byrow=T)) # Convert to dataframe
Coordinates <- dplyr::select(Coordinates, c(Longitude=X1, Latitude=X2, Height=X3)) # Rename
Coordinates
Coordinates$Longitude <- as.numeric(as.character(Coordinates$Longitude))
Coordinates$Latitude <- as.numeric(as.character(Coordinates$Latitude))
data <- Coordinates

Names <- strapplyc(Initiativen, "<name>(.*?)</name>", simplify = c) # Extract names
Names <- Names[-(1:2)]
Names <- Names[-(15:16)]

Links <- strapplyc(Initiativen, "<description>(.*?)</description>", simplify = c) # Extract Links
#Links <- Links[-(1:2)]
#Links <- Links[-(15:16)]


data <- data.frame(Coordinates, Names, Links)

### Prepare Shapefiles
Shapes_krs <- readOGR(dsn = "./Data/Shapefiles", layer = "vg2500_krs")
Shapes_krs<-spTransform(Shapes_krs, CRS("+init=epsg:4326"))
Shapes_krs$test <- rep(1:4, len = 402)

### Initiatives per district
Coordinates_transformed <- Coordinates[c("Longitude", "Latitude")] # Make new df
coordinates(Coordinates_transformed) <- ~Longitude+Latitude # Convert to spatial object
proj4string(Coordinates_transformed) <- proj4string(Shapes_krs) # Nicht ganz klar was das macht, im Internet steht assign CSR coordinates
over(Coordinates_transformed, Shapes_krs)$GEN # In welchem Kreis liegt es?
Coordinates <- cbind.data.frame(Coordinates_transformed, Kreis=over(Coordinates_transformed, Shapes_krs)$GEN) # Cbind zum dataframe
table(Coordinates$Kreis)
temp <- data.frame(Shapes_krs$GEN) # Create dataframe with one row per district
temp <- rename(temp, Kreis= Shapes_krs.GEN) # Rename
Initiatives <- Coordinates %>% count(Kreis) # Calculate number of initiatives per district
Initiatives <- rename(Initiatives, Initiatives = n) # Rename
temp <- full_join(temp, Initiatives) # Join to data
temp$Initiatives[is.na(temp$Initiatives)] <- 0 # Replace NA with 0
Shapes_krs$Initiatives <- temp$Initiatives

### Independent variables
Data <- read.csv("Data/data.csv")
Shapes_krs$numb.ini <- (Data$numb.ini-min(Data$numb.ini))/(max(Data$numb.ini)-min(Data$numb.ini))
Shapes_krs$unemployment <- (Data$unemployment-min(Data$unemployment, na.rm = TRUE))/(max(Data$unemployment, na.rm = TRUE)-min(Data$unemployment, na.rm = TRUE))
Shapes_krs$gender.ratio <- (Data$gender.ratio-min(Data$gender.ratio))/(max(Data$gender.ratio)-min(Data$gender.ratio))
Shapes_krs$young.per <- (Data$young.per-min(Data$young.per))/(max(Data$young.per)-min(Data$young.per))
Shapes_krs$abitur.per <- (Data$abitur.per-min(Data$abitur.per, na.rm = TRUE))/(max(Data$abitur.per, na.rm = TRUE)-min(Data$abitur.per, na.rm = TRUE))
Shapes_krs$GDP.cap <- (Data$GDP.cap-min(Data$GDP.cap, na.rm = TRUE))/(max(Data$GDP.cap, na.rm = TRUE)-min(Data$GDP.cap, na.rm = TRUE))
Shapes_krs$pop.dens <- (Data$pop.dens-min(Data$pop.dens, na.rm = TRUE))/(max(Data$pop.dens, na.rm = TRUE)-min(Data$pop.dens, na.rm = TRUE))
Shapes_krs$east <- (Data$east-min(Data$east))/(max(Data$east)-min(Data$east))
Shapes_krs$refugee.ratio <- (Data$refugee.ratio-min(Data$refugee.ratio, na.rm = TRUE))/(max(Data$refugee.ratio, na.rm = TRUE)-min(Data$refugee.ratio, na.rm = TRUE))

### Set color (for dots?)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Refugee map"),
  
  p("The markers are refugee initiatives in Germany. Click on the marker to 
    see additional information. You can color the administrative districts (Landkreise)
    depending on the variable you select in the dropdown menu. The map was created by
    Christopher Cosler and Lisa Schmid and is part of a larger project trying 
    identify the determinants of refugee initiatives in Germany
    (https://github.com/ChristopherCosler/CSSR_DataAnalysis)"),
  
  selectInput("data", "Data per district:",
              choices = 
                c("Unemployment ratio",
                  "Gender ratio",
                  "Young people ratio",
                  "Abitur ratio",
                  "GDP per capita",
                  "Population density",
                  "East-West",
                  "Refugee ratio",
                  "Refugee initiatives"
                )),
  
  leafletOutput("mymap")
  
  
  )

server <- function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$data,
           "Refugee initiatives" = Shapes_krs$numb.ini,
           "Unemployment ratio" = Shapes_krs$unemployment,
           "Gender ratio" = Shapes_krs$gender.ratio,
           "Young people ratio" = Shapes_krs$young.per,
           "Abitur ratio" = Shapes_krs$abitur.per,
           "GDP per capita" = Shapes_krs$GDP.cap,
           "Population density" = Shapes_krs$pop.dens,
           "East-West" = Shapes_krs$east,
           "Refugee ratio" = Shapes_krs$refugee.ratio
    )
  })
  
  points <- eventReactive(input$recalc, {
    cbind(data$Longitude, data$Latitude)
  }, ignoreNULL = FALSE)
  
  pop <- eventReactive(input$recalc, {
    paste(data$Names)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=Shapes_krs, weight=2, fillOpacity = 0.8, 
                  smoothFactor = 0.5, 
                  color = ~colorBin("YlOrBr", bins = c(0,0.1,0.2,0.3,0.5,0.7,0.9,1), pretty = TRUE,
                                    na.color = "white", Shapes_krs)
                  (datasetInput() )) %>%
      
      addMarkers(data=points(), popup= paste(data$Names)) 
    
    
  })
}

shinyApp(ui, server)