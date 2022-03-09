## Version: 4.15.2021
#  Author: James Fahlbusch

library(shiny)
library(leaflet)
library( magrittr )
library(tidyverse)
library(dplyr)
#### Setup ####
## Global Options
leafletHeight <- 650
## Load Data
load(file='./20180621-CEE2.Rdata')
PBbeg <-plotData %>% filter(.$name == "PB-Beg") %>% .$dttz
PBend <-plotData %>% filter(.$name == "PB-End") %>% .$dttz
obsDate <- as.Date(PBbeg) # Date of CEE

plotData <- plotData %>% 
  mutate(label = case_when(platform == "Hydrophones" ~ "Acoustic recorders",
                           platform == "Soundsource" ~ "Sonar source",
                           platform == "Theodolite" ~ "Second subgroup, theodolite track*",
                           platform == "UAS" ~ "Focal subgroup, drone track",
                           TRUE ~ "Default"))

if(!exists("buoydata")){
  buoydata <-subset(plotData,plotData$platform %in% c("Hydrophones"))}
buoyRadius <- plotData[plotData$platform %in% c('Hydrophones'), ]
buoyRadius$platform <- "Acoustic Recorder (1km radius)"
ssRadius <- plotData[plotData$platform %in% c('Soundsource'), ]
ssRadius$platform <- "Sonar Source (1km radius)"
# Create a combined set for the 1km radius
radiiPlot <- rbind(ssRadius, buoyRadius)
radiiPlot <- radiiPlot[order(radiiPlot$dttz),]
#remove recovery locations
radiiPlot <- subset(radiiPlot, !str_detect(radiiPlot$name,"R\\d") & !str_detect(radiiPlot$name,"PB-End")) 
row.names(radiiPlot) <- 1:nrow(radiiPlot)
radiiPlot <- radiiPlot[order(radiiPlot$dttz),]

## Static Map Setup
##PB
timeMin <- PBbeg
timeMax <- PBend
sites <- subset(plotData, plotData$dttz >= timeMin & plotData$dttz <= timeMax & !plotData$platform %in% c("Hydrophones", "Soundsource") ) 
deps <- plotData %>% filter(plotData$dttz <= PBend & plotData$platform %in% c("Hydrophones", "Soundsource") ) %>%
  filter(.,!duplicated(.$name,fromLast=TRUE)) %>%
  filter(.,!duplicated(str_detect(.$name,"B1"),fromLast=TRUE)&str_detect(.$name,"B1") | 
           !duplicated(str_detect(.$name,"B2"),fromLast=TRUE)&str_detect(.$name,"B2") | 
           !duplicated(str_detect(.$name,"B3"),fromLast=TRUE)&str_detect(.$name,"B3") | 
           !duplicated(str_detect(.$name,"PB"),fromLast=TRUE)&str_detect(.$name,"PB") & .$dttz >= timeMin) #%>%
# filter(!.$name %in% c("PB-End")) # | !str_detect(.$name,"R\\d"))
sites <- rbind(sites, deps)
sites$exp <- "Exposure Period"
##PRE
timeMin <- PBbeg-600
timeMax <- PBbeg
sitesPre <- subset(plotData, plotData$dttz >= timeMin & plotData$dttz <= timeMax & !plotData$platform %in% c("Hydrophones", "Soundsource") ) 
depsPre <- plotData %>% filter(plotData$dttz <= PBend & plotData$platform %in% c("Hydrophones")) %>% #, "Soundsource") ) %>%
  filter(.,!duplicated(.$name,fromLast=TRUE)) %>%
  filter(.,!duplicated(str_detect(.$name,"B1"),fromLast=TRUE)&str_detect(.$name,"B1") | 
           !duplicated(str_detect(.$name,"B2"),fromLast=TRUE)&str_detect(.$name,"B2") | 
           !duplicated(str_detect(.$name,"B3"),fromLast=TRUE)&str_detect(.$name,"B3") | 
           !duplicated(str_detect(.$name,"PB"),fromLast=TRUE)&str_detect(.$name,"PB") & .$dttz >= timeMin) #%>%
# filter(!.$name %in% c("PB-End")) # | !str_detect(.$name,"R\\d"))
sitesPre <- rbind(sitesPre, depsPre)
sitesPre$exp <- "Pre-Exposure"
##Post
timeMin <- PBend
timeMax <- PBend+600
sitesPost <- subset(plotData, plotData$dttz >= timeMin & plotData$dttz <= timeMax & !plotData$platform %in% c("Hydrophones", "Soundsource") ) 
depsPost <- plotData %>% filter(plotData$dttz <= PBend & plotData$platform %in% c("Hydrophones")) %>% #, "Soundsource") ) %>%
  filter(.,!duplicated(.$name,fromLast=TRUE)) %>%
  filter(.,!duplicated(str_detect(.$name,"B1"),fromLast=TRUE)&str_detect(.$name,"B1") | 
           !duplicated(str_detect(.$name,"B2"),fromLast=TRUE)&str_detect(.$name,"B2") | 
           !duplicated(str_detect(.$name,"B3"),fromLast=TRUE)&str_detect(.$name,"B3") | 
           !duplicated(str_detect(.$name,"PB"),fromLast=TRUE)&str_detect(.$name,"PB") & .$dttz >= timeMin) #%>%
# filter(!.$name %in% c("PB-End")) # | !str_detect(.$name,"R\\d"))
sitesPost <- rbind(sitesPost, depsPost)
sitesPost$exp <- "Post-Exposure"
labelsPre <- plotData %>% filter(plotData$dttz >= PBbeg-600 & plotData$dttz <= PBbeg & !plotData$platform %in% c("Hydrophones", "Soundsource")) %>% 
  filter(.,!duplicated(.$platform,fromLast=FALSE)) #| !duplicated(.$platform,fromLast=TRUE))
labelsPost <- plotData %>% filter( plotData$dttz >= PBend & plotData$dttz <= PBend+600 & !plotData$platform %in% c("Hydrophones", "Soundsource")) %>% 
  filter(.,!duplicated(.$platform,fromLast=TRUE)) #| !duplicated(.$platform,fromLast=FALSE))
labelsCEE<- plotData %>% filter(plotData$dttz >= PBbeg & plotData$dttz <= PBend & !plotData$platform %in% c("Hydrophones", "Soundsource")) %>% 
  filter(.,!duplicated(.$platform,fromLast=FALSE)| !duplicated(.$platform,fromLast=TRUE))

# Shiny Page ####
shinyApp(
  ui = fluidPage(
    fixedRow(
      column(12, 
             titlePanel("Controlled Sonar Exposure Experiment, June 21 2018"),
             # titlePanel("Long-beaked common dolphins"),
             align= "left"),
             mainPanel(h3("Long-beaked common dolphins", align= "left"),
                       p("Quick-look and dynamic maps of the experimental components described in the manuscript 'Integrating remote sensing methods during controlled exposure experiments to quantify group responses of dolphins to Navy sonar', submitted to Marine Pollution Bulletin."))
      ), 
    tabsetPanel(
      tabPanel("Quick-Look", fluid = TRUE,
               # fluidRow(      
               #   mainPanel(width=12,
               #             p("Possible description or additional txt")
               #   )
               # ), 
               mainPanel(
                 leafletOutput("MapPlotStatic", height = leafletHeight), width = 12# align= "center", #this could be changed to fill the screen better
               ),
               fluidRow(      
                 mainPanel(width=12,
                           p("*Theodolite tracking was of a second sub-group, as part of the shore-based overview tracking of 2-5 subgroups that split and rejoined during the course of the experiment")
                 )
               )
      ),
      tabPanel("Dynamic Map", fluid = TRUE,
           mainPanel(
             fluidRow(      
               mainPanel(width=12,
                         p("Use the time slider to select periods of the experiment; press the play button on the right to animate.")
               )
             ), 
             sliderInput(inputId = "time", 
                         label = "Time",
                         timeFormat = '%T',
                         animate = animationOptions(interval = 20, loop = FALSE, playButton = NULL,pauseButton = NULL),
                         width = '100%',
                         min = min(plotData$dttz,na.rm = TRUE), max = max(plotData$dttz,na.rm = TRUE), 
                         value = c(min(plotData$dttz,na.rm = TRUE),min(plotData$dttz,na.rm = TRUE)+30), step = 1),

             leafletOutput("MapPlot1", height = leafletHeight),  
             fluidRow(      
               mainPanel(width=12,
                         p("*Theodolite tracking was of a second sub-group, as part of the shore-based overview tracking of 2-5 subgroups that split and rejoined during the course of the experiment")
               )
             ),
             width = 12 # align= "center",
           )
       )
    )
  ),
  
  server = function(input, output, session) {
    ## Static Map ####
    output$MapPlotStatic <- renderLeaflet({
      leaflet(options=leafletOptions(minZoom = 11, maxZoom = 16)) %>% 
        addTiles(group = "Ocean") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(minZoom = 10, maxZoom = 20)) %>%
        #addProviderTiles("Esri.NatGeoWorldMap", group = "NatGeoWorld", options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>% 
        addProviderTiles("Esri.OceanBasemap", group = "Ocean",options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>%
        addScaleBar(position = "topright")  %>%
        addMiniMap(tiles = providers$Esri.OceanBasemap, toggleDisplay = TRUE, width = 100, height = 100, position = 'bottomleft') %>%
        addLegend(title="Legend",position = "bottomright", 
                  colors=c("navy","red","purple","yellow"), 
                  labels=c("Focal subgroup, drone track","Second subgroup, theodolite track*",
                           "Sonar source","Acoustic recorders")
                  # colors=unique(plotData$color), 
                  # labels=unique(plotData$label)
                  ) %>%
        
        # Layers control
        addLayersControl(
          baseGroups = c("Ocean (default)", "Topo"), #, "NatGeoWorld"
          overlayGroups = c("Pre-Exposure","Exposure Period","Post-Exposure","Acoustic recorder (1km radius)"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright"
        ) %>%
        addMeasure(
          position = "topleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479") %>%
        #sets start location to relevant lat/long
        setView(lng = mean(plotData$long), lat = mean(plotData$lat), zoom = 13) %>% 
        hideGroup("Acoustic recorder (1km radius)") %>% 
        addCircleMarkers(lng = sitesPre$long,
                         lat = sitesPre$lat,
                         popup = sitesPre$name,
                         radius = sitesPre$pSize, 
                         color = sitesPre$color,
                         label= paste0(sitesPre$platform," - ", as.character(sitesPre$dttz)),
                         labelOptions = labelOptions(noHide=F),
                         weight = 1.5, opacity = 0.25, fill = TRUE, 
                         fillColor = ifelse(sitesPre$dttz>=PBbeg & sitesPre$dttz<=PBend & !sitesPre$platform %in% c("Hydrophones", "Soundsource"),"white",sitesPre$color),
                         stroke = TRUE, 
                         fillOpacity = ifelse(sitesPre$platform %in% c("Hydrophones", "Soundsource"),.8,.2), 
                         group = sitesPre$exp #c(sitesPre$platform,sitesPre$exp)
        )%>%
        addLabelOnlyMarkers(lng = labelsPre$long,
                            lat = labelsPre$lat,
                            label =  as.character(format(PBbeg-600,format="%T")),
                            labelOptions = labelOptions(textsize = "13px",direction="left", offset = c(-5, 0),noHide=TRUE, textOnly = FALSE),
                            group = "Pre-Exposure"
        )%>% 
        addCircleMarkers(lng = sitesPost$long,
                         lat = sitesPost$lat,
                         popup = sitesPost$name,
                         radius = sitesPost$pSize, 
                         color = sitesPost$color,
                         label= as.character(sitesPost$dttz),
                         labelOptions = labelOptions(noHide=F),
                         weight = 1.5, opacity = 0.25, fill = TRUE, fillColor = ifelse(sitesPost$dttz>=PBbeg & sitesPost$dttz<=PBend & !sitesPost$platform %in% c("Hydrophones", "Soundsource"),"white",sitesPost$color),
                         stroke = TRUE, fillOpacity = ifelse(sitesPost$platform %in% c("Hydrophones", "Soundsource"),.8,.2), group = sitesPost$exp #c(sitesPost$platform,sitesPost$exp)
        )%>%
        addLabelOnlyMarkers(lng = labelsPost$long,
                            lat = labelsPost$lat,
                            label =  as.character(format(PBend+600,format="%T")),
                            labelOptions = labelOptions(style = list("align"="center"),textsize = "13px",direction="right", offset = c(5, 0),noHide=TRUE,textOnly = FALSE),
                            group = "Post-Exposure"
        )%>% 
        addCircles(lng = deps$long,
                   lat = deps$lat,
                   opacity = 0.1, weight = 1, 
                   fill = TRUE, fillOpacity = 0.3,
                   fillColor = deps$color,
                   popup = ifelse(deps$platform=="Hydrophones","Acoustic recorder (1km radius)","Sonar source (1km radius)"),
                   radius = 1000, 
                   group = ifelse(deps$platform=="Hydrophones","Acoustic recorder (1km radius)","Exposure Period")
        )%>%
        addLabelOnlyMarkers(lng = deps$long[4], #"<br/>"
                            lat = deps$lat[4],
                            label =  HTML(paste0("Exposure Time<br/>",
                                                 as.character(format(PBbeg,format="%T")), " - ",as.character(format(PBend,format="%T"))
                            )),
                            labelOptions = labelOptions(textsize = "14px",direction="top", offset = c(0, -15),noHide=TRUE,textOnly = FALSE),
                            group = "Exposure Period"
        ) %>%
        addCircleMarkers(lng = sites$long,
                         lat = sites$lat,
                         popup = sites$name,
                         radius = sites$pSize, 
                         color = sites$color,
                         label= as.character(sites$dttz),
                         labelOptions = labelOptions(noHide=F),
                         weight = 1.5, opacity = 0.5, fill = TRUE, fillColor = ifelse(sites$dttz>=PBbeg & sites$dttz<=PBend & !sites$platform %in% c("Hydrophones", "Soundsource"),"white",sites$color),
                         stroke = TRUE, fillOpacity = ifelse(sites$dttz>=PBbeg & sites$dttz<=PBend|sites$platform %in% c("Hydrophones", "Soundsource"),.8,.5), group = sites$exp
        )
    })
    ## Dynamic Map ####
    output$MapPlot1 <- renderLeaflet({
        leaflet() %>% 
        addTiles(group = "Ocean") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        addProviderTiles("Esri.OceanBasemap", group = "Ocean") %>%
        addScaleBar(position = "topright")  %>%
        addLegend(position = "bottomright", 
                  colors=c("navy","red","purple","yellow"), 
                  labels=c("Focal subgroup, drone track","Second subgroup, theodolite track*",
                           "Sonar source","Acoustic recorders")) %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Ocean (default)", "Topo"),
          overlayGroups = c(sort(unique(plotData$label)),
                            "Sonar source (1km radius)","Acoustic recorder (1km radius)"),
          options = layersControlOptions(collapsed = TRUE),
          position = "topright"
        ) %>%
        addMeasure(
          position = "topleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D", completedColor = "#7D4479") %>%
        #sets start location to relevant lat/long
        setView(lng = mean(plotData$long), lat = mean(plotData$lat), zoom = 13) %>% 
        hideGroup("Acoustic recorder (1km radius)") 
    })
    observe({
      timeMin <- input$time[1]
      timeMax <- input$time[2]
      sites <- subset(plotData, plotData$dttz >= timeMin & plotData$dttz <= timeMax & 
                        !plotData$platform %in% c("Hydrophones", "Soundsource") ) 
      deps <- plotData %>% filter(plotData$dttz <= timeMax & 
                                    plotData$platform %in% c("Hydrophones", "Soundsource") ) %>%
        filter(.,!duplicated(.$name,fromLast=TRUE)) %>%
        filter(.,!duplicated(str_detect(.$name,"B1"),fromLast=TRUE)&str_detect(.$name,"B1") | 
                 !duplicated(str_detect(.$name,"B2"),fromLast=TRUE)&str_detect(.$name,"B2") | 
                 !duplicated(str_detect(.$name,"B3"),fromLast=TRUE)&str_detect(.$name,"B3") | 
                 !duplicated(str_detect(.$name,"PB"),fromLast=TRUE)&str_detect(.$name,"PB") & 
                 .$dttz >= timeMin) #%>%
       # filter(!.$name %in% c("PB-End")) # | !str_detect(.$name,"R\\d"))
      sites <- rbind(sites, deps)
      if(nrow(deps) != 0 && nrow(sites)!=0) { 
        leafletProxy("MapPlot1", session) %>% 
          clearMarkers() %>% 
          clearShapes() %>%
          addCircles(lng = deps$long,
                     lat = deps$lat,
                     color = deps$color,
                     opacity = 0.1, weight = 1, fill = TRUE, 
                     fillColor = deps$color,
                     fillOpacity = ifelse(max(deps$dttz)>=PBbeg && max(deps$dttz)<=PBend && 
                                            deps$platform == "Soundsource" ,.25,
                                          ifelse(deps$platform == "Hydrophones",.25,0   )),
                     popup = ifelse(deps$platform=="Hydrophones","Acoustic recorder (1km radius)","Sonar source (1km radius)"),
                     # popup = paste(deps$name,deps$dttz,sep = " - "),
                     radius = 1000, 
                     group = ifelse(deps$platform=="Hydrophones","Acoustic recorder (1km radius)","Sonar source (1km radius)")
          )%>%  
          addCircleMarkers(lng = sites$long,
                           lat = sites$lat,
                           # popup = sites$name,
                           popup = paste0(sites$label,"<br/>", sites$dttz),
                           radius = sites$pSize, 
                           color = sites$color,
                           weight = 1.5, opacity = 0.25, 
                           fill = TRUE, 
                           fillColor = ifelse(sites$dttz>=PBbeg & 
                                                sites$dttz<=PBend & 
                                                !sites$platform %in% c("Hydrophones", "Soundsource"),"white",sites$color),
                           stroke = TRUE, 
                           fillOpacity = ifelse(sites$dttz>=PBbeg & 
                                                  sites$dttz<=PBend|sites$platform %in% c("Hydrophones", "Soundsource"),.8,.2), 
                           group = sites$label
          )
        
        } 
      else if(nrow(deps) == 0) {
        leafletProxy("MapPlot1", session) %>% 
          clearShapes() %>%
          addCircles(lng = deps$long,
                     lat = deps$lat,
                     fillColor = deps$color,
                     opacity = 0.1, weight = 1, fill = TRUE, fillOpacity = 0.25,
                     popup = paste(deps$name,deps$dttz,sep = " - "),
                     radius = 1000,
                     group = ifelse(deps$platform=="Hydrophones","Acoustic recorder (1km radius)","Sonar source (1km radius)")
          )
        
      }
      else if(nrow(sites) == 0) {
        leafletProxy("MapPlot1", session) %>% 
          clearMarkers() #%>% 
          # addCircleMarkers(lng = sites$long,
          #                  lat = sites$lat,
          #                  popup = paste0(sites$label,"<br/>", sites$dttz),
          #                  radius = sites$pSize, 
          #                  color = sites$color,
          #                  weight = 1.5, opacity = 0.25, 
          #                  fill = TRUE, 
          #                  fillColor = ifelse(sites$dttz>=PBbeg & 
          #                                       sites$dttz<=PBend & 
          #                                       !sites$platform %in% c("Hydrophones", "Soundsource"),"white",sites$color),
          #                  stroke = TRUE, 
          #                  fillOpacity = ifelse(sites$dttz>=PBbeg & 
          #                                         sites$dttz<=PBend|sites$platform %in% c("Hydrophones", "Soundsource"),.8,.2), 
          #                  group = sites$label
          # )
        
      }
      else{ 
        }

    })
    
  }
    
)
