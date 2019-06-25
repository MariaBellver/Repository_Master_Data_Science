library(shiny) 
library(shinythemes)
library(raster)
library(tmap)
library(tmaptools)
library(leaflet)
library(leaflet.extras)



load("MunComVAL.RData", verbose=TRUE)
breaks<-c(0,100,500,1000,5000,10000,30000,50000,100000,500000,800000)
paleta<-tmaptools::get_brewer_pal("Oranges", n = 10, contrast = c(0, 0.9))

ui_maps <- fluidPage(titlePanel("Población de la Comunidad Valenciana"),
                     sidebarLayout(
                       sidebarPanel = sidebarPanel(
                         selectInput( "Select_prov", "Selecciona la provincia:",
                                      c("Alicante"="Alicante",
                                        "Castellón"="Castellón", 
                                        "Valencia"=  "Valencia")),
                         sliderInput("Year", "Selecciona el año:",
                                     min=1998,
                                     max=2018,
                                     value=1998,
                                     step=1,
                                     ticks = T)
                       ),
                       mainPanel(leafletOutput("map"))
                     )
)


server_maps<-function(input, output) {
  output$map<- renderLeaflet({
    
    mapa<-tm_shape(MunComVAL[MunComVAL@data$"NAME_2"==input$Select_prov,])+
      tm_fill(paste0(input$Year),palette=paleta,convert2density = F,
              style = "fixed",breaks = breaks,group = "Municipios")+
      tm_borders(col="grey",lwd=0.3,alpha=0.9)+ 
      tm_layout( legend.show = T,legend.outside = T,
                 legend.outside.position = c("right","bottom"),
                 main.title = "Población por municipios",
                 main.title.position  = c("center","top"))
    
    tmap_leaflet(mapa)   
  })
}

shinyApp(ui_maps, server_maps)