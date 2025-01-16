# Fixem directori i obrim dades
setwd("C:/Users/guillem.castellano/OneDrive - ctfc.cat/100_UOC/Semestre_3/Visualització de dades/PAC_final")
source("C:/Users/guillem.castellano/OneDrive - ctfc.cat/100_UOC/Semestre_3/Visualització de dades/PAC_final/depuracio.R")

# Llibreries
library(shiny)
library(fastmap)
library(ggplot2)
library(sf)
library(leaflet)

# Càrrega de les dades shapefile i transformacio a WGS84
mapa_municipis <- st_read("divisions-administratives-v2r1-municipis-5000-20230511.shp")
mapa_municipis <- st_transform(mapa_municipis, crs = 4326)  

# Transformar coordenadas d'explotaciones a WGS84
dades_intermig_sf <- st_as_sf(dades_intermig, 
                              coords = c("COORDENADA X EXPLOTACIÓ", "COORDENADA Y EXPLOTACIÓ"), 
                              crs = 25831)  # Asumimos que las coordenadas están en UTM 31N (EPSG:25831)
dades_intermig_sf <- st_transform(dades_intermig_sf, crs = 4326)  # Transformar a WGS84

# Interficie d'usuari
ui <- fluidPage(
  titlePanel("Mapa interactiu de les explotacions ramaderes a Cataluny"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "especie",
        label = "Selecciona una espècie:",
        choices = unique(dades_intermig$`ESPÈCIE`),  # Niveles de ESPÈCIE
        selected = unique(dades_intermig$`ESPÈCIE`)[1]  # Selección inicial
      ),
      selectInput(
        inputId = "tipus",
        label = "Selecciona un tipus d'explotació:",
        choices = unique(dades_intermig$`TIPUS EXPLOTACIÓ`),  # Niveles de TIPUS EXPLOTACIÓ
        selected = unique(dades_intermig$`TIPUS EXPLOTACIÓ`)[1]  # Selección inicial
      ),
      selectInput(
        inputId = "sistema",
        label = "Selecciona un sistema productiu:",
        choices = unique(dades_intermig$`SISTEMA PRODUCTIU`),  # Niveles de SISTEMA PRODUCTIU
        selected = unique(dades_intermig$`SISTEMA PRODUCTIU`)[1]  # Selección inicial
      )
    ),
    
    mainPanel(
      leafletOutput("mapa", width = "100%", height = "800px")  # Salida del mapa interactivo
    )
  )
)

# Servidor
server <- function(input, output) {
  output$mapa <- renderLeaflet({
    datos_filtrados <- dades_intermig_sf[dades_intermig$`ESPÈCIE` == input$especie &
                                           dades_intermig$`TIPUS EXPLOTACIÓ` == input$tipus &
                                           dades_intermig$`SISTEMA PRODUCTIU` == input$sistema, ]
    # Paleta de colors per a URM
    color_pal <- colorNumeric(palette = "YlOrRd", domain = datos_filtrados$`TOTAL URM`, na.color = "transparent")
    
    # Creació del mapa amb leaflet
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%  # Mapa base
      addPolygons(
        data = mapa_municipis,
        color = "black",
        weight = 1,
        fillColor = "lightgray",
        fillOpacity = 0.5,
        label = ~NOMMUNI
      ) %>%
      addCircleMarkers(
        data = datos_filtrados,
        lng = ~st_coordinates(geometry)[, 1],  # Longitud
        lat = ~st_coordinates(geometry)[, 2],  # Latitud
        radius = 5,
        color = ~color_pal(`TOTAL URM`),  
        fillOpacity = 0.7,
        label = ~paste(
          "Espècie:", `ESPÈCIE`, "<br>",
          "Tipus:", `TIPUS EXPLOTACIÓ`, "<br>",
          "Sistema:", `SISTEMA PRODUCTIU`, "<br>",
          "TOTAL URM:", `TOTAL URM`
        )
      ) %>%
      addLegend(
        pal = color_pal,
        values = datos_filtrados$`TOTAL URM`,
        title = "TOTAL URM",
        position = "bottomright"
      )
  })
}

# Execució
shinyApp(ui = ui, server = server)












