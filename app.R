library(tidyverse)
library(chorddiag)
library(hms)
library(shiny)


datos <- read_csv2("data/matriz_viajes.csv")
horas <- unique(datos$MediaHora)
horast <- hms(horas)
names(horas) <- horast
comunas <- unique(datos$ComunaSubida)

ui <- shinyUI(
  fluidPage(
  br(),
  br(),
  selectInput("n_hora", label = "Hora de Salida",
              choices = horas, selected = 27000),
  chorddiagOutput("distPlot", height = 400)
))


server <- shinyServer(function(input, output) {
  
  
  output$distPlot <- renderChorddiag({
    
    library(chorddiag)
    library(tidyverse)
    library(hms)
    

    
      
    datos <- datos %>% 
      filter(MediaHora == input$n_hora) %>%
      select(1,2,4) %>%
      mutate(ComunaSubida = factor(ComunaSubida, levels = comunas),
             ComunaBajada = factor(ComunaBajada, levels = comunas)) %>% 
      mutate_if(is.character, str_to_title) %>%
      spread(value = ViajeLaboralPromedio , key= ComunaBajada, drop = F, fill = 0) %>%
      column_to_rownames("ComunaSubida") %>%
      as.matrix()
    
    ticks <- ifelse(max(datos)>1000,500, 10)
    
    chorddiag(
      datos,
      groupnameFontsize = 8,
      ticklabelFontsize = 6,
      tickInterval = ticks,
      showZeroTooltips = F,  
      chordedgeColor = NA
    )
    
  })
  
})

shinyApp(ui, server)