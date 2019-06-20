# lädt die benötigten Packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(tidyverse)
library(sqldf)
#library(lubridate)
library(shinyjs)
library(DT)

# deutsches Format für Datum
Sys.setlocale("LC_ALL", "German")

# Einlesen der Datenbanken zum Abgleich
patientenname = "C:/Users/Xenia Kobeleva/Dropbox/Beuth/patienten_daten.xls"
gcsname = "C:/Users/Xenia Kobeleva/Dropbox/Beuth/gcs_daten.xls"

# Einlesen der Patienten-Stammdaten
patientenframe <-
  read.table(
    patientenname,
    fileEncoding = "UTF-8-BOM",
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE
  )

# Einlesen der GCS-Daten
gcsframe <-read.table(gcsname, fileEncoding="UTF-8-BOM", header = TRUE, sep = "\t", stringsAsFactors=FALSE)
# Datumsformattierung
gcsframe$datum <- as.Date(gcsframe$datum, origin="1970-01-01", format = "%d.%m.%Y")
print(gcsframe)

# Definition der Variablen für die Benutzereingaben
choices_auge = c(
  "Bitte wählen" = "",
  "Spontan" = 4,
  "Bei Ansprache" =  3,
  "Bei Schmerzreiz" =  2,
  "Kein Öffnen der Augen" = 1
)

choices_verbal = c(
  "Bitte wählen" = "",
  "Konversationsfähig, orientiert" = 5,
  "Konversationsfähig, desorientiert" = 4,
  "Inadäquate Äußerungen" = 3,
  "Unverständliche Laute" = 2,
  "Keine verbale Antwort" = 1
)

choices_motorisch = c(
  "Bitte wählen" = "",
  "Auf Aufforderung" = 6,
  "Auf Schmerzreiz, gezielt" = 5,
  "Auf Schmerzreiz, Beugeabwehr" = 4,
  "Beugesynergismen" = 3,
  "Strecksynergismen" = 2,
  "Keine motorische Antwort" = 1
)

# Benutzereingabenpanel
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("simplex"),
  
  #Titel
  titlePanel("GCS2Go"),
  sidebarLayout(
    sidebarPanel(
      div(
        id = "form",
        h3("Patientendaten"),
        textInput(
          "pid",
          label = "Patienten-ID",
          value = "",
          width = "400px"
        ),
        
        textInput(
          "vorname",
          label = "Vorname",
          value = "",
          width = "400px",
          placeholder = NULL
        ),
        
        textInput(
          "name",
          label = "Name",
          value = "",
          width = "400px",
          placeholder = NULL
        ),
        
        actionButton("search", "Patienten suchen oder hinzufügen", width = "100%"),
        
        h3("GCS"),
        dateInput(
          "datum",
          label = "Untersuchungsdatum",
          value = "",
          width = "400px",
          format = "dd/mm/yyyy"
        ),
        
        selectInput("gcs_auge", label = "Öffnen der Augen", choices = choices_auge),
        
        selectInput("gcs_verbal", label = "Beste verbale Reaktion",
                    choices = choices_verbal),
        
        selectInput("gcs_motorisch", label = "Beste motorische Reaktion",
                    choices =  choices_motorisch),
        
        actionButton("save", "Speichern"),
        
        actionButton("delete", "Formular leeren")
        
      )
    ),
    mainPanel(   
      DT::dataTableOutput('table')
    )
  )
)


# Server Output

server <- function(input, output) {
  
  # Abgleich mit den vorhandenen Daten über die Suchleiste
  
  
  # Erstellung einer neuen Datenreihe
  neuerPatient <- observeEvent(input$save, {
    patient <-
      data.frame(
        vorname = input$vorname,
        name = input$name,
        pid = input$pid
      )
    
    # neue Datenreihe wird an bestehenden Datensatz angebunden
    patientenframe_neu <- rbind.data.frame(patientenframe, patient, stringsAsFactors = FALSE)
    write.table(
      patientenframe_neu,
      file = patientenname,
      sep = "\t",
      row.names = FALSE)
    return(patientenframe_neu)
  })

  # GCS wird an bestehenden Datensatz angebunden
   neuerGCS <- observeEvent(input$save, {
     # validate(
     #   need(input$datum != "", "Bitte Datum eingeben"),
     #   need(input$gcs_auge != "", "Bitte GCS Augen Öffnen ausfüllen"),
     #   need(input$gcs_verbal != "", "Bitte Datum eingeben"),
     #   need(input$gcs_motorisch != "", "Bitte Datum eingeben")
     # )
     gcs<- data.frame(pid=input$pid, datum= input$datum, gcs_auge=input$gcs_auge, gcs_verbal=input$gcs_verbal, gcs_motorisch=input$gcs_motorisch, stringsAsFactors = FALSE)
     gcsframe_neu <- rbind.data.frame(gcsframe, gcs,stringsAsFactors = FALSE)
     gcsframe_neu$datum <- as.Date(gcsframe_neu$datum, format = "%Y-%m-%d", origin="1970-01-01")
     write.table(gcsframe_neu, file = gcsname, sep = "\t", row.names=FALSE)
     return(gcsframe_neu)
   })
  
   # Löschen der Eingaben
  observeEvent(input$delete, {
    reset("form")
  })
  

}

shinyApp(ui = ui, server = server)
