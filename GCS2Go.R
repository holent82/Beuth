###
#   Version 1.6
###

# installiert die benötigten Packages (falls nicht bereits erfolgt)
packages_benoetigt <- c("DT","ggplot2","scales","shiny","shinyjs", "shinydashboard", "shinythemes", "sqldf", "tidyverse", "readxl", "writexl", "shinyFeedback", "shinyTime", "lubridate", "scales")
packages_neu <- packages_benoetigt[!(packages_benoetigt %in% installed.packages()[,"Package"])]
if(length(packages_neu)) install.packages(packages_neu, dependencies = TRUE)

# Packages werden geladen
library(DT)
library(ggplot2)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(sqldf)
library(tidyverse)
library(readxl)
library(writexl)
library(shinyFeedback)
library(shinyTime)
library(lubridate)
library(scales)
library(anytime)

# deutsches Format für Datum
Sys.setlocale("LC_ALL", "German")

#Dateipfad der Datenbank
patientenname = "C:/Users/Xenia Kobeleva/Dropbox/Beuth/patienten_daten.xlsx"
gcsname = "C:/Users/Xenia Kobeleva/Dropbox/Beuth/gcs_daten.xlsx"

#Funktion zur Validierung
leer = function(a){
  ((is.na(a)) | is.null(a))
}  

#Funktion zum Einlesen der Datenbanken zum Abgleich
daten_einlesen <- function(){
  # Einlesen der Patienten-Stammdaten
  patientenframe <<- read_excel(patientenname)
  # Einlesen der GCS-Daten
  gcsframe <<- read_excel(gcsname)
  # Datenformatierung
  gcsframe$datum          <<- as.Date(gcsframe$datum, origin="1970-01-01", format = "%d.%m.%Y")
  gcsframe$gcs_auge       <<- as.integer(gcsframe$gcs_auge)
  gcsframe$gcs_verbal     <<- as.integer(gcsframe$gcs_verbal)
  gcsframe$gcs_motorisch  <<- as.integer(gcsframe$gcs_motorisch)
  gcsframe$zeit           <<- format(gcsframe$zeit, format = "%H:%M:%S")
}

# Initialisieren der Daten
x <- tryCatch(
  {
    daten_einlesen()
  },
  error = function(e){
    winDialog(type = c("ok", "okcancel", "yesno", "yesnocancel"), "Datenbank hochladen fehlgeschlagen. Bitte manuell die Datenbanken auswählen")
    patientenname=  choose.files(caption = "Bitte die Datei patienten_daten.xlsx auswählen")
    gcsname=  choose.files(caption = "Bitte die Datei gcs_daten.xlsx auswählen")
  }
)

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

# Farben für die Grafiken
colorscale = c("red", "darkgreen", "blue")

# Benutzereingabenpanel
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  
  theme = shinytheme("united"),
  
  dashboardPage(
    dashboardHeader(title = "GCS2Go"),
    
    dashboardSidebar(
      verbatimTextOutput(outputId = "patient_gewaehlt"),
      
      sidebarUserPanel("Patient",
                       subtitle = a(textOutput('patiententitel'),style="color:red")
      ),
      sidebarMenu(
        id = "tabs",
        menuItem("Patient", icon = icon("user"),
                 menuSubItem("Patient anlegen", tabName = "patientanlegen", icon = icon("user-tag")),
                 menuSubItem("Patient suchen", tabName = "patientsuchen", icon = icon("search"))
        ),
        menuItem("Grafik", icon = icon("bar-chart-o"),
                 menuSubItem("GCS", tabName = "gcstab")
        )         
      )  
    ),
    
    dashboardBody(
      div(id = "form",
          tabItems(
            tabItem("patientanlegen", h3("Patientendaten"),
                    numericInput("pid", label="Patienten-ID", value = "", width = "200px"),
                    textInput("vorname", label="Vorname", value = "", width = "200px", placeholder = NULL),
                    textInput("name", label="Name", value = "", width = "200px", placeholder = NULL),
                    h3("GCS"),
                    dateInput("datum", label="Untersuchungsdatum", value = "", width = "200px"),
                    timeInput("zeit", "Untersuchungszeit", value = strptime("", "%T"), minute.steps = 5),
                    selectInput("gcs_auge", label = "Öffnen der Augen", choices = choices_auge),
                    selectInput("gcs_verbal", label = "Beste verbale Reaktion", choices= choices_verbal ),
                    selectInput("gcs_motorisch", label = "Beste motorische Reaktion", choices=  choices_motorisch),
                    actionButton("save", "Daten speichern",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    actionButton("delete", "Daten löschen")),
            tabItem("patientsuchen", h3("Patienten suchen"),
                    dataTableOutput('selection'),
                    actionButton("auswahl_suchen", "Patient auswählen", width = "50%")),
            tabItem("gcstab",
                    textOutput("gcsSummaryPlot"),
                    plotOutput("gcsAugePlot",width="70%",height="200px"),
                    plotOutput("gcsVerbalPlot",width="70%",height="200px"),
                    plotOutput("gcsMotorischPlot",width="70%",height="200px"),
                    plotOutput("gcsTotalPlot",width="70%",height="200px"))
          )
      )
    )
  )
)


# Server Output

server <- function(input, output, session) {
  output$patiententitel <- renderText(paste("Kein Patient gewählt"))
  output$selection <- DT::renderDataTable(patientenframe, selection = 'single')
  
  # Prüfen identischer Eingaben
  values <- reactiveValues(identical_pid = 0)
  
  test_pid<- observeEvent(input$save, {
    identical_pid<- sum(as.numeric(c(FALSE, sapply(1:nrow(patientenframe), function(i)
      any((patientenframe$pid[i] == input$pid && patientenframe$name[i] != input$name) || (patientenframe$pid[i] == input$pid && patientenframe$vorname[i] != input$vorname) )))))
  

  
     feedbackWarning(
       inputId = "pid",
       leer(input$pid)==TRUE,
       text = "Bitte Patienten-ID eingeben, nur Zahlen erlaubt"
     )
     
     
     feedbackWarning(
       inputId = "pid",
       is.numeric(input$pid)==FALSE,
       text = "Bitte nur Zahlen für Patienten-ID eingeben"
     )
     
    req(input$pid)
      feedbackWarning(
        inputId = "pid",
        is.numeric(input$pid)==FALSE && identical_pid == 0,
        text = "Bitte nur Zahlen für Patienten-ID eingeben"
      )
      
      
      feedbackDanger(
        inputId = "pid",
        is.numeric(input$pid)==TRUE && identical_pid == 1,
        text = "PID bereits vergeben"
      )
    
  })
  
  test_vorname<- observeEvent(input$save, {
  feedbackWarning(
    inputId = "vorname",
    condition = input$vorname== "",
    text = "Bitte Vornamen eingeben"
  )
  })
  
  test_name<- observeEvent(input$save, {
    feedbackWarning(
      inputId = "name",
      condition = input$name== "",
      text = "Bitte Namen eingeben"
    )
  })
  
  test_datum<- observeEvent(input$save, {
  feedbackWarning(
    inputId = "datum",
    condition = leer(input$datum) == TRUE,
    text = "Bitte Untersuchungsdatum eingeben"
  )
  })
  
  test_zeit<- observeEvent(input$save, {
    feedbackWarning(
      inputId = "zeit",
      condition = leer(input$zeit) == TRUE,
      text = "Bitte Untersuchungszeit eingeben"
    )
  })
  
  test_gcs_auge<- observeEvent(input$save, {
  feedbackWarning(
    inputId = "gcs_auge",
    condition = input$gcs_auge== "",
    text = "Bitte GCS (Auge) eingeben"
  )
  })
  
  test_gcs_verbal<- observeEvent(input$save, {
  feedbackWarning(
    inputId = "gcs_verbal",
    condition = input$gcs_verbal== "",
    text = "Bitte GCS (verbal) eingeben"
  )
  })
  
  test_gcs_motorisch<- observeEvent(input$save, {
  feedbackWarning(
    inputId = "gcs_motorisch",
    condition = input$gcs_motorisch== "",
    text = "Bitte GCS (motorisch) eingeben"
  )
  })
  
  dataModal <- function(failed = FALSE){
    modalDialog(
      title = "Daten verifizieren",
      "Bitte überprüfen Sie die Daten!",
      
      renderText({paste("PID: ", input$pid)}),
      renderText({paste("Name: ",input$name)}),
      renderText({paste("Vorname: ",input$vorname)}),
      renderText({paste("Datum: ",input$datum)}),
      renderText({paste("Zeit: ",input$zeit)}),
      renderText({paste("Score Auge: ",input$gcs_auge)}),
      renderText({paste("Score Verbal: ",input$gcs_verbal)}),
      renderText({paste("Score Motorisch: ",input$gcs_motorisch)}),
      
      footer = tagList(
        actionButton("modal_cancel", "Abbrechen"),
        actionButton("modal_ok", "Daten bestätigen")
      )
    )
  }
  
  observeEvent(input$modal_cancel, {
    dataModal(failed = TRUE)
    removeModal()
  })
  
  
  observeEvent(input$modal_ok, {
    
    patient <-
      data.frame(
        vorname = input$vorname,
        name = input$name,
        pid = input$pid
      )
    
    # neue Datenreihe wird an bestehenden Datensatz angebunden
    patientenframe_neu <- rbind.data.frame(patientenframe, patient, stringsAsFactors = FALSE)
    write_xlsx(patientenframe_neu, path = patientenname)
    gcs<- data.frame(pid=input$pid, datum= input$datum, zeit=  format(input$zeit, format = "%H:%M:%S"), gcs_auge=input$gcs_auge, gcs_verbal=input$gcs_verbal, gcs_motorisch=input$gcs_motorisch, stringsAsFactors = FALSE)
    gcsframe_neu <- rbind.data.frame(gcsframe, gcs,stringsAsFactors = FALSE)
    gcsframe_neu$datum <- as.Date(gcsframe_neu$datum, format = "%Y-%m-%d", origin="1970-01-01")
    write_xlsx(gcsframe_neu, path = gcsname)
    
    # Aktualisieren der Daten und Graphen
    daten_einlesen()
    output$selection <- DT::renderDataTable(patientenframe, selection = 'single')
    
    gcs_selected <- gcs_selected_anlegen(input$pid)
    patient_selected <- patient_selected_anlegen(input$pid)
    graphen_zeichnen(gcs_selected)
    
    output$patiententitel <- renderText({ paste(patient_selected$vorname, patient_selected$name, ",", patient_selected$pid)})
    output$gcsSummaryPlot <- renderText(paste("Letzte GCS erfolgt am", patient_selected$datum))
    removeModal()
  })
  
  # Erstellung einer neuen Datenreihe
  neuerPatient <- observeEvent(input$save, {
    req(input$pid)
    req(input$vorname)
    req(input$name)
    
    req(input$datum)
    req(input$zeit)
    req(input$gcs_auge)
    req(input$gcs_verbal)
    req(input$gcs_motorisch)
    
    showModal(dataModal())
    
  })
  
  # Löschen der Eingaben
  observeEvent(input$delete, {
    reset("form")
  })
  
  
 
  
  #Aufruf aus Anlage-Formular
  
  patient_selected_anlegen <- function(patientenframe_source){
    patient_selected <- filter(patientenframe, pid == patientenframe_source)
    return(patient_selected)
  }
  
  patient_selected <- eventReactive(input$auswahl_suchen,{
    req(input$selection_rows_selected)
    row <- input$selection_rows_selected
    patient_selected <- slice(patientenframe, row)
    patient_selected
  })
  
  #Aufruf aus Anlage-Formular 
  gcs_selected_anlegen <- function(gcsframe_source){
    gcs_selected <- filter(gcsframe, pid == gcsframe_source)
    gcs_selected2 <- mutate(gcs_selected, totalScore = gcs_auge+gcs_verbal+gcs_motorisch)
    gcs_selected$zeit= format(gcs_selected$zeit,"%H:%M:%S")
    return(gcs_selected2)
  }
  
  gcs_selected <- eventReactive(input$auswahl_suchen,{
    req(input$selection_rows_selected)
    gcs_selected <- filter(gcsframe, pid == patient_selected()$pid)
    gcs_selected2 <- mutate(gcs_selected, totalScore = gcs_auge+gcs_verbal+gcs_motorisch)
    gcs_selected2
  })
  
  graphen_zeichnen <- function(x){
    x$datumzeit <- paste(x$datum, x$zeit)
    x$datumzeit <- as.POSIXct(x$datumzeit,  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    output$gcsAugePlot <- renderPlot({ggplot(data =  x)  +
        geom_point(mapping = aes(x = datumzeit, y = gcs_auge, color = gcs_auge == 4)) +
        scale_color_manual(values = colorscale) +
        scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
        geom_text(aes(label= gcs_auge, x = datumzeit, y = gcs_auge), position = position_nudge(y = -0.25))}+ 
        ylab("GCS(Öffnen der Augen)")+
        ylim(1,4) +theme_bw()+ ggtitle("GCS(Öffnen der Augen)") +
        theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
    output$gcsVerbalPlot <- renderPlot({ggplot(data =  x) +
        geom_point(mapping = aes(x = datumzeit, y = gcs_verbal, color = gcs_verbal == 5))+
        scale_color_manual(values = colorscale)+
        scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
        geom_text(aes(label= gcs_verbal, x = datumzeit, y = gcs_verbal), position = position_nudge(y = -0.25))}+ 
        ylab("GCS(verbale Antwort)")+
        ylim(1,5) +theme_bw()+ ggtitle("GCS(verbale Antwort)") +
        theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
    output$gcsMotorischPlot <- renderPlot({ggplot(data =  x) +
        geom_point(mapping = aes(x = datumzeit, y = gcs_motorisch, color = gcs_motorisch==6))+
        scale_color_manual(values = colorscale)+ 
        scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
        geom_text(aes(label= gcs_motorisch, x = datumzeit, y = gcs_motorisch), position = position_nudge(y = -0.25))}+ 
        ylab("GCS(motorische Antwort)")+
        ylim(1,6) +theme_bw()+ ggtitle("GCS(motorische Antwort)") +
        theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
    output$gcsTotalPlot <- renderPlot({ggplot(data =  x) +
        geom_point(mapping = aes(x = datumzeit, y = totalScore, color = totalScore ==15)) +
        scale_color_manual(values = colorscale)+ 
        scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
        geom_text(aes(label= totalScore, x = datumzeit, y = totalScore), position = position_nudge(y = -0.75))}+ 
        ylab("GCS(Gesamt)")+
        ylim(1,15) +theme_bw()+ggtitle("GCS(Gesamt)") +
        theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
  }
  
  observeEvent(input$auswahl_suchen,{
  
    graphen_zeichnen(gcs_selected())
    
    ## Daten werden in Inputfelder geschrieben
    updateNumericInput(session, "pid", value = paste(as.integer(patient_selected()$pid)))
    updateTextInput(session, "name", value = paste(patient_selected()$name))
    updateTextInput(session, "vorname", value = paste(patient_selected()$vorname))
  })
  
  
  observeEvent(input$auswahl_suchen, {
    req(patient_selected)
    output$patiententitel <- renderText({ paste(patient_selected()$vorname, patient_selected()$name, ",", patient_selected()$pid)})
    output$gcsSummaryPlot <- renderText({ paste("Letzte GCS erfolgt am", max(gcs_selected()$datum))})  
    
  })
} 

shinyApp(ui = ui, server = server)