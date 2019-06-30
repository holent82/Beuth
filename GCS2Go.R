###
#   Version 1.2
###

# lädt die benötigten Packages
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
#library(lubridate)

# deutsches Format für Datum
Sys.setlocale("LC_ALL", "German")

# Einlesen der Datenbanken zum Abgleich
patientenname = "C:/Users/holen/Documents/GCS2Go2/patienten_daten.xlsx"
gcsname = "C:/Users/holen/Documents/GCS2Go2/gcs_daten.xlsx"

# Einlesen der Patienten-Stammdaten
patientenframe <- read_excel(patientenname)

# Einlesen der GCS-Daten
gcsframe <-read_excel(gcsname)

# Datenformattierung
gcsframe$datum <- as.Date(gcsframe$datum, origin="1970-01-01", format = "%d.%m.%Y")
gcsframe$gcs_auge <- as.integer(gcsframe$gcs_auge)
gcsframe$gcs_verbal <- as.integer(gcsframe$gcs_verbal)
gcsframe$gcs_motorisch <- as.integer(gcsframe$gcs_motorisch)

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
    
    dashboardPage(
        dashboardHeader(title = "Glasgow Coma Scale 2 Go"),
        
        dashboardSidebar(
            verbatimTextOutput(outputId = "patient_gewaehlt"),
            
            ### noch zu Coden,evtl weglassen oder anderes Element ###
            sidebarUserPanel("Patient",subtitle = a(icon("circle", class = "text-success"), "Kein Patient gewÃ¤hlt",style="color:red")),
            
            sidebarMenu(
                id = "tabs",
                menuItem("Patient", icon = icon("user"),
                         menuSubItem("Patient anlegen", tabName = "patientanlegen", icon = icon("user-tag")),
                         menuSubItem("Patient suchen", tabName = "patientsuchen", icon = icon("search"))
                ),
                menuItem("Grafik", icon = icon("bar-chart-o"),
                         menuSubItem("Einzel", tabName = "einzel"),
                         menuSubItem("Gesamt", tabName = "gesamt")
                )         
            )  
        ),
        
        dashboardBody(
            tabItems(
                tabItem("patientanlegen", "Patient anlegen", h3("Patientdaten"),
                        textInput("pid", label="Patienten-ID", value = "", width = "200px"),
                        textInput("vorname", label="Vorname", value = "", width = "200px", placeholder = NULL),
                        textInput("name", label="Name", value = "", width = "200px", placeholder = NULL),
                        actionButton("search", "Patienten anlegen", width = "50%"),
                        h3("GCS"),
                        dateInput("datum", label="Datum", value = "", width = "200px"),
                        selectInput("gcs_auge", label = "Öffnen der Augen", choices = choices_auge),
                        selectInput("gcs_verbal", label = "Beste verbale Reaktion", choices= choices_verbal ),
                        selectInput("gcs_motorisch", label = "Beste motorische Reaktion", choices=  choices_motorisch),
                        actionButton("save", "Daten speichern",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("delete", "Daten löschen")),
                tabItem("patientsuchen", "Patient suchen", h3("Patientdaten"),
                        dataTableOutput('selection'),
                        actionButton("auswahl_suchen", "Patient auswählen", width = "50%")),
                tabItem("einzel","Einzeldaten",
                        plotOutput("gcsAugePlot",width="50%",height="150px"),
                        plotOutput("gcsVerbalPlot",width="50%",height="150px"),
                        plotOutput("gcsMotorischPlot",width="50%",height="150px")),
                tabItem("gesamt","Gesamtdaten",
                        plotOutput("gcsTotalPlot",width="50%",height="150px"))
            )
        )
    )
)

# Server Output

server <- function(input, output, session) {
    output$selection <- DT::renderDataTable(patientenframe, selection = 'single')

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
        write_xlsx(
            patientenframe_neu,
            path = patientenname#(fileext = ".xls")
    )})
    
    # GCS wird an bestehenden Datensatz angebunden
    neuerGCS <- observeEvent(input$save, {
        # validate(
        #   need(input$datum != "", "Bitte Datum eingeben"),
        #   need(input$gcs_auge != "", "Bitte GCS Augen ?ffnen ausf?llen"),
        #   need(input$gcs_verbal != "", "Bitte Datum eingeben"),
        #   need(input$gcs_motorisch != "", "Bitte Datum eingeben")
        # )
        gcs<- data.frame(pid=input$pid, datum= input$datum, gcs_auge=input$gcs_auge, gcs_verbal=input$gcs_verbal, gcs_motorisch=input$gcs_motorisch, stringsAsFactors = FALSE)
        gcsframe_neu <- rbind.data.frame(gcsframe, gcs,stringsAsFactors = FALSE)
        gcsframe_neu$datum <- as.Date(gcsframe_neu$datum, format = "%Y-%m-%d", origin="1970-01-01")
        write_xlsx(gcsframe_neu, path = gcsname)#(fileext = ".xls"))#, sep = "\t", row.names=FALSE)
    })
    
    # Löschen der Eingaben
    observeEvent(input$delete, {
        reset("form")
    })
    
    # Selektion des Patienten mit Graphenerstellung
    observeEvent(input$auswahl_suchen,{
        row <- input$selection_rows_selected
        reihe_separieren <- slice(patientenframe, row)
        temporaere_daten_waehlen <- filter(gcsframe, pid == reihe_separieren$pid)
        score <- mutate(temporaere_daten_waehlen, totalScore = gcs_auge+gcs_verbal+gcs_motorisch)
        output$gcsAugePlot <- renderPlot({ggplot(data = score) +
                geom_point(mapping = aes(x = datum, y = gcs_auge, color = pid))}+ ylim(0,3))
        output$gcsVerbalPlot <- renderPlot({ggplot(data = score) +
                geom_point(mapping = aes(x = datum, y = gcs_verbal, color = pid))}+ ylim(0,4))
        output$gcsMotorischPlot <- renderPlot({ggplot(data = score) +
                geom_point(mapping = aes(x = datum, y = gcs_motorisch, color = pid))}+ ylim(0,5))
        output$gcsTotalPlot <- renderPlot({ggplot(data = score) +
                geom_point(mapping = aes(x = datum, y = totalScore, color = pid))}+ ylim(0,12))
    })
}


shinyApp(ui = ui, server = server)