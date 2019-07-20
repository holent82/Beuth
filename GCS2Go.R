###
#   Version 1.3 beta
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
library(shinyFeedback)
#library(lubridate)

# deutsches Format für Datum
Sys.setlocale("LC_ALL", "German")

#Dateipfad der Datenbank
patientenname = "C:/Users/holen/Documents/GCS2Go2/patienten_daten.xlsx"
gcsname = "C:/Users/holen/Documents/GCS2Go2/gcs_daten.xlsx"

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
}
# Initialisieren der Daten
daten_einlesen()

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
        dashboardHeader(title = "Glasgow Coma Scale 2 Go"),
        
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
                            selectInput("gcs_auge", label = "Öffnen der Augen", choices = choices_auge),
                            selectInput("gcs_verbal", label = "Beste verbale Reaktion", choices= choices_verbal ),
                            selectInput("gcs_motorisch", label = "Beste motorische Reaktion", choices=  choices_motorisch),
                            actionButton("save", "Daten speichern",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("delete", "Daten löschen")),
                    tabItem("patientsuchen", h3("Patienten suchen"),
                            dataTableOutput('selection'),
                            actionButton("auswahl_suchen", "Patient auswählen", width = "50%")),
                    tabItem("gcstab",
                            plotOutput("gcsAugePlot",width="50%",height="150px"),
                            plotOutput("gcsVerbalPlot",width="50%",height="150px"),
                            plotOutput("gcsMotorischPlot",width="50%",height="150px"),
                            plotOutput("gcsTotalPlot",width="50%",height="150px"))
                )
            )
        )
    )
)


# Server Output

server <- function(input, output, session) {
    output$patiententitel <- renderText(paste("Kein Patient gewählt"))
    output$selection <- DT::renderDataTable(patientenframe, selection = 'single')

    observeEvent(input$save, {
        feedbackWarning(
            inputId = "pid",
            input$pid== "",
            text = "Bitte Patienten-ID eingeben"
        )
        
        feedbackWarning(
            inputId = "pid",
            is.numeric(input$pid)==FALSE,
            text = "Bitte nur Zahlen für Patienten-ID eingeben"
        )
        
        feedbackWarning(
            inputId = "vorname",
            input$vorname== "",
            text = "Bitte Vornamen eingeben"
        )
        feedbackWarning(
            inputId = "name",
            input$name== "",
            text = "Bitte Namen eingeben"
        )

        feedbackWarning(
            inputId = "datum",
            input$name== "",
            text = "Bitte Untersuchungsdatum eingeben"
        )
        
        feedbackWarning(
            inputId = "gcs_auge",
            input$name== "",
            text = "Bitte GCS (Auge) eingeben"
        )
        
        feedbackWarning(
            inputId = "gcs_verbal",
            input$name== "",
            text = "Bitte GCS (verbal) eingeben"
        )
        
        feedbackWarning(
            inputId = "gcs_motorisch",
            input$name== "",
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
        
        ######## Überprüfen Eingabe, Entwurf, funzt noch nicht
            # if(!is.null(filter(patientenframe, pid == input$pid)$pid)){
            #     print("Daten anlegen")
            # }
            # else if(!is.null(filter(patientenframe, pid == input$pid))){
            #     print("Weiter prüfen ob Name und Vorname übereinstimmen")
            #     if(!is.null(filter(patientenframe, name == input$name)) &&
            #         !is.null(filter(patientenframe, vorname == input$vorname))){
            #         print("nur gcs hinzufügen")
            #     } else {
            #         print("PID bereits vergeben")
            #     }
            # }
        ###########
        
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
            path = patientenname
        )
        gcs<- data.frame(pid=input$pid, datum= input$datum, gcs_auge=input$gcs_auge, gcs_verbal=input$gcs_verbal, gcs_motorisch=input$gcs_motorisch, stringsAsFactors = FALSE)
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
        
        removeModal()
    })
    
    # Erstellung einer neuen Datenreihe
    neuerPatient <- observeEvent(input$save, {
        req(input$pid)
        req(input$vorname)
        req(input$name)
        
        req(input$datum)
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
        return(gcs_selected2)
    }
        
    gcs_selected <- eventReactive(input$auswahl_suchen,{
        req(input$selection_rows_selected)
        gcs_selected <- filter(gcsframe, pid == patient_selected()$pid)
        gcs_selected2 <- mutate(gcs_selected, totalScore = gcs_auge+gcs_verbal+gcs_motorisch)
        gcs_selected2
    })

    graphen_zeichnen <- function(x){
        output$gcsAugePlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datum, y = gcs_auge, color = gcs_auge == 4))+ 
                scale_color_manual(values = colorscale) +
                geom_text(aes(label= gcs_auge, x = datum, y = gcs_auge), position = position_nudge(y = -0.25))}+ 
                    xlab("Datum") + ylab("GCS(Öffnen der Augen)")+
                    ylim(1,4) +theme_bw()+ theme(legend.position = "none"))
        output$gcsVerbalPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datum, y = gcs_verbal, color = gcs_verbal == 5))+
                scale_color_manual(values = colorscale)+
                geom_text(aes(label= gcs_verbal, x = datum, y = gcs_verbal), position = position_nudge(y = -0.25))}+ 
                    xlab("Datum") + ylab("GCS(verbale Antwort)")+
                    ylim(1,5) +theme_bw()+ theme(legend.position = "none"))
        output$gcsMotorischPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datum, y = gcs_motorisch, color = gcs_motorisch==6))+
                scale_color_manual(values = colorscale)+ 
                geom_text(aes(label= gcs_motorisch, x = datum, y = gcs_motorisch), position = position_nudge(y = -0.25))}+ 
                    xlab("Datum") + ylab("GCS(motorische Antwort)")+
                    ylim(1,6) +theme_bw()+ theme(legend.position = "none"))
        output$gcsTotalPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datum, y = totalScore, color = totalScore ==15)) +
                scale_color_manual(values = colorscale)+ 
                geom_text(aes(label= totalScore, x = datum, y = totalScore), position = position_nudge(y = -0.75))}+ 
                    xlab("Datum") + ylab("GCS(Gesamt)")+
                    ylim(1,15) +theme_bw()+ theme(legend.position = "none"))
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
        output$patiententitel <- renderText({ paste(patient_selected()$vorname, patient_selected()$name, ",", patient_selected()$pid)
        })
    })
} 

shinyApp(ui = ui, server = server)