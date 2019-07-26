###
#   Version 1.8 ohne Zeit
###

# installiert die benÃ¶tigten Packages (falls nicht bereits erfolgt)
packages_benoetigt <- c("anytime", "DT", "ggplot2", "lubridate", "scales", "shiny", "shinyjs", "shinydashboard", "shinyFeedback", "shinythemes", "shinyTime", "sqldf", "tidyverse", "readxl", "writexl")
packages_neu <- packages_benoetigt[!(packages_benoetigt %in% installed.packages()[,"Package"])]
if(length(packages_neu)) install.packages(packages_neu, dependencies = TRUE)

# Packages werden geladen
library(anytime)
library(DT)
library(ggplot2)
library(lubridate)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(shinythemes)
library(shinyTime)
library(sqldf)
library(tidyverse)
library(readxl)
library(writexl)

# deutsches Format fÃ¼r Datum
Sys.setlocale("LC_ALL", "German")

#Dateipfad der Datenbank
patientenname = "C:/Users/holen/Documents/GCS2Go2/patienten_daten.xlsx"
gcsname = "C:/Users/holen/Documents/GCS2Go2/gcs_daten.xlsx"

#Funktion zur Validierung
leer = function(a){
    ((is.na(a)) | is.null(a))
}  

#Funktion zum Einlesen der Datenbanken zum Abgleich
daten_einlesen <- function(){
    # Einlesen der Patienten-Stammdaten
    patientendaten_excel <<- read_excel(patientenname)
    patientendaten <<- reactiveValues()

    # Einlesen der GCS-Daten
    gcsdaten_excel <<- read_excel(gcsname)
    gcsdaten <<- reactiveValues()
}

# Initialisieren der Daten
x <- tryCatch(
    {
        daten_einlesen()
    },
    error = function(e){
        winDialog(type = c("ok", "okcancel", "yesno", "yesnocancel"), "Datenbank hochladen fehlgeschlagen. Bitte manuell die Datenbanken auswÃ¤hlen")
        patientenname=  choose.files(caption = "Bitte die Datei patienten_daten.xlsx auswÃ¤hlen")
        gcsname=  choose.files(caption = "Bitte die Datei gcs_daten.xlsx auswÃ¤hlen")
    }
)

#Einmalige Initialierung der Daten und Ãbertragung in reactiveValue
startmodul <- function(input, output, session){
    patientendaten$df <<- rbind(isolate(patientendaten$df), data.frame(pid = as.numeric(patientendaten_excel$pid),
                                                                           name = patientendaten_excel$name,
                                                                           vorname = patientendaten_excel$vorname))
    gcsdaten$df <<- rbind(isolate(gcsdaten$df), data.frame(pid = as.numeric(gcsdaten_excel$pid),
                                                  datum = as.Date(gcsdaten_excel$datum, origin="1970-01-01", format = "%d.%m.%Y"),
                                                  gcs_auge = as.integer(gcsdaten_excel$gcs_auge),
                                                  gcs_verbal = as.integer(gcsdaten_excel$gcs_verbal),
                                                  gcs_motorisch = as.integer(gcsdaten_excel$gcs_motorisch)))
}



identical_pid <- 0
identical_name <- 0

# Definition der Variablen fÃ¼r die Benutzereingaben
choices_auge = c(
    "Bitte wÃ¤hlen" = "",
    "Spontan" = 4,
    "Bei Ansprache" =  3,
    "Bei Schmerzreiz" =  2,
    "Kein Ãffnen der Augen" = 1
)

choices_verbal = c(
    "Bitte wÃ¤hlen" = "",
    "KonversationsfÃ¤hig, orientiert" = 5,
    "KonversationsfÃ¤hig, desorientiert" = 4,
    "InadÃ¤quate ÃuÃerungen" = 3,
    "UnverstÃ¤ndliche Laute" = 2,
    "Keine verbale Antwort" = 1
)

choices_motorisch = c(
    "Bitte wÃ¤hlen" = "",
    "Auf Aufforderung" = 6,
    "Auf Schmerzreiz, gezielt" = 5,
    "Auf Schmerzreiz, Beugeabwehr" = 4,
    "Beugesynergismen" = 3,
    "Strecksynergismen" = 2,
    "Keine motorische Antwort" = 1
)

# Farben fÃ¼r die Grafiken
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
                             subtitle = h5(textOutput('patiententitel'),style="color:red")),
            sidebarMenu(
                id = "tabs",
                actionButton("patient_fertig", "Patient fertig", icon = icon("power-off")),
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
                            #    timeInput("zeit", label = "Untersuchungszeit", value = strptime("", "%T"), minute.steps = 5),
                            selectInput("gcs_auge", label = "Ãffnen der Augen", choices = choices_auge),
                            selectInput("gcs_verbal", label = "Beste verbale Reaktion", choices= choices_verbal ),
                            selectInput("gcs_motorisch", label = "Beste motorische Reaktion", choices=  choices_motorisch),
                            actionButton("save", "Daten speichern",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("delete", "Daten lÃ¶schen")),
                    tabItem("patientsuchen", h3("Patienten suchen"),
                            dataTableOutput('selection'),
                            actionButton("auswahl_suchen", "Patient auswÃ¤hlen", width = "50%")),
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
    #Initialisierung
    output$patiententitel <- renderText(paste("Kein Patient gewÃ¤hlt"))
    daten_einlesen()
    callModule(startmodul, "startmodul1")
    output$selection <- DT::renderDataTable(patientendaten$df, selection = 'single')
    hideElement(id='patient_fertig') #Button wird ausgeblendet initialisiert
    
    # PrÃ¼fen identischer Eingaben
    values <- reactiveValues(identical_pid = 0, identical_name = 0)
    
    #Button wird auf betÃ¤tigen ausgeblendet
    observeEvent(input$patient_fertig,{
        patient_fertig()
        hideElement(id='patient_fertig')
    })
    
    #Patient wird abgemeldet
    patient_fertig <- function(){
        output$patiententitel <- renderText(paste("Kein Patient gewÃ¤hlt")) 
        hideElement("gcsAugePlot")
        hideElement("gcsVerbalPlot")
        hideElement("gcsMotorischPlot")
        hideElement("gcsTotalPlot")
        output$selection <- DT::renderDataTable(isolate(patientendaten$df), selection = 'single')
        output$gcsSummaryPlot <- renderText({})
        reset("form")
    }
    
    #Einblenden der ausgeblendeten Elemente
    patient_gewaehlt_elemente_anzeigen <- function(){
        showElement("gcsAugePlot")
        showElement("gcsVerbalPlot")
        showElement("gcsMotorischPlot")
        showElement("gcsTotalPlot")
    }
    
    test_pid<- observeEvent(input$save, {
        identical_pid <<- sum(as.numeric(c(FALSE, sapply(1:nrow(patientendaten$df), function(i)
            any(patientendaten$df$pid[i] == input$pid)))))
        identical_name <<- sum(as.numeric(c(FALSE, sapply(1:nrow(patientendaten$df), function(i)
            any(patientendaten$df$name[i] == input$name && patientendaten$df$vorname[i] == input$vorname)))))

        
        feedbackWarning(
            inputId = "pid",
            leer(input$pid)==TRUE,
            text = "Bitte Patienten-ID eingeben, nur Zahlen erlaubt"
        )
        
        
        feedbackWarning(
            inputId = "pid",
            is.numeric(input$pid)==FALSE,
            text = "Bitte nur Zahlen fÃ¼r Patienten-ID eingeben"
        )
        
        req(input$pid)
        feedbackWarning(
            inputId = "pid",
            is.numeric(input$pid)==FALSE && identical_pid == 0,
            text = "Bitte nur Zahlen fÃ¼r Patienten-ID eingeben"
        )
        
        feedbackWarning(
            inputId = "pid",
            is.numeric(input$pid)==TRUE && identical_pid !=0 && identical_name == 0,
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
    # 
    # test_zeit<- observeEvent(input$save, {
    #   feedbackWarning(
    #     inputId = "zeit",
    #     condition = leer(input$zeit) == TRUE,
    #     text = "Bitte Untersuchungszeit eingeben"
    #   )
    # })
    
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
    
    #ÃberprÃ¼fung der Eingabefelder und Aufruf Ãbersichtsfenster
    neuerPatient <- observeEvent(input$save, {
        req(input$pid)
        req(input$vorname)
        req(input$name)
        
        req(input$datum)
        #  req(input$zeit)
        req(input$gcs_auge)
        req(input$gcs_verbal)
        req(input$gcs_motorisch)
        if (identical_pid !=0 && identical_name != 0 ||identical_pid ==0)
        {
            showModal(dataModal())
        }
        identical_name=0
        identical_pid=0
    })
    
    
    #Ãbersichtsfenster zur EingabeprÃ¼fung
    dataModal <- function(failed = FALSE){
        modalDialog(
            title = "Daten verifizieren",
            "Bitte Ã¼berprÃ¼fen Sie die Daten!",
            
            renderText({paste("PID: ", input$pid)}),
            renderText({paste("Name: ",input$name)}),
            renderText({paste("Vorname: ",input$vorname)}),
            renderText({paste("Datum: ",input$datum)}),
            # renderText({paste("Zeit: ",input$zeit)}),
            renderText({paste("Score Auge: ",input$gcs_auge)}),
            renderText({paste("Score Verbal: ",input$gcs_verbal)}),
            renderText({paste("Score Motorisch: ",input$gcs_motorisch)}),
            
            footer = tagList(
                actionButton("modal_cancel", "Abbrechen"),
                actionButton("modal_ok", "Daten bestÃ¤tigen")
            )
        )
    }
    
    observeEvent(input$modal_cancel, {
        dataModal(failed = TRUE)
        removeModal()
    })
    
    #PrÃ¼fung: Neuer Patient oder nur GCS zufÃ¼gen
    observeEvent(input$modal_ok, {
        if(identical_pid == 0 && identical_name == 0){
            patientendaten_speichern()    
            gcsdaten_speichern()
        } else if(identical_pid > 0 && identical_name > 0){
            gcsdaten_speichern()
        }
        auswahl_vorbereiten(input$pid)
        removeModal()
    })
    
    #Patientendaten werden gespeichert
    patientendaten_speichern <- eventReactive(input$modal_ok, {
        patient <- data.frame(pid = input$pid, name = input$name, vorname = input$vorname)
        patientendaten$df <- rbind.data.frame(patientendaten$df, patient, stringsAsFactors = FALSE)
        
        write_xlsx(patientendaten$df, path = patientenname)
    })
    
    #GCSdaten werden gespeichert
    gcsdaten_speichern <- eventReactive(input$modal_ok, {
        gcs<- data.frame(pid=input$pid, datum= input$datum, 
                         #  zeit=  format(input$zeit, format = "%H:%M:%S"), 
                         gcs_auge=input$gcs_auge, gcs_verbal=input$gcs_verbal, gcs_motorisch=input$gcs_motorisch, stringsAsFactors = FALSE)
        gcsdaten$df <- rbind.data.frame(gcsdaten$df, gcs, stringsAsFactors = FALSE)
        gcsdaten$df$datum <- as.Date(gcsdaten$df$datum, format = "%Y-%m-%d", origin="1970-01-01")
        gcsdaten$df$gcs_auge <- as.integer(gcsdaten$df$gcs_auge)
        gcsdaten$df$gcs_verbal <- as.integer(gcsdaten$df$gcs_verbal)
        gcsdaten$df$gcs_motorisch <- as.integer(gcsdaten$df$gcs_motorisch)
        
        write_xlsx(gcsdaten$df, path = gcsname)
    })
    

    # LÃ¶schen der Eingaben
    observeEvent(input$delete, {
        reset("form")
    })


    #Selektion der Patientendaten aus Liste
    patient_selected <- observeEvent(input$auswahl_suchen,{
        req(input$selection_rows_selected)
        row <- input$selection_rows_selected
        patient_selected <- slice(isolate(patientendaten$df), row)
        auswahl_vorbereiten(patient_selected$pid)
    })
    
    #Aktualisieren des Hauptfensters, schreiben Patientendaten in Eingabefelder, Daten fÃ¼r Graphen aufbereiten
    auswahl_vorbereiten <- function(x){
        patient_selected <- filter(isolate(patientendaten$df), pid == x)

        updateNumericInput(session, "pid", value = paste(as.integer(patient_selected$pid)))
        updateTextInput(session, "name", value = paste(patient_selected$name))
        updateTextInput(session, "vorname", value = paste(patient_selected$vorname))
        
        gcs_selected <- filter(isolate(gcsdaten$df), pid == x)
        gcs_selected2 <- mutate(gcs_selected, totalScore = as.integer(gcs_auge)+as.integer(gcs_verbal)+as.integer(gcs_motorisch))

        output$patiententitel <- renderText({ paste(patient_selected$vorname, patient_selected$name, ",", patient_selected$pid)})
        output$gcsSummaryPlot <- renderText({ paste("Letzte GCS erfolgt am", max(gcs_selected2$datum))})
        
        showElement(id='patient_fertig')
        patient_gewaehlt_elemente_anzeigen()
        
        graphen_zeichnen(gcs_selected2)
    }
    
    # Graphen werden gezeichnet
    graphen_zeichnen <- function(x){
        #  x$datumzeit <- paste(x$datum, x$zeit)
        x$datumzeit <- x$datum
        #  x$datumzeit <- as.POSIXct(x$datumzeit,  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
        output$gcsAugePlot <- renderPlot({ggplot(data =  x)  +
                geom_point(mapping = aes(x = datumzeit, y = gcs_auge, color = gcs_auge == 4)) +
                scale_color_manual(values = colorscale) +
                #     scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
                geom_text(aes(label= gcs_auge, x = datumzeit, y = gcs_auge), position = position_nudge(y = -0.25))}+ 
                    ylab("GCS(Ãffnen der Augen)")+xlab("")+
                    ylim(1,4) +theme_bw()+ ggtitle("GCS(Ãffnen der Augen)") +
                    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
        output$gcsVerbalPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datumzeit, y = gcs_verbal, color = gcs_verbal == 5))+
                scale_color_manual(values = colorscale)+
                #    scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
                geom_text(aes(label= gcs_verbal, x = datumzeit, y = gcs_verbal), position = position_nudge(y = -0.25))}+ 
                    ylab("GCS(verbale Antwort)")+ xlab("")+
                    ylim(1,5) +theme_bw()+ ggtitle("GCS(verbale Antwort)") +
                    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
        output$gcsMotorischPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datumzeit, y = gcs_motorisch, color = gcs_motorisch==6))+
                scale_color_manual(values = colorscale)+ 
                #    scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
                geom_text(aes(label= gcs_motorisch, x = datumzeit, y = gcs_motorisch), position = position_nudge(y = -0.25))}+ 
                    ylab("GCS(motorische Antwort)")+xlab("")+
                    ylim(1,6) +theme_bw()+ ggtitle("GCS(motorische Antwort)") +
                    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
        output$gcsTotalPlot <- renderPlot({ggplot(data =  x) +
                geom_point(mapping = aes(x = datumzeit, y = totalScore, color = totalScore ==15)) +
                scale_color_manual(values = colorscale)+ 
                #   scale_x_datetime(breaks = x$datumzeit, labels = date_format("%d-%m %H:%M"))+
                geom_text(aes(label= totalScore, x = datumzeit, y = totalScore), position = position_nudge(y = -0.75))}+ 
                    ylab("GCS(Gesamt)")+xlab("")+
                    ylim(1,15) +theme_bw()+ggtitle("GCS(Gesamt)") +
                    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust = 1)))
    }
} 
shinyApp(ui = ui, server = server)
