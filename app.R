# Lade alle nötigen Pakete
library(shiny)
library(shinymaterial)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(DBI)
library(RMySQL)
library(leaflet)
library(leafgl)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(plotly)

# Verbinde mit MySQL database
con <- dbConnect(drv = RMySQL::MySQL(),
                 host = "freedb.tech",
                 username = "freedbtech_guest",
                 password = "guest",
                 dbname= "freedbtech_trees")

# Kappe die MySQL-Verbindung nach Schließen der App
onStop(function() {
    dbDisconnect(conn=con)
})

# Hole registrierte Baumarten als Liste
load("www/treechoices.txt")

# UI Layout
ui <- material_page(
    title = "APPleTree",
    nav_bar_color = "light-green",
    primary_theme_color = "red",
    nav_bar_fixed = TRUE,
    material_side_nav(
        fixed = TRUE,
        image_source = "akazie_light-green.png",
        actionButton('getdata',
                     label = tags$div(HTML('Hole Datensatz vom Server')),
                     style="color: #FFF; background-color: #8bc34a; border-color: #000"),
        br(),
        br(),
        br(),
        br(),
        material_row(
            material_column(
                width = 11,
                material_modal(
                    modal_id = "filter",
                    button_text = "Hinweis zu Filtern",
                    button_icon = "info_outline",
                    button_depth = 1,
                    button_color = "grey lighten-1",
                    title = "Hinweis zum Filtern der Daten",
                    tags$p("Du kannst hier die Daten den Gesundheitszustand und Stadtteile betreffend mit Mehrfachauswahl filtern. Weitere Einfach-Filter kannst du direkt in der Tabelle vornehmen, in dem du die gewünschten Werte in das Feld einträgst, in dem ausgegraut 'ALL' zu lesen ist.")
                ),
                br(),
                material_dropdown(
                    input_id = "healthselect",
                    label = "Wähle Gesundheitszustand des Baumes",
                    choices = c("gut" = "Good",
                                "mittelmäßig" = "Fair",
                                "schlecht" = "Poor"),
                    multiple = TRUE,
                    selected = c("Good",
                                 "Fair",
                                 "Poor")
                ),
                br(),
                material_dropdown(
                    input_id = "boroughselect",
                    label = "Wähle NYC Stadtteil",
                    choices = c("Manhattan",
                                "Brooklyn",
                                "Queens",
                                "Bronx",
                                "Staten Island"),
                    multiple = TRUE,
                    selected = c("Manhattan",
                                 "Brooklyn",
                                 "Queens",
                                 "Bronx",
                                 "Staten Island")
                )
            )
        )
    ),
    material_row(
        material_column(
            width = 8,
            material_card(
                title = "Daten",
                DT::DTOutput('data'),
                downloadButton('save',
                               label = 'Speichere Tabelle | CSV',
                               style="color: #FFF; background-color: #bdbdbd; border-color: #000")
            )
        ),
        material_column(
            width = 4,
            material_card(
                title = "Dateninformation",
                depth=2,
                br(),
                p("Anzahl der Variablen:"),
                verbatimTextOutput(
                    outputId="rawcol"
                ),
                p("Anzahl aller Einträge im Originaldatensatz:"),
                verbatimTextOutput(
                    outputId="rawrow"
                ),
                p("Anzahl der gefilterten Einträge:"),
                verbatimTextOutput(
                    outputId="filtered"
                ),
                br(),
                actionButton('info', , onclick = "window.open('StreetTreeCensus2015TreesDataDictionary20161102.pdf')",
                             label = tags$div(HTML('Information zu Kürzeln von Variablen | PDF')),
                             style="color: #FFF; background-color: #8bc34a; border-color: #000")
            ),
            material_card(
                title = "Karte mit Baumbestand",
                depth=2,
                leafletOutput("treemap", width="100%", height="600")
            )
        )
    ),
    material_row(
        material_column(
            width = 8,
            material_card(
                title = "Diagramm",
                depth=2,
                material_row(
                    material_column(
                        width=2,
                        material_dropdown(
                            input_id = "variableselect",
                            label = "Wähle Variable",
                            choices = NULL, multiple = FALSE, selected = NULL
                        ),
                        br(),
                        material_dropdown(
                            input_id = "classselect",
                            label = "Wähle Klassifizierung",
                            choices = NULL, multiple = FALSE, selected = NULL
                        ),
                        br(),
                        material_radio_button(
                            input_id = "radio",
                            label = "Darstellung Balkendiagramm",
                            choices = c("nebeneinander" = "dodge",
                                        "gestapelt" = "stack",
                                        "relativ" = "fill"),
                            selected = "dodge",
                            color = "#8bc34a", with_gap = FALSE
                        ),
                        br(),
                        br(),
                        material_modal(
                            modal_id = "tipp",
                            button_text = "Tipp",
                            button_icon = "info_outline",
                            button_depth = 1,
                            button_color = "grey lighten-1",
                            title = "Kleine Tipps am Rande",
                            tags$p("Willst du dir eine Variable ohne Klasseneinteilung anschauen, also unabhängig von Unterschieden zwischen z. B. Stadtteilen, dann wähle unter Klassifizierung einfach 'state' aus.
                                   Diese Variable hat 'New York' als einzigen Wert und wird somit als eine Klasse behandelt."),
                            br(),
                            tags$p("Einzelne Klassen kannst du direkt in der Legende deaktivieren, um sie im Diagramm auszublenden.")
                        )
                    ),
                    material_column(
                        width = 10,
                        plotlyOutput(
                            outputId = "plot", width = "100%", height = "550",
                            inline = FALSE, reportTheme = TRUE
                        )
                    )
                )
            )
        ),
        material_column(
            width = 4,
            material_card(
                title = "Baumfibel",
                depth=2,
                br(),
                material_dropdown(
                    input_id = "imageselect",
                    label = "Wähle Baumart",
                    choices = treechoices,
                    multiple = FALSE,
                    selected = c("American beech")
                ),
                material_row(
                    material_column(
                    width=2),
                    material_column(
                        width=8,
                imageOutput(outputId="image", width = "100%", height = "auto", inline = TRUE)
                    ),
                material_column(
                    width=2)
                )
            )
        )
    )
)

server <- function(input, output, session) {

    # Lade Daten auf Knopfdruck herunter, ersetze alle Nullen in 'stump_diam' und entferne Anführungszeichen in 'Schubert' chokecherry, da sonst Probleme beim Filtern entstehen
    trees <- eventReactive(input$getdata,{
        material_spinner_show(session, "data")
        trees <- read_csv('https://data.cityofnewyork.us/api/views/uvpi-gqnh/rows.csv?accessType=DOWNLOAD', na = c("", NA)) %>%
            mutate(stump_diam = na_if(stump_diam, "0")) %>%
            mutate(spc_common = replace(spc_common, which(spc_common == "'Schubert' chokecherry"), "Schubert chokecherry"))
        material_spinner_hide(session, "data")
        trees
    })
    
    # Filtere die Daten mithilfe der Dropdown-Menüs in der Sidebar. Weitere Filtereinstellungen können direkt in der Tabelle vorgenommen werden
    df <- reactive({
        trees() %>%
            dplyr::filter(is.na(borough) | borough %in% input$boroughselect) %>%
            dplyr::filter(is.na(health) | health %in% input$healthselect)
    })
    
    # Rendere die (gefilterten) Daten
    output$data <- DT::renderDT({
        req(input$getdata)
        datatable(df(),
                  options = list(dom = '<"top">lrt<"bottom">ip', pageLength = 10, autoWidth = FALSE, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  rownames= FALSE, filter = "top", selection = "multiple", escape = FALSE) %>%
            formatStyle(columns = c(1:45), 'text-align' = 'center')
    })
    
    # Befehle um den Umfang des Datensatzes zu extrahieren und anzuzeigen
    rawcoldim <- reactive({
        req(input$getdata)
        dim(trees())[2]
    })
    rawrowdim <- reactive({
        req(input$getdata)
        dim(trees())[1]
    })
    filterdim <- reactive({
        req(input$getdata)
        length(input$data_rows_all)
    })
    
    output$rawcol <- renderText(rawcoldim())
    output$rawrow <- renderText(rawrowdim())
    output$filtered <- renderText(filterdim())
    
    # Download der (gefilterten) Daten als CSV
    output$save <- downloadHandler(
        filename = "Tabelle_APPleTree.csv",
        content = function(file) {
            material_spinner_show(session, "data")
            s = input$data_rows_all
            write.csv(df()[s, , drop = FALSE], file, row.names = FALSE)
            material_spinner_hide(session, "data")
        }
    )
    
    # Zeige die Positionen der Bäume in einer Karte
    output$treemap <- renderLeaflet({
        req(df())
        # Definiere die Daten, die der Karte zugrunde liegen
        s = input$data_rows_all
        mapdf <- df()[s, , drop = FALSE]
        # Definiere Farbskala
        pal <- colorFactor(
            palette = "YlGn",
            domain = mapdf[[input$variableselect]]
        )
        # Lese Koordinaten in extra Datei mit 'sf' package, ansonsten Karte viel zu lahm
        tree_coord = data.frame(id = mapdf$tree_id,
                                x = mapdf$longitude,
                                y = mapdf$latitude)
        pts = st_as_sf(tree_coord, coords = c("x", "y"), crs = 4326)
        # Rendere die Karte
        leaflet() %>%
            addProviderTiles(provider = providers$CartoDB.Positron) %>%
            addGlPoints(data = pts, lng = "x", lat = "y", group = "mapdf", popup = "id", fillOpacity = 1, fillColor = pal(mapdf[[input$variableselect]])) %>%
            setView(lng = -73.91730147, lat = 40.70592, zoom = 10) %>%
            addLayersControl(overlayGroups = "mapdf") %>%
            addLegend("bottomright", pal = pal, values = mapdf[[input$variableselect]],
                      title = "Klassen", opacity = 1)
    })
    
    # Wähle die zu untersuchende Variable und Klassen
    observeEvent(df(), {
        update_material_dropdown(session, "variableselect", value = names(df()[8]), choices = names(df()))
        update_material_dropdown(session, "classselect", value = names(df()[30]), choices = names(df()))
    })
    
    # Rendere das Diagramm
    output$plot <- renderPlotly({
        req(df())
        # Definiere Daten, die dem Diagramm zugrunde liegen
        s = input$data_rows_all
        plotdf <- df()[s, , drop = FALSE]
        # Checke, ob Variablen numerisch oder Zeichenfolgen sind, und nehme passendes Diagramm (Balken- oder Violinplot)
        if (typeof(plotdf[[input$variableselect]]) == "character") {
            
            p <- ggplot(subset(plotdf, !is.na(get(input$variableselect)) & !is.na(get(input$classselect))), aes_string(fill=input$variableselect, x=input$classselect)) +
                geom_bar(position = input$radio, alpha = 0.8, color = "black", size = 0.05) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8)) +
                theme(axis.title = element_text(size=12)) +
                theme(panel.grid.major = element_line(size=0.3, color="lightgrey"),
                      panel.background = element_blank()) +
                theme(axis.line = element_line(colour = "grey", size=0.8)) +
                theme(axis.ticks.length = unit(0.15,"cm"), axis.ticks = element_line(colour = "grey", size = 0.5)) +
                theme(legend.title = element_blank()) +
                scale_fill_brewer(palette="YlGn")
            ggplotly(p) %>%
                layout(legend=list(yanchor="middle", y=0.5))
            
        } else {
            
            p <- ggplot(subset(plotdf, !is.na(get(input$variableselect)) & !is.na(get(input$classselect))), aes_string(fill=input$classselect, x=input$classselect, y=input$variableselect)) +
                geom_violin(trim=F, alpha=0.8, scale="width", color = "black", size=0.1, width=0.8, show.legend = F) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8)) +
                theme(axis.title = element_text(size=12)) +
                theme(panel.grid.major = element_line(size=0.3, color="lightgrey"),
                      panel.background = element_blank()) +
                theme(axis.line = element_line(colour = "grey", size=0.8)) +
                theme(axis.ticks.length = unit(0.15,"cm"), axis.ticks = element_line(colour = "grey", size = 0.5)) +
                theme(legend.title = element_blank()) +
                scale_fill_brewer(palette="YlGn")
            ggplotly(p) %>%
                layout(legend=list(yanchor="middle", y=0.5))
        }
    })
    
    # Rendere die Baumfibel (Bilder werden von Database auf MySQL-Server abgerufen)
    output$image <- renderImage({
        query.string <- paste0("SELECT image FROM trees_images WHERE species ='", input$imageselect, "';")
        rs <- dbSendQuery(con, query.string)
        img <- fetch(rs)
        bytes <- as.raw(strtoi(substring(img, seq(1,nchar(img), by=2), seq(2,nchar(img), by=2)), base=16))
        writeBin(bytes, "./www/image.jpeg")
        filename <- normalizePath(file.path('./www',
                                            paste('image', '.jpeg', sep='')))
        list(src = filename,
             contentType = 'image/jpg',
             width = '100%', height = 'auto',
             alt = "Oops... etwas ist schiefgelaufen")
    }, deleteFile = FALSE)
    
    # Stoppe die App, wenn das Browserfenster geschlossen wird
    session$onSessionEnded(stopApp)
}

shinyApp(ui=ui, server=server)
