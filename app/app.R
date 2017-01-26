require(shiny)
require(elastic)
require(ggmap)
require(leaflet)
require(shinysky)
require(shinythemes)
require(tidyverse)
require(stringr)
require(googlesheets)

CTstudies <- gs_read(gs_title("CTstudies"))
connect( es_host = "http://search-clinical-trials-net4nc47vfs2s526mzqpb3tc5y.us-west-2.es.amazonaws.com", es_port = 80)

# Tag which fields are mandatory
fieldsMandatory <- c("city", "state")

# Generate output file()
tableOutput <- function(ct) {
    trials <- unique(ct$`NCT Number`)
    CT <- filter(CTstudies, `NCT Number` %in% trials)
    CT
}

# Run elastic search query and filter results based on form inputs
runQuery <- function(ct, patient.data) {
    body <- paste0('{"size" : 10000,',
                   '"query": {',
                   '"bool": {',
                   '"filter": {',
                   '"geo_distance": {',
                   '"distance": "', patient.data[6],'mi",',
                   '"facility.geo": {',
                   '"lat": ', patient.data[4],',',
                   '"lon": ', patient.data[5], 
                   '}}}}}}')
    out <- Search(index="trialsgeo", body = body )
    count <- length( out$hits$hits )
    id <- unlist(lapply( 1:count, function(i) try( out$hits$hits[[i]]$`_source`$nctId )))
    id.unique.geo <- unique(id)
    CT <- filter(ct, `NCT Number` %in% id.unique.geo)
    
    # Filter by age
    if (!patient.data[1] == ""){
        age <- as.integer(patient.data[1])
        CT <- filter(CT, minage <= age  & age <= maxage)
    }
    
    # Filter by gender
    if (!patient.data[2] == ""){
        g <- patient.data[2]
        if (g == "male"){
            CT <- filter(CT, !(Gender == "Female"))
        }
        if (g == "female"){
            CT <- filter(CT, !(Gender == "Male"))
        }
    }
    
    # Filter by Trial Type
    if (!patient.data[7] == ""){
        x <- patient.data[7]
        if (x == "Interventional"){
            CT <- filter(CT, `Study Types` == x | `Study Types` == "Expanded Access")
        } else if (x == "Observational"){
            CT <- filter(CT, `Study Types` == x)
        }
    }
    
    # Filter by title and disease
    if (!patient.data[3] == ""){
        d <- tolower(patient.data[3])
        d <- unlist(strsplit(d, c(" ", ",", "|")))
        
        # Return entries where all search terms are in conditions
        CT <- filter(CT, sapply(paste(tolower(Conditions), tolower(Title), tolower(Interventions)), 
                                function(x) all(sapply(d, str_detect, string = x))))
    }
    return(CT)
}

# add an asterisk to an input label
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

# query google API with city, state for latitude, longitude and return
# a c(lat, lon) vector
latlon <- function(city, state) {
    q <- paste(city, state, sep = ", ")
    geo_reply <- geocode(q, output='latlon')
    lat <- geo_reply$lat
    lon <- geo_reply$lon
    return(c(lat, lon))
}

# CSS to use in the app
appCSS <-
    ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
.select2-container {
    width: 100% !important;
}
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

shinyApp(
    ui = fluidPage(
        theme = shinytheme("journal"),
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        title = "Subset Clinical Trials",
        div(id = "header",
            h1("Subset Clinical Trials"),
            strong( 
                span("Source"),
                a("on GitHub", href = "https://github.com/tempuslabs/compbio/tree/martin/clinical_trials"))
        ),
        
        fluidRow(
            column(3,
                   div(
                       id = "form",
                       textInput(inputId = "disease", 
                                 label = "Search Terms (tissue type):", 
                                 placeholder = "e.g. breast stage III",
                                 width = 300),
                       textInput(inputId = "age", 
                                 label = "Age:",
                                 width = 300),
                       selectInput(inputId = "gender", 
                                   label = "Gender:",
                                   c(" " = "",  
                                     "Male" = "Male",
                                     "Female" = "Female",
                                     "Unknown" = "unknown")),
                       selectInput(inputId = "type", 
                                   label = "Study Type:",
                                   c(" " = "",  
                                     "Interventional" = "Interventional",
                                     "Observational" = "Observational",
                                     "Any" = "Any")),
                       textInput(inputId = "city", 
                                 label = labelMandatory("Patient City:"),
                                 value = "Chicago",
                                 width = 300), 
                       textInput(inputId = "state", 
                                 label = labelMandatory("State:"), 
                                 value = "IL"),
                       sliderInput("dist", "Distance from Patient (Miles):",
                                   min = 0, max = 1000, value = 100
                       ),
                       actionButton("submit", "Search", class = "btn-primary"),
                       downloadButton("downloadData", "Download")
                   )
            ),
            column(9,
                   uiOutput("ctrials")
            )
        )
    ),
    
    
    server = function(input, output, session) {
        
        # Enable the Submit button when all mandatory fields are filled out
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        updateTable <- eventReactive(input$submit, {
            patient.data <- c(input$age, 
                              input$gender,
                              input$disease,
                              latlon(input$city, input$state),
                              input$dist,
                              input$type)
            CT <- runQuery(CTstudies, patient.data)
            CT <- select(CT, `NCT Number`, Title, `Conditions`, `Sponsor/Collaborators`, Interventions)
            CT
        })
        
        output$map <- renderLeaflet({
            loc <- latlon(input$city, input$state)
            leaflet() %>%
                setView(lng = loc[2], lat = loc[1], zoom = 7) %>%
                #                addProviderTiles("Stamen.Watercolor") %>%
                addProviderTiles("Stamen.TonerLite") %>%
                addProviderTiles("Stamen.TonerLines",
                                 options = providerTileOptions(opacity = 0.35)) %>%
                addProviderTiles("Stamen.TonerLabels") %>%
                addMarkers(lng = loc[2], lat = loc[1]) %>%
                addCircles(lng = loc[2], 
                           lat = loc[1], 
                           100000 * 1.61, 
                           stroke = F,
                           color = "purple")
        })
        
        # render the clinical trials panel
        output$ctrials <- renderUI({
            div(
                id = "ctrialsPanel",
                h2(renderText(paste(nrow(updateTable()), " Trials"))),
                DT::renderDataTable(updateTable())
            )
        })
        
        # Allow user to download responses
        output$downloadData <- downloadHandler(
                filename = function () { paste(Sys.Date(), "clinical_trials.csv", sep = "") },
                content = function(file) {
                    write.csv(tableOutput(updateTable()), file)
                }
        )
    }
)
