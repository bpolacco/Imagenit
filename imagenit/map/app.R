
library(shiny)
library (leaflet)
library(DT)
library(shinyjs)

library(shinycssloaders)


source("./Helpers.R")
source("./sourceHtml.R")


# UI Definition ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Imagenit : Metagenome HMM mapper"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    # _ sidebarPanel  --------------------------------------
    sidebarPanel(
      radioButtons("hmmType", label = "HMMs from:", choices = c("PFAM domains",
                                                                "SFLD families",
                                                                "SFLD subgroups"),
                   selected="PFAM domains"),
      #textInput("pfamAccessionEntry", label = "Enter a PFAM Accession (e.g. PF00032)"),
      shinyjs::hidden(
        div(id="familySelector", #conditionalPanel( condition = 'input.hmmType =="SFLD families" ',
                        selectInput("familySelect", label = h4("Choose an SFLD family"), 
                                    choices = namedFamilies))),
      shinyjs::hidden(
        div(id="subgroupSelector", #conditionalPanel( condition = 'input.hmmType =="SFLD subgroups" ',
                        selectInput("subgroupSelect", label = h4("Choose an SFLD subgroup"), 
                                    choices = namedSubgroups))),
      
      div(id="domainSelector", #conditionalPanel( condition = 'input.hmmType =="PFAM domains" ',
                        selectInput("pfamSelect", label = h4("Choose a PFAM"), 
                                    choices = availablePfam)),
      
      
      #radioButtons("dataSource", label="Plot Data from ",choices = c("SFLD", "remote"), inline=TRUE),
      #selectInput("remoteSelect", label=h4("Choose an HMM from remote set"),
      #            choices = c("Not yet loaded")),
      sliderInput("minLog10EValue",
                  "Min Log 10 P value:",
                  min = -1,
                  max = 20,
                  value = 4),
      checkboxInput("showAllSurveys", "Show all surveys", value = FALSE),
      HTML ( antarcticaMessageHTML )
      
      ),
      
    
    # Show a plot of the generated distribution
    # _ mainPanel  --------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Map View", value="mapPanel",
                 leafletOutput ("map", height="600px")  %>% withSpinner(color="#0dc5c1")
        ),
        tabPanel("Table View", value = "table",
                 fluidRow(
                   column(4,checkboxInput("limitTableToMapZoom", "Limit Table to Map Zoom", value = FALSE)),
                   column(6,htmlOutput("linkOutToJGI"))
                   #column(6,downloadButton('downloadSeqIDs', 'Download Sequence Identifiers for Selected Rows'),
                   #      htmlOutput("selectionText"))
                 ),
                hr(),
                dataTableOutput("resultsTable"))
      )
    )
  )
)


#
# Server Definition ----

server <- function(input, output, session) {
  #remoteSummaryData = NULL
  #remoteData = NULL
  #remoteHMMs = c()
  
  # _ url parsing ----
  
  #load data from url...occurs when url changes...new session
   observe({
     query <- parseQueryString(session$clientData$url_search)
     if (!is.null(query$hmm) & !is.null(query$hmmType)){
       if (query$hmmType == "pfam"){
         updateRadioButtons(session, "hmmType", selected = "PFAM domains")
         pfam = GetValidPFAMAccession(query$hmm)
         if ( pfam %in% availablePfam){
          updateSelectInput (session, "pfamSelect", selected = pfam)
         }

       }
       if (query$hmmType == "sfldFamily"){
         updateRadioButtons(session, "hmmType", selected = "SFLD families")
         if ( query$hmm %in% familyNames$hmmName){
           updateSelectInput (session, "familySelect", selected = query$hmm)
         }
           
       }
       if (query$hmmType == "sfldSubgroup"){
         updateRadioButtons(session, "hmmType", selected = "SFLD subgroups")
         if ( query$hmm %in% subgroupNames$hmmName){
           updateSelectInput (session, "subgroupSelect", selected = query$hmm)
         }
         
       }
     }
 })
    # manage the hidden selectors
    observe({
      shinyjs::hide("subgroupSelector")
      shinyjs::hide("familySelector")
      shinyjs::hide("domainSelector")
      if(input$hmmType =="SFLD subgroups"){
        shinyjs::show("subgroupSelector")
      }
      else if (input$hmmType =="SFLD families"){
        shinyjs::show("familySelector")
      }
      else if(input$hmmType =="PFAM domains"){
        shinyjs::show("domainSelector")
      }
      })
    hmmSelected <- reactiveValues(hmm="pfam00001")

    observe({
      hmmSelected$hmm = switch (input$hmmType,
                                "SFLD families" = input$familySelect,
                                "SFLD subgroups" = input$subgroupSelect,
                                "PFAM domains" = input$pfamSelect)
    })
    
    # _ data processing ----
    
  plottedData <- reactive({
      #Helpers.R
      pd = computeMapData(hmmSelected$hmm)
      if (is.null(pd)){
        showNotification (paste("Error loading data for ", hmmSelected$hmm), type="error")
      }
      pd
  })

  dataAnnotatedByPValue <- reactive({
    #set plottedLog2Ratio to NA when beyond limits so they are potted colorless
    data = plottedData()[]
    req(data)
    data$plottedLog2Ratio = data$log2Ratio
    data$plottedLog2Ratio[-input$minLog10EValue < data$poissonTestScore 
                          & data$poissonTestScore < input$minLog10EValue] = NA
    data
    
  })
  
  dataLimitedByPValue <- reactive({
    data = dataAnnotatedByPValue()
    if(!input$showAllSurveys)
      return( data[!(-input$minLog10EValue < poissonTestScore 
                     & poissonTestScore < input$minLog10EValue)])
    else
      return(data)
  })
  
  #ranges <- reactiveValues(x = NULL, y = NULL)
  #histRange <- reactiveValues(range =)
  
 

  # _ table ----
  
  tableRows <- reactive({
    if (input$limitTableToMapZoom)
      subset = dataLimitedByPValue()[Latitude >= map_bounds$south & Latitude <= map_bounds$north 
                                     & Longitude >= map_bounds$west & Longitude <= map_bounds$east]
    else subset = dataLimitedByPValue()
    subset
  })
  

  output$resultsTable <- DT::renderDataTable({
    subset = tableRows()
    
    t = subset[,.(taxon_oid,
              sample = GenomeSample_Name,
              numSeqs,
              expectedCount,
              log2Ratio,
              log10P = poissonTestScore)] 
    
    DT::datatable(t) %>% formatSignif(~expectedCount + log2Ratio + log10P, 2)
  })
  
  output$textOut <- renderText({
    #xyStr = paste0("x:", input$plotHover$x, "\ny: ", input$plotHover$y)
    pointStr = ""
    if (!is.null(input$plotHover)){
      point = nearPoints(dataLimitedByPValue(), input$plotHover,  maxpoints=1,
                         xvar="Longitude", yvar="Latitude")
      if (nrow(point) > 0)
        pointStr = as.character(point[1,]$GenomeSample_Name)
    }
    pointStr
    #paste (xyStr, pointStr, sep="\n")
  })

  observe({
    if (input$hmmType=="PFAM domains"){
      shinyjs::show("linkOutToJGI")
    }
    else{shinyjs::hide("linkOutToJGI")}
  })  
  output$linkOutToJGI <- renderText({
    hmm = hmmSelected$hmm
    taxonOIDs = tableRows()[input$resultsTable_rows_selected, taxon_oid]
    #taxonOIDs = c("3300010033") 
    CreateJGIFormManyPfamManyMetagenome (hmm, taxonOIDs)
    
  })
  
  # _ leaflet map ----
  
  output$map <- renderLeaflet({
    leaflet(metagenomeInfo) %>% addTiles () %>% 
      fitBounds (~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
 
  # observeEvent(hmmSelected,{
  #   leafletProxy ("map") %>%
  #     clearMarkers()
  #   
  # })
  # draw points on leafletMap based on p value slider, HMM selected, and checkbox
  observe({
    hmm = hmmSelected$hmm
    mgData = dataLimitedByPValue()
    #input$showAllSurveys  already handled above
    
    maxVal = max(abs(range(mgData$plottedLog2Ratio)))
    if(is.na(maxVal)){
      maxVal <- 10
    }
    pal = colorBin ("PuOr", bins = c(-Inf,-6,-4,-2,0,2,4,6, +Inf), reverse=TRUE) 
      #colorNumeric("RdBu", c(-maxVal,maxVal), reverse=TRUE)
    
    leafletProxy ("map", data = mgData) %>%
      clearMarkers() %>%
      addCircleMarkers (radius = ~log(Gene_Count_assembled)/3, 
                  stroke=TRUE, weight=0.5, col="black", fill=TRUE,
                  fillOpacity = 0.5, popup = ~paste(GenomeSample_Name),
                  fillColor = ~pal(plottedLog2Ratio))
  })
  
  # use reactiveValues to wrap input$map_bounds because sometimes input$map_bounds is unreasonable
  # example: when the panel is resized out of view (while table panel is showing)
  map_bounds <- reactiveValues (north=90, south=-90, west=-180, east = 180)
  observeEvent(input$map_bounds,{
    #if new bounds are reasonable, update map_bounds reactiveValues
    if (!is.null(input$map_bounds) &
        -180.0 <= input$map_bounds$west &
        input$map_bounds$west < input$map_bounds$east &
        input$map_bounds$east <= 180.0 &
        -90.0 <= input$map_bounds$south &
        input$map_bounds$south < input$map_bounds$north &
        input$map_bounds$north <= 90.0
    ){
      map_bounds$north = input$map_bounds$north
      map_bounds$south = input$map_bounds$south
      map_bounds$west = input$map_bounds$west
      map_bounds$east = input$map_bounds$east
    }
    
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

