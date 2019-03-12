
library(shiny)
library (leaflet)
library(DT)

# some global-ish variables... 
#remoteHMMs <- c()
#remoteData <- NULL
#remoteSummaryData <- list()


source("./Helpers.R")
source("./sourceHtml.R")


# Define UI 
ui <- fluidPage(
  
  #source ("./RemoteDataLoad.R", local=TRUE),
  
  # Application title
  titlePanel("Imagenit : Metagenome HMM mapper"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("hmmType", label = "HMMs from:", choices = c("PFAM domains",
                                                                "SFLD families",
                                                                "SFLD subgroups"),
                   selected="PFAM domains"),
      #textInput("pfamAccessionEntry", label = "Enter a PFAM Accession (e.g. PF00032)"),
      conditionalPanel( condition = 'input.hmmType =="SFLD families" ',
                        selectInput("familySelect", label = h4("Choose an SFLD family"), 
                                    choices = c("Not working, Please choose PFAM above"))),
      conditionalPanel( condition = 'input.hmmType =="SFLD subgroups" ',
                        selectInput("subgroupSelect", label = h4("Choose an SFLD subgroup"), 
                                    choices = c("Not working, Please choose PFAM above"))),
      
      conditionalPanel( condition = 'input.hmmType =="PFAM domains" ',
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
      checkboxInput("showAllSurveys", "Show all surveys", value = FALSE)
      
      ),
      
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Map View", value="mapPanel",
                 leafletOutput ("map", height="600px")
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
# This is the server logic of a Shiny web application. 

library(ggplot2)


server <- function(input, output, session) {
  #remoteSummaryData = NULL
  #remoteData = NULL
  #remoteHMMs = c()
  
  
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
         
       }
       if (query$hmmType == "sfldSubgroup"){
         
       }
       
     }
 })
  

    hmmSelected <- reactiveValues(hmm="pfam00001")

    observe({
      hmmSelected$hmm = input$pfamSelect
      #updateTextInput(session, "pfamAccessionEntry", value = input$select)
    })
    

  plottedData <- reactive({
      computeMapData(hmmSelected$hmm)
    
  })
  

  dataAnnotatedByPValue <- reactive({
    data = plottedData()[]
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
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  #histRange <- reactiveValues(range =)
  

  observeEvent(input$map_dblclick, {
    brush <- input$map_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
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
              numGenes,
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
  
  output$map <- renderLeaflet({
    leaflet(metagenomeInfo) %>% addTiles () %>% 
      fitBounds (~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
 
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
  

    
  output$linkOutToJGI <- renderText({
    hmm = hmmSelected$hmm
    taxonOIDs = tableRows()[input$resultsTable_rows_selected, taxon_oid]
    #taxonOIDs = c("3300010033") 
    CreateJGIFormManyPfamManyMetagenome (hmm, taxonOIDs)
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

