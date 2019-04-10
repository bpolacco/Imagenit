# Main Shiny app for Imagenit, a tool to enable exploration of gene families in metagenome
# sequence data.  A collaboration of UCSF and JGI


library(shiny)
library (shinyjs)
library (data.table)
library (qvalue)

library(promises)
library(future)
plan(multiprocess)


source ("./LoadScript.R")
source ("./asyncStatus.R")


source (config::get("sourceHtmlR"))#"./sourceHtml.R")


# UI Definition ----
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  #shinythemes::themeSelector(),
  
  # Application title
  titlePanel("Imagenit: Finding genes in contrasting metagenomes"),
  
  tabsetPanel(
    # _ tabPanel 1 Instructions --------------------------------------
    tabPanel("1. Instructions", value="instructionsTab",
             HTML(instructionHTML)
             ),
    # _ tabPanel 2 Select --------------------------------------
    tabPanel("2. Select Metagenome Surveys",
             fluidRow(
               HTML (setSelectionInstructionsHTML),
               HTML ("<hr/>"),
               column(6,style = "border: 3px solid #F8766D; ", 
                      textInput ('set1Name', "Name to use for set 1", "set 1"),
                      selectInput('showSet1BoxSelect', label = "Metagenomes in Set 1 (click to view all)", 
                                           choices = c(), multiple=FALSE, width="100%",
                                           selectize = FALSE), 
                      htmlOutput ('set1SelectionNotice'),
                      actionButton ('addToSet1Button', "Add"),
                      actionButton('removeOneFromSet1Button', "Remove"),
                      actionButton ('clearSet1Button', "Clear All Set 1")
                      
               ),
               #column(4,verbatimTextOutput('showSelectionBox')),
               column(6,style = "border: 3px solid #00BFC4; ",
                      textInput ('set2Name', "Name to use for set 2", "set 2"),
                      fluidRow(selectInput('showSet2BoxSelect', label = "Metagenomes in Set 2 (click to view all)", 
                                           choices = c(), multiple=FALSE, width="100%",
                                           selectize = FALSE)),
                      htmlOutput ('set2SelectionNotice'),
                      actionButton('addToSet2Button', "Add"),
                      actionButton('removeOneFromSet2Button', "Remove"),
                      actionButton('clearSet2Button', "Clear All Set 2")
               )
             ),
             HTML ("<hr/>"),
             HTML ("<strong>Available metagenome surveys and their metadata</strong>"),
             radioButtons("selectTableRadioButtons",label=NULL, choices = c("All metagenomes"="all",
                                                                             "Set 1"= "set1",
                                                                             "Set 2"= "set2"),
                          inline=TRUE),
             fluidRow(column(12,
                             DT::dataTableOutput("selectionTable"), 
                             style = "overflow-y: visible; overflow-x: auto;")
             )
    ),

    # _ tabPanel 3 Computations --------------------------------------
    tabPanel("3. Computations",
             htmlOutput('resultsSetsMismatchWarning'),
             fluidRow (
                       column(6,
                              HTML(instructionsComputationsHTML)
                       ), 
                       column(6, 
                       checkboxGroupInput('hmmTypes', label="HMMs from:", 
                                          inline = TRUE,selected = c("pfam"),
                                          choices = c("PFAM models" = "pfam",
                                                      "SFLD families" = "families",
                                                      "SFLD subgroups" = "subgroups"
                                                      )),
                       actionButton('doComparisonButton', "Do Comparison"),
                       htmlOutput ('statusText'))
                       
             ),
             fluidRow(
               HTML ("<hr/>"),
               column(4,
                      HTML ("<strong>Size and PFAM counts in selected metagenomes</strong>"),
                      fluidRow(plotOutput("metagenomesInSetsPlot",
                                          click = "metagenomeScatter_click")),
                      downloadButton("downloadMetagenomeScatter", "Download as PDF"),
                      htmlOutput("selectedMetagenome_label")
                      
                      ),
               column(8,
                      fluidRow(
                        plotOutput("resultsScatter",
                                   click = "scatter_click",
                                   dblclick = "scatter_dblclick",
                                   hover = "scatter_hover",
                                   brush = brushOpts(
                                     id = "scatter_brush",
                                     resetOnNew = TRUE
                                   ))),
                      fluidRow(column (6,
                        actionButton("clearSelect_button", "Clear Highlighting"),
                        downloadButton("downloadHMMScatter", "Download as PDF"),
                        htmlOutput("scatter_pfamLabel"),
                        HTML ("<p> Selected points (double click, or drag a rectangle to select multiple) above appear in detail plot below </p>")
                        ),
                        column(6,
                                HTML ("Use slider to limit HMMs by significance"),
                        sliderInput('qValueSelector', label="max log10(q-value)",
                                     max = 0, min=-20, value=-2)
                        )
                      ),
                      fluidRow()
               )
             ),
             fluidRow(
               HTML ("<hr/>"),
               HTML ("<strong>Detail view of HMM matches in metagenomes</strong>"),
               
                      column(12,plotOutput("resultsPlot",
                                           click = "results_click",
                                           dblclick = "results_dblclick",
                                           hover = "results_hover",
                                           brush = brushOpts(
                                             id = "results_brush",
                                             resetOnNew = TRUE
                                           )
                                           ),
                             fluidRow(column(6,radioButtons("rateOrCount_radioButtons", label = "X axis shows", 
                                          choices = c("Rate of sequences" = "rate","Count of sequences" = "count"),
                                          inline=TRUE)),
                                      column(6,downloadButton("downloadDetailPlot", label = "Download as PDF"))
                             )
                             )
                      
                      )
             ),
    # _ tabPanel 4 Download data --------------------------------------
    tabPanel("4. Download data and results",
             htmlOutput("tableSetsMismatchWarning"),
             tabsetPanel(tabPanel("Metagenome Table",
                                  downloadButton ("downloadMetagenomeTableButton", "Download Table"),
                                  DT::dataTableOutput("resultsByMetagenomeTable")),
                         tabPanel("HMM Table",
                                  
                                  fluidRow(
                                    column(6,
                                      radioButtons ("radioButtons_HowManyHMMRows", label="Show Rows For:", 
                                                    choices = c("Plot-highlighted HMM" = "selected",
                                                                "Plotted HMM" = "plotted",
                                                                "All HMM" = "all"),
                                                    inline = TRUE),
                                      actionButton("selectHMMButton", label="Highlight selected HMM in plot"),
                                      downloadButton ("downloadHMMTableButton", "Download Table")
                                    ),
                                    column(6,
                                      htmlOutput("downloadSet1SeqsForSelectedHMMs"),
                                      htmlOutput("downloadSet2SeqsForSelectedHMMs"),
                                      htmlOutput("linkToMapForSelectedHMM")
                                    )
                                    ),
                                  fluidRow(htmlOutput("hmmTableMessage")),
                                  DT::dataTableOutput("resultsByHMMTable")
                                  )
                          )
             
            
             )
    
    
    
  )
)



# Server definition ---------------------------------------
server <- function(input, output, session) {
  
  # instructions page  ------------------------------

  
  ## preloaded set defs  --------------------------------
  highSalt = c("3300000930",
               "3300000928",
               "3300000864",
               "3300000860")
  lowSalt = c("3300000853",
              "3300000867",
              "3300000929",
              "3300000883"
  )
  
  twitchellIsland = c("3300000030",
               "3300000094",
               "3300000316",
               "3300000318",
               "3300000854",
               "3300000894",
               "3300000917",
               "3300001281",
               "3300001547",
               "3300002961",
               "3300003432")
  saltPond = c("3300000374",
               "3300000381",
               "3300000386",
               "3300000412",
               "3300000427",
               "3300000488",
               "3300000526",
               "3300007532",
               "3300007609",
               "3300007611",
               "3300007616",
               "3300007623",
               "3300007631",
               "3300007725",
               "3300007778",
               "3300008963",
               "3300009000",
               "3300009001",
               "3300009027")
  
  saltPond2 = c("3300007532",
                "3300007533",
                "3300007535",
                "3300007536",
                "3300007537",
                "3300007614",
                "3300007616",
                "3300007619",
                "3300007623",
                "3300007626",
                "3300007628",
                "3300007633",
                "3300007635",
                "3300007668",
                "3300007712",
                "3300007723",
                "3300007725",
                "3300007757",
                "3300007764",
                "3300007769",
                "3300007775",
                "3300007778",
                "3300007784",
                "3300007983",
                "3300007984",
                "3300009000",
                "3300009001",
                "3300009027",
                "3300009033",
                "3300009035",
                "3300009060",
                "3300009138",
                "3300009145")
  twitchellIsland2 = c("3300002961",
                       "3300005873",
                       "3300005874",
                       "3300005875",
                       "3300005876",
                       "3300005879",
                       "3300005881",
                       "3300005883",
                       "3300005884",
                       "3300005885",
                       "3300005886",
                       "3300005887",
                       "3300005888",
                       "3300005889",
                       "3300005890",
                       "3300005891",
                       "3300005892",
                       "3300005893",
                       "3300005894",
                       "3300005895",
                       "3300005896",
                       "3300005897",
                       "3300005898",
                       "3300005899",
                       "3300005900",
                       "3300005901",
                       "3300005902",
                       "3300005903",
                       "3300005904",
                       "3300005905")
  
  ## selection page  --------------------------------
  
  sets <- reactiveValues(set2=twitchellIsland2, set1=saltPond2)

  # attempts to change sets after computations are started should
  # raise a warning
  warnedAboutDataMismatch = FALSE
  # removed for now because the red box warning on page may be enough
  # observeEvent (c(sets$set1, sets$set2),{
  #   #sets$set1; sets$set2
  #   if (!warnedAboutDataMismatch){
  #     if (nclicks() != 0 | !is.null(asyncData())){
  #     shinyjs::alert ("WARNING: You changed sets after starting computations. You can continue, but the results will not apply to the new sets until you complete the computations again.")
  #     warnedAboutDataMismatch <<- TRUE
  #     }
  #   }
  # })
  
  #switch data in table based on radiobutton selected
  metaDataToDisplay = reactive({
    if (input$selectTableRadioButtons == "all"){
      metaData
      }
    else if (input$selectTableRadioButtons == "set1"){
      metaData[taxon_oid %in% sets$set1]
      }
    else if (input$selectTableRadioButtons == "set2"){
      metaData[taxon_oid %in% sets$set2]
      }
  })
  
  
  output$selectionTable <- DT::renderDataTable({
    DT::datatable(metaDataToDisplay(), extensions=c("Buttons"),
                  style="bootstrap", class = 'table-condensed table-bordered',
                  rownames=FALSE,
                  colnames = c("Study Name" = "Study.Name",
                               "Sample Name" = "Genome.Name...Sample.Name"),
                  options = list(dom = 'Bfrtip',
                                 buttons=list('pageLength', 'csv', 'colvis'),
                                 pageLength=25,
                                 orderClasses =TRUE,
                                 autoWidth = TRUE,  #needed to set width below
                                 scrollX=TRUE,   #needed to set width below
                                 columnDefs = list(list(
                                   targets = c(1,2),
                                   width="300px"
                                 ))))
  })
  
  selectionTableProxy = DT::dataTableProxy('selectionTable')
  
  #update the values in showSet1BoxSelect as sets$set1 changes
  observe({
    dataMap = metaData[taxon_oid %in% sets$set1, .(taxon_oid, as.character(Genome.Name...Sample.Name))]
    dm = dataMap$taxon_oid
    names(dm) = paste (dataMap$taxon_oid, dataMap$V2, sep=" ")
    lab = sprintf ("%d metagenomes in Set 1 ", length(dm))
    if (length(dm) > 1) lab = paste (lab, "(click to view all)")
    updateSelectInput(session, "showSet1BoxSelect", choices = dm, label = lab)
  })

  #update the values in showSet2BoxSelect as sets$set2 changes
  observe({
    dataMap = metaData[taxon_oid %in% sets$set2, .(taxon_oid, as.character(Genome.Name...Sample.Name))]
    dm = dataMap$taxon_oid
    names(dm) = paste (dataMap$taxon_oid, dataMap$V2, sep=" ")
    lab = sprintf ("%d metagenomes in Set 2 ", length(dm))
    if (length(dm) > 1) lab = paste (lab, "(click to view all)")
    updateSelectInput(session, "showSet2BoxSelect", choices = dm, label = lab)
  })
  
  
  output$showSet2Box <-  renderPrint({
    s = metaData[taxon_oid %in% sets$set2, .(as.character(Genome.Name...Sample.Name))]
    if (nrow(s) > 0){
      cat ('<ol><li>')
      cat(unlist(s), sep = '</li>\n<li>')
      cat('</li></ol>')
    }

  })
  
  selLength <- reactive ({length(input$selectionTable_rows_selected)})
  
  selectionNotice = reactive({
    if (selLength() > 0){
      sprintf("%d selected metagenomes in table", selLength())
    }
    else{
      "Select rows below to add/remove to either set"
    }
  })
    
  output$set1SelectionNotice <- renderText({
    selectionNotice()
  })
  output$set2SelectionNotice <- renderText({
    selectionNotice()
  })
    
  
  observeEvent(input$addToSet1Button, {
    req(input$selectionTable_rows_selected)
    s = metaDataToDisplay()[input$selectionTable_rows_selected,taxon_oid,]
    DT::selectRows(selectionTableProxy, NULL)
    sdiff = setdiff(s, sets$set2)
    if (length(sdiff)  != length(s)){
      showNotification("Some metagenomes already in other set, not added", type = "warning") 
    }
    sets$set1 = union(sets$set1,sdiff)
  })
  
  observeEvent(input$addToSet2Button, {
    req(input$selectionTable_rows_selected)
    s = metaDataToDisplay()[input$selectionTable_rows_selected,taxon_oid,]
    DT::selectRows(selectionTableProxy, NULL)
    sdiff = setdiff(s, sets$set1)
    if (length(sdiff)  != length(s)){
     showNotification("Some metagenomes already in other set, not added", type = "warning") 
    }
    sets$set2 = union(sets$set2,sdiff)
  })
  
  
  observeEvent(input$clearSet1Button, {
    sets$set1 = character(0)
  })
  observeEvent(input$clearSet2Button, {
    sets$set2 = character(0)
  })
  
  observeEvent (input$removeOneFromSet1Button,{
    #sets$set1 = setdiff(sets$set1, input$showSet1BoxSelect)
    req(input$selectionTable_rows_selected)
    s = metaDataToDisplay()[input$selectionTable_rows_selected,taxon_oid,]
    
    #we're trying to remove from set1, find out if there's actually overlap
    sintersect = intersect (s, sets$set1)
    if (length(sintersect) > 0){
      sets$set1 = setdiff (sets$set1, sintersect)
      DT::selectRows(selectionTableProxy, NULL)
    }
    else{
      showNotification("No selected metagenomes are in set 1" ,type="warning")
    }
  })
  
  observeEvent (input$removeOneFromSet2Button,{
    #sets$set2 = setdiff(sets$set2, input$showSet2BoxSelect)
    req(input$selectionTable_rows_selected)
    s = metaDataToDisplay()[input$selectionTable_rows_selected,taxon_oid,]
    
    #we're trying to remove from set1, find out if there's actually overlap
    sintersect = intersect (s, sets$set2)
    if (length(sintersect) > 0){
      sets$set2 = setdiff (sets$set2, sintersect)
      DT::selectRows(selectionTableProxy, NULL)
    }
    else{
      showNotification("No selected metagenomes are in set 2" ,type="warning")
    }
  })
  
  
  # an obsolete map showing selected sets  
  output$selectMap <- renderPlot({
    selectedGenomes = rbind(data.frame(taxon_oid = sets$set1, condition = rep("set1", length(sets$set1))),
                            data.frame(taxon_oid = sets$set2, condition = rep("set2", length(sets$set2)))
    )
    
    metagenomes = merge (metagenomeInfo, selectedGenomes, by = "taxon_oid")
    
    ggp = drawWorldMap(ggplot())
    ggp +
      geom_point (data= metagenomes, 
                  aes (x=Longitude, 
                       y=Latitude,
                       color=condition,
                       size = Gene_Count_assembled
                  ), 
                  alpha=0.5) +
      labs(size= "Survey Gene Count", color="Condition")
    
  })
  
   
  
  setsMismatch = function (array1, array2){
    any (is.na(match(array1, array2))) |
      any (is.na(match(array2, array1)))
  }
  
  output$resultsSetsMismatchWarning <- renderText ({
    req(asyncData()$sets)
    if (setsMismatch(unlist(asyncData()$sets), c(sets$set1, sets$set2))){
      dataMismatchWarningHTML
    }
    else{""}
  })
  
  output$tableSetsMismatchWarning <- renderText ({
    req(asyncData()$sets)
    if (setsMismatch(unlist(asyncData()$sets), c(sets$set1, sets$set2))){
      dataMismatchWarningHTML
    }
    else{""}
  })
  
  
    
  ## computation page  ===========================
  statInfo <- statusSetup()
  set_status(statInfo, "Ready")
  onStop(function(){statusCleanUp(statInfo)})
  
  #when loading and computations are done, this will hold results
  asyncData = reactiveVal(NULL)
  nclicks = reactiveVal(0)

  output$statusText <- renderText ({
    #not sure if this is necessary/functional, but putting this request to read asyncData here
    # is meant to invalidate statusText when asyncData changes so that we dont' have to 
    # worry about timing/order of setting nclicks 
    asyncData()
    if (nclicks() > 0){
      invalidateLater(1000,session)
    }
    msg = get_status(statInfo)
    if (msg != "Ready"){
      openTag = "<strong>"
      closeTag = "</strong>"
    } else{
      openTag = ""
      closeTag = ""
    }
    paste (openTag, "Status: ", get_status(statInfo), closeTag)
  })
  
  # this is the main action button on this page.
  # many things should happen when its clicked
  # some ideas inspired by  http://blog.fellstat.com/?p=407
  observeEvent (input$doComparisonButton,{
    disable("doComparisonButton")
    warnedAboutDataMismatch <<- FALSE # reset this to force warning again.
    #alert ("testing")
    #ranges$x <- ranges$y <- NULL
    #with (ranges, {x <- y <- NULL})
    #selectedPoints$points <- selectedPoints$metagenome <- NULL
    #with (selectedPoints, {points <<- metagenome <- NULL})
    
    if(nclicks() != 0){
      #disable button up top should avoid this, but just in case...
      showNotification (paste ("Already running one analysis; please wait: ", get_status(statInfo)))
      return (NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    fire_running(statInfo)
    
    # start the computation, store the promise in result_val
    
    #prepare data to pass to future call
    setss = list(set1 = sets$set1, set2  = sets$set2 )
    hmmTypes = input$hmmTypes
    statusFUN = (function(message){
      set_status(statInfo, message)
    })
    result <- future({
      workingData <- LoadData(setss, hmmTypes, statusFUN)
      DoComputation(setss, workingData, statusFUN) 
      }) %...>% asyncData()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      asyncData(NULL)
                      print(e$message)
                      showNotification(paste ("Error:", e$message), type="error")
                    })
    
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready(statInfo) 
                        nclicks(0)
                        enable("doComparisonButton")
                      })
    
    # Return something other than the promise so shiny remains responsive
    NULL    
    
  })
  
  
  # 
  # tabulate necessary data for taxonOIDs and hmmTypes
  # this gets called in a future, so beware of using global variables
  # 
  LoadData <- function (sets, hmmTypes, statusFUN = NULL){
    taxonIDs = unlist(sets)
    summaryDataList = list()
    if (!is.null(statusFUN)) statusFUN("Loading pfam data for selected metagegenomes")
    if ("pfam" %in% hmmTypes){
      for (taxon in taxonIDs){
        if (is.null(summaryDataList[[taxon]])){
          summaryDataList[[taxon]] <- loadOneTaxonSummary(taxon)
        }
      }
    }
    if ("pfam" %in% hmmTypes){
      result = do.call(rbind, summaryDataList)
    }
    else{
      result = data.table (taxon_oid = factor(NULL),
                           hmm = character(0),
                           numSeqs =integer(0),
                           sumCoverage = numeric(0))
    }
    if ("families" %in% hmmTypes){
      result = rbind (result, getSFLDFamilyData(taxonIDs))
    }
    if ("subgroups" %in% hmmTypes){
      result = rbind (result, getSFLDSubgroupData(taxonIDs))
    }
    result = fillInZeros (result, sets)
    result = addMetagenomeInfo(result, metagenomeInfo)
    result[,numSeqsLogFriendly:=as.numeric(numSeqs)]
    result[numSeqs==0, numSeqsLogFriendly := 0.1]
    result
  }
  
  DoComputation <- function(setss, workingData,statusFUN = NULL){
    if (!is.null(statusFUN)) statusFUN("Comparing HMMSEARCH results, calculating statistics")
    
    allHmm = levels(workingData$hmm)
      #n = length(dataByHMM)
      n = length(allHmm)
      df = data.table(hmm = levels(workingData$hmm), 
                      meanSet1 = numeric(n), 
                      meanSet2 = numeric(n), 
                      ratioLog2 = numeric(n), 
                      pValue = numeric(n), 
                      qValue = numeric(n)
      )
      for (i in 1:n){
        #hmm = names(dataByHMM)[i]
        hmmI = allHmm[i]
        #pValue = pPoisson(dataByHMM[[hmm]])
        #stats = lm.summary (workingData[hmm==hmmI])
        stats = t.test.summary(workingData[hmm==hmmI])
        #stats = pPoisson.summary(workingData[hmm==hmmI])
        df$pValue[i] = stats$pValue
        df$meanSet1[i] = stats$meanSet1
        df$meanSet2[i] = stats$meanSet2
        df$ratioLog2[i]=stats$ratioLog2
        #df$effectError[i] = stats$effectError
        if (!(i%%200)){
          if (!is.null(statusFUN)) statusFUN(sprintf("Comparing HMMSEARCH results, calculating statistics  %d%% done", round(i/n * 100)))
        }
      }
  if (!is.null(statusFUN)) statusFUN("Comparing HMMSEARCH results, calculating q-values")
  df$qValue = tryCatch( { qvalue::qvalue(df$pValue)$qvalues},
            error=function(cond){
              print("Failure while computing q values")
              print(cond)
              print(setss)
              return(df$pValue)
              })
  list(sets = setss, hmmMatchTable = workingData, hmmStats = df)    
  }
  
  # computedData
  computedData = reactive({
    req(asyncData())
    list(hmmStats = asyncData()$hmmStats, qValues=NULL)
  })
  
  # dataForChosenSets
  dataForChosenSets = reactive({
    req(asyncData())
    asyncData()$hmmMatchTable
  })


  selectedMetagenomes = reactive({
    selectedGenomes = rbind(data.frame(taxon_oid = sets$set1, condition = rep("set1", length(sets$set1))),
                            data.frame(taxon_oid = sets$set2, condition = rep("set2", length(sets$set2)))
    )
    
    merge (metagenomeInfo, selectedGenomes, by = "taxon_oid")
    
  })
  
  # _metagenome scatter -------------------------------
  output$metagenomesInSetsPlot <- renderPlot({
    metagenomeScatter()
  })
  
  metagenomeScatter <- reactive ({
    xRange = range(selectedMetagenomes()$Gene_Count_assembled)
    yRange = range(selectedMetagenomes()$totalPfamCount)
    #plot(totalPfamCount~Genome_Size_assembled, data = metagenomes, color = condition)
    ggp = ggplot(data=selectedMetagenomes()) + 
      geom_point(aes(x=Gene_Count_assembled, y=totalPfamCount, color = condition)) +
      scale_color_discrete(name="Set",
                           breaks=c("set1", "set2"),
                           labels=c(input$set1Name, input$set2Name))  + 
      scale_x_log10(
                    limits = niceLimitsInLog10Space(xRange) #c(5e5, 2e6)#expand=expand_scale(0.4)
      ) + scale_y_log10(limits = niceLimitsInLog10Space(yRange)
      ) +
      labs (y = "Total PFAM matches")  +
      annotation_logticks(sides = "trbl") + theme(panel.grid.minor = element_blank())
    
    
    if (!is.null(selectedPoints$metagenome) ){
      ggp = ggp + geom_point(aes(x=Gene_Count_assembled, y=totalPfamCount),data=selectedMetagenomes()[taxon_oid == selectedPoints$metagenome],  size=5, shape=21)
    }
    
    ggp
    
  })  

  output$downloadMetagenomeScatter <- downloadHandler(
    filename = function(){
      paste ("metagenomeScatter",
             input$set1Name, input$set2Name,
             Sys.Date(), "pdf", sep="." )
    },
    content = function(file) {
      ggsave(file, plot = metagenomeScatter(), height=6, width=8)
    }
  )
  
  
  observeEvent(input$metagenomeScatter_click,{
    clickedPoint = nearPoints(selectedMetagenomes(),coordinfo=input$metagenomeScatter_click, 
                              maxpoints=1)
    if (length(clickedPoint) > 0){
      selectedPoints$metagenome = clickedPoint[1]$taxon_oid
      
      updateSelectInput(session, "showSet1BoxSelect", selected = selectedPoints$metagenome)
      updateSelectInput(session, "showSet2BoxSelect", selected = selectedPoints$metagenome)
      
    }
    else{
      selectedPoints$metagenome = NULL
    }
  })
  
  # _HMM scatter ----------------------------
  observeEvent (computedData(),{
    r = range(log10(computedData()$hmmStats$qValue[computedData()$hmmStats$qValue > 0]))
    updateSliderInput(session, 'qValueSelector', min = floor(r[1]), max=ceiling(r[2]))
  })
  
  subsetStats = reactive({
    computedData()$hmmStats[qValue < (10^input$qValueSelector)]
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)   #obsolete
  selectedPoints <- reactiveValues(points=NULL, metagenome=NULL)
  
  
  selectPoints <- function(rows, toggle=FALSE){
    if (nrow(rows) > 0){
      if(is.null(selectedPoints$points) || dim(selectedPoints$points)[1]==0){
        selectedPoints$points = rows
        return(nrow(rows))
      }
      matches = which (selectedPoints$points$hmm %in% rows$hmm)
      if (toggle & length(matches >0)){
        selectedPoints$points = selectedPoints$points[-matches,]
        return (-(length(matches)))
      }
      else{
        indeces = setdiff(1:nrow(selectedPoints$points), matches)
        selectedPoints$points = rbind (selectedPoints$points[indeces,], rows, fill=TRUE)
        return (nrow(rows) - length(matches))
      }
    }
  }
  
  observeEvent(input$scatter_dblclick, {
    clickedPoint = nearPoints(subsetStats(),coordinfo=input$scatter_dblclick, maxpoints=1)
    if (nrow(clickedPoint) > 0){
      if(is.null(selectedPoints$points) || dim(selectedPoints$points)[1]==0){
        selectedPoints$points = clickedPoint
        return()
      }
      clickedPoint = clickedPoint[1,]
      matches = which (selectedPoints$points$hmm == clickedPoint$hmm[1])
      if (length(matches >0)){
        selectedPoints$points = selectedPoints$points[-matches,]
      }
      else{
        selectedPoints$points = rbind (selectedPoints$points, clickedPoint, fill=TRUE)
      }
    }
  })

 
  observeEvent(input$scatter_brush, { #input$selectRect_button,
    brush <- input$scatter_brush
    if (!is.null(brush)){
     selectPoints (brushedPoints(subsetStats(),brush)) 
    }
  })
  
  observeEvent(input$clearSelect_button,{
    selectedPoints$points = NULL
  })

  output$scatter_pfamLabel <- renderText({
    pointStr = "Hover over point above to see HMM name"
    if (!is.null(input$scatter_hover)){
      point = nearPoints(computedData()$hmmStats,input$scatter_hover,  maxpoints=1)
      if (nrow(point) > 0){
        hmm = point[1,]$hmm
        pointStr = pfamInfoString(hmm)
      }
    }
    pointStr
  })
    
  output$resultsScatter <- renderPlot({
    hmmScatter()
  })
  hmmScatter <- reactive ({
    stats = subsetStats()
    req(stats)
    ggp = ggplot (data = stats) +
      geom_point (aes(x=meanSet2, y=meanSet1, color=log10(qValue)), alpha=0.5
      ) + scale_x_log10() + scale_y_log10()
    if (!is.null(selectedPoints$points) & length(selectedPoints$points) > 0){
      ggp = ggp + geom_point(aes(x=meanSet2, y=meanSet1),data=selectedPoints$points,  size=5, shape=23)
    }
    if (!is.null(ranges$x)){
      ggp = ggp + coord_cartesian(xlim=ranges$x, ylim=ranges$y)#+
    }
    ggp= ggp + labs(x=input$set2Name, y=input$set1Name, title=sprintf("HMM Match Rates Compared %s vs %s", input$set1Name, input$set2Name), subtitle="Points off the diagonal show strongest differences")
  
    xrange = range(stats$meanSet2)
    yrange = range(stats$meanSet1)
    xyrange =c(max(xrange[1], yrange[1]), min(xrange[2], yrange[2]))
    
    x4line = 10^ seq(log10(xyrange[1]), log10(xyrange[2]), length.out=20)
    ggp = ggp + geom_line(data=data.frame(x=x4line, y=x4line), aes(x=x,y=y), linetype=2)
    ggp
  })

  output$downloadHMMScatter <- downloadHandler(
    filename = function(){
      paste ("hmmScatter",
             input$set1Name, input$set2Name,
             Sys.Date(), "pdf", sep="." )
    },
    content = function(file) {
      ggsave(file, plot = hmmScatter(), height=6, width=8)
    }
  )
  

  focusedData = reactive({
    req(selectedPoints$points)
    dataForChosenSets()[hmm %in% selectedPoints$points$hmm][, hmm:=droplevels(hmm)]
  })

  
  selectedHMMNames = reactive({
    ydata <- pfamInfo[pfamAcc %in% levels(focusedData()$hmm), .(pfamAcc, description)]
    ydata <- rbind (ydata, subgroupNames[hmmAcc %in% levels(focusedData()$hmm),
                                         .(pfamAcc = hmmAcc, description)
                                         ]
                    )
    ydata <- rbind (ydata, familyNames[hmmAcc %in% levels(focusedData()$hmm),
                                         .(pfamAcc = hmmAcc, description)
                                         ]
    )
    names(ydata) = c("hmm", "description")
    ydata
  })
  
  allHMMNames = reactive ({
    ydata <- pfamInfo[pfamAcc %in% levels(dataForChosenSets()$hmm), .(pfamAcc, description)]
    ydata <- rbind (ydata, subgroupNames[hmmAcc %in% levels(dataForChosenSets()$hmm),
                                         .(pfamAcc = hmmAcc, description)
                                         ]
    )
    ydata <- rbind (ydata, familyNames[hmmAcc %in% levels(dataForChosenSets()$hmm),
                                       .(pfamAcc = hmmAcc, description)
                                       ]
    )
    names(ydata) = c("hmm", "description")
    ydata
    
  })
  
  # _data detail plot --------------------
  output$resultsPlot <- renderPlot({
    resultsDetailPlot()
  }, width=1300)
  
  resultsDetailPlot <- reactive ({
  
    req(selectedPoints$points)
    #plotData = dataForChosenSets()
    ydata = selectedHMMNames()

    ggp = ggplot(data = focusedData())
    if (input$rateOrCount_radioButtons == "rate"){
      ggp = ggp + geom_point(aes(y=hmm, 
                                       x=  ratio, 
                                       color = condition, group=condition, size=Gene_Count_assembled),
                                   alpha=0.6)+
        scale_x_log10() +
        xlab("# HMM matches per # Metagenome Sequences")
    }
    else{
      recodeZero = function(breaks){
        breaks[breaks == 0.1] = 0
        breaks
      }
      
      ggp = ggp + geom_point(aes(y=hmm, 
                                 x = numSeqsLogFriendly,
                                 color = condition, group=condition, size=Gene_Count_assembled),
                             alpha=0.6
                             ) +
        xlab("# of sequences (+1)") +
        scale_x_log10(labels=recodeZero)
    }
    
    if (!is.null(selectedPoints$metagenome)){
      if (input$rateOrCount_radioButtons == "rate"){
        ggp = ggp + geom_point(aes(y=hmm, x = ratio), size=5, shape=21, data=focusedData()[taxon_oid == selectedPoints$metagenome])
      }
      else {
        ggp = ggp + geom_point(aes(y=hmm, x = numSeqsLogFriendly), size=5, shape=21, data=focusedData()[taxon_oid == selectedPoints$metagenome])
      }
    }
    
    ggp = ggp + ylab("")+
      
      theme(axis.text.y=element_text(size=12, face ="bold"), legend.text=element_text(size=rel(1.0)))+
      scale_color_discrete(name="Set",
                          breaks=c("set1", "set2"),
                          labels=c(input$set1Name, input$set2Name)) +
      scale_y_discrete (breaks = ydata$hmm, 
                        labels = ydata$description,
                        position = "right")
    #coord_flip()
    ggp = ggp + theme(legend.position="top")
    
    ggp
  })

  observeEvent(input$results_click, {
    clickedPoint = nearPoints(focusedData(),coordinfo=input$results_click, maxpoints=1)
    if (length(clickedPoint) > 0){
      selectedPoints$metagenome = clickedPoint[1]$taxon_oid
    }
    else{
      selectedPoints$metagenome = NULL
    }
  })
  
  
  output$results_metagenomeLabel <- renderText({
    pointStr = "test2"
    if (!is.null(input$results_hover)){
      point = nearPoints(focusedData(),input$results_hover,  maxpoints=1)
      if (nrow(point) > 0){
        pointStr = paste (point[1,.(taxon_oid, GenomeSample_Name)])
      }
    }
    pointStr

  })
  
  
  output$selectedMetagenome_label <- renderText({
    if (isTruthy(selectedPoints$metagenome)){
    paste ("Metagenome in &#9711;:", metagenomeInfo[taxon_oid == selectedPoints$metagenome]$GenomeSample_Name)
    }
    else{"Click on point above or below to see name of metagenome sample here"}
  })
  
  output$downloadDetailPlot <- downloadHandler(
    filename = function(){
      paste ("resultsDetail",
             input$set1Name, input$set2Name,
             Sys.Date(), "pdf", sep="." )
    },
    content = function(file) {
      ggsave(file, plot = resultsDetailPlot(), height=6, width=20)
    }
  )

  ##  results and download page  -----------------------------
  
  hmmTableProxy = DT::dataTableProxy('resultsByHMMTable')
  
  
  dataForMetagenomeTable <- reactive ({
    metagenomeTable = selectedMetagenomes()
    if (input$set1Name != "set2"){  #if set1Name == "set2", we're in trouble...
      metagenomeTable[condition == "set1", condition := input$set1Name]
      metagenomeTable[condition == "set2", condition := input$set2Name]
    }
    
    if (length(selectedPoints$points) > 0) {
    hmmHits = dcast(focusedData(), taxon_oid~hmm, value.var="numSeqs")
    return (merge(metagenomeTable, hmmHits, by = "taxon_oid") )
    }
    else return (metagenomeTable)
  })
  
  
  output$resultsByMetagenomeTable <- DT::renderDataTable({
    MetagenomeTable_ =function(){} # for editor navigation
    
    DT::datatable (dataForMetagenomeTable(), 
                   style="bootstrap", class = 'table-condensed table-bordered',
                   rownames=FALSE,
                   colnames = c("Sample Name" = "GenomeSample_Name",
                                "total PFAM matches" = "totalPfamCount",
                                "Metagenome Size" = "Genome_Size_assembled",
                                "Metagenome Gene Count" = "Gene_Count_assembled",
                                "Metagenome Scaffold Count" = "Scaffold_Count_assembled",
                                "Metagenome rRNA 16S Count" = "rRNA_16S_Count_assembled"),
                   
                   options = list(dom = 'Bfrtip',
                                  buttons=list('pageLength'),
                                  pageLength=25,
                                  orderClasses =TRUE,
                                  autoWidth = TRUE,  #needed to set width below
                                  scrollX=TRUE,   #needed to set width below
                                  columnDefs = list(list(
                                    targets = c(7),
                                    width="500px"
                                  ))) )
  })
  

  hmmTableDataFull <- reactive({
    #build up table in 'wide' format from
    # summary data in long format e.g.: hmm,taxon,count
    # computed data : hmm, mean1, mean2, pValue
    # hmm info table:  hmmName mostly
    
    # dataForChosenSets()
    # computedData()$hmmStats
    # allHMMNames()
    hmmTable = merge (computedData()$hmmStats, allHMMNames(), by = "hmm")
    #subset the summary then convert to wide format using dcast:
    hmmHits = dcast(dataForChosenSets()[taxon_oid %in% c(sets$set1, sets$set2), .(taxon_oid,hmm, numSeqs)],
                    hmm~taxon_oid, value.var="numSeqs")
#    for (tid in c(sets$set1, sets$set2)){
#      taxonColumn = hmmHits[taxon_oid == tid, .(hmm, numSeqs)]
#      setnames(taxonColumn, old = "numSeqs", new = tid)
#      hmmTable = merge(hmmTable, taxonColumn, by = "hmm", all.x=TRUE)
#    }
    hmmTable = merge (hmmTable, hmmHits, by = "hmm")
    hmmTable
  })
  
  hmmTableDataSubset <- reactive({
    fullTable = hmmTableDataFull()
    if (input$radioButtons_HowManyHMMRows == "selected"){
      subTab = fullTable[hmm %in% selectedPoints$points$hmm]
    } else if (input$radioButtons_HowManyHMMRows == "plotted"){
      subTab = fullTable[hmm %in%  computedData()$hmmStats[qValue < (10^input$qValueSelector)]$hmm]
    } else if (input$radioButtons_HowManyHMMRows == "all"){
      subTab = fullTable
    }
    subTab
  })
   
  # I"m guessing because my column names are edited after the DT::dataTable call, this doesn't work.
  # sort out the names and potentially we can avoid rebuilding the table when the subset changes.
  #observe({
  #  DT::replaceData(hmmTableProxy, hmmTableDataSubset())
  #})
  
  output$resultsByHMMTable <- DT::renderDataTable({
    HMMTable_ =function(){} # ignore, only here for editor navigation
    
    #hmmTableDataFull()  #invalidate based on full data but not the subset so I use isolate below
    #hmmTable = isolate(hmmTableDataSubset()) #hmmTableDataWithMetagenomeCounts()
    hmmTable = hmmTableDataSubset()
    #jump through a few hoops to get name conversion data strucutre as needed
    meanSet1ColName = paste (input$set1Name, "HMM rate")
    meanSet2ColName = paste (input$set2Name, "HMM rate")
    colNamesToAdjust = c("meanSet1", "meanSet2", "ratioLog2")
    names(colNamesToAdjust) = c(meanSet1ColName, meanSet2ColName, "log2 ratio")
   
    hmmTable %>%
      DT::datatable(style="bootstrap", 
                    class = 'table-condensed table-bordered',
                    rownames=FALSE,
                    colnames = colNamesToAdjust,
                    options(pageLength=25,
                            orderClasses=TRUE, #highlight the ordered column
                            autoWidth=TRUE, scrollX=TRUE,  #needed to set width below
                            columnDefs = list(list(
                              targets = c(6),
                              width="300px"
                            ))
                    )
                            
      
                    ) %>%

      #formatRound(columns=c('ratioLog2'), digits=1) %>%
      DT::formatSignif(columns=c(meanSet1ColName,
                             meanSet2ColName,
                             'pValue',
                             'qValue',
                             'log2 ratio'),
                   digits=2)
  })

  output$downloadMetagenomeTableButton <- downloadHandler(
    filename = function(){
      paste ("metagenomeTable",
             input$set1Name, input$set2Name,
             Sys.Date(), "csv", sep="." )
    },
    content = function(file){
      write.csv ( dataForMetagenomeTable() , file)
    }
  )
    
  output$downloadHMMTableButton  <- downloadHandler(
    filename = function(){
      paste ("hmmTable",
             input$set1Name, input$set2Name,
             input$radioButtons_HowManyHMMRows,
             Sys.Date(), "csv", sep="." )
    },
    content = function(file){
     write.csv (hmmTableDataWithMetagenomeCounts(), file) 
    }
  )
  
  selectHMMButton_ = function(){} #for editor navigation
  observe({
    if (input$radioButtons_HowManyHMMRows == "selected" |
        length(input$resultsByHMMTable_rows_selected)==0){
      shinyjs::disable(id="selectHMMButton")
    }
    else{
      shinyjs::enable(id="selectHMMButton")
    }
  })
  
  observeEvent(input$selectHMMButton, {
    #if any rows are selected, add it to the list of selected points
    req(input$resultsByHMMTable_rows_selected)
    numSelected = selectPoints(hmmTableDataSubset()[input$resultsByHMMTable_rows_selected])
    
    if (numSelected == 0){
      showNotification("All selected rows already highlighted", type="warning")
    }
    else{
      showNotification(sprintf ("%d selected rows newly highlighted. View in Computations tab or with 'Plot-highlighted HMM' radio button.", numSelected), type="message")
    }
    
  })
  
  output$downloadSet1SeqsForSelectedHMMs <- renderText({
    hmm = hmmTableDataSubset()[input$resultsByHMMTable_rows_selected]$hmm
    pfams = hmm[grep("pfam", hmm)]
    if ( length(pfams) == 0 ) return ("")
    setName = input$set1Name
    taxonOIDs = sets$set1
    
    CreateJGIFormManyPfamManyMetagenome(pfams, taxonOIDs, setName)
    
  })

  output$downloadSet2SeqsForSelectedHMMs <- renderText({
    hmm = hmmTableDataSubset()[input$resultsByHMMTable_rows_selected]$hmm
    pfams = hmm[grep("pfam", hmm)]
    if ( length(pfams) == 0 ) return ("")
    setName = input$set2Name
    taxonOIDs = sets$set2
    
    CreateJGIFormManyPfamManyMetagenome(pfams, taxonOIDs, setName)
    
  })
  
  output$linkToMapForSelectedHMM <- renderText({
    hmm = hmmTableDataSubset()[input$resultsByHMMTable_rows_selected]$hmm
    CreateImagenitMapForm (hmm)
  })
  
  output$hmmTableMessage <- renderText({
    hmmTableMessageHTML
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

