
library (data.table)
library(ggplot2)
library (RColorBrewer)

config <- config::get()
dataFile = function(name){
  paste(config$dataDir, config[[name]], sep='/')
}


backgroundHMMRate = fread(dataFile("backgroundPfamRateFile")) 
pfamInfo = fread (dataFile("pfamInfoFile")) 
pfamInfo  = merge (pfamInfo, backgroundHMMRate, by.x = "pfamAcc", by.y = "pfam", all.y = TRUE)
#pfamInfo[is.na(pfamA_id), :=(pfamA_id = pfamA_acc, description = pfamA_acc)]
pfamInfo[is.na(pfamA_id), pfamA_id := pfamAcc]
pfamInfo[is.na(description), description := pfamAcc]



#metagenome table
metagenomeInfo = fread(dataFile("metagenomeInfoFile") , integer64="character")
metagenomeInfo[,taxon_oid := as.character(taxon_oid)]
#where lat/long is missing array them along the south pole.
missingLatLong = is.na(metagenomeInfo$Latitude) | is.na(metagenomeInfo$Longitude)
metagenomeInfo$Longitude[missingLatLong] = seq (-180,180, by = 360/(sum(missingLatLong)-1))
metagenomeInfo$Latitude[missingLatLong] = -90


studyNames = fread (dataFile("studyNamesFile"), integer64="character")

availablePfam = pfamInfo$pfamAcc
names(availablePfam) = paste ("(PF", substr(pfamInfo$pfamAcc, 5,10), ") ", pfamInfo$description, sep="")

#a function to convert any text (but here a user input) into a valid pfam to check the
# availableSubgroups above.  so target is something like "pfam00032"
# goal is that 32, 0032, PF0032, PF32, pfam32  etc will translate to pfam00032
# a bit hackish, basically we spilt the string at any letters, than find the first thing that looks like an integer
GetValidPFAMAccession = function(input){
  numberCode = NA
  formatNumberCode = function(numberCode){
    if (is.na(numberCode) | numberCode > 99999){NA}
    else {sprintf("pfam%05d", as.integer(numberCode))}
  }
  words = unlist(strsplit(input, "[a-zA-Z]+"))
  if (length(words) > 0)
    for (numberCode in words)
      if (!is.na(as.integer(numberCode)))
          break
  formatNumberCode(numberCode)
}


#function to compute our deviation statistic
poissonTestOnArrays = function (eventsArray, expectedCounts){
  pValues = rep(0, length(eventsArray))
  for (i in 1:length(pValues)){
    pValues[i] = -log10(poisson.test (eventsArray[i], expectedCounts[i])$p.value)
    if (eventsArray[i] < expectedCounts[i])
      pValues[i]  = -pValues[i]
  }
  pValues
}

poissonTestPValue = function(observed, expected){
	pValue = -log10(poisson.test (observed, expected)$p.value)
	if (observed < expected)
		pValue = -pValue
	pValue	
}


hmmData = list()

getPfamHMMFileName = function (hmm){
  # due to a 10,000 file limit in cloud Shiny Server, I put multiple pfam hmm per file
  # simple hack is to leave the last char off of pfam name and combine names with same "prefix"
  
  #hmmSummaryFileFormat = "./data/shortSummariesCombinedPerHMM/hmm.%s.csv"
  
  sprintf (dataFile("hmmSummaryFileF"), substr(hmm,1,nchar(hmm)-1))
  
}

getHMMData = function (hmm){
  if (!is.null(hmmData[[hmm]])){
    summaryData = hmmData[[hmm]]
  }
  else{
    #hmmSummaryFileFormat = "./data/shortSummariesPerHMM/%s/hmm.%s.csv"
    tryCatch({
      #todo fix this so it works for sfld hmms as well...a
      summaryData = fread(getPfamHMMFileName(hmm), integer64="character")[pfam==hmm]
      hmmData[[hmm]] = summaryData 
    }, error = function(e){
      summaryData <<- NULL
    })
  }

  summaryData
}


# For now, do this per HMM when view is requested using following function
#allHitsLongNorm$poissonTestScore = merge(allHitsLongNorm,backgroundHMMRate,by = "hmm"
#                                         )[,poissonTestOnArrays(numSeqs,Gene_Count_assembled, backgroundHMMRate)]


computeMapData = function(hmmName){
  
  hmmData = getHMMData(hmm=hmmName)
  req(hmmData)
  
  #get a subset of the summary data specific to a single pfam, and merge with the metagenomeInfo
  #mergedData = merge ( metagenomeInfo, summaryDataLong[pfam==hmmName], by.x = "taxon_oid", by.y = "taxonOID", all.x = TRUE)
  mergedData = merge ( metagenomeInfo, hmmData, by.x = "taxon_oid", by.y = "taxonOID", all.x = TRUE)
  
  mergedData[is.na(numGenes), numGenes := 0]
  
  #get expectedCount which is simply totalPfam hits per metagenome times global fraction of all pfam hits  to this hmm
  mergedData[,expectedCount := totalPfamCount * backgroundHMMRate[pfam==hmmName, perTotalPfam]]
  mergedData$log2Ratio = log2((mergedData$numGenes+1)/(mergedData$expectedCount+1))
  mergedData$poissonTestScore = with(mergedData,
                                     {
                                       poissonTestOnArrays(numGenes,expectedCount) 
                                     }
  )
  mergedData
}




# function to draw world map on existing ggplot
# drawWorldMap <- function(ggp){
#   ggp + geom_polygon(data = world, 
#                      aes(x=long, y = lat, group = group, text=region),
#                      fill="darkgray",
#                      color="white") + 
#     coord_fixed(1.3) +
#     guides(fill=FALSE)  # do this to leave off the color legend
#   
# }



colorsGrad = colorRampPalette(rev(brewer.pal(11,config$brewerColorPalette)))(22)



