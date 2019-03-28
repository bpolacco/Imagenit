
library (data.table)
library(ggplot2)
library (RColorBrewer)

config <- config::get()
dataFile = function(name){
  paste(config$dataDir, config[[name]], sep='/')
}


backgroundHMMRate = rbind(fread(dataFile("backgroundPfamRateFile"))[,.(hmm=pfam, perTotalPfam)],
                          fread(dataFile("backgroundFamilyRateFile"))[,.(hmm, perTotalPfam)],
                          fread(dataFile("backgroundSubgroupRateFile"))[,.(hmm, perTotalPfam)]
                          
)

pfamInfo = fread (dataFile("pfamInfoFile")) 
#pfamInfo  = merge (pfamInfo, backgroundHMMRate, by.x = "pfamAcc", by.y = "hmm", all.y = TRUE)
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


#studyNames = fread (dataFile("studyNamesFile"), integer64="character")

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

# store globally the so we don't repeatedly read the same data
hmmData = list()

getPfamHMMFileName = function (hmm){
  # due to a 10,000 file limit in cloud Shiny Server, I put multiple pfam hmm per file
  # simple hack is to leave the last char off of pfam name and combine names with same "prefix"
  
  #hmmSummaryFileFormat = "./data/shortSummariesCombinedPerHMM/hmm.%s.csv"
  
  sprintf (dataFile("hmmSummaryFileF"), substr(hmm,1,nchar(hmm)-1))
  
}
LoadPfamSummary <- function(hmm){    
  summaryData = NULL
  tryCatch({
    summaryData = fread(getPfamHMMFileName(hmm), integer64="character")[pfam==hmm]
  }, error = function(e){
    print(e)
    summaryData <<- NULL
  })
  #easier here then editing my 10K files
  setNames(summaryData, c("hmm", "taxon_oid","numSeqs", "sumCoverage"))
}

#load whole table into memory, subset table based on hmm
familyHitCountsFull <- NULL
LoadFamilySummary <- function(hmmIn){
  if (is.null(familyHitCountsFull)){
    familyHitCountsFull <<- fread (dataFile("sfldFamilyHitCountsFile"), integer64="character")
  }
  familyHitCountsFull[hmm==hmmIn]
}

#load whole table into memory, subset table based on hmm
subgroupHitCountsFull <- NULL
LoadSubgroupSummary <- function(hmmIn){
  if (is.null(subgroupHitCountsFull)){
    subgroupHitCountsFull <<- fread (dataFile("sfldSubgroupHitCountsFile"), integer64="character")
  }
  subgroupHitCountsFull[hmm==hmmIn]
}

getHMMData = function (hmm){
  if (is.null(hmmData[[hmm]])){
    if (length(grep ("pfam", hmm))){loader = LoadPfamSummary }
    else if (length(grep("Family_",hmm))){loader = LoadFamilySummary }
    else if (length(grep("Subgroup_",hmm))){loader = LoadSubgroupSummary }
    hmmData[[hmm]] <<- loader(hmm)
  }
  hmmData[[hmm]]
}


# For now, do this per HMM when view is requested using following function
#allHitsLongNorm$poissonTestScore = merge(allHitsLongNorm,backgroundHMMRate,by = "hmm"
#                                         )[,poissonTestOnArrays(numSeqs,Gene_Count_assembled, backgroundHMMRate)]


computeMapData = function(hmmName){
  
  hmmData = getHMMData(hmm=hmmName)
  if (is.null(hmmData)){
    return (NULL)
  }
  
  #merge with the metagenomeInfo which lets us fill out zeros and compute expected counts
  mergedData = merge ( metagenomeInfo, hmmData, by.x = "taxon_oid", by.y = "taxon_oid", all.x = TRUE)
  mergedData[is.na(numSeqs), numSeqs := 0]
  
  #get expectedCount which is simply totalPfam hits per metagenome times global fraction of all pfam hits  to this hmm
  mergedData[,expectedCount := totalPfamCount * backgroundHMMRate[hmm==hmmName, perTotalPfam]]
  mergedData$log2Ratio = log2((mergedData$numSeqs+1)/(mergedData$expectedCount+1))
  mergedData$poissonTestScore = with(mergedData,
                                     {
                                       poissonTestOnArrays(numSeqs,expectedCount) 
                                     }
  )
  mergedData
}



colorsGrad = colorRampPalette(rev(brewer.pal(11,config$brewerColorPalette)))(22)



#format the SFLD subgroup names with superfamily information

subgroupNames = as.data.table( read.csv(dataFile("sfldSubgroupsNamesFile"), header=TRUE, sep="\t") )
subgroupNames$hmmName = with(subgroupNames,
                             {paste(Superfamily_ID, 
                                    ".Subgroup_", 
                                    Subgroup_ID, 
                                    sep="")
                             }
)

subgroupNames$dispText = with(subgroupNames,
                              {paste(superfamily,
                                     "--",
                                     subgroup
                              )
                              }
)

#this structure is used for the subgroup selection box. 
# result is a list of named vectors of hmm ids. Each item in the list
# is all subgroups per a superfamily. 
namedSubgroups = with(subgroupNames[, .(hmmName, subgroup, superfamily)],
                      split(setNames(as.character(hmmName), subgroup), superfamily)
)

#format the SFLD family names with superfamily information
familyNames = as.data.table( read.csv(dataFile("sfldFamiliesNamesFile"), header=TRUE, sep="\t") )
familyNames$hmmName = with(familyNames,
                           {paste(Superfamily_ID, 
                                  ".Family_", 
                                  Family_ID, 
                                  sep="")
                           }
)

familyNames$dispText = with(familyNames,
                            {paste(superfamily,
                                   "--",
                                   family)
                            }
)

#this structure is used for the family selection box. 
# result is a list of named vectors of hmm ids. Each item in the list
# is all familys per a superfamily. 
namedFamilies = with(familyNames[, .(hmmName, family, superfamily)],
                     split(setNames(as.character(hmmName), family), superfamily)
)

