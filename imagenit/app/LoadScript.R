
library (data.table)
library(ggplot2)
library (RColorBrewer)
config <- config::get()

dataFile = function(name){
  paste(config$dataDir, config[[name]], sep='/')
}


# data loading ---------------------------------

summaryDataList = list()
metagenomeDataFileFormat = dataFile("metagenomeDataFileNameF")
metagenomeDataFile <- function(taxon_oid){
  sprintf (metagenomeDataFileFormat, as.character(taxon_oid))
}
loadOneTaxonSummary <- function(taxon_oid){
  tryCatch({
    summaryData = fread(metagenomeDataFile(taxon_oid), integer64="character")
  }, error = function(e){
    summaryData <<- NULL
  })
  summaryData
}


#metagenome table
metagenomeInfo = fread(dataFile("metagenomeInfoFile"),  integer64="character")
metagenomeInfo[,taxon_oid := as.character(taxon_oid)]
#where lat/long is missing array them along the south pole.
missingLatLong = is.na(metagenomeInfo$Latitude) | is.na(metagenomeInfo$Longitude)
metagenomeInfo$Longitude[missingLatLong] = seq (-180,180, by = 360/(sum(missingLatLong)-1))
metagenomeInfo$Latitude[missingLatLong] = -90



#backgroundHMMRate = fread("./data/backgroundPFAMRate.txt")
pfamInfo = fread (dataFile("pfamInfoFile") )
#pfamInfo  = merge (pfamInfo, backgroundHMMRate, by.x = "pfamAcc", by.y = "pfam", all.y = TRUE)
#pfamInfo[is.na(pfamA_id), :=(pfamA_id = pfamA_acc, description = pfamA_acc)]
pfamInfo[is.na(pfamA_id), pfamA_id := pfamAcc]
pfamInfo[is.na(description), description := pfamAcc]

pfamInfoString <- function(acc){
  paste (pfamInfo[pfamAcc == acc,.(pfamA_id, description)][1], " : ")
  
}


#allHitsLongNorm = as.data.table(read.csv ("./data/topHitCountsPerHMMAndSurveyWithNormalizingData.csv"))

# backgroundHMMRate = allHitsLongNorm[,
#                                     .(backgroundHMMRate = sum(as.numeric(numSeqs))
#                                       /sum(as.numeric(Gene_Count_assembled))),
#                                     by = hmm]

#meanHitRates = lapply (split(allHitsLongNorm$numSeqs/allHitsLongNorm$Gene_Count_assembled, allHitsLongNorm$hmm), mean)
#dataSplitByHMM = split(allHitsLongNorm, allHitsLongNorm$hmm)

# allHitsLongNorm$expectedCount = merge(allHitsLongNorm,backgroundHMMRate,by = "hmm"
#                                       )[,.(expectedCount=Gene_Count_assembled*backgroundHMMRate)]

#for (name in names(dataSplitByHMM) )
#  dataSplitByHMM[[name]]$expectedCounts = dataSplitByHMM[[name]]$Gene_Count_assembled * meanHitRates[[name]]

#for (name in names(dataSplitByHMM) )
#  dataSplitByHMM[[name]]$poissonProbability = ppois(dataSplitByHMM[[name]]$numSeqs,  dataSplitByHMM[[name]]$expectedCounts)

# poissonTestOnArrays = function (eventsArray, expectedCounts){
#   pValues = rep(0, length(eventsArray))
#   for (i in 1:length(pValues)){
#     pValues[i] = -log10(poisson.test (eventsArray[i], expectedCounts[i])$p.value)
#     if (eventsArray[i] < expectedCounts[i])
#       pValues[i]  = -pValues[i]
#   }
#   pValues
# }

#allHitsLongNorm$poissonTestScore = merge(allHitsLongNorm,backgroundHMMRate,by = "hmm"
#                                         )[,poissonTestOnArrays(numSeqs,Gene_Count_assembled, backgroundHMMRate)]
  
  
#for (name in names(dataSplitByHMM) )
#  dataSplitByHMM[[name]]$poissonTestScore = poissonTestOnArrays (dataSplitByHMM[[name]]$numSeqs, dataSplitByHMM[[name]]$Gene_Count_assembled, meanHitRates[[name]])

#allHitsLongNorm$log2Ratio = allHitsLongNorm[,log2((numSeqs+1)/(expectedCount+1))]

#for (name in names(dataSplitByHMM))
#  dataSplitByHMM[[name]]$log2Ratio = log2((dataSplitByHMM[[name]]$numSeqs+1)/(dataSplitByHMM[[name]]$expectedCounts+1))



#world = map_data("world")



#genomeNames = read.csv("./data/genomeNames.csv")
# this inserts breaks into long names...useful for tooltips, but not used for now
#wrap40 = function(s){gsub('(.{1,40})(\\s|$)', '\\1<br>', s)}
#test = lapply (genomeNames[,2], wrap40)
#genomeNames[,2] = unlist(test)

#genomeNames$taxon_oid = as.factor(genomeNames$taxon_oid)
#allHitsLongNorm = merge (allHitsLongNorm, genomeNames)

#studyNames = read.csv ("./data/studyNamesByTaxonOID.csv")
#studyNames$taxon_oid = as.factor(studyNames$taxon_oid)
#allHitsLongNorm = merge (allHitsLongNorm, studyNames)

# availableSubgroups = as.character(unique(allHitsLongNorm$hmm))
# 
 subgroupNames = as.data.table( read.csv(dataFile("sfldSubgroupNameFile"), header=TRUE, sep="\t") )
 subgroupNames$hmmAcc = with(subgroupNames,
                             {paste(Superfamily_ID,
                                    ".Subgroup_",
                                    Subgroup_ID,
                                    sep="")}
                             )
 
 subgroupNames$description = with(subgroupNames,
                                  {paste(superfamily,
                                         "--",
                                         subgroup
                                         )
                                  }
                                )

 familyNames = as.data.table( read.csv(dataFile("sfldFamilyNameFile"), header=TRUE, sep="\t") )
 familyNames$hmmAcc = with(familyNames,
                             {paste(Superfamily_ID,
                                    ".Family_",
                                    Family_ID,
                                    sep="")}
 )
 
 familyNames$description = with(familyNames,
                                  {paste(superfamily,
                                         "--",
                                         subgroup,
                                         "--",
                                         family
                                  )
                                  }
 )
 
 

#metaData = fread ("./data/dereplicated_metagenomes_10_01_2017.edits1.tab", integer64 ="character")
metaData =   read.delim(dataFile("metadataFile"),
             header=TRUE,
             sep="\t")
metaData$taxon_oid = as.character(metaData$taxon_oid)
metaData = as.data.table(metaData)

# limit it to just columns I'm interested in:
metaData = metaData[,.(taxon_oid, 
                       Study.Name,
                       Genome.Name...Sample.Name,
                       Ecosystem,
                       Ecosystem.Category,
                       Ecosystem.Subtype,
                       Ecosystem.Type,
                       Geographic.Location,
                       Gene.Count.....assembled,
                       Altitude,
                       Sample.Collection.Date,
                       Isolation,
                       Isolation.Country,
                       Latitude,
                       Longitude,
                       Sample.Body.Site,
                       Sample.Body.Subsite,
                       Specific.Ecosystem,
                       Temperature.Range,
                       Depth,
                       Habitat,
                       Longhurst.Code,
                       Longhurst.Description,
                       pH,
                       Relevance,
                       Salinity.Concentration,
                       Sample.Collection.Temperature,
                       Host.Gender,
                       Host.Name
)]

# Load cluster info and add to metadata
#metagenomeClusterInfo = fread ("./data/metagenomeClusterInfo.txt")
#metaData = merge (metagenomeClusterInfo,metaData, by = "taxon_oid", all.y = TRUE)



colorsGrad = colorRampPalette(brewer.pal(11,config::get("brewerColorPalette")))(22)

subsetData  <- function(allData, metagenomeInfo, sets){
  result = NULL
  for (name in names(sets)){
    dataSet = allData[taxonOID %in% sets[[name]],
                      .(taxon_oid = taxonOID, hmm=pfam, numSeqs=numGenes, sumCoverage,
                        condition = name)
                      ]
    #fill in zeros
    
    if (is.null(result))
      result = dataSet
    else
      result = rbind (result, dataSet)
    
  }
  #fill in zeros
  allPairs = merge(data.frame(hmm=unique(result$hmm)), data.frame(taxon_oid=unique(result$taxon_oid)))
  result = merge(result, allPairs, by = c("hmm", "taxon_oid"), all.y=TRUE)
  result[is.na(numSeqs),numSeqs := 0]
  for (name in names(sets))
    result[taxon_oid %in% sets[[name]], condition:=name]
  merge(result, metagenomeInfo[,.(taxon_oid,Gene_Count_assembled, totalPfamCount, GenomeSample_Name)], by="taxon_oid")
}

fillInZeros <- function(summaryDataLong, sets){
  allPairs = merge(data.frame(hmm=unique(summaryDataLong$hmm)), data.frame(taxon_oid=unique(summaryDataLong$taxon_oid)))
  result = merge(summaryDataLong, allPairs, by = c("hmm", "taxon_oid"), all.y=TRUE)
  result[is.na(numSeqs),numSeqs := 0]
  for (name in names(sets))
    result[taxon_oid %in% sets[[name]], condition:=name]
  result
}

addMetagenomeInfo <-function(summaryDataLong, metagenomeInfo){
  a = merge(summaryDataLong, metagenomeInfo[,.(taxon_oid,Gene_Count_assembled, totalPfamCount, GenomeSample_Name)], by="taxon_oid")
  a[,ratio := (numSeqs+1)/Gene_Count_assembled]
}

# statistics --------------------------------

lm.summary = function(dataset){
  s = summary(lm ((numSeqs+1)/Gene_Count_assembled~condition, data=dataset))
  pValue = s$coefficients[2,4]
  meanSet1 = s$coefficients["(Intercept)","Estimate"]
  meanSet2 = meanSet1 + s$coefficients["conditionset2","Estimate"] 
  ratioLog2  = log2(meanSet1/meanSet2)
  effectError = s$coefficients["conditionset2", "Std. Error"]
  list(pValue = pValue, meanSet1 = meanSet1, meanSet2 = meanSet2, ratioLog2=ratioLog2, effectError = effectError)
}

t.test.summary = function (dataset){
  s = t.test(log((numSeqs+1)/Gene_Count_assembled)~condition, data=dataset)
  pValue = s$p.value
  meanSet1 = mean (dataset[condition=="set1"]$ratio)
  meanSet2 = mean (dataset[condition=="set2"]$ratio)
  ratioLog2 = log2(meanSet1/meanSet2)
  list(pValue = pValue, meanSet1 = meanSet1, meanSet2 = meanSet2, ratioLog2=ratioLog2)
  
}

pPoisson.summary = function(dataset){
  s = summary(glm (numSeqs~offset(log(Gene_Count_assembled))+condition, family = 'poisson', data=dataset))
  pValue = s$coefficients[2,4]
  meanSet1 = mean (dataset[condition=="set1"]$ratio)
  meanSet2 = mean (dataset[condition=="set2"]$ratio)
  ratioLog2 = log2(meanSet1/meanSet2)
  effectError = NA
  list(pValue = pValue, meanSet1 = meanSet1, meanSet2 = meanSet2, ratioLog2=ratioLog2, effectError = effectError)
}

pPoisson = function(dataset){
  summary(glm (numSeqs~offset(log(Gene_Count_assembled))+condition, family = 'poisson', data=dataset))$coefficients[2,4]
}

calculateQValues=function(subsetData){
  subsetData %>% split(subsetData$hmm) %>% lapply (pPoisson) %>% unlist %>% qvalue()
}


# SFLD data --------------------
#  # this gets called in a future, so beware of assigning to  global variables
getSFLDFamilyData <- function (taxonOIDSet){
  sfldFamilyDataLong = NULL
  if (is.null(sfldFamilyDataLong)){
    sfldFamilyDataLong <- fread (dataFile("sfldFamilyDataFile"), integer64 = "character")
    sfldFamilyDataLong[,taxon_oid := as.character(taxon_oid)]
    #sfldFamilyDataLong[,ID := as.integer(sapply ( strsplit(hmm, "_"), "[[", 2)) ]
  }
  sfldFamilyDataLong[taxon_oid %in% taxonOIDSet]
}

#  # this gets called in a future, so beware of using global variables
getSFLDSubgroupData <- function (taxonOIDSet){
  sfldSubgroupDataLong = NULL
  if (is.null(sfldSubgroupDataLong)){
    sfldSubgroupDataLong <- fread (dataFile("sfldSubgroupDataFile"), integer64 = "character")
    sfldSubgroupDataLong[,taxon_oid := as.character(taxon_oid)]
    #sfldSubgroupDataLong[,ID := as.integer(sapply ( strsplit(hmm, "_"), "[[", 2)) ]
    
  }
  sfldSubgroupDataLong[taxon_oid %in% taxonOIDSet]
}

# others --------------------

# next three functions attempt to get pretty upper and lower limits
# in log10 space to help the extreme points have bounding tick marks 
stepAboveInLog10Space = function(x, steps=c(1,2,5,10)){
  transX = log10(x) # work in log space
  transSteps = log10(steps)
  dec = transX - floor(transX) # get the fractional part of the log
  transStep = transSteps[which(transSteps > dec)][1]
  if (is.na(transStep)) transStep = 0  #
  10^(floor(transX) + transStep)
}


stepBelowInLog10Space = function(x, steps=c(1,2,5,10)){
  steps = rev(steps)
  transX = log10(x) # work in log space
  transSteps = log10(steps)
  dec = transX - floor(transX) # get the fractional part of the log
  transStep = transSteps[which(transSteps < dec)][1]
  if (is.na(transStep)) transStep = 0  #
  10^(floor(transX) + transStep)
}


niceLimitsInLog10Space <- function(range, steps = c(1,2,3,5,7,10)){
  c(stepBelowInLog10Space (range[1]), stepAboveInLog10Space(range[2]))
}



