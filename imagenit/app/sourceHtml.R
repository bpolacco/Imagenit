instructionHTML = "
<h1> Welcome to Imagenit </h1>

<p><strong> Under construction </strong></p>

<p> This web tool is arranged as a series of tabs that are designed to be visited in order. 
You are currently on the instructions tab.  Read below for background and tips on using this tool. </p>
<ol>
<li> Instructions (you are here!)</li>
<li> Select Metagegenome Surveys </li>
<li> Computations </li>
<li> Download data and results </li>
</ol>

<h2> 1. Instructions </h2>
<p> You are here!  </p>
<p> The aim of this tool is to facilitate the identification of genes and protein functions that are 
causally associated with environments. Briefly, you choose a contrasting set of metagenome samples, for
example freshwater vs saline wetland samples, and then the tool shows which types of genes are found more
often in one than the other.</p>

<h2> 2. Select Metagenome Surveys </h2>
<p> This tab allows you to define two sets of metagenomes that you would like to compare. </p>

<h2> 3. Computations </h2>
<p> Run the computations here and view your results.  The main panels here show </p>
<ul>
<li> Scatterplot of metagenome sizes </li>
<li> Scatterplot of HMM abundances in sets </li>
<li> Detailed view of HMM hits per metagenome</li>
<li> Something else</li>
</ul>

<h2> 4. Download data and results </h2>
<p> Data and results in table form.  Please cite us </p>

"


setSelectionInstructionsHTML = "
<p> Use this tab to define the environments to compare. For demonstration purposes, the sets are pre-filled 
with sets for  a high salt 
and a low salt set. To choose your own, first use the Clear Set buttons, then use the table below
to select and add rows to each set. The Search field is useful for limiting the metagenomes in the table.
</p>"

instructionsComputationsHTML = "
<p> Use this tab to compute the comparison and explore the results. First make sure the type of HMMs
you are interested in (SFLD or PFAM) are selected, then click the <em>Do Comparison</em> button to 
start the computations. Comparisons at all PFAM HMMs could take several minutes.
"

#  The next html blocks and function assemble the JGI form to 
#  access the list of genes matching one or more pfam and multiple taxonOID

jgiFormManyPfamManyMetagenomeTemplate = '

<form action="https://img.jgi.doe.gov/cgi-bin/m/main.cgi" method="post" target="_blank">
<input type="hidden" name="searchFilter" value="domain_list"/>
<input type="hidden" name="section" value="FindGenes"/>
<input type="hidden" name="page" value="geneSearchForm"/>
<input type="hidden" name="seqstatus1" value="Finished"/>
<input type="hidden" name="seqstatus0" value="Finished"/>
<input type="hidden" name="domainfilter0" value="Archaea"/>
<input type="hidden" name="seqstatus" value="both"/>
<input type="hidden" name="domainfilter" value="*Microbiome"/>
<input type="hidden" name="displayType" value="list"/>
<input type="hidden" name="q_data_type" value="assembled"/>
<input type="hidden" name="page" value="fgFindGenes"/>
<input type="hidden" name="fgFindGenes" value="fgFindGenes"/>

<input type="hidden" name="searchTerm" value="%s"/>

%s

<input type="submit" value = "Get Sequences for %d selected PFAM model(s) for %s from JGI">


</form>

'
taxonOIDTemplate = '
<input type="hidden" name="genomeFilterSelections" value="%s"/>
<input type="hidden" name="selectedGenome1" value="%s"/>
'

CreateJGIFormManyPfamManyMetagenome=function(hmm, taxonOIDs, setName){
  pfamListString = paste (hmm, collapse=",")
  taxonOIDInputs = c()
  for (taxonOID in taxonOIDs){
    taxonOIDInputs = c(taxonOIDInputs, sprintf(taxonOIDTemplate,taxonOID, taxonOID))
  }
  taxonOIDInputs = paste (taxonOIDInputs, collapse="")
  
  sprintf(jgiFormManyPfamManyMetagenomeTemplate, pfamListString, taxonOIDInputs, length(hmm), setName)
}

ImagenitMapFormTemplate = '
<form action="%s" method="get" target="_blank">
<input type="hidden" name="hmm" value ="%s" />
<input type="hidden" name="hmmType" value = "%s" />

<input type="submit" value="View selected hmm %s on world map" />
</form>
'

CreateImagenitMapForm = function(hmm){
  if (length(grep("pfam", hmm))>0) hmmType="pfam"
  else if (length(grep("Subgroup", hmm))>0) hmmType = "sfldSubgroup"
  else if (length(grep("Family", hmm))>0) hmmType = "sfldFamily"
  else return(NULL)
  #for now just get the first hmmm
  sprintf(ImagenitMapFormTemplate, config::get("mapURL"), hmm[1], hmmType, hmm[1])
}
