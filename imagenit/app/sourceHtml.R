instructionHTML = "
<h1> Welcome to Imagenit </h1>

<p><strong> The most up-to-date info, including tutorial, source code and citation information is at <a href='https://github.com/bpolacco/Imagenit'> github</a> </strong></p>

<p> <strong> Looking for the <a href='/map' target='_blank'>map? Click here.</strong>(opens in new window)</a> </p>

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
example freshwater vs saline wetland samples, and then the tool computes and shows which sets of homologous genes are found more
often in one than the other.</p>

<h2> 2. Select Metagenome Surveys </h2>
<p> This tab allows you to define two sets of metagenomes that you would like to compare. It is initially populated with
a test set of saline and freshwater wetland samples as used in the tutorial. For your own analyses, first click Clear Set 1 and Clear Set 2, then proceed to
define your sets.  Set names can be defined which will be used in later tabs in images and tables.  The table at the bottom of the page
allows for sorting and searching based on any of the meta data columns. Partial matches are allowed, so '13272' is enough to find taxon_OID 3300013272 
(which is a unique identifier to specific metagenomic sample),
and 'termes' enough for the termites <i>Amitermes</i> and <i>Nasutitermes</i>, but be careful because 'oral' will find many c<i>oral</i> 
metagenomes.</p> To add metagenomes, select rows in the table by clicking (they'll turn blue), then click 'Add selected rows to Set 1(2)'.
</p><p> Once your contrasting sets of metagenomes are defined, proceed to the next tab.  Hint:  To save your sets before proceeding, you
can jump to Tab 4 'Download data...' and choose to Download the metagenome table.
</p>
<h2> 3. Computations </h2>
<p> Run the computations here and view your results.  The main panels here show </p>
<ul>
<li> Scatterplot of metagenome sizes </li>
<li> Compute button and status message </li>
<li> Scatterplot of HMM abundances in sets </li>
<li> Detailed view of HMM hits per metagenome</li>
</ul>

<h4> Metagenome size scatterplot </h4>
<p> This view allows you to compare the sets of metagenomes you have picked, in terms of total gene count and total PFAM matches.
Notice that the scales are in log space, so metagenomes could differ in size by orders of magnitude. If the rate of PFAM matches is
consistent, the metagenomes should fall on a straight line on this plot. If they do not, as in the tutorial fresh-/salt- water case,
results should be interpeted cautiously. For the tutorial example, it appears that the saltwater metagenomes have rates of PFAM matches
20-25% higher than the freshwater cases. This could lead to some proteins showing higher abundance in the saltwater case when it could
be just an artifact of metagenome data processing.</p>

<h4> Compute button and status message </h4>
<p>Select the type of HMMs you are interested in then click submit. You should see a real-time progress message.  If load on the server is light, results will be ready in a couple minutes. Please be patient.</p>

<h4> HMM abundance comparison scatter plot </h4>
<p> This and the next <strong> Detailed HMM Hits</strong> view will not show up until after computations are finished. Points are HMMs that pass the
significance threshold chosen. Select points to view in more detail by clicking or dragging a selection box.</p>

<h4> Detailed HMM Hits </h4>
<p> This view shows the details for selected HMMs in the HMM comparison scatter plot above. Each point is the number or rate
(set by radio buttons at bottom of plot) of HMM
matches in a single metagenome, and the point is colored by which set the metagenome belongs to. </p>

<h2> 4. Download data and results </h2>
<p> Data and results in table form</p>

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

hmmTableMessageHTML = " <p>  Recent testing has shown this table to be slow to respond on JGI servers. Please be patient with it.
We are investigating the cause and hope to have a solution soon.
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
