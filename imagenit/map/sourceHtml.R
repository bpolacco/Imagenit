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

<input type="submit" value = "Access Sequences for %s for %d selected Metagenomes at JGI">


</form>

'


taxonOIDTemplate = '
<input type="hidden" name="genomeFilterSelections" value="%s"/>
<input type="hidden" name="selectedGenome1" value="%s"/>
'

CreateJGIFormManyPfamManyMetagenome=function(hmm, taxonOIDs){
  pfamListString = paste (hmm, collapse=",")
  taxonOIDInputs = c()
  for (taxonOID in taxonOIDs){
    taxonOIDInputs = c(taxonOIDInputs, sprintf(taxonOIDTemplate,taxonOID, taxonOID))
  }
  taxonOIDInputs = paste (taxonOIDInputs, collapse="")
  
  sprintf(jgiFormManyPfamManyMetagenomeTemplate, pfamListString,  taxonOIDInputs, pfamListString,length(taxonOIDs))
}


antarcticaMessageHTML = "<div><small><p>Color indicates relative abundance (orange = high, purple = low) compared to global rates for chosen HMM</p><p>Metagenomes with no available coordinate data are spread at latitude -90&#176;, at bottom of map</small></div>"
