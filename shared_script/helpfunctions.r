

loadhelp <- function(tabname){
  
  l <- vector('character',5)
  names(l) <- c('loaddata', 'overview', 'overviewside', 'LRT','about')
#*1**********************************
  l['loaddata'] <- "<h3>Loading Data</h3>This program can read data in text, .xpt, 
  and saved R dataframe formats. <br>
  Select the file type, and the click on the 'Browse'
  button the file you want to upload<br>
  Please be aware that if you are running this sofware
  on a remote server the data may be available to unauthorized individuals."
#2*********************************** 
  l['overviewside'] <- "<h4>Reporting Ratios (PRR and ROR)</h4>
The proportional reporting ratio (PRR) is a simple way to get a measure of how common an adverse
event for a particular drug is compared to how common the event is in the overall database.  <br>
A PRR > 1 for a drug-event combination indicates that a greater proportion of the reports for
the drug are for the event than the proportion of events in the rest of the database.  
For example, a PRR of 2 for a drug event combination indicates that the proportion of reports for
the drug-event combination is twice the proportion of the event in the overall database<br>
<br>
PRR = (m/n)/( (M-m)/(N-n) )<br>
 Where <br>
      m = #reports with drug and event<br>
      n = #reports with drug<br>
      M = #reports with event in database<br>
      N = #reports in database<br>
<br><br>
A similar measure is the reporting odds ratio (ROR).<br>
<br><br>
ROR = (m/d)/(M/D)
<br><br>
 Where <br>
      m = #reports with drug and event<br>
      d = n-m<br>
      M = #reports with event in database<br>
      D = N-M<br>
<br><br>

 Often PRR analyses are stratified by various attributes of
 the report such as patient age, gender, report date in an effort to improve precision.
Other approaches, such as Bayesian shrinkage estimates of the PRR (e.g. MGPS) are also used.
<br><br>

<h4>References</h4>
<br>
Guidance for Industry. Good Pharmacovigilance Practices and Pharmacoepidemiologic Assessment. 
Food and Drug Administration, US Department of Health and Human Services. March 2005. 
<a href='Good Pharmacovigilance Practices ucm126834.pdf' target='_blank'>
See Document here</a> .  Accessed Dec 2014.
<br><br>
Bate A, Evans, S. Quantitative signal detection using spontaneous ADR reporting.  
<i>Pharmacoepidemiol and Drug Saf</i>  2009 Jun;18(6):427-36. doi: 10.1002/pds.1742.
<br><br>
Szarfman A, Tonning JM, Doraiswamy PM.
 Pharmacovigilance in the 21st century: new systematic tools for an old problem.  
<i>Pharmacotherapy</i> 2004 Sep;24(9):1099-104.
<br><br>
Dumouchel W. Bayesian data mining in large frequency tables,
with an application to the FDA Spontaneous Reporting System,
<i>American Statistician</i> 1999; 53(3):177-190.

"

#2a*********************************** 
l['overview'] <- "<h3>Data Reference</h3> <br>
<h4>Background</h4>
From the <a href='https://open.fda.gov/drug/event/reference'>OpenFDA Website</a><br>
<br>
<p>The openFDA drug adverse event API returns data from the <a href='https://open.fda.gov/data/faers/'>FDA Adverse Event Reporting System (FAERS)</a>, a database that contains information on adverse event and medication error reports submitted to FDA. Currently, this data covers publically releasable records submitted to the FDA from 2004-2013. The data is updated quarterly.</p>

  <p>An adverse event is submitted to the FDA to report any undesirable experience associated with the use of a medical product in a patient. For drugs, this includes serious drug side effects, product use errors, product quality problems, and therapeutic failures for prescription or over-the-counter medicines and medicines administered to hospital patients or at outpatient infusion centers.</p>

<p>Reporting of adverse events by healthcare professionals and consumers is voluntary in the United States. FDA receives some adverse event reports directly from healthcare professionals (such as physicians, pharmacists, nurses and others) and consumers (such as patients, family members, lawyers and others). Healthcare professionals and consumers may also report adverse events to the products’ manufacturers. If a manufacturer receives an adverse event report, it is normally required to send the report to FDA.</p>

FAERS data does have limitations. There is no certainty that the reported event (adverse event or medication error) was actually due to the product. FDA does not require that a causal relationship between a product and event be proven, and reports do not always contain enough detail to properly evaluate an event.<br /><br />Further, FDA does not receive reports for every adverse event or medication error that occurs with a product. Many factors can influence whether or not an event will be reported, such as the time a product has been marketed and publicity about an event.<br /><br />Submission of a safety report does not constitute an admission that medical personnel, user facility, importer, distributor, manufacturer or product caused or contributed to the event. The information in these reports has not been scientifically or otherwise verified as to a cause and effect relationship and cannot be used to estimate the incidence of these events.


<p>In 2012, FDA changed from the Adverse Event Reporting System (AERS) to the FDA Adverse Event Reporting System (FAERS). There was a minor shift in terms as part of this transition. If you are using data from before December 2012, you should be aware of this shift.</p>

<h3 id='responsible-use-of-the-data'>Responsible use of the data</h3>

<p>Adverse event reports submitted to FDA do not undergo extensive validation or verification. Therefore, <strong>a causal relationship cannot be established between product and reactions listed in a report.</strong> While a suspected relationship <em>may</em> exist, it is not medically validated and should not be the sole source of information for clinical decision making or other assumptions about the safety or efficacy of a product.</p>

<p>Additionally, it is important to remember that adverse event reports represent a small percentage of total usage numbers of a product. Common products may have a higher number of adverse events due to the higher total number of people using the product. In recent years the FDA has undertaken efforts to increase collection of adverse events. Increases in the total number of adverse events is likely caused by improved reporting.</p>

<h3 id='how-adverse-events-are-organized'>How adverse events are organized</h3>

<p>Adverse events are collected through a series of <em>safety reports.</em> Each is identified by a 8-digit string (for instance, <code>6176304-1</code>). The first 7 digits (before the hyphen) identify the individual report, and the last digit (after the hyphen) is a checksum. Rather than updating individual records in FAERS, subsequent updates are submitted in seperate reports.</p>


"

#5***********************************  
l['about'] <- "<h3>About</h3>
This is a beta product,developed based on OpenFDA ShinyApps.<br>
DO NOT use as sole evidence to support regulatory decisions or to make decisions regarding <br>
medical care. Always speak to your health care provider about the risks and benefits of Health Canada regulated Products.<br>
This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative.<br>
This is a prototype experiment that utilizes publically available data (Canada Vigilance Adverse Reaction Online Database)<br>
Data provided by the Canada Vigilance Adverse Reaction Online Database.<br>
<a href ='http://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html' target='_blank'>Website</a><br>

<h4>Development Team</h4>
<b>Daniel Buijs</b><br>
Manager,Data Science,Health Products and Food Branch<br>
Health Canada/Government of Canada<br>
<a href='mailto:daniel.buijs@hc-sc.gc.ca'>daniel.buijs@hc-sc.gc.ca</a><br><br>

<b>Nanqing Zhu</b><br>
Jr.Data Scientist, Intern<br>
Health Canada/Government of Canada<br>
<a href='mailto:nanqing.zhu@mail.mcgill.ca'>nanqing.zhu@mail.mcgill.ca</a><br><br>

<b>Source code:</b><br>
<a href='http://github.com/hres/cvopenfda' target='_blank'>Github</a><br><br>

<h4>Reference</h4>
OpenFDA ShinyApps<br>
For demonstration, <a href='https://openfda.shinyapps.io/ChangePoint'target='_blank'>Click here</a>
"


#6***********************************  
l['LRT'] <- '<h3>Likelihood Ratio Test (LRT) Methodology</h3>

The RR is defined as the ratio of reporting rate for a particular AE for a specified drug/drug class relative to the reporting rate for all other AEs for the fixed drug/drug group.

RR >1 implies that the observed reporting rate for the particular AE is higher than the reporting rate for other AEs for the (fixed) drug/drug group. 

An AE with RR>1 can be a potential signal for the drug/drug group of interest.

RR = (a/(a+b))/(c/(c+d))
( See Table 2 in <a href="lrtmethod.pdf"  target="_blank"> Likelihood Ratio Test (LRT) Methodology document </a> for letter definitions. )
LogLR (LLR) represents the logarithm of likelihood ratio test statistic by AE expressed in terms of SOC, PT, etc. 

The larger the logLR value is, the stronger is the association between the particular AE and (fixed) drug.

logLR = a x &#91;log(a) – log(a +b)&#93; +c x &#91;log(c)-log (c + d)&#93; - (a + c) x &#91;log(a + c)-log(a + b +c + d)&#93;
Is calculated using LogLR. AE represents the significance of the observed association between the AE and a fixed drug/drug group.  P-values less than 0.05 are indicative of those AEs being signals for the (fixed) drug. Users can use different threshold for the p-values for signal detection (such as 0.025, 0.01, etc). 
'
return(l[tabname])
}

#Help strings
gettt <- function(){
  l <- vector('character' )
  #*1**********************************
  l <- append( l, c('cocloud' =  "Word cloud for concomitant medications") )
  
  #2*********************************** 
  l <- append( l, c('eventtable' =  "Table of counts for selected drug") )
  #3*********************************** 
  l <- append( l, c('drugvar1' =  "<b>Select Drug Variable</b>" ) )
  l <- append( l, c('drugvar2' =  "Select the Canada Vigilance drug variable to search") )
  l <- append( l, c('v1head' =  "<b>Select Drug Variable</b>" ) )
  l <- append( l, c('v1text' =  "Select the Canada Vigilance drug variable to search") )
  l <-append(l,c('ing1'='<b>Select Active Ingredient</b>'))
  l <-append(l,c('ing2'='Enter the name of an active ingredient'))
  l <- append( l, c('drugname1' =  '<b>Select Drug Name</b>') )
  l <- append( l, c('drugname2' =  'Enter the name of a drug to analyze' ) )
  l <- append( l, c('eventname1' =  '<b>Select Event Name</b>' ) )
  l <- append( l, c('eventname2' =  'Enter the name of an event to analyze' ) )
  l <- append( l, c('gcount1' =  '<b>Record Count</b>' ) )
  l <- append( l, c('gcount2' =  'Number of records that match search criteria in openFDA' ) )
  l <- append( l, c('gquery1' =  '<b>openFDA Query</b>' ) )
  l <- append( l, c('gquery2' =  'Click the query to see the results of the query in JSON format' ) )
  l <- append( l, c('freqtab1' =  'Frequency Table' ) )
  l <- append( l, c('freqtab2' =  'Counts' ) )
  l <- append( l, c('word1' =   '<b>Word Cloud</b>'  ) )
  l <- append( l, c('word2' =  'Size of words are proportional to the frequency of the word.' ) )
  l <- append( l, c('cloudprrhead' =   'a<b>Word Cloud</b>'  ) )
  l <- append( l, c('cloudprrtext' =  'Size of words are proportional to the PRR of the word.' ) )
  l <- append( l, c('wordPRR' =  'Size of words are proportional to the PRR of the word.' ) )
  l <- append( l, c('wordLRT' =  'Size of words are proportional to the LLR of the word.' ) )
  l <- append( l, c('textplot1' =   '<b>Text Plot</b>'  ) )
  l <- append( l, c('textplot2' =  'Plot of number of events and PRRs for terms.  Selecting a region of terms displays a table of the selected terms' ) )
  l <- append( l, c('textplot3' =  'Plot of number of events and LLRs for terms.  Selecting a region of terms displays a table of the selected terms' ) )
  l <- append( l, c('limit1' =  'Maximum Number of Terms' ) )
  l <- append( l, c('limit2' =  'Maximum number of terms to evaluate.  Most frequent terms are returned first.' ) )
  l <- append( l, c('cplimit1' =  'Maximum Number of Change Points' ) )
  l <- append( l, c('cplimit2' =  'Maximum number of change points to calculate' ) )
  
  l <- append( l, c('drugprr' =  'Drug name is linked to PRR results for drug-event combinations.' ) )
  l <- append( l, c('eventprr' =  'Drug name is linked to PRR results for drug-event combinations.' ) )
  
  l <- append( l, c('count' =  'Frequency is linked to report that meet the search criteria.' ) )
  
  l <- append( l, c('codrug1' =   'Concomitant Medications'   )  )
  l <- append( l, c('drug1' =   'Drug Name' ) )
  l <- append( l, c('codrug1a' =  paste('Frequency table for drugs found in selected reports.', l['drugprr'] ) ) )
   l <- append( l, c('codrug2' =  paste(l['codrug1a']) ) )
   l <- append( l, c('codrug3' =  paste( l['codrug2'] ) ) )
   
   l <- append( l, c('event1' =   'Reported Events'  ) )
   l <- append( l, c('event2' =  'Frequency table of events found in selected reports.  Event term is linked to PRR results for the event. "M" is linked to medline dictionary definition for event term' ) )
   
   l <- append( l, c('indication1' =   'Reported Indication for Drug'  ) )
   l <- append( l, c('indication2' =  'Frequency table of reported indication for which the drug was administered.  Indication is linked to medline dictionary definition for event term' ) )
   
   l <- append( l, c('prr1' =  "Proportional Reporting Ratio"   ) )
   l <- append( l, c('prr2' =  "The proportional reporting ratio (PRR) is a simple way to get a measure of how common an adverse event for a particular drug is compared to how common the event is in the overall database.  <br><br>" ) )
   l <- append( l, c('prr3' =  "A PRR > 1 for a drug-event combination indicates that a greater proportion of the reports for the drug are for the event than the proportion of events in the rest of the database.<br><br>" ) )
   l <- append( l, c('prr4' =  "For example, a PRR of 2 for a drug event combination indicates that the proportion of reports for the drug-event combination is twice the proportion of the event in the overall database." ) )
   l <- append( l, c('prr5' =  paste( l['prr2'], l['prr3'],l['prr4'] ) ) )
   
   l <- append( l, c('numsims1' = 'Number of Simulations'  ) )
   l <- append( l, c('numsims2' = 'The number of simulations to perform.  Must be between 1,000 and 100,000' ) )
   
   l <- append( l, c('cpamean' ='Change in Mean Analysis'  ) )
   l <- append( l, c('cpamean1' = 'Change-point analysis (CPA) is a statistical method for determining whether a change in either the slope or variability has taken place in a time series or sequence in very large databases. <br><br>'))
   l<-  append(l,c('cpamean2'='Binary Segmentation algorithm is used here to detect multiple change points <br><br>'  ) )
   l<-  append(l,c('cpamean3'='<b>It is advised to increase the number of maximum changepoints to make sure changepoints have not been missed</b>'  ) )
   l<-  append(l,c('cpa'=paste(l['cpamean1'],l['cpamean2'],l['cpamean3'])))
   
   l <- append( l, c('cpavar' =   'Change in Variance Analysis'  ) )
   l <- append( l, c('bcp' =   'Bayesian Change Point Analysis'  ) )
   l <- append( l, c('bcp1' =   'The Bayesian change point analysis method implemented is given in Wang and Emerson (2015)'  ) )
   l <- append( l, c('bcp2' =   '<em>Bayesian Change Point Analysis of Linear Models on Graphs</em>'  ) )
   l<- append(l,c('bcp3'=paste(l['bcp1'],l['bcp2'])))
#   browser()
                             return( l)
}

ttstrings <- gettt()
tt <- function( tabname, l=ttstrings ){
  out <- l[tabname]
  return( as.character(out) )
}