
appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.95;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #808080;
}
"
renderEventName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 

renderLimit <- function() { 
  
  ( htmlOutput('limit') )
  
}  

renderStart <- function() { 
  
  
  ( htmlOutput('start') )
  
}  

renderNumsims <- function() { 
  
  ( htmlOutput('numsims') )
  
} 

shinyUI(fluidPage(
  
    useShinyjs(),
    inlineCSS(appCSS),
    div(
    id = "loading-content",
    h1("Please Wait")%>%withSpinner(proxy.height='300px')
  ),
    
    div(
      id = "main_content",
  
  
                  fluidRow(
                    column(width=4,
                           a(href='https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/canada-vigilance-program.html', 
                             img(src='healthcanada.png',width="150",height="150",align='bottom')),
                           renderDates()
                     ),
                    column(width=8,
                           titlePanel("LRT Signal Analysis for an Event" ) )
                  ),

  sidebarLayout(
    sidebarPanel(
#     tabsetPanel(
#        tabPanel('Select Inputs',
                 selectInput_p("v1", 'Drug Variable' ,c('Active_Ingredient','DRUGNAME'), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
                   conditionalPanel(
                     condition = "1 == 2",
                   
                   selectizeInput("t2", "Adverse Event",
                                      choices=NULL
                                   ),
                   
                   numericInput('limit', 'Maximum number of event terms', 50,
                                  1, 100, step=1),
                                 
                   
                   numericInput('start', 'Rank of first event', 1,
                                  1, 999, step=1 
                                 ),
                  
                    numericInput('numsims', 'Number of Simulations', 1000,
                                  1000, 50000, step=1 
                                )
                   
                 ),
                 wellPanel(
                 bsButton("tabBut", "Select Event and # of Drugs...", style='primary'),
                 br(),
                 renderEventName(),
                 renderLimit(),
                 renderStart()
                 ),
                 dateRangeInput('daterange', 'Use Reports between',
                                format="yyyy-mm-dd",start ="1965-01-30", end = Sys.Date() ),
                 
                 bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                            htmlOutput('mymodal'),
                          
                          selectizeInput_p("eventname", "Adverse Event",
                                           choices= c("Start typing to search..."="",pt_choices),
                                           HTML( tt('eventname1') ), tt('eventname2'),
                                           placement='left'),
                            
                          numericInput_p('limit2', 'Number of most frequent drugs to analyze:', 50,
                                         1, 100, step=1, 
                                         HTML( tt('limit1') ), tt('limit2'),
                                         placement='left'), 
                          
                          numericInput_p('start2', 'Start with ranked frquency #', 1,
                                         1, 999, step=1, 
                                         HTML( tt('limit1') ), tt('limit2'),
                                         placement='left'),
                          numericInput_p('numsims2', 'Number of Simulations', 1000,
                                         1000, 50000, step=1, 
                                         HTML( tt('numsims1') ), tt('numsims2'),
                                         placement='left'),
                          bsButton("update", "Update Variables", style='primary')),
                 
                 wellPanel(  
                 helpText( HTML('<b>Download Report</b>') ),
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton('downloadReport', 'Download LRT Report')
                 ),

                 HTML( (loadhelp('LRT') ) )    ),


# id='sidetabs', selected='Select Inputs')
#    ),
mainPanel(
  tabsetPanel(
    tabPanel("LRT Results based on Total Drugs",
             wellPanel(
               htmlOutput( 'prrtitle' ), 
               helpText('Results sorted by LRR')
             ),
             
             maketabset( c('prr', 'cloudprr', 'textplot'), 
                         types=c('datatable', "plot", 'plot'),
                         names=c("Table","Word Cloud", "Text Plot"),
                         popheads = c(tt('LRT1'), tt('word1'), tt('textplot1')) , 
                         poptext = c(tt('LRTtext1'),tt('wordLRT'),tt('textplot3')) )
    ),
    
    tabPanel("Simulation Results for Drug Based LRT",
             wellPanel( 
               plotOutput( 'simplot')
             )
    ),
    
    tabPanel("Analyzed Event Counts for Specified Drug"   ,
             wellPanel( 
               htmlOutput( 'alldrugtext' )
             ), 
             wordcloudtabset('cloudquery', 'specifieddrug', 
                             popheads=c( tt('event1'), tt('word1') ), 
                             poptext=c( tt('event2'), tt('word2') )
             )
    ),
    
    tabPanel("Analyzed Drug Counts for All Events",
             wellPanel( 
               htmlOutput( 'alltext' )
             ),
             wordcloudtabset('cloudall', 'all2',  
                             popheads=c( tt('event1'), tt('word1') ), 
                             poptext=c( tt('event2'), tt('word2') ))
    ),
    
    tabPanel("Counts For Events In Selected Reports",
             wellPanel(
               htmlOutput( 'cotitleE' )
             ),
             wordcloudtabset('cloudcoqueryE', 'coqueryE',
                             popheads=c( tt('codrug1'), tt('word1') ), 
                             poptext=c( tt('codrug3'), tt('word2') ))
    ),
    
    tabPanel("Counts For Indications In Selected Reports",
             wellPanel(
               htmlOutput( 'indtitle' )
             ),
             wordcloudtabset('cloudindquery', 'indquery2',
                             popheads=c( tt('indication1'), tt('word1') ),
                             poptext=c( tt('indication2'), tt('word2') ) )
    ),
    
    #tabPanel('Data Reference',
    # HTML( renderiframe("https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html"))),
    tabPanel("Other Apps",  
             HTML( makeapplinks())
    ),
    
    tabPanel('About', 
             
             HTML( (loadhelp('about')))),
    
    id='maintabs', selected = 'LRT Results based on Total Drugs'
  )
)
    )
)
)
)
