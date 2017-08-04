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
renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
renderEventName <- function() { 
  
  ( htmlOutput('eventname') )
  
}  
rendermaxcp <- function() { 
  
  ( htmlOutput('maxcp') )
  
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
                           titlePanel("Change Point Analysis" ) )
                  ),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Select Inputs',
                 selectInput_p("v1", 'Drug Variable' ,c('Active_Ingredient','DRUGNAME'), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
                 conditionalPanel(
                   condition = "1 == 2",
                   conditionalPanel(
                     condition="input.v1=='Active_Ingredient'",
                     selectizeInput_p("t1", "Active Ingredient",
                                      choices= c("Start typing to search..."="",ing_choices),
                                      HTML( tt('ing1') ), tt('ing2'),
                                      placement='left')
                   ),
                   
                   conditionalPanel(
                     condition="input.v1=='DRUGNAME'",
                     selectizeInput_p("t1_1", "Name of Drug",
                                      choices= c("Start typing to search..."="",drug_choices),
                                      HTML( tt('drugname1') ), tt('drugname2'),
                                      placement='left')),
                   
                   selectizeInput_p("t2", "Adverse Events",choices= c("Start typing to search"=""), 
                                    HTML( tt('eventname1') ), tt('eventname2'),
                                    placement='left'),               
                   numericInput('maxcp', "Maximum Number of Change Points", 3, 1, step=1)
                                  
                 ),
                 wellPanel(
                 bsButton("tabBut", "Select Drug and Event...", style='primary'),
                 br(),
                 renderDrugName(),
                 renderEventName(),
                 rendermaxcp()
                 ),
                 dateRangeInput('daterange', 'Date Report Was First Received by Health Canada.',
                                format="yyyy-mm-dd",start ="1965-01-30", end = Sys.Date() ),
                 
                 bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                            htmlOutput('mymodal'),
                          conditionalPanel(
                            condition="input.v1=='Active_Ingredient'",
                            selectizeInput_p("ingname", "Active Ingredient",
                                             choices= c("Start typing to search..."="",ing_choices),
                                             HTML( tt('drugname1') ), tt('drugname2'),
                                             placement='left')
                          ),
                          
                          conditionalPanel(
                            condition="input.v1=='DRUGNAME'",
                            selectizeInput_p("drugname", "Name of Drug",
                                             choices= c("Start typing to search..."="",drug_choices),
                                             HTML( tt('drugname1') ), tt('drugname2'),
                                             placement='left')),
                            
                            selectizeInput_p("eventname", "Adverse Events",choices= c("Start typing to search"=""), 
                                             HTML( tt('eventname1') ), tt('eventname2'),
                                             placement='left'),               
                            numericInput('maxcp2', "Maximum Number of Change Points", 3, 1, step=1
                                            ),
                            bsButton("update", "Update Variables", style='primary')),
                 
                 
                
                bsAlert("alert")
                  )
        ,
    id='sidetabs', selected='Select Inputs')
    ),
    mainPanel(
      
      tabsetPanel(
                 tabPanel("Change in Mean Analysis",  
                          wellPanel( 
                            plotOutput( 'cpmeanplot'), 
                            htmlOutput( 'cpmeantext'),
                            bsPopover('cpmeanplot','Change in Mean Analysis',tt('cpa'),'left')
                            )
                          ),
                tabPanel("Change in Variance Analysis",  
                         wellPanel( 
                           plotOutput( 'cpvarplot'),
                           htmlOutput( 'cpvartext',pophead=NULL ),
                           bsPopover('cpvarplot','Change in Variance Analysis',tt('cpa'),'left')
                         )),
                 tabPanel("Bayesian Changepoint Analysis",  
                          wellPanel( 
                            plotOutput_p( 'cpbayesplot',pophead=tt('bcp'),poptext=tt('bcp3'),
                                          placement='left'), 
                            verbatimTextOutput( 'cpbayestext' )
                            )
                          ),
                tabPanel("Report Counts by Date",  
                         wellPanel( 
                           htmlOutput( 'allquerytext')),
                         wellPanel( 
                           htmlOutput( 'querytitle' ),
                           DT::dataTableOutput("query"),
                        bsPopover('query','Time-Series','Monthly counts for drug-event combination.','top')
                           )
                ),
                tabPanel("Counts For Drugs In Selected Reports",
                           wellPanel(
                           htmlOutput( 'cotitle' )
                         ),
                         wordcloudtabset('cloudcoquery', 'coquery',
                                         popheads=c( tt('codrug1'), tt('word1') ), 
                                         poptext=c( tt('codrug3'), tt('word2') ))
                ),
                tabPanel("Counts For Events In Selected Reports",
                         wellPanel(
                           htmlOutput( 'cotitleE' )
                         ),
                         wordcloudtabset('cloudcoqueryE', 'coqueryE',
                                         popheads=c( tt('codrug1'), tt('word1') ), 
                                         poptext=c( tt('codrug3'), tt('word2') ))
                ),
            
                #tabPanel('Data Reference',
                        # HTML( renderiframe("https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html"))),
                tabPanel("Other Apps",  
                         HTML( makeapplinks())
                ),
                
                 tabPanel('About', 
                         
                         HTML( (loadhelp('about') ) )  ),

              id='maintabs', selected = 'Change in Mean Analysis'
            )
          )
        )
      )
    )
  )
