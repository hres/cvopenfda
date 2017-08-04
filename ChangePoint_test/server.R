
#**************************************************
#CPA
#**************************************************
shinyServer(function(input, output, session) {
  Sys.sleep(2)
  hide(id="loading-content",anim=TRUE,animType="fade")
  
timerange<-reactive({

start<-input$daterange[1]
end<-input$daterange[2]
timerange<-c(start,end)
return(timerange)
})


drugvar<-reactiveValues()

observe({
  if(input$v1=="Active_Ingredient"){
    name<-input$t1
  }else if(input$v1=="DRUGNAME"){
    name<-input$t1_1
  }
  
  drugvar$name<-name
})
  
observe({  
    pt_choices <- drug_PT_HLT
    if (input$v1=="Active_Ingredient") {
      pt_choices %<>% filter(ing==input$ingname)
    }else if(input$v1=="DRUGNAME"){
      pt_choices %<>% filter(DRUGNAME==input$drugname)
    }
    
    pt_choices<-pt_choices %>% distinct(PT_NAME_ENG)%>% `$`("PT_NAME_ENG") %>% sort()
    
    updateSelectizeInput(session, "eventname",
                         choices = c("Start typing to search..." = "", pt_choices))
})

#reactive value upon updating:

updatevars <- reactive({
  
  input$update
  isolate( {
    
    updateSelectizeInput(session, "t1", choices=(input$ingname ) )
    updateSelectizeInput(session, "t1_1", choices=( input$drugname ) )
    updateSelectizeInput(session, "t2", choices= ( input$eventname ) )
    updateNumericInput(session, "maxcp", value=input$maxcp2)
  })
})


#build the modal: 


output$mymodal <- renderText({
  if (input$update > 0)
  {
    updatevars()
    toggleModal(session, 'modalExample', 'close')
  }
  return('')
})



table<-reactive({
#select drug event combinations, instead of unique reports  
  withProgress(message='Calculating',value=0,{
 
    start<-floor_date(timerange()[1],unit="month")
    end<-floor_date(timerange()[2],unit="month")
    
  table<-cv_drug_rxn_meddra%>%filter(month>=start&month<=end)
    
  if(drugvar$name!=""){
    if(input$v1=="Active_Ingredient"){
      table%<>%filter(ing==drugvar$name)
    }else if(input$v1=="DRUGNAME"){
      table%<>%filter(DRUGNAME==drugvar$name)
    }
  }
  
  if(input$t2!=""){
    table%<>%filter(PT_NAME_ENG==input$t2)
  }
  
  reportid<-table%>%distinct(REPORT_ID)
 
  table<-table%>%
    group_by(month)%>%
    summarise(Count=n())%>%
    mutate(month=format(as.Date(month),"%Y-%m"))
  
  datax<-zoo(table[[2]],table[[1]])

  incProgress(0.8)
  return(list(table=table,datax=datax,reportid=reportid))
  })
})


calccpmean<- reactive({
  datax<-table()$datax
  datax_changepoint <- cpt.mean(datax, Q=input$maxcp, method='BinSeg')
  return(datax_changepoint)
}) 

calccpvar<- reactive({
  datax <- table()$datax
  datax_changepoint <- cpt.var(datax, Q = input$maxcp, method='BinSeg')
  return(datax_changepoint)
}) 

calccpbayes<- reactive({
  
  table<-table()$table
  datax <- table()$datax
  
#add if statement to prevent crash:
  if(nrow(table)>=4){
  bcp.flu<-bcp(as.double(datax),p0=0.3)
  }else{
  bcp.flu<-NULL
  }
  return(bcp.flu)
}) 

#reactive for co-drug list:
codrug<-reactive({
 
  withProgress(message='Preparing table',value=0,{
 
  incProgress(0.25)
  ing<-cv_drug_rxn_meddra%>%
    select(REPORT_ID,ing)%>%
    semi_join(table()$reportid)%>%
    count(ing)%>%
    arrange(desc(n))
    
  
  ing<-ing[1:min(900,nrow(ing)),]
  
  incProgress(0.75)
 
  return(ing)
  })
})

coevent<-reactive({
  withProgress(message='Preparing table',value=0,{
  
  incProgress(0.25)
  pt<-cv_drug_rxn_meddra%>%
    select(REPORT_ID,PT_NAME_ENG)%>%
    semi_join(table()$reportid)%>%
    count(PT_NAME_ENG)%>%
    arrange(desc(n))
  
  
  pt<-pt[1:min(900,nrow(pt)),]
  
  incProgress(0.75)
  return(pt)
  })
})

output$drugname <- renderText({
  s <- drugvar$name
  if(s=='') {
    s <- 'All'
  }
  out <- paste( '<br><b>Drug Name:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$eventname <- renderText({
  s <-input$t2
  if(s=='') {
    s <- 'All'
  }
  out <- paste( '<b>Event Term:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$maxcp <- renderText({
  s <-input$maxcp
  if(s=='') {
    s <- 'None'
  }
  out <- paste( '<b>Maximum Number of Changepoints:<i>', s, '</i></b>' )
  return(out)
})

output$cpmeantext <- renderText ({
  table<-table()$table
  validate (
    need(nrow(table) >=4,'Insufficient Data')
  )
    
  s <- calccpmean()
    mycpts <- attr( s@data.set, 'index')[s@cpts[1:length(s@cpts)-1] ]
    mycpts <-paste(mycpts, collapse=', ')
    out <- paste( 'Changepoint type      : Change in', s@cpttype, '<br>' )
    out <- paste(out,  'Method of analysis    :' , s@method , '<br>' )
    out <- paste(out, 'Test Statistic  :' , s@test.stat, '<br>' )
    out <- paste(out, 'Type of penalty       :' , s@pen.type, 'with value', round(s@pen.value, 6), '<br>' )
    out <- paste(out, 'Maximum no. of cpts   : ' , s@ncpts.max, '<br>' )
    out <- paste(out, 'Changepoint Locations :' , mycpts , '<br>' )
  
  
  return(out)
})

output$cpmeanplot <- renderPlot ({
  
  datax<-table()$datax
  table<-table()$table
  
  validate(
    need(nrow(table)>=4,"Insufficient Data to fit a changepoint model,at least 4 observations are needed,
         please make a new selection or decrease number of Changepoints")
  )
    s <- calccpmean()
    labs <-    index(datax)
    pos <- seq(1, length(labs), 3)
    
    if (drugvar$name=='')
    {
      mydrugs <- 'All Drugs'
    }
    else 
    {
      mydrugs <-drugvar$name
    }
  
    if (input$t2=='' )
    {
      myevents <- 'All Events'
    }
    else 
    {
      myevents <- input$t2
    }
    mytitle <- paste( "Change in Mean Analysis for", mydrugs, 'and', myevents )
    plot(s, xaxt = 'n', ylab='Count', xlab='', main=mytitle)
    axis(1, pos,  labs[pos], las=2  )
    grid(nx=NA, ny=NULL)
    abline(v=pos, col = "lightgray", lty = "dotted",
           lwd = par("lwd") )
  
  
})

output$cpvartext <- renderText ({
 table<-table()$table
  validate ( 
    need(nrow(table) >=4,'Insufficient Data')
    )
  
    s <- calccpvar()
    mycpts <- attr( s@data.set, 'index')[s@cpts[1:length(s@cpts)-1] ]
    mycpts <-paste(mycpts, collapse=', ')
    out <- paste( 'Changepoint type      : Change in', s@cpttype, '<br>' )
    out <- paste(out,  'Method of analysis    :' , s@method , '<br>' )
    out <- paste(out, 'Test Statistic  :' , s@test.stat, '<br>' )
    out <- paste(out, 'Type of penalty       :' , s@pen.type, 'with value', round(s@pen.value, 6), '<br>' )
    out <- paste(out, 'Maximum no. of cpts   : ' , s@ncpts.max, '<br>' )
    out <- paste(out, 'Changepoint Locations :' , mycpts , '<br>' )
    return(out)
  
})

output$cpvarplot <- renderPlot ({
  table<-table()$table
 
   validate(
    need(nrow(table)>=4,"Insufficient Data to fit a changepoint model,at least 4 observations are needed,
         please make a new selection or decrease number of Changepoints")
  )
   
   s <- calccpvar()
    labs <-    index(table()$datax)
    pos <- seq(1, length(labs), 3)
    if (drugvar$name=='')
    {
      mydrugs <- 'All Drugs'
    }
    else 
    {
      mydrugs <-drugvar$name
    }
    if (input$t2=='')
    {
      myevents <- 'All Events'
    }
    else 
    {
      myevents <-input$t2
    }
    mytitle <- paste( "Change in Variance Analysis for", mydrugs, 'and', myevents )
    plot(s, xaxt = 'n', ylab='Count', xlab='', main=mytitle)
    axis(1, pos,  labs[pos], las=2  )
    grid(nx=NA, ny=NULL)
    abline(v=pos, col = "lightgray", lty = "dotted",
           lwd = par("lwd") )
  
})

output$cpbayestext <- renderPrint ({
  table<-table()$table
  validate(
    need(nrow(table)>=4,"Insufficient Data to fit a changepoint model,please make a new selection or decrease number of Changepoints")
  )
    
    bcp.flu <-calccpbayes()
    table$postprob <- bcp.flu$posterior.prob
    table2<-table[order(table$postprob,decreasing = TRUE),]
    table2[1:input$maxcp,]
  
})

output$cpbayesplot <- renderPlot ({
  table<-table()$table
  validate(
    need(nrow(table)>=4,"Insufficient Data to fit a changepoint model,at least 4 observations are needed,
         please make a new selection or decrease number of Changepoints")
  )
    s <- calccpbayes()
    labs <-index(table()$datax)
    plot(s)
    grid()
  
})


output$allquerytext <- renderText({
 out <- paste('<h4>Meta Data</h4>',
               '<b>Last Update =</b>',ymd(data_date), 
               '<br><b>Total =</b>', prettyNum( nrow(table()$reportid), big.mark=',' ), 'reports for', drugvar$name,'and',input$t2,
               'for dates from', timerange()[1],'to',timerange()[2]
               )
  return(out)
})


output$querytitle<-renderText({
  table<-table()$table
  out <- paste('<h4>Counts for',drugvar$name,'with event',input$t2,'</h4>',
                '<b>Total =</b>', prettyNum(sum(table$Count), big.mark=',' ), 'reports for drug-event combination')
  return(out)
})

output$query <- DT::renderDataTable(
  table()$table%>%
    rename(Month=month),
  options=list(
    pageLenth=25
  )
)

#build tab for codrugs in report
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Drugs In Selected Reports</h4>') ) )
})


output$coquery<-DT::renderDataTable(
  datatable(codrug(),
  colnames=c(input$v1,"Count"), 
  options=list(
    pageLenth=25
  ))
)

output$cloudcoquery <- renderPlot({  
  mydf <-codrug()
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Drug in Reports That Contain',drugvar$name)
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for',drugvar$name) ) )
  }  
  
}, height=900, width=900 )


#build tab for events in report
output$cotitleE <- renderText({ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4>') ) )
})

output$coqueryE<-DT::renderDataTable(
  datatable(coevent(),
  colnames=c("Adverse Event Preferred Term","Count"), 
  options=list(
  pageLenth=25
  )
 )
)

output$cloudcoqueryE <- renderPlot({  
  mydf <- coevent()
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Events in Reports That Contain',input$t2)
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for',input$t2 ) ) )
  }  
  
}, height=900, width=900 )



#output$frame<-renderUI({
# url<-"https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database/canada-vigilance-online-database-data-extract.html"  
# iframe<-tags$iframe(src=url,height=600,width=500)
# print(iframe)
#  iframe
#})

output$date1 <- renderText({ 
  l <- timerange()
  paste( '<b>','Reports received from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(data_date,  "%Y%m%d"), '</b>')
})

})
  