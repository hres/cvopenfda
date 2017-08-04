
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

floordate<-reactive({
  start<-floor_date(timerange()[1],unit="month")
  end<-floor_date(timerange()[2],unit="month")
  return(c(start,end))
})


#reactive value upon updating:

updatevars <- reactive({
  
  input$update
  isolate( {
#    updateSelectizeInput(session, "v1", choices=("Active_Ingredient,"Drugname),selected=input$v1 )
    updateSelectizeInput(session, "t2", choices=(input$eventname ) )
    updateNumericInput(session, "limit", value= ( input$limit2 ) )
    updateNumericInput(session, "start", value= ( input$start2 ) ) 
  })
})


#reportid<-reactive({
# reportid<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])  
    
#   if(input$t2!=""){
#     reportid%<>%filter(PT_NAME_ENG==input$t2)
#   }
    
#    reportid<-reportid%>%select(REPORT_ID)
    
#    return(reportid)

#})


#build tables for prr calulation:

drugcount<-reactive({

  mydf1<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])
#get total count of all drugs and AE  
  total<-nrow(mydf1)
  totalreport<-n_distinct(mydf1$REPORT_ID)
  
  if(input$t2!=""){
    mydf2<-mydf1%>%filter(PT_NAME_ENG==input$t2)
  }else{
    mydf2<-mydf1
  }
 
#reportid:
  
  reportid<-mydf2%>%distinct(REPORT_ID)
  
#get total count of selected drug:
  
  totalevent<-nrow(mydf2)
  totaleventreport<-n_distinct(mydf2$REPORT_ID)
  
#get events count for selected drug:
  if(input$v1=="Active_Ingredient"){
    drugcount<-mydf2%>%count(ing)%>%arrange(desc(n))   
  }else if(input$v1=="DRUGNAME"){
    drugcount<-mydf2%>%count(DRUGNAME)%>%arrange(desc(n))   
  }
      
  
#apply filter for the most frequent terms:
  start<-input$start
  end<-min(input$limit+start-1,nrow(drugcount))
  
  drugcount%<>%slice(start:end)
  
  
#alldrugcount:
  
  alldrugcount<-semi_join(mydf1,drugcount[,1])
  
  if(input$v1=="Active_Ingredient"){
    alldrugcount%<>%count(ing)%>%arrange(desc(n))   
  }else if(input$v1=="DRUGNAME"){
    alldrugcount%<>%count(DRUGNAME)%>%arrange(desc(n))   
  }
  
  return(list(drugcount=drugcount,total=total,totalevent=totalevent,
              totalreport=totalreport,totaleventreport=totaleventreport,
              reportid=reportid,totaldrugcount=alldrugcount))
})



#table for PRR,ROR output
getprr <-reactive({
  
  withProgress(message="Calculating...",value=0,{
  drugcount<-drugcount()$drugcount
  alldrugcount<-drugcount()$totaldrugcount
  totalevent<-drugcount()$totalevent
  total<-drugcount()$total
  
  comb<-makecomb(drugcount,alldrugcount,total,totalevent,"Event")
  
  incProgress(0.5)
  
  countname <- paste( 'Counts for', input$t2)
  names(comb)<-c(input$v1,countname,"Counts for all Reports","PRR","ROR","nij")
  return(comb)
  })
})


prr <- reactive({
  
  if (input$t2=="") {
    mydf<-data.frame(Term=c('Please enter an Adverse Event'), Count=0,All_counts=0, PRR=0, ROR=0)
    return(mydf)
  } else {
    mydf<-getprr()%>%`[`(c(1:5))
    tableout(mydf,error = paste('No records for',input$t2))
  }
})

#cloudprr <- reactive({ 
# if(drugSearch$name==""){
# plot(c('Please enter a',input$v1))
# }else{
  
# mydf <- getprr()
# mydf <- data.frame(mydf[,1], mydf[,'PRR']*10)
# cloudout(mydf, paste('PRR for Events in Reports That Contain',drugSearch$name) )  
# }
#})

textplot <- reactive({ 
  if (input$t2!="") {
    mydf <- getprr()
    
    y <- mydf[,4]
    x <- mydf[,2]
    w <- mydf[,1]
  } else {
    w <- NULL
    y <-NULL
    x <- NULL
  }
  
  return ( mytp(x, y, w, myylab='PRR') )

})

#build the modal: 
output$mymodal <- renderText({
  if (input$update > 0){
    updatevars()
    toggleModal(session, 'modalExample', 'close')
  }
    return('')

})


#reactive for co-event list: events appear in the same report
coevent<-reactive({
  withProgress(message='Loading table',value=0,{
    
    incProgress(0.25)
    pt<-cv_drug_rxn_meddra%>%
      select(REPORT_ID,PT_NAME_ENG)%>%
      semi_join(drugcount()$reportid)%>%
      count(PT_NAME_ENG)%>%
#      as.data.frame()%>%
      arrange(desc(n))
    
    pt<-pt[1:min(900,nrow(pt)),]
    incProgress(0.75)
    return(pt)
  })
})

#take indications from the reports:
indication<-reactive({
  withProgress(message='Loading table',value=0,{
    
    incProgress(0.25)
    
    indt<-semi_join(cv_indication,drugcount()$reportid,copy=T)%>%
          count(INDICATION_NAME_ENG)%>%
          arrange(desc(n))%>%
          as.data.table()
    
    incProgress(0.75)    
    indt<-indt[1:min(500,nrow(indt)),]
    
    
    return(indt)
  })
})

output$eventname <- renderText({ 
  s <-input$t2
  if(s=='') {
    s <- 'None'
  }
  renderterm( s, 'Event Name:') 
} )


#output limit and start number:
output$limit <- renderText({ renderterm(input$limit, 'Limit Analysis to', 'most frequent terms') } )

output$start <- renderText({ 
  startfont <- '<i><font color="navy" size="3">'
  endfont <- '</font></i>'
  renderterm( input$start, 'Start analysis at ranked frequency count # ',
              label2=paste( '<br>Analyzing counts with ranked frequencies from',
                            startfont,input$start,endfont,
                            'to', 
                            startfont, input$start+input$limit-1, endfont  ) ) 
} )

#tab for PRR and ROR result:
output$prrtitle <- renderText({ 
  return('<h4>Reporting Ratios: Results sorted by PRR</h4>')
})

output$prr2 <- DT::renderDataTable(  
  prr(),
  options = list(
  pageLength=10
  )
)

output$cloudprr <- renderPlot({  
  validate(
    need(input$t2,"Please enter an Adverse Event")
  )
   
    mydf <- getprr()
    mydf <- data.frame(mydf[,1], mydf[,'PRR']*10)
    cloudout(mydf, paste('PRR for Events in Reports That Contain',input$t2) )

  
}, height=900, width=900)

output$textplot <- renderPlot({ 
  textplot()
}, height=400, width=900)

#show table when draw box in text plot:
output$info <- DT::renderDataTable({
  mydf <- getprr()
  brushedPoints(mydf, input$plot_brush, yvar = "PRR", xvar = 'nij' )
})

#tab for drug count for specified event:
output$alldrugtext <- renderText({ 
  l <-drugcount()$totaleventreport
  return( 
    paste('<h4>Meta Data</h4>',
          '<b>Last Update =</b>',ymd(data_date),'<br>',
          '<b>Total reports with Adverse Event',input$t2,'between</b>',timerange()[1],'and',timerange()[2], 
          '<b>in database:</b>', prettyNum(l, big.mark=',' ), '<br>') )
})

cloudquery <- reactive({  
  cloudout(drugcount()$drugcount, paste('Terms in Reports That Contain',input$t2))
})

output$cloudquery <- renderPlot({  
  cloudquery()
}, height=900, width=900 )

output$specifieddrug<-DT::renderDataTable(
  datatable(drugcount()$drugcount,
    colnames=c(input$v1,"Count"),
  options = list(
    pageLength=10,
    autoWidth = TRUE
  )
 )
)

#tab for drug counts for all report:
output$alltext <- renderText({ 
  l <- drugcount()$totalreport
  paste( '<b>Total reports between',timerange()[1],'and',timerange()[2],'in database:</b>', prettyNum(l, big.mark=',' ), '<br>')
})

cloudall <- reactive({  
  cloudout(drugcount()$totaldrugcount, 
           paste('Drugs in Reports That Contain',input$t2) ) 
})

output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

output$all2<-DT::renderDataTable(
  datatable(drugcount()$totaldrugcount,
  colnames=c(input$v1,"Count"),
  options = list(
    pageLength=10,
    autoWidth = TRUE
  )
 )
)

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
 cloudout(coevent(),
          paste('Events in Reports That Contain',input$t2 ))
}, height=900, width=900 )


#build tab for indication:
output$indtitle <- renderText({ 
  return( ( paste0('<h4>Most Common Indications In Selected Reports</h4>') ) )
})

output$cloudindquery <- renderPlot({  
  cloudout(indication(), 
            paste('Drug indications in Reports That Contain',input$t2 ))
}, height=1000, width=1000)

output$indquery2<-DT::renderDataTable(
  datatable(indication(),
            colnames=c("Drug Indication","Count"), 
            options=list(
              pageLenth=25
            )
  )
)
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
  