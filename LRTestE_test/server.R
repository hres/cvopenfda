
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
  totaleventreport<-n_distinct(mydf1$REPORT_ID)
  
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
  
  drugcount<-rbind(drugcount,c('Other',totalevent-sum(drugcount$n)))
  drugcount$n<-as.integer(drugcount$n)
  
#all drug count  
  alldrugcount<-semi_join(mydf1,drugcount[,1])
  
  if(input$v1=="Active_Ingredient"){
    alldrugcount%<>%count(ing)%>%arrange(desc(n))   
  }else if(input$v1=="DRUGNAME"){
    alldrugcount%<>%count(DRUGNAME)%>%arrange(desc(n))   
  }
  
  #Add last row as 'Other' with the sum of all other events:  
  alldrugcount<-rbind(alldrugcount,c('Other',total-sum(alldrugcount$n)))
  alldrugcount$n<-as.integer(alldrugcount$n) 
  
  return(list(drugcount=drugcount,total=total,totalevent=totalevent,totalreport=totalreport,
              totaleventreport=totaleventreport,reportid=reportid,alldrugcount=alldrugcount))
})



#table for PRR,ROR output
getprr <-reactive({
  
  withProgress(message="Calculating...",value=0,{
  drugcount<-drugcount()$drugcount
  alldrugcount<-drugcount()$alldrugcount
  totalevent<-drugcount()$totalevent
  total<-drugcount()$total
  
  colname<-colnames(drugcount[,1])
  comb <- inner_join(drugcount,alldrugcount,by=colname)
  
  n.j <- totalevent
  
  validate(
    need(n.j>0,"No match from the search")
  )
  #Total reports for DE combination
  nij <-  comb$n.x
  
  #Total reports for outcome interested:
  ni. <- comb$n.y
  n.. <- total
  
  rr <- rr( n.., ni., n.j, nij ) 
  LLR <- LLRD( n.., ni., n.j, nij )
  LLR[rr<1]<-0
  
  comb <- data.frame(comb[,1], LLR=round(LLR, digits = 2), RR=round(rr,digits=2),nij,nij)%>%arrange(desc(LLR))
  
  maxLLR<-max(comb$LLR)
  incProgress(0.3,detail="Calculating LLR")
  
  pi. <- ni./n..
  
  numsims <- input$numsims
  mycritival<-getCritVal2(numsims,n.j,ni.,n..,pi.,0.95)
  critval05<-mycritival$critval
  
  comb%<>%mutate(signal=ifelse((LLR>critval05),"p<0.05","Not signal"))
  
  incProgress(0.8,detail="Monte Carlo simulation for null hypothesis")
  
  colname<-paste("Counts for",input$t2)
  names(comb)<-c(input$v1,"LLR","RR",colname,"nij","Significant?")
  
  return(list(comb=comb,critival=mycritival,maxLLR=maxLLR))
  })
})


prr <- reactive({
  
  if (input$t2=="") {
    mydf<-data.frame(Term=c('Please enter an Adverse Event'), Count=0,All_counts=0, PRR=0, ROR=0)
    return(mydf)
  } else {
    mydf<-getprr()$comb%>%`[`(c(1,2,3,4,6))
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
    mydf <- getprr()$comb
    
    validate(
      need(any(mydf[,2])>0,'Insufficient data to calculate LLR')
    )
    
    
    y <- mydf[,2]
    x <- mydf[,4]
    w <- mydf[,1]
  } else {
    w <- NULL
    y <-NULL
    x <- NULL
  }
  
  return ( mytp(x, y, w, myylab='LLR') )

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
  dplyr::select(REPORT_ID,PT_NAME_ENG)%>%
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
  renderterm( input$limit, 'Start analysis at ranked frequency count # ',
              label2=paste( '<br>Analyzing counts with ranked frequencies from',
                            startfont,input$start,endfont,
                            'to', 
                            startfont, input$start+input$limit-1, endfont  ) ) 
} )

output$numsims <- renderText({renderterm( input$numsims, 'Number of simulations:')} )


#Download report:
output$downloadReport<-downloadHandler(
  filename=function(){
    paste('my-report',sep=".",switch(input$format,PDF='pdf',HTML='html',Word="docx"))
  },
  
  content=function(file){
    rmdfile<-'report.Rmd'
    src <- normalizePath( rmdfile )
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, rmdfile, overwrite = TRUE)
    
    #   library(rmarkdown)
    out <- rmarkdown::render(rmdfile, switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)

#tab for LLR result:
output$prrtitle <- renderText({ 
  critival<-getprr()$critival
  maxLLR<-max(getprr()$maxLLR)
  
  return(paste('<h4>Reporting Ratios: Results sorted by LLR</h4><br>',
               '<b>Critical Value:</b>',round(critival$critval,2),'<br>',
               '<b>Maxiumn Log Likelihood ratio:</b>',prettyNum(maxLLR)))
})

output$prr <- DT::renderDataTable(  
  prr(),
  options = list(
  pageLength=10
  )
)

output$cloudprr <- renderPlot({  
  validate(
    need(input$t2,"Please enter a Drug Name")
  )
   
  mydf <- getprr()$comb
  
  validate(
    need(any(mydf[,'LLR'])>0,'Insufficient data to calculate LLR')
  )
  
  mydf <- data.frame(mydf[,1], mydf[,'LLR'])
  cloudout(mydf, paste('LLR for Events in Reports That Contain',input$t2) )
  
}, height=900, width=900)

output$textplot <- renderPlot({ 
  textplot()
}, height=400, width=900)


output$simplot<-renderPlot({
  validate(
    need(input$t2!="","Please enter an Adverse Event term")
  )
  
  mydf<-getprr()
  
  validate(
    need(any(mydf$comb$LLR)>0,'Insufficient data to calculate LLR')
  )
  
  mycrit <- mydf$critival$critval
  vals <- mydf$critival$mymax
  myrange <- range(vals)
  interval <- (mycrit - myrange[1])/20
  mybreaks <- c( seq(myrange[1], mycrit, interval ),  seq(mycrit+interval,  myrange[2] + interval, interval ) )
  hist(vals , breaks=mybreaks,prob=TRUE,col="cyan", 
           main="Histogram of Simulated Distribution of LLR", 
           xlab='Loglikelihood Ratio', xaxt='n')
  d<-density(vals)
  dd<-approxfun(d$x,d$y)
  text(x=mycrit,y=dd(mean(vals)), paste('Rejection Region, LLR >', round(mycrit, 2) ), pos=4, col='red')
  smallbreaks <- seq(0, max(mybreaks), 1)
  
  smallbreaks <-  c( round(mycrit, 2), smallbreaks )
  axis(1, smallbreaks, las=3 )
  abline(v=mycrit, col='red', lwd=2)
})

#show table when draw box in text plot:
output$info <- DT::renderDataTable({
  mydf <- getprr()$comb
  brushedPoints(mydf, input$plot_brush, yvar = "LLR", xvar = 'nij' )
})

#tab for drug count for specified event:
output$alldrugtext <- renderText({ 
  l <-drugcount()$totaleventreport
  return( 
    paste('<h4>Meta Data</h4>',
          '<b>Last Update =</b>',ymd(data_date),'<br>',
          '<b>From:</b>',timerange()[1],'<b>to</b>',timerange()[2],'<br>',
          '<b>Total reports with Adverse Event',input$t2, 'in database:</b>', prettyNum(l, big.mark=',' ), '<br>') )
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
    pageLength=10
  )
 )
)

#tab for drug counts for all report:
output$alltext <- renderText({ 
  l <- drugcount()$totalreport
  paste( '<b>From:</b>',timerange()[1],'<b>to</b>',timerange()[2],'<br>',
    '<b>Total reports in database:</b>', prettyNum(l, big.mark=',' ), '<br>')
})

cloudall <- reactive({  
  cloudout(drugcount()$alldrugcount, 
           paste('Drugs in Reports That Contain',input$t2) ) 
})

output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

output$all2<-DT::renderDataTable(
  datatable(drugcount()$alldrugcount,
  colnames=c(input$v1,"Count"),
  options = list(
    pageLength=10
  )
 )
)

#build tab for coevents in report
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
  