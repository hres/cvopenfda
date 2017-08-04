
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


drugSearch<-reactiveValues()
observe({
  if(input$v1=="Active_Ingredient"){
    name<-input$t1
  }else if(input$v1=="DRUGNAME"){
    name<-input$t1_1
  }
  
  drugSearch$name<-name
})

#reactive value upon updating:

updatevars <- reactive({
  
  input$update
  isolate( {
    updateSelectizeInput(session, "t1", choices=(input$ingname ) )
    updateSelectizeInput(session, "t1_1", choices=( input$drugname ) )
    updateNumericInput(session, "limit", value= ( input$limit2 ) )
    updateNumericInput(session, "start", value= ( input$start2 ) ) 
    updateNumericInput(session, "numsims", value= ( input$numsims2 ) ) 
  })
})


#reportid<-reactive({
    
#  reportid<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])  
    
#  if(drugSearch$name!=""){
#     if(input$v1=="Active_Ingredient"){

#       reportid%<>%filter(ing==drugSearch$name)
#     }else if(input$v1=="DRUGNAME"){
#       reportid%<>%filter(DRUGNAME==drugSearch$name)
#     }
#   }
    
#   reportid<-reportid%>%distinct(REPORT_ID)
    
#   return(reportid)

#})


#build tables for prr calulation:

eventcount<-reactive({

  mydf1<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])
#get total count of all drugs and AE  
  total<-nrow(mydf1)
  totalreport<-n_distinct(mydf1$REPORT_ID)
  
  if(drugSearch$name!=""){
    if(input$v1=="Active_Ingredient"){
      mydf2<-mydf1%>%filter(ing==drugSearch$name)
    }else if(input$v1=="DRUGNAME"){
      mydf2<-mydf1%>%filter(DRUGNAME==drugSearch$name)
    }
  }else{
      mydf2<-mydf1
  }

#get report id:
  reportid<-mydf2%>%distinct(REPORT_ID)
#get total event count of selected drug:
  
  totaldrug<-nrow(mydf2)
  totaldrugreport<-n_distinct(mydf2$REPORT_ID)
#get events count for selected drug:
  evcount<-mydf2%>%count(PT_NAME_ENG)%>%arrange(desc(n))       
  
#apply filter for the most frequent terms:
  start<-input$start
  end<-min(input$limit+start-1,nrow(evcount))
  
  evcount%<>%slice(start:end)
  
#Add last row as 'Other' with the sum of all other events:
  evcount<-rbind(evcount,c('Other',totaldrug-sum(evcount$n)))
  evcount$n<-as.integer(evcount$n)
  
  
#alleventcount:
  allevcount<-semi_join(mydf1,evcount[,1])%>%count(PT_NAME_ENG)%>%arrange(desc(n))
  allevcount<-rbind(allevcount,c('Other',total-sum(allevcount$n)))
  allevcount$n<-as.integer(allevcount$n)
  
  return(list(evcount=evcount,total=total,totaldrug=totaldrug,totalreport=totalreport,
              totaldrugreport=totaldrugreport,totaleventcount=allevcount,reportid=reportid))
})

#totaleventcount<-reactive({
  
# mydf1<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])
# total<-nrow(mydf1)
  
# evcount<-eventcount()$evcount%>%filter(PT_NAME_ENG!='Other')
# allevcount<-semi_join(mydf1,evcount[,1])%>%count(PT_NAME_ENG)%>%arrange(desc(n))

#Add last row as 'Other' with the sum of all other events:  
# allevcount<-rbind(allevcount,c('Other',total-sum(allevcount$n)))
# allevcount$n<-as.integer(allevcount$n)
  
# return(allevcount)
#})

#table for RR,LR output
getprr <-reactive({
  
  withProgress(message='Calculating...',value=0,{
  evcount<-eventcount()$evcount
  allevcount<-eventcount()$totaleventcount
  totaldrug<-eventcount()$totaldrug
  total<-eventcount()$total
  
  
  colname<-colnames(evcount[,1])
  comb <- inner_join(evcount,allevcount,by=colname)
  
  n.j <- totaldrug
  
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
  
  colname<-paste("Counts for",drugSearch$name)
  names(comb)<-c("Preferred Term","LLR","RR",colname,"nij","Significant?")
  
  return(list(comb=comb,critival=mycritival,maxLLR=maxLLR))
 })
})


prr <- reactive({
  
  if (drugSearch$name=="") {
    mydf<-data.frame(Term=paste('Please enter a',input$v1), Count=0,All_counts=0, PRR=0, ROR=0)
    return(mydf)
 
     } else {
    mydf<-getprr()$comb[,c(1,2,3,4,6)]
    tableout(mydf,error = paste('No records for',drugSearch$name))
  }
})



textplot <- reactive({ 
  if (drugSearch$name!="") {
    mydf <- getprr()$comb
    
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


#reactive for co-drug list: drugs appear in the same report
codrug<-reactive({
 
  withProgress(message='Loading table',value=0,{
 
  incProgress(0.25)
  ing<-cv_drug_rxn_meddra%>%
 dplyr::   select(REPORT_ID,ing)%>%
    semi_join(eventcount()$reportid)%>%
    count(ing)%>%
    arrange(desc(n))
  
  ing<-ing[1:min(900,nrow(ing)),]
  
  incProgress(0.75)
 
  return(ing)
  })
})

indication<-reactive({
  withProgress(message='Loading table',value=0,{
    
    incProgress(0.25)
    
    indt<-semi_join(cv_indication,eventcount()$reportid,copy=T)%>%
      count(INDICATION_NAME_ENG)%>%
      arrange(desc(n))%>%
      as.data.table()
    
    incProgress(0.75)    
    indt<-indt[1:min(500,nrow(indt)),]
    
    
    return(indt)
  })
})

output$drugname <- renderText({ 
  s <-drugSearch$name
  if(s=='') {
    s <- 'None'
  }
  renderterm( s, 'Drug Name:') 
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




#tab for LLR and RR result:
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
    need(drugSearch$name!="","Please enter a drug variable")
  )
   
    mydf <- getprr()$comb
    mydf <- data.frame(mydf[,1], mydf[,'LLR'])
    cloudout(mydf, paste('LLR for Events in Reports That Contain',drugSearch$name) )

}, height=900, width=900)


output$textplot <- renderPlot({ 
  textplot()
}, height=400, width=900)


output$simplot<-renderPlot({
  validate(
    need(drugSearch$name!="","Please enter a drug variable")
  )
  
  mydf<-getprr()
 
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

#tab for event count for specified drug:
output$alldrugtext <- renderText({ 
  l <-eventcount()$totaldrugreport
  return( 
    paste('<h4>Meta Data</h4><br>',
          '<b>Last Update =</b>',ymd(data_date),'<br>',
          '<b>From:</b>',timerange()[1],'and',timerange()[2],'<br>',
          '<b>Total report number with',input$v1,drugSearch$name, 'in database:</b>', prettyNum(l, big.mark=',' ), '<br>') )
})

cloudquery <- reactive({  
  cloudout(eventcount()$evcount, paste('Terms in Reports That Contain',drugSearch$name))
})

output$cloudquery <- renderPlot({  
  cloudquery()
}, height=900, width=900 )

output$specifieddrug<-DT::renderDataTable(
  eventcount()$evcount%>%
    rename(`Preferred Term`=PT_NAME_ENG,Count=n),
  options = list(
    pageLength=10
  )
)

#tab for event counts for all drug:
output$alltext <- renderText({ 
  l <- eventcount()$totalreport
  paste('<b>From:</b>',timerange()[1],'and',timerange()[2],'<br>',
       '<b>Total reports in database:</b>', prettyNum(l, big.mark=',' ),'<br>')
})

cloudall <- reactive({  
  cloudout(eventcount()$totaleventcount, 
           paste('Events in Reports That Contain',drugSearch$name) ) 
})

output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

output$all2<-DT::renderDataTable(

     eventcount()$totaleventcount%>%
    rename(`Preferred Term`=PT_NAME_ENG,Count=n),
  options = list(
    pageLength=25)
  
)

#build tab for codrugs in report
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Drugs In Selected Reports</h4>') ) )
})


output$coquery<-DT::renderDataTable(
  datatable(codrug(),
  colnames=c("Active Ingredient","Count"), 
  options=list(
    pageLenth=25,
    autoWidth=TRUE
  ))
)

output$cloudcoquery <- renderPlot({  
  mydf <-codrug()
  
  validate(
    need(nrow(mydf)>0,"No Results to show")
  )
  
    mytitle <- paste('Drug in Reports That Contain',drugSearch$name)
    return( getcloud(mydf, title=mytitle ) ) 
  
}, height=900, width=900 )


#build tab for indication:
output$indtitle <- renderText({ 
  return( ( paste0('<h4>Most Common Indications In Selected Reports</h4>') ) )
})

output$cloudindquery <- renderPlot({ 
  cloudout(indication(), 
           paste('Drug indications in Reports That Contain',drugSearch$name ))
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
  