

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
  start<-floor_date(ymd(timerange()[1]),unit="month")
  end<-floor_date(ymd(timerange()[2]),unit="month")
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

observe({    
    pt_choices <- drug_PT_HLT
    if (input$v1=="Active_Ingredient") {
      pt_choices %<>% filter(ing==input$ingname)
    }else if(input$v1=="DRUGNAME"){
      pt_choices %<>% filter(DRUGNAME==input$drugname)
    }
    
    pt_choices %<>% distinct(PT_NAME_ENG) %>% as.data.frame() %>% `$`("PT_NAME_ENG") %>% sort()
    
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
  })
})


#reportid<-reactive({
 
# reportid<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2])  
    
#  if(drugSearch$name!=""){
#     if(input$v1=="Active_Ingredient"){
#       reportid%<>%filter(ing==drugSearch$name)
#     }else if(input$v1=="DRUGNAME"){
#       reportid%<>%filter(DRUGNAME==drugSearch$name)
 #     }
#    }
  #   if(input$t2!=""){
#     reportid%<>%filter(PT_NAME_ENG==input$t2)
#   }
#   reportid<-reportid%>%select(REPORT_ID)
#   return(reportid)
  
#})
#filter cv_drug_rxn_meddra with date:

mydf_date<-reactive({
  
   mydf<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2]) 
   return(mydf)
})

#build tables for prr overtime:
#table for monthly count of selected drug-event and all drug events 
mydf1<-reactive({
  
  mydf1<-mydf_date()
  
  allcount<-mydf1%>%group_by(month)%>%summarise(Count=n())%>%as.data.table()%>%ungroup()%>%arrange(month)
  allcount$Cumsum<-cumsum(allcount$Count)
  allcount$month<-format(allcount$month,"%Y-%m")
  
  
  if(drugSearch$name!=""){
    if(input$v1=="Active_Ingredient"){
      mydf1%<>%filter(ing==drugSearch$name)
    }else if(input$v1=="DRUGNAME"){
      mydf1%<>%filter(DRUGNAME==drugSearch$name)
    }
  }
  
  if(input$t2!=""){
    mydf1%<>%filter(PT_NAME_ENG==input$t2)
  }
  
  reportid<-mydf1%>%distinct(REPORT_ID)
  
  mydf1%<>%group_by(month)%<>%summarise(Count=n())%>%as.data.table()%>%ungroup()%>%arrange(month)
  mydf1$Cumsum<-cumsum(mydf1$Count)
  mydf1$month<-format(mydf1$month,"%Y-%m")
  
  return(list(mydf1=mydf1,reportid=reportid,allcount=allcount))
})


#table for monthly count of drug
mydf2<-reactive({
  
  mydf2<-mydf_date()
  
  if (drugSearch$name!=""){
    if(input$v1=="Active_Ingredient"){
    mydf2<-mydf2%>%filter(ing==drugSearch$name)
  }else if(input$v1=="DRUGNAME"){
    mydf2<-mydf2%>%filter(DRUGNAME==drugSearch$name)
  }
}
  
  mydf2<-mydf2%>%group_by(month)%>%summarise(Count=n())%>%as.data.table()%>%ungroup()%>%arrange(month)
  mydf2$Cumsum<-cumsum(mydf2$Count)
  mydf2$month<-format(mydf2$month,"%Y-%m")
  return(mydf2)
  
})

#table for monthly count of event
mydf3<-reactive({
  
  mydf3<-mydf_date()
  
  if (input$t2!=""){
    mydf3<-mydf3%>%filter(PT_NAME_ENG==input$t2)
  }
  
  mydf3<-mydf3%>%group_by(month)%>%summarise(Count=n())%>%as.data.table()%>%ungroup()%>%arrange(month)
  mydf3$Cumsum<-cumsum(mydf3$Count)
  mydf3$month<-format(mydf3$month,"%Y-%m")
  return(mydf3)
  
})

#table for monthly count of All
#mydf4<-reactive({
  
# mydf4<-cv_drug_rxn_meddra%>%filter(month>=floordate()[1]&month<=floordate()[2]) 
# mydf4<-mydf4%>%group_by(month)%>%summarise(Count=n())%>%as.data.table()%>%ungroup()%>%arrange(month)
  
# mydf4$Cumsum<-cumsum(mydf4$Count)
# mydf4$month<-format(mydf4$month,"%Y-%m")
# return(mydf4)
#})

#merged table for ploting

mergetable<-reactive({
  
  withProgress(message="Calculating...",value=0,{
  df1<-mydf1()$mydf1
  df2<-mydf2()
  df3<-mydf3()
  df4<-mydf1()$allcount
  
  mydf_d <- inner_join(df1[,c(1,3)], df2[,c(1,3)], by="month")
  
  withProgress(0.25)
  names(mydf_d) <- c('Month', 'Drug_Event_Counts', 'Drug_Counts')
  mydf_all <- inner_join(df3[,c(1,3)], df4[,c(1,3)], by="month")
  
  withProgress(0.5)
  names(mydf_all) <- c('Month', 'Event_Counts', 'Total_Counts')
  mydf <- inner_join(mydf_d, mydf_all, by="Month")
  comb <- mydf%>%filter(Event_Counts>0,Total_Counts>2)
  
  withProgress(0.75)
  nij <- comb$Drug_Event_Counts
  n.j <- comb$Drug_Counts
  ni. <- comb$Event_Counts
  n.. <- comb$Total_Counts
  prrci <- prre_ci( n.., ni., n.j, nij )
  comb <- data.frame(comb, prr=round(prrci[['prr']], 2), 
                     sd=round(prrci[['sd']], 2), lb=round(prrci[['lb']], 2), 
                     ub=round(prrci[['ub']], 2) )
  return(comb)
  })

})

#build the modal: 
output$mymodal <- renderText({
  if (input$update > 0){
    updatevars()
    toggleModal(session, 'modalExample', 'close')
  }
    return('')

})


#reactive for co-drug list:
codrug<-reactive({
 
  withProgress(message='Preparing table',value=0,{
 
  incProgress(0.25)
  ing<-cv_drug_rxn_meddra%>%
    select(REPORT_ID,ing)%>%
    semi_join(mydf1()$reportid)%>%
    count(ing)%>%
    as.data.frame()%>%
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
    semi_join(mydf1()$reportid)%>%
    count(PT_NAME_ENG)%>%
    as.data.frame()%>%
    arrange(desc(n))
  
  pt<-pt[1:min(900,nrow(pt)),]
  
  incProgress(0.75)
  return(pt)
  })
})


output$prrplot<-renderPlot({
  mydf<-mergetable()
  mydf<-mydf%>%filter(is.finite(sd))
  
  if ( !is.null(mydf) & drugSearch$name!='' & input$t2!='' ){
    mydrugs<-drugSearch$name
    myevents<-input$t2
    
    myylim <- c( min(.5, min(mydf$lb)), max(2, max(mydf$ub) ) )
    xloc <-parse_date_time(mydf$Month,"ym")
    labs <- mydf$Month
    
    lbgap <-   exp(log(mydf$lb) + .96*mydf$sd) #exp ( log( prr ) - 1.96*sd )
    ubgap <-   exp(log(mydf$ub) - .96*mydf$sd)
    
    mytitle <- paste( "PRR Plot for", mydrugs, 'and', myevents )
    plot( xloc, mydf$prr, ylim=myylim,main=mytitle, ylab='95% Confidence Interval for PRR',
          xlab='', las=2, xaxt='n', bg='red', cex=.5, pch=21)
    axis(1, at=xloc[index(xloc)%%4==0], labels=labs[index(labs)%%4==0],cex.axis=0.8, las=2   )
    if( ! isTRUE( all.equal(mydf$prr, mydf$lb) ) )
    {
      arrows(x0=xloc[ mydf$prr!=mydf$lb ], x1=xloc[ mydf$prr!=mydf$lb ],
             y0=lbgap[ mydf$prr!=mydf$lb ], y1=mydf$lb[ mydf$prr!=mydf$lb ], angle=90, length=.025)
      arrows(x0=xloc[ mydf$prr!=mydf$ub ], x1=xloc[ mydf$prr!=mydf$ub ],
             y1=mydf$ub[ mydf$prr!=mydf$ub ], y0=ubgap[ mydf$prr!=mydf$ub ], angle=90, length=.025)
    }
    abline(h=1)
    grid()
   
  }else{
    mytitle <-  "Please select a drug and event" 
    plot( c(0,1), c(0,1),  main=mytitle )
    text(.5, .5, "Please select a drug and event")
  }
})

output$drugname <- renderText({
  s <- drugSearch$name
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


#change reports number: Caution: report number indicates the number of Reports, ie unique report ID, not drug occurance. Single
#report ID can have the same drug with multiple PT events.
output$querytitle <- renderText({
  table<-mydf1()$reportid
  out <- paste('<h4>Meta Data</h4>',
               '<b>Last Update =</b>',ymd(data_date), 
               '<br><b>Total =</b>', prettyNum( tail(nrow(table),n=1), big.mark=',' ), 'reports for', drugSearch$name,'and',input$t2,
               'for dates from', timerange()[1],'to',timerange()[2],'<br><br>'
               )
  return(out)
})

#PRR plot:


#PRR table:
output$query_counts2 <- DT::renderDataTable(  
   mergetable()%>%
    rename(SD=sd,`95% CI low`=lb,`95% CI high`=ub),
  options=list(
    pageLenth=25,
    scrollX=TRUE
 )
)


#build tab for codrugs in report
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Drugs In Selected Reports</h4><br>') ) )
})


output$coquery<-DT::renderDataTable(
  datatable(codrug(),
  colnames=c("Active Ingredient","Count"), 
  options=list(
    pageLenth=25
  ))
)

output$cloudcoquery <- renderPlot({  
  mydf <-codrug()
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Drug in Reports That Contain',drugSearch$name)
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for',drugSearch$name) ) )
  }  
  
}, height=900, width=900 )


#build tab for events in report
output$cotitleE <- renderText({ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4><br>') ) )
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
  