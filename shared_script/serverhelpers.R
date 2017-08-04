require(RColorBrewer)
require(wordcloud)
require(stringi)


renderterm <-  function( term, label, label2='' ){ 
  if(term == '') {
    term <- 'All'
  }
  if (label2 != '')
  { 
    out <- paste( '<br><b>', label, '<i><font color="navy" size="3">', term, '</font></i><br>', label2,'</b><br>' )
  } else {
    out <- paste( '<br><b>', label, '<i><font color="navy" size="3" >', term, '</font></i></b><br><br>' )
  }
  return(out)
}

makecomb <- function(mydf1, mydf2, total,totalexposure, type){
  
  if ( !is.data.frame(mydf1) )  {
    return(data.frame(Term=paste( 'No events for selected',type)))
  }
  colname<-colnames(mydf1[,1])
  comb <- inner_join(mydf1,mydf2,by=colname)
  
    n.j <- totalexposure
    #Total reports for DE combination
    nij <-  comb$n.x
    
    #Total reports for outcome interested:
    ni. <- comb$n.y
    n.. <- total
  
  prr <- prre( n.., ni., n.j, nij ) 
  ror <- ror( n.., ni., n.j, nij )
  
  comb <- data.frame(comb, prr=round( prr, digits = 2), ror=round(ror,digits=2),nij)%>%arrange(desc(prr))    
  return(comb)
}

getcloud_try <- function(mydf, scale1=9, name=1, freq=2, title='Terms') {
 
  par(mar=c(0,0,2,0))
  mydf[ which( is.nan(mydf[[freq]]) ), freq] <- 1
  options(warn=5)
  curscale <- scale1
  for (i in 1:(2*scale1-1))
  { 
    #  print(curscale)
    out <-  try(  ( wordcloud(mydf[[name]], mydf[[freq]], max.words = 100 ,
                              color=rep('white', 8), random.order=FALSE, 
                              scale=c(curscale, .5) ) ),silent=TRUE )
    if( class(out) == "try-error" )
    {   
      curscale <- curscale-.5
    } else {
      break()
    }
  }
  options(warn=0)
  return( c(curscale, .5) )
}

getcloud <- function(mydf, scale1=9, name=1, freq=2, title='Terms', scale=NULL) {

  if ( is.null(scale) & nrow(mydf) > 1)
  {
    scale <- invisible(getcloud_try(mydf, scale1, name, freq, title))
  }
  par(mar=c(0,0,2,0))
  
  mylabel <- paste('Word Cloud for', title)
  mylabel <- title
  #text(.5, .5, labels=mylabel, cex=2)
  mydf[ which( is.nan(mydf[[freq]] ) ), freq] <- 1
  if( nrow(mydf) > 1 )
    {
    wordcloud(mydf[[name]], mydf[[freq]], max.words = 100 ,
                color=brewer.pal(8, "Dark2"), random.order=FALSE, 
                scale=scale )
    }
  text(.5, 1, labels=mylabel, cex=1.5)

}

cloudout <- function(mydf, title)
{
  if ( is.data.frame(mydf) )
  {
    mydf <- mydf[ 1:min( 200, nrow(mydf) ), ]
    #   print(mydf)
    return( getcloud(mydf,  title = title ) )  
  } else  {
    return( data.frame(Term="No terms to display") )
  }  
  
}

tableout <- function( mydf,error)
{ 
  if ( length(mydf) > 0 ){
  return(mydf) 
  } else  {return(data.frame(Term=error, Count=0))
  }
}


mytp <- function(x, y, w, refline=1, 
                 mytitle="Text Plot for Terms.  Draw a box around terms to see more details",
                 myylab='LLR') { 
  # browser()
  mycex=1
  if (length(w) > 0)
  {
    xlim <-  c( min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    xlim[2] <- xlim[2] + 0.3*(xlim[2]-xlim[1] ) 
    plot(x,y,type="p", 
         xlim = xlim,
         ylim = c( 1.0, max(y, na.rm = TRUE) ),
         log='y',
         xlab= 'Number of Events',
         ylab= myylab,
         col='red',
         main=mytitle,
         cex=mycex)
    text(x, y, w, pos=4, cex=mycex)
  } else {
    plot(1,1,type="n", 
         log='y',
         xlab= 'Number of Events',
         ylab= myylab,
         col='red',
         main='Please enter a term',
         cex=mycex)
  }
  #  text(x,y,w, pos=4, cex=.75)
  grid()
  abline(h=refline, col='red')
}


makeapplinks <- function()
{
  mynames<-c('DA','DISP','D','E', 'P', 'Z', 'LRE', 'LR')
  
  labels <- vector(mode='character', length = length(mynames))
  names(labels)<-mynames
  labels['DA'] <- '<h4>Drug Apps</h4><b>Dashboard-</b> Overview of reports for a drug'
  labels['DISP']<-'<b>Disproportioanlity Analysis for a Drug-Event-</b> Calculate Information Component for Common Events for a drug'
  labels['D'] <- '<b>PRR for a Drug-</b> Calculate Proportional Reporting Rates for Common Events for a drug'
  labels['E'] <-  '<b>PRR for an Event-</b> Calculate Proportional Reporting Rates for Common Drugs that have a specified event'
  labels['P'] <-  '<b>Dynamic PRR-</b> Calculate Proportional Reporting Rates for a drug-event pair over time'
  labels['Z'] <- '<b>Change Point Analysis-</b> Change point analysis for a drug-event pair over time'
  labels['LR'] <- '<b>Likelihood Ratio Test for Drug-</b> Calculate Likelihood Ratio Tests for Common Events for a drug'
  labels['LRE'] <-'<b>Likelihood Ratio Test for Event-</b> Calculate Likelihood Ratio Tests for Common Drugs for an event'
  
  link<-vector(mode='character', length = length(mynames))
  names(link)<-mynames
  link['DA']<-'https://shiny.hres.ca/CVShiny'
  link['DISP']<-'https://shiny.hres.ca/shinydisp_test2'
  link['D']<-'https://shiny.hres.ca/OpenFDA_CV/prrD_test'
  link['E']<-'https://shiny.hres.ca/OpenFDA_CV/prrE_test'
  link['P']<-'https://shiny.hres.ca/OpenFDA_CV/dynprr_test'
  link['Z']<-'https://shiny.hres.ca/OpenFDA_CV/ChangePoint_test'
  link['LR']<-'https://shiny.hres.ca/OpenFDA_CV/LRTestD_test'
  link['LRE']<-'https://shiny.hres.ca/OpenFDA_CV/LRTestE_test'
  
  out<-vector()
  for (i in seq_along(mynames)){
    out[i] <- paste0('<a href=',link[i],' target=_blank >',labels[i],'</a><br>')
  }
  return(out)
}






