#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(ggplot2)
library(scales)
library(doBy)

populationdata<-data.frame(ID=1:100000,Party=sample(c("Independent Alliance Party","Universal Prosperity Party","Other"),size = 100000,replace=TRUE,prob = c(0.52,0.48,0.05)),
                           Gender=sample(c("Male","Female"),size = 100000,replace=TRUE,prob = c(0.88,0.88)),
                           Age=round(runif(100000,18,80)))
populationdata$Party<-factor(populationdata$Party,levels=c("Other","Independent Alliance Party","Universal Prosperity Party"))
pdat<-populationdata
pdat$Gender<-100*as.numeric(pdat$Gender=="Female")   
pdat$P1<-100*as.numeric(pdat$Party=="Independent Alliance Party")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title

    sidebarLayout(
 
 
  sidebarPanel(style = "position:fixed;width:inherit",
    
               numericInput(inputId = "Add10",
                label="Sample Size",min = 0,max=10000,value = 100,step=10),
  numericInput(inputId = "conf",
              label="Confidence Level",min = 0.5,max=1,value = 0.95),
      actionButton(inputId = "refresh",
                "New Survey"),
    actionButton(inputId = "reset",
                 "Reset"),width = 2),
mainPanel(
  titlePanel("Confidence Intervals"),
  tabsetPanel(
   
    tabPanel("Summary Statistics",tableOutput("stats1"),plotlyOutput("plot1")),
    
    tabPanel("Repeatability",actionButton(inputId = "nrep",
                                          label="Do 100 Surveys"),plotlyOutput("plot3"),
             checkboxInput("meanline","Add 'Real' Value",value=FALSE), checkboxInput("ciline","Add Confidence Interval",value=FALSE), 
             checkboxInput("fix","Fix Axis",value=FALSE)),
    tabPanel("Confidence Intervals",tableOutput("stats2"),plotlyOutput("plot2")))

  
    )
  )
)


  yaxis<-list(axis.automargin=TRUE)
  

# Define server logic required to draw a histogram
server <- function(input, output) {

  counter <- reactiveValues(n = 0)
  sampledata<-eventReactive(input$refresh|counter$n==0,{


    data<-populationdata[sample(1:100000,as.numeric(as.character(input$Add10))),]
    
  data})


  observeEvent(input$refresh, {counter$n <- counter$n + 1})
  observeEvent(input$reset, {  set.seed(13560)
    counter$n <- 0})
  observeEvent(input$nrep, {counter$n <- 100})
  

  output$plot1<-renderPlotly({
    
    p1<-ggplot(data=sampledata(),aes(y = (..count..)/sum(..count..),x=Party,fill=Party))+geom_bar(position="dodge",col="black",show.legend=FALSE)+
        xlab("")+ylab("% of Respondents")+ggtitle("Voting Intentions",subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0,1,by=0.2))+coord_flip()+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
             legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14),legend.position='none'  )
   yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
   new_plot<-layout(ggplotly(p1),yaxis=yaxis1)
   new_plot
  })  
 
  
  output$stats1<-renderTable({

    
        df1<- data.frame(data.frame(table(sampledata()$Party)), Percent=data.frame(prop.table(table(sampledata()$Party)))[,2])
      colnames(df1)<-c("Party","Frequency","Percentage")
      df1$Percentage<-paste(round(100*df1$Percentage),"%",sep="")
      
    df1[c(3,2,1),]
  })  

  output$stats2<-renderTable({
    tmp<-sampledata()
    
   
    tmp$P1<-as.numeric(tmp$Party=="Independent Alliance Party")   
    tmp$P2<-as.numeric(tmp$Party=="Universal Prosperity Party") 
    tmp$P3<-as.numeric(tmp$Party=="Other") 
    out1<-data.frame(Party=c("Independent Alliance Party","Universal Prosperity Party","Other"),Sample=rep(input$Add10,3),
                     Estimate=c(mean(tmp$P1),mean(tmp$P2),mean(tmp$P3)),
                     SD=c( sqrt(mean(tmp$P1)*(1-mean(tmp$P1))),
                          sqrt(mean(tmp$P2)*(1-mean(tmp$P2))),
                          sqrt(mean(tmp$P3)*(1-mean(tmp$P3)))))
    
    out1$SE=out1$SD/sqrt(as.numeric(as.character(input$Add10)))
   out1$LCI=out1$Estimate-qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*out1$SE
  out1$UCI=out1$Estimate+qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*out1$SE
  out1$UCI[out1$UCI>1]<-1
  out1$LCI[out1$LCI<0]<-0
  
  out1$Estimate<-scales::percent(out1$Estimate)
  out1$UCI<-scales::percent(out1$UCI)
  out1$LCI<-scales::percent(out1$LCI)
  out1$SE<-scales::percent(out1$SE)
  
  colnames(out1)[4:7]<-c("Standard Deviation","Standard Error","95% Confidence Interval (Lower)","95% Confidence Interval (Upper)")
out1[,c(1,3,5:7)]
  })
  
  
  
  output$plot2<-renderPlotly({
    tmp<-sampledata()
    
    
    tmp$P1<-as.numeric(tmp$Party=="Independent Alliance Party")   
    tmp$P2<-as.numeric(tmp$Party=="Universal Prosperity Party") 
    tmp$P3<-as.numeric(tmp$Party=="Other") 
    out1<-data.frame(Party=factor(c("Independent Alliance Party","Universal Prosperity Party","Other"),levels=c("Other","Independent Alliance Party","Universal Prosperity Party")),
                     Sample=rep(input$Add10,3),
                     Estimate=c(mean(tmp$P1),mean(tmp$P2),mean(tmp$P3)),
                     SD=c( sqrt(mean(tmp$P1)*(1-mean(tmp$P1))),
                           sqrt(mean(tmp$P2)*(1-mean(tmp$P2))),
                           sqrt(mean(tmp$P3)*(1-mean(tmp$P3)))))
    
    out1$SE=out1$SD/sqrt(as.numeric(as.character(input$Add10)))
    out1$LCI=out1$Estimate-qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*out1$SE
    out1$UCI=out1$Estimate+qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*out1$SE
    out1$UCI[out1$UCI>1]<-1
    out1$LCI[out1$LCI<0]<-0
    
p1<-ggplot(data=out1,aes(y=Estimate,ymax=UCI,ymin=LCI,x=Party,col=Party))+geom_point()+geom_errorbar(width=0.1)+coord_flip()+
  xlab("")+ylab("% of Respondents")+ggtitle("Voting Intentions",subtitle = paste("N=",nrow(sampledata())))+
  scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0,1,by=0.2))+coord_flip()+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
                                                                                             legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14),legend.position='none'  )
yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
new_plot<-layout(ggplotly(p1),yaxis=yaxis1)
new_plot
    
  })
  
  
  
  
  
  
  output$plot3<-renderPlotly({
     
 
  reps<-data.frame(Survey=1:counter$n,Estimate=NA,sd=NA)
  set.seed(13560)
for(i in 1:counter$n){
  tmp<-pdat[sample(1:100000,as.numeric(as.character(input$Add10))),"P1"]
  reps$Estimate[i]<-mean(tmp)
  reps$sd[i]<-100*sqrt(mean(tmp/100)*(1-mean(tmp/100)))
}
  reps$upper<-reps$Estimate+qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
  reps$lower<-reps$Estimate-qt(as.numeric(as.character(input$conf))+0.5*(1-as.numeric(as.character(input$conf))),as.numeric(as.character(input$Add10))-1)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
  reps$upper[reps$upper>100]<-100
  reps$lower[reps$lower<0]<-0
  
  p1<-ggplot(data=reps,aes(y=Estimate,x=Survey))+ggtitle("% Voting for Independent Alliance Party")
  if(input$meanline==TRUE){
  p1<-p1+geom_hline(aes(yintercept=mean(Estimate)),col="blue")
  }
if(input$ciline==TRUE){
  
  
  p1<-p1+
    geom_errorbar(aes(ymax=upper,
                      ymin=lower),col="red",linetype=2)
}
if(input$fix==TRUE|input$fix==FALSE){
  p1<-p1+ylim(0,100)
}

    p2<-p1+geom_point(size=2,col="forestgreen")+
      theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )+
      ylab(input$Variable)+xlab("Survey Number")
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  
  })
}



  
  
 
  
 


# Run the application 
shinyApp(ui = ui, server = server)


