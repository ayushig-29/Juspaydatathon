library(shiny)
library(semantic.dashboard)
library(DT)
library(tidyverse)


trans <- read.csv("https://docs.google.com/spreadsheets/d/1bmo3ypctvDR2LtGC5BWqa94XJh57_gT10QvU4vqknKU/edit?usp=sharing")
Z <- mutate(trans,sr = success*100/t)
res1 <- summarise(group_by(Z,hr,mid,sr), t=sum(t), sr = mean(sr))
res1 <-mutate(res1, Date=str_sub(hr,1,10),Hour=strtoi(str_sub(hr,-2,-1)))
res2 <- summarise(group_by(Z,hr,t), t=sum(t))
res3 <- summarise(group_by(Z,hr,sr), t=mean(sr))


ui <- dashboardPage(
  dashboardHeader(color = "blue",title = "Datathon", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "graphs", "Dynamic Graphs and Tables"),
      menuItem(tabName = "tables", "Static Graohs and Tables")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "graphs",
        fluidRow(
          column(width = 10,h1(("Select Input"))),
          column(width = 10,selectInput(inputId="date",label=("Choose Date"),choices = c("February 12"="2020-02-12","February 13"="2020-02-13","February 14"="2020-02-14"),
                      selected = "February 12",multiple = F)),
          
          column(width = 10, selectInput(inputId="xaxis",label="Choose Y Variable",choices = c("Transaction"="t","success_rate" = "sr"),
                      selected = "t",multiple = F)),
          
          column(width = 10,sliderInput(inputId = "range1",
                      label = "Time Range",
                      min = 00,
                      max = 23,
                      value = c(00,23)),
                 column(width = 10, h1(textOutput("text1"))),
                 column(width = 10,(plotOutput(outputId = "graph", width = "100%"))),
          column(width = 10,h1(textOutput("text2"))),
        column(width = 10,DT::dataTableOutput("test")) 
               )
                  )
        ),
      tabItem(
        tabName = "tables",
        fluidRow(
        column(width = 8, h1(textOutput("text3"))),
        (plotOutput(outputId = "graph1", width = "100%")),
        (plotOutput(outputId = "graph2", width = "100%")),
        column(width = 8,h1(textOutput("text4"))),
          DT::dataTableOutput("dynamic"),
          DT::dataTableOutput("dynamic1")
        )
      )
    )
  ), theme = "cerulean"
)

server <- shinyServer(function(input, output, session) {
  output$text1 <- renderText({ 
    "Graph Representation" 
  })
  output$graph <- renderPlot({
    if(input$date=="2020-02-12"){
      x=filter(res1, Date=="2020-02-12")
    }else if(input$date=="2020-02-13"){
      x=filter(res1, Date=="2020-02-13")
    }else if(input$date=="2020-02-14"){
      x=filter(res1, Date=="2020-02-14")
    }
    x <- filter(x,Hour >= input$range1[1] & Hour <= input$range1[2])
    if(input$xaxis == "t"){
      ggplot(x)+ geom_point(aes(x=hr,y=t))+  theme_bw()+
        theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
              axis.text = element_text(size=12,color="BLACK",face="bold"))+
        labs(x="HR",y="Transaction",input$channel1,sep = " ")
    }else if(input$xaxis == "sr"){
      y <- select(x,hr,sr)
      ggplot(x)+ geom_point(aes(x=hr,y=sr))+  theme_bw()+
        theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
              axis.text = element_text(size=12,color="BLACK",face="bold"))+
        labs(x="HR",y="Success_Rate",input$channel1,sep = " ")
    }
  },height = 400, width =1050)

output$text2 <- renderText({ 
    ("Table Representation") 
  })
output$test <- DT::renderDT({
    if(input$date=="2020-02-12"){
      x=filter(res1, Date=="2020-02-12")
    }else if(input$date=="2020-02-13"){
      x=filter(res1, Date=="2020-02-13")
    }else if(input$date=="2020-02-14"){
      x=filter(res1, Date=="2020-02-14")
    }
  x <- filter(x,Hour >= input$range1[1] & Hour <= input$range1[2])
  if(input$xaxis == "t"){
    y <- select(x,hr,t)
  }else if(input$xaxis == "sr"){
    y <- select(x,hr,sr)}
  },height = 250)
output$text3 <- renderText({ 
  "Graph Representation" 
})
output$graph1 <- renderPlot({ggplot(res2)+ geom_point(aes(x=hr,y=t))+  theme_bw()+
    theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
          axis.text = element_text(size=12,color="BLACK",face="bold"))+
    labs(x="HR",y="Transaction",input$channel1,sep = " ")})
output$graph2 <- renderPlot({ggplot(res3)+ geom_point(aes(x=hr,y=sr))+  theme_bw()+
    theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
          axis.text = element_text(size=12,color="BLACK",face="bold"))+
    labs(x="HR",y="SUCCESS RATE",input$channel1,sep = " ")})

output$text4 <- renderText({ 
  "Table Representation" 
})
output$dynamic <- DT::renderDataTable(res2,options = list(pageLength = 15))
output$dynamic1 <- DT::renderDataTable(res3,options = list(pageLength = 15))

})

shinyApp(ui, server)