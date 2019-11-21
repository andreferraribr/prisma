#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(plotly)
library(broman)
library(ggpubr)


prisma <- read_excel("prisma.xlsx")
prisam<- na.omit(prisma)
colnames(prisma)<- c("ano_lei", "ano_despesa", "gnd", "exec","uo", "valor")

prisma$ano_lei<- as.numeric(prisma$ano_lei)
prisma$ano_despesa<- as.numeric(prisma$ano_despesa)

prisma<- na.omit(prisma)


# Define UI for application that draws a histogram
ui <- fluidPage(

    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(height =  50,
            
            # Input: Simple integer interval ----
            sliderInput("ano_loa", "ano do empenho:",
                        min = min(prisma$ano_lei), max = max(prisma$ano_lei),
                        value = c(min(prisma$ano_lei), max = max(prisma$ano_lei)),
                        step = 1),
            
            
            # Input: Decimal interval with step value ----
            sliderInput("ano_pgt", "ano do pagamento:",
                        min = min(prisma$ano_despesa), max = max(prisma$ano_despesa),
                        value = c(min(prisma$ano_despesa), max = max(prisma$ano_despesa)),
                        step = 1),
            
            # Input: Decimal interval with step value ----
            selectInput ("gnd", label = "grupo da despesa",
                         choices = unique(prisma$gnd),
                         selected = "INVESTIMENTOS" )
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ano_lei", height =  200),
           plotOutput("ano_pgt", height =  200)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   

    output$ano_lei <- renderPlot({
      prisma_lei<- prisma%>%
        filter(ano_lei >=  min(input$ano_loa) & ano_lei <=  max(input$ano_loa),
               ano_despesa >=  min(input$ano_pgt) & ano_despesa <=  max(input$ano_pgt),
               gnd %in% input$gnd)%>%
        group_by(ano_lei)%>%
        summarise(pago=sum(valor/1000000000))
      
      prisma_pago<- prisma%>%
        group_by(ano_despesa)%>%
        select(ano_despesa, valor)%>%
        summarise(pago=sum(valor/1000000000))
      
     
      
        ggplot(prisma_lei) +
        
          geom_bar(data=prisma_lei, aes(x = ano_lei, y = pago, fill="#000000" ), stat = "identity", colour="#000000", width = .9)+
          guides(fill=FALSE)+
          scale_fill_manual(values = "#E69F00")+
          theme_bw()+
          scale_x_continuous(limits=c(2007,2020), breaks  = c(2008, 2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019))+
          ggtitle("ano do empenho (R$ bi)")+
          theme(plot.title=element_text(family='', face='bold', colour='#cc6600', size=11))+
          theme(axis.title.x = element_blank())+
          theme(axis.title.y = element_blank())+
          theme(axis.text.x = element_text(size = 8)) +
          geom_text(aes(x=ano_lei, y=pago, label = round(pago)), hjust = 1, vjust = 0, nudge_x = 0.1, nudge_y = 1)+ theme(
            # Remove panel border
            panel.border = element_blank(),  
            # Remove panel grid lines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey")
          )
      
      
      
      
      
      
      
      
      
      
      
    })
    
    output$ano_pgt <- renderPlot({
      
      
      prisma_pago<- prisma%>%
        filter(ano_lei >=  min(input$ano_loa) & ano_lei <=  max(input$ano_loa),
               ano_despesa >=  min(input$ano_pgt) & ano_despesa <=  max(input$ano_pgt),
               gnd %in% input$gnd)%>%
        group_by(ano_despesa)%>%
        select(ano_despesa, valor)%>%
        summarise(pago=sum(valor/1000000000))
      

      
      ano<-plot_ly(prisma_pago)%>%
        ggplot() +
        geom_bar(data=prisma_pago, aes(x = ano_despesa, y = pago, fill="#ff0000" ), stat = "identity",  colour="red", width = .9)+
        guides(fill=FALSE)+
        theme_bw()+
        scale_x_continuous(limits=c(2007,2020), breaks  = c(2008, 2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019))+
        ggtitle("ano do pagamento (R$ bi)")+
        theme(plot.title=element_text(family='', face='bold', colour='#ff0000', size=11))+
        theme(axis.title.x = element_blank())+
        theme(axis.title.y = element_blank())+
        theme(axis.text.x = element_text(size = 8)) + 
        geom_text(aes(x=ano_despesa, y=pago, label = round(pago)), hjust = 1, vjust = 0, nudge_x = 0.1, nudge_y = 1)+ theme(
          # Remove panel border
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey")
        )
      
     
      
      ano
      
      
      
      
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
