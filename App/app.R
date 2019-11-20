#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

prisma <- read_excel("prisma.xlsx")
colnames(prisma)<- c("ano_lei", "ano_despesa", "gnd", "exec","uo", "valor")

prisma$ano_lei<- as.numeric(prisma$ano_lei)
prisma$ano_despesa<- as.numeric(prisma$ano_despesa)

prisma<- na.omit(prisma)
ano<- unique(prisma$ano_lei)
valor<- c(rep(0.0001,length(ano)))
ano_plot<- data.frame(ano,valor)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Input: Simple integer interval ----
            sliderInput("ano_loa", "ano da LOA:",
                        min = min(prisma$ano_lei), max = max(prisma$ano_lei),
                        value = c(min(prisma$ano_lei), max = max(prisma$ano_lei)),
                        step = 1),
            
            # Input: Decimal interval with step value ----
            sliderInput("ano_pgt", "ano do pagamento:",
                        min = min(prisma$ano_despesa), max = max(prisma$ano_despesa),
                        value = c(min(prisma$ano_despesa), max = max(prisma$ano_despesa)),
                        step = 1)
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ano_lei"),
           plotOutput("ano_pgt")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  


    output$ano_lei <- renderPlot({
      prisma_lei<- prisma%>%
        filter(ano_lei %in% input$ano_loa,ano_despesa %in% input$ano_pgt )%>%
        group_by(ano_lei)%>%
        summarise(pago=sum(valor/1000000000))
      

      
      yl <-ifelse (max(prisma_pago$pago)>max(prisma_lei$pago), max(prisma_pago$pago),max(prisma_lei$pago))
      
      
        ggplot(prisma_lei) +
        
        geom_bar(data=prisma_lei, aes(x = ano_lei, y = pago, fill="#000000" ), stat = "identity", colour="#000000")+
        geom_bar(data=ano_plot, aes(x = ano, y= valor ), stat = "identity")+
        ylim(0,yl)+
        guides(fill=FALSE)+
        scale_fill_manual(values = "#E69F00")+
        theme_bw()+
        ggtitle("Ano do empenho (R$ bi)")
      
      
      
      
      
      
      
      
      
      
      
    })
    
    output$ano_pgt <- renderPlot({
      
      
      prisma_pago<- prisma%>%
        filter(ano_lei %in% input$ano_loa,ano_despesa %in% input$ano_pgt )%>%
        group_by(ano_despesa)%>%
        select(ano_despesa, valor)%>%
        summarise(pago=sum(valor/1000000000))
      
      yl <-ifelse (max(prisma_pago$pago)>max(prisma_lei$pago), max(prisma_pago$pago),max(prisma_lei$pago))
      
      ano<-plot_ly(prisma_pago)%>%
        ggplot() +
        geom_bar(data=prisma_pago, aes(x = ano_despesa, y = pago, fill="#ff0000" ), stat = "identity",  colour="red")+
        ylim(0,yl)+
        guides(fill=FALSE)+
        theme_bw()+
        ggtitle("Ano do pagamento (R$ bi)")
      
     
      
      ano
      
      
      
      
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
