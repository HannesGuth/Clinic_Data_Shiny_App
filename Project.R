#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)
setwd("D:/Dokumente/Sonstiges/R/Shiny clinic data/Apps")
hc = fread("healthcare_dataset.csv")
hc$`Date of Admission` = as.Date(hc$`Date of Admission`, format = "%d.%m.%Y")
hc$`Discharge Date` = as.Date(hc$`Discharge Date`, format = "%d.%m.%Y")
Patient_Age = cut(hc$Age, 
              breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
              labels = c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"), 
              right = TRUE)
hc = add_column(hc, Patient_Age, .after = 2)

Bill = cut(hc$`Billing Amount`, 
              breaks = c(-10000000, 10000, 20000, 30000, 40000, 50000, 60000), 
              labels = c("1-10000", "10001-20000", "20001-30000", "30001-40000", "40001-50000", "50001-60000"), 
              right = TRUE)
hc = add_column(hc, Bill, .after = 10)

Room = cut(hc$`Room Number`, 
             breaks = c(0, 100, 200, 300, 400, 500, 600), 
             labels = c("1-100", "101-200", "201-300", "301-400", "401-500", "501-600"), 
             right = TRUE)
hc = add_column(hc, Room, .after = 13)

hc$`Admission Type` = sample(c(rep("Urgent",3), rep("Elective", 10), rep("Emergency", 1)), nrow(hc), replace = "TRUE")
hc$Medication = sample(c(rep("Paracetamol", 20), rep("Ibuprofen", 30), rep("Aspirin", 10), rep("Penicillin", 15), rep("Lipitor", 5)), nrow(hc), replace = "TRUE")

hc$stay = round(runif(nrow(hc)) * (hc$Age * as.numeric(hc$Bill)),0)

# hc <- data.frame(
#   Age = sample(18:80, 100, replace = TRUE),
#   Bill = rnorm(100, mean = 500, sd = 100),
#   stay = rnorm(100)
# )

# Define UI
ui <- fluidPage(
  titlePanel("Small Shiny App for synthetic clinic data"),
  sidebarLayout(
    mainPanel(
      textOutput("Introduction"),
      plotOutput("boxplot"),
      plotOutput("barplot")
    ),
    sidebarPanel(
      # Dropdown to choose the x-axis variable
      selectInput("xvar", "Choose X-axis Variable:", 
                  choices = c("Patient_Age", "Bill", "Room", "Medication")),
      sliderInput("age", "Filter by Age:", min = 0, max = max(hc$Age), step=1, value=c(0,max(hc$Age))),
      sliderInput("sta", "Filty length of stay:", min = 0, max = max(hc$stay), step=1, value=c(0,max(hc$stay))),
      sliderInput("bil", "Filter by Billing amount:", min = 0, max = max(hc$`Billing Amount`), step=1, value=c(0,max(hc$`Billing Amount`))),
      plotOutput("donutplot")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  data = reactive({
    hc = hc[Age >= min(input$age) & Age <= max(input$age) & stay >= min(input$sta) & stay <= max(input$sta) & `Billing Amount` >= min(input$bil) & `Billing Amount` <= max(input$bil), ]
    return(hc)
  })
  
  output$Introduction = renderText({ "This dashboard is based on a synthetic clinical data set and does not provide any deeper insights. It should mainly show various applications of interactive content and allow the user to 1. filter the underlying data and 2. change the fundamentals of the diagrams by editing their x-axis." })
  # Render boxplot with reactive x-axis
  output$boxplot <- renderPlot({
    hc = data()
    title = paste("Development of stay time with", input$xvar)
    ggplot(hc, aes_string(x = input$xvar, y = "stay")) +
      geom_boxplot() +
      labs(x = input$xvar, y = "Stay") +
      theme(plot.title = element_text(size=20),
            legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5)) +
      labs(title = title)
  })

  dat = reactive({
    hc = data()
    hc_summary = hc %>% 
      group_by(!!sym(input$xvar), `Admission Type`) %>% 
      summarise(count = n(), .groups = 'drop') %>%
      mutate(`Admission Type` = factor(`Admission Type`, levels = unique(hc$`Admission Type`)))
    #hc_summary = hc_summary[order(hc_summary$count),]
    return(hc_summary)
  })
  
  output$barplot = renderPlot({
    hc_summary <- dat() # Use reactive data
    title = paste("Distribution of Admission Type by", input$xvar)
    
    ggplot(hc_summary, aes(x = !!sym(input$xvar), y = count, fill = `Admission Type`)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = input$xvar, y = "Count", fill = "B values", title = title) +
      theme(plot.title = element_text(size=20),
            legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5)) +
      scale_fill_discrete(breaks=c('Emergency', 'Urgent', 'Elective'))
  })
  
  don = reactive({
    hc = data()
    medication_summary = hc[, sum(`Billing Amount`), by = eval(input$xvar)]  # Use input$xvar directly
    medication_summary = medication_summary[order(-V1)]  # Sort by size (descending)
    return(medication_summary)
  })
  
  output$donutplot = renderPlot({
    medication_summary = don()
    title = paste("Revenue by", input$xvar)
    ggplot(medication_summary, aes(x = 2, y = V1, fill = as.factor(get(input$xvar)))) +  # Use get() for dynamic fill
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(V1 / sum(V1) * 100, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      xlim(0.5, 2.5) +  # Create space for the donut hole
      theme(plot.title = element_text(size=20),
            legend.title = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5)) +
      labs(title = title)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



