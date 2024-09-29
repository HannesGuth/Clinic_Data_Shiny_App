library(data.table)
library(ggplot2)
library(dplyr)
library(tibble)

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
      plotOutput("boxplot"),
      plotOutput("barplot")
    ),
    sidebarPanel(
      sliderInput("age", "Age:", min = 0, max = max(hc$Age), step=1, value=c(0,max(hc$Age))),
      sliderInput("sta", "Stay:", min = 0, max = max(hc$stay), step=1, value=c(0,max(hc$stay))),
      sliderInput("bil", "Billing amount:", min = 0, max = max(hc$`Billing Amount`), step=1, value=c(0,max(hc$`Billing Amount`))),
      # Dropdown to choose the x-axis variable
      selectInput("xvar", "Choose X-axis Variable:", 
                  choices = c("Patient_Age", "Bill", "Room", "Medication")),
      plotOutput("donutplot")
    )
  )
)