library(readr)
library(shiny)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(DT)
library(stringr)
library(lubridate)
library(plotly)
library(tidyr)
library(ggpubr)

# Data importing
students <- read_csv("nyuclasses.csv")

# Formatting the data
# students$sub_date <- parse_datetime(students$sub_date, "mdy HM")
# students$due_date <- parse_datetime(students$due_date, "mdy HM")

# Create a status indicator for each student
students$status <- ifelse(is.na(students$sub_date), "Uncompleted ",
                          ifelse(!is.na(students$sub_date) & students$sub_date > students$due_date, "Late Completion", "Completed"))

# Convert to factor
students$status <- as.factor(students$status)
students$assessment <- as.factor(students$assessment)

# Define server logic to plot
server <- function(input, output, session) {
  
  # Set up data table input form
  s <- select(students, Name = name, ID = id)
  output$students_table <- renderDT(s, server = FALSE, options = list(searching = FALSE))
  
  # Handling selections in the student table
  observeEvent(input$students_table_rows_selected, {
    isolate({
      reset("assignment")
      reset("student")
      reset("status")
    })
    st <- input$students_table_rows_selected
    # Turn row index into name
    row_to_name <- function(row, data) {
      pull(data[row, 1])
    }
    snames <- row_to_name(st, s)
    
    # # Prepare the data, filter for the names, order, select what we need, then filter
    pdata <- students %>%
      dplyr::filter(name %in% snames) %>%
      arrange(assessment) %>%
      select(name, assessment, score) %>%
      dplyr::filter(str_detect(assessment, "Assignment"))
    
    # # Show a point chart with that student's performance across the quizzes
    # output$main_Panel <- renderUI({
    #   column(11,
    #          plotOutput("student_plot")
    #   )
    # })
    
    # output$student_plot <- renderPlot({
    #   ggplot(pdata, aes(x = assessment, y = score)) +
    #     geom_point() +
    #     labs(x = "Assessment", y = "Score") +
    #     ggtitle("Student Performance") +
    #     theme_minimal()
    # })
    
  })
  
  # This will be opening plot with the most recent assignments
  output$mainPanel <- renderUI({
    div(
      fluidRow(
        column(11,
               style = "border: 1px dashed grey;",
               h3("Most recent assignments due"),
               plotOutput("assignment_plot")
        )
      )
    )
  })
  
  # output$assignment_plot <- renderPlot({
  #   # # Pull up the most recent assignments, then apply doughnut chart producing function
  #   # most_recent <- unique(students$assessment)[1:5]
  #   # ps <- lapply(most_recent, status_doughnut, input_data = students)
  #   # 
  #   # # Arrange in one big plot
  #   # # Future- look at individual cells
  #   # big_plot <- ggpubr::ggarrange(plotlist = ps, labels = most_recent, nrow = 1,
  #   #                               common.legend = TRUE, legend = "bottom",
  #   #                               font.label = list(size = 12, color = "blue"))
  #   # 
  #   # big_plot
  #   
  #   temp_students <-students %>%
  #     filter(str_detect(assessment, "^Assignment 1"))
  #   
  #   priorR <- temp_students %>% group_by(status) %>% 
  #     summarise(total_count=n(),
  #               .groups = 'drop')
  #   
  #   # 
  #   # Compute percentages
  #   priorR$fraction <- priorR$total_count / sum(priorR$total_count) 
  #   
  #   priorR$percentage <- 100 * priorR$fraction 
  #   
  #   # Compute the cumulative percentages (top of each rectangle)
  #   priorR$ymax <- cumsum(priorR$fraction)
  #   
  #   # Compute the bottom of each rectangle
  #   priorR$ymin <- c(0, head(priorR$ymax, n=-1))
  #   
  #   # Compute label position
  #   priorR$labelPosition <- (priorR$ymax + priorR$ymin) / 2
  #   
  #   # Compute a good label
  #   priorR$label <- paste0(priorR$status, "\n", priorR$percentage, "%")
  #   
  #   # Make the plot
  #   priorR %>% ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=status)) +
  #     geom_rect() +
  #     geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  #     scale_fill_brewer(palette=4) +
  #     coord_polar(theta="y") +
  #     xlim(c(2, 4)) +
  #     theme_void() +
  #     theme(legend.position = "none")
  # })
  
  
  output$assignment_plot <- renderPlot({
    assignment_plots <- list()  # Initialize a list to store the plots for each assignment
    
    for (i in 1:5) {
      assignment <- paste0("Assignment ", i)
      
      temp_students <- students %>%
        filter(str_detect(assessment, assignment))
      
      priorR <- temp_students %>% 
        group_by(status) %>% 
        summarise(total_count = n(), .groups = 'drop')
      
      # Compute percentages
      priorR$fraction <- priorR$total_count / sum(priorR$total_count)
      priorR$percentage <- 100 * priorR$fraction
      
      # Compute the cumulative percentages (top of each rectangle)
      priorR$ymax <- cumsum(priorR$fraction)
      
      # Compute the bottom of each rectangle
      priorR$ymin <- c(0, head(priorR$ymax, n = -1))
      
      # Compute label position
      priorR$labelPosition <- (priorR$ymax + priorR$ymin) / 2
      
      # Compute a good label
      priorR$label <- paste0(priorR$percentage, "%")
      
      # Make the plot and store it in the list
      assignment_plots[[i]] <- priorR %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = status)) +
        geom_rect() +
        geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
        scale_fill_brewer(palette = 4) +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    }
    
    # Arrange all plots in one big plot
    big_plot <- ggpubr::ggarrange(plotlist = assignment_plots, labels = paste0("Assignment ", 1:5),
                                  nrow = 1, common.legend = TRUE, legend = "bottom",
                                  font.label = list(size = 12, color = "blue"))
    
    big_plot
  })
  
  
  # Create student list
  output$studentList <- renderUI({
    div(
      h4("Students List"),
      selectInput("student", "Select Student:", choices = unique(students$name), multiple = TRUE)
    )
  })
  
}

# Run the application
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        DTOutput("students_table"),
        uiOutput("studentList")
      ),
      mainPanel(
        uiOutput("mainPanel")
      )
    )
  ),
  server = server
)
