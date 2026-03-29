library(shiny)
library(ggplot2)

# -------------------------------
# Helper functions for persistence
# -------------------------------

counts_file <- "counts.rds"
reset_password <- "456"

initialize_counts <- function() {
  list(
    question1 = c(Confirmed = 0, `Not confirmed` = 0),
    question2 = c(Confirmed = 0, `Not confirmed` = 0)
  )
}

read_counts <- function() {
  if (!file.exists(counts_file)) {
    counts <- initialize_counts()
    saveRDS(counts, counts_file)
    return(counts)
  }
  
  counts <- readRDS(counts_file)
  
  # Safety check
  if (!all(c("question1", "question2") %in% names(counts))) {
    counts <- initialize_counts()
    saveRDS(counts, counts_file)
  }
  
  counts
}

write_counts <- function(counts) {
  saveRDS(counts, counts_file)
}

# -------------------------------
# UI
# -------------------------------

ui <- fluidPage(
  titlePanel("Shared Response Count App"),
  
  fluidRow(
    column(
      width = 6,
      h3("Question block 1"),
      radioButtons(
        inputId = "q1",
        label = "Please select:",
        choices = c("Confirmed", "Not confirmed"),
        selected = character(0)
      ),
      actionButton("submit1", "Submit block 1"),
      br(), br(),
      plotOutput("plot1"),
      uiOutput("reset_ui1")
    ),
    column(
      width = 6,
      h3("Question block 2"),
      radioButtons(
        inputId = "q2",
        label = "Please select:",
        choices = c("Confirmed", "Not confirmed"),
        selected = character(0)
      ),
      actionButton("submit2", "Submit block 2"),
      br(), br(),
      plotOutput("plot2"),
      uiOutput("reset_ui2")
    )
  )
)

# -------------------------------
# Server
# -------------------------------

server <- function(input, output, session) {
  
  refresh_trigger <- reactiveVal(0)
  reset_ui_trigger1 <- reactiveVal(0)
  reset_ui_trigger2 <- reactiveVal(0)
  
  # Dynamically rebuild reset area for block 1
  output$reset_ui1 <- renderUI({
    reset_ui_trigger1()
    tagList(
      br(),
      passwordInput("reset_pw1", "Password for reset diagram 1:"),
      actionButton("reset1", "Reset diagram 1")
    )
  })
  
  # Dynamically rebuild reset area for block 2
  output$reset_ui2 <- renderUI({
    reset_ui_trigger2()
    tagList(
      br(),
      passwordInput("reset_pw2", "Password for reset diagram 2:"),
      actionButton("reset2", "Reset diagram 2")
    )
  })
  
  # Submit for block 1
  observeEvent(input$submit1, {
    if (is.null(input$q1) || input$q1 == "") {
      showNotification("Please select an answer for question block 1.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question1[input$q1] <- counts$question1[input$q1] + 1
    write_counts(counts)
    
    updateRadioButtons(session, "q1", selected = character(0))
    refresh_trigger(refresh_trigger() + 1)
    
    showNotification("Answer for question block 1 has been counted.", type = "message")
  })
  
  # Submit for block 2
  observeEvent(input$submit2, {
    if (is.null(input$q2) || input$q2 == "") {
      showNotification("Please select an answer for question block 2.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question2[input$q2] <- counts$question2[input$q2] + 1
    write_counts(counts)
    
    updateRadioButtons(session, "q2", selected = character(0))
    refresh_trigger(refresh_trigger() + 1)
    
    showNotification("Answer for question block 2 has been counted.", type = "message")
  })
  
  # Reset diagram 1 with password protection
  observeEvent(input$reset1, {
    if (is.null(input$reset_pw1) || input$reset_pw1 != reset_password) {
      showNotification("Incorrect password for diagram 1 reset.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question1 <- c(Confirmed = 0, `Not confirmed` = 0)
    write_counts(counts)
    
    refresh_trigger(refresh_trigger() + 1)
    reset_ui_trigger1(reset_ui_trigger1() + 1)  # rebuild password field to clear it
    
    showNotification("Diagram 1 has been reset.", type = "warning")
  })
  
  # Reset diagram 2 with password protection
  observeEvent(input$reset2, {
    if (is.null(input$reset_pw2) || input$reset_pw2 != reset_password) {
      showNotification("Incorrect password for diagram 2 reset.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question2 <- c(Confirmed = 0, `Not confirmed` = 0)
    write_counts(counts)
    
    refresh_trigger(refresh_trigger() + 1)
    reset_ui_trigger2(reset_ui_trigger2() + 1)  # rebuild password field to clear it
    
    showNotification("Diagram 2 has been reset.", type = "warning")
  })
  
  output$plot1 <- renderPlot({
    refresh_trigger()
    counts <- read_counts()
    
    df1 <- data.frame(
      Response = factor(
        names(counts$question1),
        levels = c("Confirmed", "Not confirmed")
      ),
      Count = as.numeric(counts$question1)
    )
    
    ggplot(df1, aes(x = Response, y = Count, fill = Response)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Confirmed" = "#2C7FB8", "Not confirmed" = "#D95F0E")) +
      labs(
        title = "Question block 1",
        x = NULL,
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$plot2 <- renderPlot({
    refresh_trigger()
    counts <- read_counts()
    
    df2 <- data.frame(
      Response = factor(
        names(counts$question2),
        levels = c("Confirmed", "Not confirmed")
      ),
      Count = as.numeric(counts$question2)
    )
    
    ggplot(df2, aes(x = Response, y = Count, fill = Response)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Confirmed" = "#2C7FB8", "Not confirmed" = "#D95F0E")) +
      labs(
        title = "Question block 2",
        x = NULL,
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)