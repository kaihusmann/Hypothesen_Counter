library(shiny)
library(ggplot2)

# -------------------------------
# Hilfsfunktionen für Speicherung
# -------------------------------

counts_file <- "counts.rds"
reset_password <- "456"

labels_de <- c(
  agree = "Stimme zu",
  disagree = "Stimme nicht zu"
)

initialize_counts <- function() {
  list(
    question1 = c(agree = 0, disagree = 0),
    question2 = c(agree = 0, disagree = 0)
  )
}

read_counts <- function() {
  if (!file.exists(counts_file)) {
    counts <- initialize_counts()
    saveRDS(counts, counts_file)
    return(counts)
  }
  
  counts <- readRDS(counts_file)
  
  valid_structure <- is.list(counts) &&
    all(c("question1", "question2") %in% names(counts)) &&
    all(c("agree", "disagree") %in% names(counts$question1)) &&
    all(c("agree", "disagree") %in% names(counts$question2))
  
  if (!valid_structure) {
    counts <- initialize_counts()
    saveRDS(counts, counts_file)
  }
  
  counts
}

write_counts <- function(counts) {
  saveRDS(counts, counts_file)
}

make_plot_data <- function(x) {
  data.frame(
    Antwort = factor(
      labels_de[c("agree", "disagree")],
      levels = labels_de[c("agree", "disagree")]
    ),
    Anzahl = as.numeric(x[c("agree", "disagree")]),
    stringsAsFactors = FALSE
  )
}

# -------------------------------
# UI
# -------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .reset-row {
        display: flex;
        align-items: center;
        gap: 4px;
        margin-top: 10px;
      }
      .reset-password-box {
        width: 120px;
        flex: 0 0 120px;
        margin-bottom: 0 !important;
      }
      .reset-password-box .form-group {
        margin-bottom: 0 !important;
      }
      .reset-password-box input {
        width: 120px !important;
      }
    "))
  ),
  
  titlePanel("Gemeinsame Antwortauswertung"),
  
  fluidRow(
    column(
      width = 6,
      h3("Zu Beginn der Veranstaltung"),
      radioButtons(
        inputId = "q1",
        label = "Bitte wählen Sie:",
        choices = c("Stimme zu" = "agree", "Stimme nicht zu" = "disagree"),
        selected = character(0)
      ),
      actionButton("submit1", "Antwort absenden"),
      br(), br(),
      plotOutput("plot1"),
      uiOutput("reset_ui1")
    ),
    column(
      width = 6,
      h3("Nach der Veranstaltung"),
      radioButtons(
        inputId = "q2",
        label = "Bitte wählen Sie:",
        choices = c("Stimme zu" = "agree", "Stimme nicht zu" = "disagree"),
        selected = character(0)
      ),
      actionButton("submit2", "Antwort absenden"),
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
  
  output$reset_ui1 <- renderUI({
    reset_ui_trigger1()
    div(
      class = "reset-row",
      div(
        class = "reset-password-box",
        passwordInput("reset_pw1", label = NULL, placeholder = "Passwort")
      ),
      actionButton("reset1", "Diagramm 1 zurücksetzen")
    )
  })
  
  output$reset_ui2 <- renderUI({
    reset_ui_trigger2()
    div(
      class = "reset-row",
      div(
        class = "reset-password-box",
        passwordInput("reset_pw2", label = NULL, placeholder = "Passwort")
      ),
      actionButton("reset2", "Diagramm 2 zurücksetzen")
    )
  })
  
  observeEvent(input$submit1, {
    if (is.null(input$q1) || input$q1 == "") {
      showNotification("Bitte wählen Sie eine Antwort für den ersten Block aus.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question1[input$q1] <- counts$question1[input$q1] + 1
    write_counts(counts)
    
    updateRadioButtons(session, "q1", selected = character(0))
    refresh_trigger(refresh_trigger() + 1)
    
    showNotification("Die Antwort für „Zu Beginn der Veranstaltung“ wurde gezählt.", type = "message")
  })
  
  observeEvent(input$submit2, {
    if (is.null(input$q2) || input$q2 == "") {
      showNotification("Bitte wählen Sie eine Antwort für den zweiten Block aus.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question2[input$q2] <- counts$question2[input$q2] + 1
    write_counts(counts)
    
    updateRadioButtons(session, "q2", selected = character(0))
    refresh_trigger(refresh_trigger() + 1)
    
    showNotification("Die Antwort für „Nach der Veranstaltung“ wurde gezählt.", type = "message")
  })
  
  observeEvent(input$reset1, {
    if (is.null(input$reset_pw1) || input$reset_pw1 != reset_password) {
      showNotification("Falsches Passwort für Diagramm 1.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question1 <- c(agree = 0, disagree = 0)
    write_counts(counts)
    
    refresh_trigger(refresh_trigger() + 1)
    reset_ui_trigger1(reset_ui_trigger1() + 1)
    
    showNotification("Diagramm 1 wurde zurückgesetzt.", type = "warning")
  })
  
  observeEvent(input$reset2, {
    if (is.null(input$reset_pw2) || input$reset_pw2 != reset_password) {
      showNotification("Falsches Passwort für Diagramm 2.", type = "error")
      return()
    }
    
    counts <- read_counts()
    counts$question2 <- c(agree = 0, disagree = 0)
    write_counts(counts)
    
    refresh_trigger(refresh_trigger() + 1)
    reset_ui_trigger2(reset_ui_trigger2() + 1)
    
    showNotification("Diagramm 2 wurde zurückgesetzt.", type = "warning")
  })
  
  output$plot1 <- renderPlot({
    refresh_trigger()
    counts <- read_counts()
    df1 <- make_plot_data(counts$question1)
    
    ggplot(df1, aes(x = Antwort, y = Anzahl, fill = Antwort)) +
      geom_col(width = 0.6) +
      scale_fill_manual(
        values = c("Stimme zu" = "#2C7FB8", "Stimme nicht zu" = "#D95F0E")
      ) +
      labs(
        title = "Zu Beginn der Veranstaltung",
        x = NULL,
        y = "Anzahl"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$plot2 <- renderPlot({
    refresh_trigger()
    counts <- read_counts()
    df2 <- make_plot_data(counts$question2)
    
    ggplot(df2, aes(x = Antwort, y = Anzahl, fill = Antwort)) +
      geom_col(width = 0.6) +
      scale_fill_manual(
        values = c("Stimme zu" = "#2C7FB8", "Stimme nicht zu" = "#D95F0E")
      ) +
      labs(
        title = "Nach der Veranstaltung",
        x = NULL,
        y = "Anzahl"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)