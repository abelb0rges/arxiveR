library(shiny)
library(shinydashboard)
library(magrittr)
library(ggplot2)

s16 <- readRDS('data/stat2016_up2aug.rds')
s17 <- readRDS('data/stat2017.rds')
s <- as.data.frame(rbind(s17, s16))
special_categories <- s$primary_category %>%
  lapply(function(pc) {length(unlist(strsplit(pc, '[.]'))) < 2}) %>%
  unlist()
prim_cat <- s$primary_category[!special_categories] %>%
  strsplit('[.]') %>% unlist() %>% {.[seq(1, length(.), by = 2)]} %>%
  table() %>% as.data.frame() %>% {names(.) <- c('category', 'freq'); .}

my_theme <- theme(axis.title = element_text(size=15, family="URWGothic"),
                  axis.text = element_text(size=12, family="URWGothic"),
                  legend.text = element_text(size=10, family="URWGothic"),
                  legend.title = element_text(size=12, family="URWGothic"),
                  title = element_text(size=13, family="URWGothic"),
                  plot.title = element_text(size=18, face="bold",
                                            family="URWGothic"),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "white"))

server <- function(input, output) {
  output$cat_freq <- renderPlot(
    ggplot(prim_cat, aes(category, freq)) +
      geom_col(fill = 'Blue', alpha = .5) +
      labs(x = 'Category', y = 'Frequency') +
      theme(axis.text.x = element_text(angle=30)) +
      my_theme
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "arxiveR"),
  dashboardSidebar(
    # sliderInput("obs", "Number of observations:", 10, 500, 100, 1)
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("cat_freq", height = 400, width = 700))
    )
  )
)

shinyApp(ui = ui, server = server)
