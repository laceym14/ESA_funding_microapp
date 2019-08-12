
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
library(shiny)
library(stringr)
library(viridis)

# dat <- readRDS("/Users/jacobmalcom/Work/Repos/analyses/signatory_listing/current_signatories.rds")
dat <- readRDS("/home/jacobmalcom/cronjobs/border_signatories/current_signatories.rds")

ui <- fluidPage(
  column(2),
  column(8,
    span(style="text-align:right;",
         helpText("(Refresh page if figures grey out)")),
    h2(paste0("Scientists and Supporters (", length(dat$category), " Total)")),
    plotlyOutput("cat_plt"),
    h2("Signatories of Countries (Top 10)"),
    plotlyOutput("country_plt"),
    h2("Signatories by State (Top 10)"),
    plotlyOutput("state_plt"),
    h2("Signatories by Degree"),
    plotlyOutput("deg_plt") #,
  ),
  column(2)
)


server <- shinyServer(function(input, output) {
  cat <- as.data.frame(table(dat$category))
  output$cat_plt <- renderPlotly({
    plot_ly(data = cat,  x = ~Var1,  y = ~Freq, type = "bar",
            marker = list(color = "#7A3777")) %>%
      layout(xaxis = list(title = "", size = 18),
             yaxis = list(title = "# Signatories", size = 18))
             
  })
  
  cnt <- table(dat$country) %>%
    sort(decreasing = TRUE) %>%
    head(10) %>%
    as.data.frame()
  output$country_plt <- renderPlotly({
    plot_ly(data = cnt, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#C41230")) %>%
      layout(xaxis = list(title = "", size = 18),
             yaxis = list(title = "# Signatories", size = 18))
  })
  
  ste <- table(dat$state) %>%
    sort(decreasing = TRUE) %>%
    head(10) %>%
    as.data.frame()
  output$state_plt <- renderPlotly({
    plot_ly(data = ste, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#E36F1E")) %>%
      layout(xaxis = list(title = "", size = 18),
             yaxis = list(title = "# Signatories", size = 18))
  })
  
  ph_pat <- "PhD|DVM|DPhil|Post-doc|Postdoc|Ph D|MD|Dr|Doctor|Doctorado"
  ms_pat <- "Master|MS|MA|MBA|MPH" 
  dat$degree_short <- lapply(dat$degree, function(x){
    x <- str_replace_all(x, "\\.", "")
    phd <- length(grep(x, pattern = ph_pat, ignore.case = TRUE)) > 0
    mas <- length(grep(x, pattern = ms_pat, ignore.case = TRUE)) > 0
    if_else(phd, "Doctorate", if_else(
      mas, "Master's", "Other"))
  }) %>% unlist()
  
  deg <- table(dat$degree_short) %>%
    sort(decreasing = TRUE) %>%
    head(10) %>%
    as.data.frame()
  output$deg_plt <- renderPlotly({
    plot_ly(data = deg, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#005596")) %>%
      layout(xaxis = list(title = "", size = 18),
             yaxis = list(title = "# Signatories", size = 18))
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)

