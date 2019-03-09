# Thu Feb 28 01:10:53 2019 ------------------------------
# UI interface for the shiny app

# Source components -------------------------------------------------------
source("components/libs.R")
source("components/data_loader.R")


ui <- argonDash::argonDashPage(
  title = "Porfact",
  description = "Personal Trading Portfolio Monitoring Dashboard",
  author = "Shawn Lin",
  sidebar = argonDash::argonDashSidebar(
    vertical = TRUE,
    skin = 'light',
    background = "white",
    size = "md",
    side = "left",
    id = "sidebar",
    brand_url = "http://www.xiaolianglin.com",
    brand_logo = "logo.png",
    argonDash::argonSidebarMenu(
      #Defining the tabs
      argonDash::argonSidebarItem(
        tabName = 'overview',
        icon = 'bullet-list-67',
        icon_color = '#4298b5',
        'Overview'
      ),
      argonDash::argonSidebarItem(
        tabName = 'about',
        icon = 'single-02',
        icon_color = '#4298b5',
        'About'
      ),
      
      br(),
      
      selectizeInput(
        inputId = "account",
        label = "Account",
        choices = unique(nav$account),
        selected = account_use,
        multiple = TRUE
      )
    )
  ),
  
  header = argonDash::argonDashHeader(
    gradient = FALSE,
    color = 'info',
    h4('Porfact Portfolio Monitor', style = 'color:white;text-align:center;font-size:2em;')
  ),
  
  body = argonDash::argonDashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    argonDash::argonTabItems(
      argonDash::argonTabItem(
        tabName = "overview",
        argonR::argonRow(
          argonR::argonCard(
            title = "Cumulative Returns",
            icon = "chart-bar-32",
            width = 12,
            highcharter::highchartOutput("vami_chart")
          )
        ),
        
        argonR::argonRow(
          argonR::argonCard(
            width = 6,
            title = "Summary",
            icon = "collection",
            uiOutput("performance_summary")
          ),
          argonR::argonCard(
            width = 6,
            title = "Allocation",
            icon = "chart-pie-35",
            uiOutput("allocation")
          )
        ),
        
        
        argonR::argonRow(
          argonR::argonCard(
            title = "Calendar Returns",
            icon = "calendar-grid-58",
            width = 12,
            uiOutput("calendar_returns")
          )
        ),
        
        argonR::argonRow(
          argonR::argonCard(
            title = "Stock Explorer",
            icon = "zoom-split-in",
            width = 12,
            argonR::argonRow(
              argonR::argonColumn(
                width = 6,
                selectizeInput(
                  inputId = "ticker",
                  label = "Ticker",
                  choices = unique(prices$ticker),
                  selected = unique(prices$ticker[1:3]),
                  multiple = TRUE
                )
              ),
              
              argonR::argonColumn(
                width = 6,
                dateInput(
                  inputId = "start_date",
                  label = "Start Date",
                  min = min(prices$tradeday),
                  max = max(prices$tradeday),
                  value = "2018-01-01",
                )
              )
            ),
            
            argonR::argonRow(argonR::argonColumn(width = 12,
                                                 htmlOutput("hcplot")))
            
          )
        )
        
      ),
      argonDash::argonTabItem(
        tabName = "about",
        argonR::argonUser(
          title = 'Shawn Lin',
          url = 'http://xiaolianglin.com',
          src = 'user.jpg',
          style = "text-align:left",
          p("This project uses Shiny to monitor my personal stock portfolio. When I built this project, I wanted to demonstrate the power of R shiny, while not making it over complicated. Therefore, I chose to fix some dataset and limit the amount of customization. The full version should retrieve data from database and various online apis."),
          br(),
          p("Libraries used:"),
          div(tags$ul(
            tags$li("Highcharter: For static analysis, I love to use ggplot2. However, for financial time series, I find highchart to be both professional and easy to use. It adds a lot of interactivity to the plot and allows you the analyze your data in a flexible way."),
            tags$li("PerformanceAnalytics: I didn't write any of the financial metrics calculation myself. PerformanceAnalytics covers many metrics that are more than sufficient for this Shiny app."),
            tags$li("Argon: Beautiful and ready to use bootstrap 4 theme. I see this as a replacement for shiny builtin ui/shinydashboard which are a little bit dated"),
            tags$li("memoise: When running functions with repeated parameters, memoise allows you to cache and therefore speed up the calculation."),
            tags$li("tidyverse: Do I need to mention this? Simply the best data manipulation colleccion.")
          ))
          
        )
      )
      
    )
  )
)