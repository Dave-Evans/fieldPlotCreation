library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Field Parameters",
                            sliderInput("slider", "Number of observations:", 1, 100, 50),
                            numericInput("field_length", "Field length:", 10, min = 1),
                            numericInput("field_width", "Field width:", 10, min = 1),
                            
                            numericInput("field_setback_x", "Field setback X:", 10, min = 1),
                            numericInput("field_setback_y", "Field setback Y:", 10, min = 1),
                            
                            numericInput("left_subguard_width", "Left subguard width:", 10, min = 1),
                            numericInput("right_subguard_width", "Right subguard width:", 10, min = 1),
                            numericInput("bottom_subguard_length", "Bottom subguard width:", 10, min = 1),
                            numericInput("top_subguard_length", "Top subguard width:", 10, min = 1), 
                            
                            numericInput("left_guard_width", "Left guard width:", 10, min = 1),
                            numericInput("right_guard_width", "Right guard width:", 10, min = 1),
                            numericInput("bottom_guard_length", "Bottom guard width:", 10, min = 1),
                            numericInput("top_guard_length", "Top guard width:", 10, min = 1),
                            
                            numericInput("spray_alley_width", "Spray Alley width:", 10, min = 1),
                            
                            numericInput("row_width", "Row width:", 10, min = 1),
                            numericInput("row_per_subplot", "Rows per subplot:", 10, min = 1),
                            numericInput("subplot_width", "Subplot width:", 10, min = 1),
                            numericInput("subplot_length", "Subplot length:", 10, min = 1),
                            numericInput("subplot_n", "Number of subplots:", 10, min = 1),
                            numericInput("subplot_col_n", "Number of subplot columns:", 10, min = 1),
                            numericInput("subplot_row_n", "Number of subplot rows:", 10, min = 1),
                            
                            numericInput("plot_length", "Plot length:", 10, min = 1),
                            numericInput("plot_width", "Plot width:", 10, min = 1),
                            numericInput("plot_n", "Number of plots:", 10, min = 1),
                            numericInput("plot_col_n", "Number of plot columns:", 10, min = 1),
                            numericInput("plot_row_n", "Number of plot columns:", 10, min = 1),
                            numericInput("sub_field_break_col", "Number of subfield break columns:", 10, min = 1),
                            numericInput("sub_field_break_row", "Number of subfield break rows:", 10, min = 1)                        
                            
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "about",
                    h2("Field Plot Plotter"),
                    includeMarkdown("about.md")
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)