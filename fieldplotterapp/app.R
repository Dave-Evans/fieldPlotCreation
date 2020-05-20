library(shinydashboard)
library(tidyverse)
source("iter_plot_map_engine.R")

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Parameter file", tabName = "parameter_file", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    
                    
                    
                    fluidRow(
                        # downloadButton("downloadData", "Download"),
                        box(plotOutput('plot1')),    
                        box(
                            title = "Parameters",
                            numericInput("row_width", "Row width:", 3, min = 1),
                            numericInput("row_per_subplot", "Rows per subplot:", 4, min = 1),
                            numericInput("subplot_width", "Subplot width:", 12, min = 1),
                            numericInput("subplot_length", "Subplot length:", 25, min = 1),
                            numericInput("subplot_n", "Number of subplots:", 6, min = 1),
                            numericInput("subplot_col_n", "Number of subplot columns:", 3, min = 1),
                            numericInput("subplot_row_n", "Number of subplot rows:", 2, min = 1),
                            
                            numericInput("plot_length", "Plot length:", 68, min = 1),
                            numericInput("plot_width", "Plot width:", 48, min = 1),
                            numericInput("plot_n", "Number of plots:", 21, min = 1),
                            numericInput("plot_col_n", "Number of plot columns:", 3, min = 1),
                            numericInput("plot_row_n", "Number of plot rows:", 7, min = 1),
                            numericInput("sub_field_break_col", "Number of subfield break columns:", 0, min = 0),
                            numericInput("sub_field_break_row", "Number of subfield break rows:", 0, min = 0), 
                            numericInput("field_length", "Field length:", 560, min = 1),
                            numericInput("field_width", "Field width:", 215, min = 1),
                            
                            numericInput("field_setback_x", "Field setback X:", 0, min = 0),
                            numericInput("field_setback_y", "Field setback Y:", 10, min = 0),
                            
                            numericInput("left_subguard_width", "Left subguard width:", 3, min = 0),
                            numericInput("right_subguard_width", "Right subguard width:", 0, min = 0),
                            numericInput("bottom_subguard_length", "Bottom subguard width:", 4, min = 0),
                            numericInput("top_subguard_length", "Top subguard width:", 4, min = 0), 
                            
                            numericInput("left_guard_width", "Left guard width:", 0, min = 0),
                            numericInput("right_guard_width", "Right guard width:", 3, min = 0),
                            numericInput("bottom_guard", "Bottom guard width:", 1, min = 0),
                            numericInput("top_guard", "Top guard width:", 1, min = 0),
                            
                            numericInput("spray_alley_width", "Spray Alley width:", 12, min = 1)
                            
                            
                            
                        )
                    )
                    
            ),
            
            # Second tab content
            tabItem(tabName = "about",
                    h2("Field Plot Plotter"),
                    includeMarkdown("about.md")
            ),
            # Third tab content
            tabItem(tabName = "parameter_file",
                    h2("Download the parameter file"),
                    downloadButton("downloadData", "Download"),
                    box(tableOutput("raw_params"))
            )            
            
        )
    )
)

server <- function(input, output) {
    
    names_params = c("field_length", 'field_width', 'field_setback_x', 'field_setback_y',
                     'left_subguard_width', 'right_subguard_width', 'bottom_subguard_length',
                     'top_subguard_length', 'top_guard', 'bottom_guard', 'left_guard_width',
                     'right_guard_width', 'spray_alley_width', 'row_width', 'row_per_subplot',
                     'subplot_width', 'subplot_length', 'subplot_n', 'subplot_col_n',
                     'subplot_row_n', 'plot_length', 'plot_width', 'plot_n', 'plot_col_n',
                     'plot_row_n', 'sub_field_break_col', 'sub_field_break_row')

    param_dat = reactive({
        test_params = data.frame()
        for (nm in names_params){
            test_params = rbind(test_params, c(nm, input[[nm]]))
        }
        colnames(test_params) = c('variable', 'value')
        
        # params <- read_params(test_params)
        test_params
    })
    
    output$raw_params <- renderTable({
        
        param_dat()
        
    })
    
    output$plot1 <- renderPlot({
       
        
        params <- read_params(param_dat())
        # params <- params %>% mutate(plot_length = (bottom_subguard_length + top_subguard_length + subplot_length)*subplot_row_n + top_guard + bottom_guard)
        
        
        field <- init_field_map(params$field_width, params$field_length)
        # ggplot() + geom_tile(data=field, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = layer_id),alpha =0.5)
        # next, the main plot boundaries
        plot_area <- init_plot_map(x_length = params$plot_width, 
                                   y_length = params$plot_length,
                                   plot_n = params$plot_n,
                                   plot_col_n = params$plot_col_n,
                                   plot_row_n = params$plot_row_n
                               ) %>% mutate(
                                   x_mid = x_mid+params$field_setback_x,
                                   y_mid = y_mid+params$field_setback_y
                                   )
        
        field_boundary <- bind_rows(field,plot_area)
        
        one_subplot_area <- make_one_subplot_area(params)
        many_subplot_area <-make_many_subplot_area(plot_area, one_subplot_area)
        
        plot_origins <- get_plot_origins(plot_area)
        
        many_subplots_area_origins <- get_many_subplots_area_origins(many_subplot_area, plot_origins, plot_area, one_subplot_area)
        subplot_area<-spread_from_origin_subplot_area(many_subplots_area_origins)
        
        # okay show subplot boundaries 
        field_boundary <- bind_rows(field,plot_area,subplot_area) %>%
            mutate(id = as.character(id), id = str_pad(id, 4, pad = "0")) %>%
            mutate(fill = paste(layer_id,id,sep="_")) %>%
            mutate(x_mid_temp =  x_mid + params$spray_alley_width*plot_col) %>% # add spray alley
            mutate(x_mid = ifelse(layer_id == "study",x_mid,x_mid_temp)) %>%
            select(-x_mid_temp)
    
        # For displaying different componenets
        field_boundary %>% mutate(
            fill_plots = case_when(
                layer_id == 'study' ~ 'study',
                layer_id == 'plot' ~ fill,
                layer_id == 'subplot' ~ 'subplot',
            )
        ) -> field_boundary
        # debugging subplots, plots and field 
        field_plot_subplots <-  ggplot() +
            geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill_plots)) 
        field_plot_subplots
        
        
        
    })
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("test_data.csv", sep = "")
        },
        content = function(file) {
            write.csv(param_dat(), file, row.names = FALSE)
        }
    )

}

shinyApp(ui, server)