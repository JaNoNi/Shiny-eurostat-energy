
ui <- dashboardPage(
    title = "Eurostat Energy",
    
    # Header -------------------------------------------------------------------
    dashboardHeader(
        title = "Energy",
        titleWidth = 200
    ),
    
    # Sidebar ------------------------------------------------------------------
    
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem(text = "Overview", 
                     tabName = "tab_overview", 
                     icon = icon("chart-line")),
            menuItem(text = "Flow", 
                     tabName = "tab_flowdiagram", 
                     icon = icon("burn"))
        )  
    ),
    
    # Body ---------------------------------------------------------------------
    
    dashboardBody(
        tabItems(
            
            # Overview Tab -----------------------------------------------------
            
            tabItem(
                tabName = "tab_overview",
                # Zero row (select country)
                fluidRow(
                    box(
                        width = 12,
                        selectInput(inputId  = "countryvar",
                                    label    = "Select Country",
                                    choices  = eu_country_label$name)
                        )
                    ),
                
                # First row
                fluidRow(
                    box(
                        width = 4,
                        title = "Share of energy from renewable sources",
                        plotlyOutput("plottab11")
                    ),
                    box(
                        width = 4,
                        title = "Energy imports dependency",
                        plotlyOutput("plottab12")
                    ),
                    box(
                        width = 4,
                        title = "Final energy consumption by product",
                        plotlyOutput("plottab13")
                    )
                ),
                
                # Second row
                fluidRow(
                    box(
                        width = 4,
                        title = "Final energy consumption by sector",
                        plotlyOutput("plottab21")
                    ),
                    box(
                        width = 4,
                        title = "Final energy consumption in households by fuel type"
                    ),
                    box(
                        width = 4,
                        title = "Final energy consumption in industry by fuel type"
                    )
                ),
                
                #Third row
                fluidRow(
                    box(
                        width = 4,
                        title = "Final energy consumption in transport by fuel type"
                    ),
                    box(
                        width = 4,
                        title = "Final energy consumption in services by fuel type"
                    ),
                    box(
                        width = 4,
                        title = "Population unable to keep home adequately warm",
                        plotlyOutput("plottab33")
                    )
                    
                )
            ),
            
            # Flow Tab ---------------------------------------------------------
            
            tabItem(
                tabName = "tab_flowdiagram",
                # Zero row (select country/year/fuel)
                fluidRow(
                    box(
                        width = 4,
                        selectInput(inputId  = "flow_country",
                                    label    = "Select Country",
                                    choices  = c("dummy1", "dummy2"))
                    ),
                    box(
                        width = 4,
                        selectInput(inputId  = "flow_year",
                                    label    = "Select Year",
                                    choices  = c("dummy1", "dummy2"))
                    ),
                    box(
                        width = 4,
                        selectInput(inputId  = "flow_fuel",
                                    label    = "Select Fuel Type",
                                    choices  = c("dummy1", "dummy2"))
                    )
                ),
                
                # First row
                # sankey
            )
        )
    )
)