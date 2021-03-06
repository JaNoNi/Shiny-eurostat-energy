
ui <- dashboardPage(
    title = "Eurostat Energy",
    
    # Header -------------------------------------------------------------------
    dashboardHeader(
        title = "Energy",
        titleWidth = 200,
        dropdownMenuOutput("messageMenu")
    ),
    
    # Sidebar ------------------------------------------------------------------
    
    dashboardSidebar(
        width = 200,
        shinyjs::useShinyjs(),
        sidebarMenu(
            menuItem(text = "Overview", 
                     tabName = "tab_overview", 
                     icon = icon("home")),
            menuItem(text = "Time",
                     tabName = "tab_timediagram",
                     icon = icon("chart-line")),
            menuItem(text = "Flow", 
                     tabName = "tab_flowdiagram", 
                     icon = icon("burn")),
            menuItem(text = "Settings",
                     tabName = "tab_settings",
                     icon = icon("sliders-h"))
        )  
    ),
    
    # Body ---------------------------------------------------------------------
    
    dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
            
            # Overview Tab -----------------------------------------------------
            
            tabItem(
                tabName = "tab_overview",
                # Zero row (select country)
                fluidRow(
                    box(
                        width = 12,
                        pickerInput(
                            inputId = "countryvar",
                            label = "Select Country", 
                            choices = list(
                                `European Union` = eu_country_label$name[c(1,2)],
                                Countries = eu_country_label$name[-c(1,2)]))
                        )
                    ),
                
                # First row
                fluidRow(
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Share of energy from renewable sources"),
                        shinycssloaders::withSpinner(plotlyOutput("plottab11"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Energy imports dependency"),
                        shinycssloaders::withSpinner(plotlyOutput("plottab12"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption by product"),
                        shinycssloaders::withSpinner(highchartOutput("plottab13"))
                    )
                ),
                
                # Second row
                fluidRow(
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption by sector"),
                        shinycssloaders::withSpinner(highchartOutput("plottab21"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption in households by fuel type"),
                        shinycssloaders::withSpinner(highchartOutput("plottab22"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption in industry by fuel type"),
                        shinycssloaders::withSpinner(highchartOutput("plottab23"))
                    )
                ),
                
                #Third row
                fluidRow(
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption in transport by fuel type"),
                        shinycssloaders::withSpinner(highchartOutput("plottab31"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Final energy consumption in services by fuel type"),
                        shinycssloaders::withSpinner(highchartOutput("plottab32"))
                    ),
                    box(
                        width = 4,
                        div(style = "font-size:16px;",
                            "Population unable to keep home adequately warm"),
                        shinycssloaders::withSpinner(plotlyOutput("plottab33"))
                    )
                    
                )
            ),
            
            # Time Tab ---------------------------------------------------------
            tabItem(
                tabName = "tab_timediagram",
                # Zero row
                fluidRow(
                    box(
                        width = 4,
                        pickerInput(
                            inputId = "time_countryvar",
                            label = "Select Country", 
                            choices = list(
                                `European Union` = eu_country_label$name[c(1,2)],
                                Countries = eu_country_label$name[-c(1,2)]))
                    ),
                    box(
                        width = 4,
                        selectInput(inputId  = "time_fuel",
                                    label    = "Select Fuel Type",
                                    choices  = eu_siec_lael_time$siec,
                                    selected = "Total")
                    ),
                    box(
                        width = 4,
                        uiOutput("timepicker")
                    )
                ),
                # First row
                fluidRow(
                    box(
                     width = 12,
                     plotlyOutput("plottime")
                    )
                    
                ),
                # Second row
                fluidRow(
                    box(
                        width = 12,
                        materialSwitch(
                            inputId = "time_table_switch",
                            label = "Show Table?", 
                            status = "primary",
                            right = TRUE
                        ),
                        conditionalPanel(
                            condition = "input.time_table_switch == true",
                            DTOutput("tabletime")
                        )
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
                        pickerInput(
                            inputId = "flow_countryvar",
                            label = "Select Country", 
                            choices = list(
                                `European Union` = eu_country_label$name[c(1,2)],
                                Countries = eu_country_label$name[-c(1,2)]))
                    ),
                    box(
                        width = 4,
                        selectInput(inputId  = "flow_year",
                                    label    = "Select Year",
                                    choices  = eu_year_label$time)
                    ),
                    box(
                        width = 4,
                        selectInput(inputId  = "flow_fuel",
                                    label    = "Select Fuel Type",
                                    choices  = eu_siec_label$siec,
                                    selected = "Total")
                    )
                ),
                
                # First row
                shinycssloaders::withSpinner(plotlyOutput("plotflow"))
            ),
            # Settings Tab -----------------------------------------------------
            tabItem(
                tabName = "tab_settings", 
                div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                shinyauthr::loginUI(
                    id = "login",
                    error_message = "You shall not pass!!!!"),
                uiOutput("settings_selection")
            )
        )
    )
)