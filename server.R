
server <- function(input, output, session) {
 
  # Create Message Header -------------------------------------------------------
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]], icon = icon(row[["icon"]]), time = row[["time"]])
      })
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  # Get geo_code from input ----------------------------------------------------
  getGeocode <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$countryvar)]
  })
  getGeocodetime <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$time_countryvar)]
  })
  getGeocodeflow <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$flow_countryvar)]
  })
  getGeocodetime_country <- reactive({
    eu_country_label$code[which(eu_country_label$name %in% input$time_picker)]
  })
  
  # Create reactive inputs -----------------------------------------------------
  observeEvent(input$countryvar, {
    updateSelectInput(inputId = "time_countryvar", selected = input$countryvar)
    updateSelectInput(inputId = "flow_countryvar", selected = input$countryvar)
  }) # TODO: Show input
  
  # Create Plots for Overview Tab ----------------------------------------------
  
  output$plottab11 <- renderPlotly({
    
    # Get data ----
    dtab_11 <- nrg_ind_ren %>% filter(geo_code == getGeocode())
    dtab_11_xaxis_nticks <- dtab_11 %>% distinct(time) %>% count()
    # Plot options ----
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab_11_xaxis_nticks[[1]],
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y",
                  tickfont = list(size=8))
    
    yaxis = list(title = "Percentage",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    
    # Create plot ----
    ptab_11 <- plot_ly(dtab_11, x = ~time, y = ~values, 
                       color = ~nrg_bal,
                       type = 'scatter', mode = 'lines+markers',
                       symbol = ~nrg_bal,
                       symbols = c('circle','triangle-up','square', 'diamond'),
                       hovertemplate = paste(
                         "%{y:.1f} %",
                         "<extra></extra>")
                       )
    ptab_11 <- plotly::config(ptab_11, 
                              displaylogo=FALSE,
                              modeBarButtonsToRemove = c('autoScale', 
                                                         'zoomIn', 
                                                         'zoomOut', 
                                                         'toImage')) %>%  
      layout(legend = list(orientation = 'h'), 
             xaxis = xaxis, yaxis = yaxis,
             hovermode = 'x',
             dragmode = 'pan')
    ptab_11
    
  })
  output$plottab12 <- renderPlotly({
    
    # Get data ----
    dtab_12 <- nrg_ind_id %>% filter(geo_code == getGeocode())
    dtab_12_xaxis_nticks <- dtab_12 %>% distinct(time) %>% count()
    
    # Plot options ----
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab_12_xaxis_nticks[[1]]%/%2,
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y",
                  tickfont = list(size=8))
    
    yaxis = list(title = "Percentage",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    
    # Create plot ----
    ptab_12 <- plot_ly(dtab_12, x = ~time,
                       hovertemplate = paste(
                         "%{y:.1f} %",
                         "<extra></extra>")
                       )
    
    # Create Subplots and buttons
    dtab_12_siec <- dtab_12 %>% distinct(siec) %>% arrange(siec)
    buttonlist <- vector("list", nrow(dtab_12_siec)) #empty list
    for (row in 1:nrow(dtab_12_siec)) {
      visiblelist <- rep(F, nrow(dtab_12_siec))
      visiblelist[row] <- T
      # List for dropdown-buttons
      buttonlist[[row]] <- list(method = "restyle",
                                args   = list("visible", visiblelist),
                                label  = dtab_12_siec[[1]][row])
      # create all subplots
      ptab_12 <- ptab_12 %>% add_lines(y = ~values, 
                               data = dtab_12 %>% filter(siec == dtab_12_siec[[1]][row]), 
                               name = dtab_12_siec[[1]][row],
                               visible = visiblelist[nrow(dtab_12_siec)]) # Last plot is displayed
    }
    
    ptab_12 <- plotly::config(ptab_12, 
                              displaylogo=FALSE,
                              modeBarButtonsToRemove = c('autoScale', 
                                                         'zoomIn', 
                                                         'zoomOut', 
                                                         'toImage', 
                                                         'hoverCompareCartesian',
                                                         'hoverClosestCartesian')) %>% 
        layout(showlegend = FALSE, 
               xaxis  = xaxis, yaxis = yaxis,
               hovermode = 'x',
               dragmode = 'pan',
               updatemenus = list(
                 list(
                   x = 1,
                   y = 1.2,
                   active  = nrow(dtab_12_siec)-1,
                   font = list(size=10),
                   buttons = buttonlist) # previously created list
               ))
    ptab_12
  }) # TODO: Show Updatemenu
  
  # Data function for plots 13/22/23/31/32
  getSectordata <- function(nrg_bal_code) {
    data <- nrg_bal_s %>% filter(geo_code == getGeocode(),
                                 nrg_bal == nrg_bal_code,
                                 siec != "Total")
    return(data)
  }
  
  # Creates the stacked area charts for the plots 13/21/22/23/31/32
  createPlot <- function(data) {
    
    plot <- hchart(data, "area", hcaes(x = time, y = values, group = siec)) %>% 
      hc_xAxis(
        title = FALSE,
        tickInterval = 2 * 365 *(24 * 3600 * 1000), # tickinterval in milliseconds
        tickLength = 0,
        labels = list(
          rotation = 315,
          style = list(fontSize = 8),
          y = 10
          )
        ) %>% 
      hc_yAxis(
        title = list(
          text = "Thousand tonnes of oil equivalent"
          ),
        labels = list(
          style = list(fontSize = 8)
          )
        ) %>% 
      hc_tooltip(
        useHTML = TRUE,
        dateTimeLabelFormats = list(
          day = "%Y",
          year = "%Y"
          ),
        pointFormat = "
          {point.siec}<br>
          <b>{point.y:.0f} KTOE</b><br>
          <b>{point.total:.0f} KTOE</b>"
        ) %>% 
      hc_plotOptions(
        area = list(
          stacking = "normal",
          lineWidth = 1,
          marker = list(
            radius = 2
          )
        )
      ) %>% 
      hc_legend(enabled = FALSE)
    return(plot)
  } # TODO: Highchart library
  
  output$plottab13 <- renderHighchart({
    
    # Get data ----
    dtab_13 <- getSectordata("Final consumption - energy use")
    # Create plot ----
    ptab_13 <- createPlot(dtab_13)
    ptab_13
  })
  output$plottab21 <- renderHighchart({
    
    # Get data ----
    sectors <- c("Final consumption - industry sector - energy use",
                 "Final consumption - other sectors - agriculture and forestry - energy use",
                 "Final consumption - other sectors - commercial and public services - energy use",
                 "Final consumption - other sectors - fishing - energy use",
                 "Final consumption - other sectors - households - energy use",
                 "Final consumption - other sectors - not elsewhere specified - energy use",
                 "Final consumption - transport sector - energy use")
    dtab_21 <- nrg_bal_s %>% filter(geo_code == getGeocode(),
                                    siec == "Total",
                                    nrg_bal %in% sectors)
    # Create plot ----
    #   Not using the createPlot function, because could not forward the column name
    ptab_21 <- hchart(dtab_21, "area", hcaes(x = time, y = values, group = nrg_bal)) %>% 
      hc_xAxis(
        title = FALSE,
        tickInterval = 2 * 365 *(24 * 3600 * 1000), # tickinterval in milliseconds
        tickLength = 0,
        labels = list(
          rotation = 315,
          style = list(fontSize = 8),
          y = 10
        )
      ) %>% 
      hc_yAxis(
        title = list(
          text = "Thousand tonnes of oil equivalent"
        ),
        labels = list(
          style = list(fontSize = 8)
        )
      ) %>% 
      hc_tooltip(
        useHTML = TRUE,
        dateTimeLabelFormats = list(
          day = "%Y",
          year = "%Y"
        ),
        pointFormat = "
          {point.nrg_bal}<br>
          <b>{point.y:.0f} KTOE</b><br>
          <b>{point.total:.0f} KTOE</b>"
      ) %>% 
      hc_plotOptions(
        area = list(
          stacking = "normal",
          lineWidth = 1,
          marker = list(
            radius = 2
          )
        )
      ) %>% 
      hc_legend(enabled = FALSE)
    ptab_21
  })
  output$plottab22 <- renderHighchart({
    
    # Get data ----
    dtab_22 <- getSectordata("Final consumption - other sectors - households - energy use")
    # Create plot ----
    ptab_22 <- createPlot(dtab_22)
    ptab_22
  })
  output$plottab23 <- renderHighchart({
    
    # Get data ----
    dtab_23 <- getSectordata("Final consumption - industry sector - energy use")
    # Create plot ----
    ptab_23 <- createPlot(dtab_23)
    ptab_23
    
  })
  output$plottab31 <- renderHighchart({
    
    # Get data ----
    dtab_31 <- getSectordata("Final consumption - transport sector - energy use")
    # Create plot ----
    ptab_31 <- createPlot(dtab_31)
    ptab_31
    
  })
  output$plottab32 <- renderHighchart({
    
    # Get data ----
    dtab_32 <- getSectordata("Final consumption - other sectors - commercial and public services - energy use")
    # Create plot ----
    ptab_32 <- createPlot(dtab_32)
    ptab_32
    
  })
  output$plottab33 <- renderPlotly({
    
    # Get data ----
    dtab_33 <- ilc_mdes01 %>% filter(geo_code == getGeocode(),
                                     hhtyp == "Total")
    dtab_33_xaxis_nticks <- dtab_33 %>% distinct(time) %>% count()
    # Plot options ----
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab_33_xaxis_nticks[[1]]%/%2,
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y",
                  tickfont = list(size=8))
    
    yaxis = list(title = "Percentage",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    
    # Create plot ----
    ptab_33 <- plot_ly(dtab_33, x = ~time, y = ~values, 
                       color = ~incgrp,
                       type = 'scatter', mode = 'lines+markers',
                       symbol = ~incgrp,
                       symbols = c('circle','triangle-up','square'),
                       hovertemplate = paste(
                         "%{y:.1f} %",
                         "<extra></extra>")
                       )
    ptab_33 <- plotly::config(ptab_33, 
                              displaylogo=FALSE,
                              modeBarButtonsToRemove = c('autoScale', 
                                                         'zoomIn', 
                                                         'zoomOut', 
                                                         'toImage')) %>%  
      layout(legend = list(orientation = 'h'), 
             xaxis = xaxis, yaxis = yaxis,
             hovermode = 'x',
             dragmode = 'pan')
    ptab_33
  })
  
  # Create Plots for time tab --------------------------------------------------
  
  othercountries <- reactive({
    # Selection for the picker
    eu_country_label %>% filter(code != getGeocodetime())
  })
  output$timepicker <- renderUI({
    pickerInput(
      inputId = "time_picker",
      label = "Select/deselect countries to compare", 
      choices = othercountries()$name,
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    )
  })
  
  # Data frame for added countries
  addComparecountries <- reactive({
    geo_code <- c(getGeocodetime(), getGeocodetime_country())
    label <- c(input$time_countryvar, input$time_picker)
    
    df <- data.frame(geo_code = geo_code,
                     label = label)
    df
  })
  
  timedata <- reactive({
    data <- nrg_cb_pem %>%
      filter(siec == input$time_fuel)
  })
  
  # TODO: Observe is not adding the traces
  # observeEvent(input$time_picker, {
  #   dtime <- timedata()
  #   dtime_filter <- addComparecountries()
  #   for (row in 1:nrow(dtime_filter)) {
  #     plotlyProxy("plottime", session) %>% 
  #       plotlyProxyInvoke("addTraces", list(data = dtime %>% filter(geo_code %in% dtime_filter$geo_code[[row]]),
  #                                           type = "scatter", 
  #                                           mode = "lines",
  #                                           x = ~time,
  #                                           y = ~values,
  #                                           name = dtime_filter$label[[row]])
  #                         )
  #   }
  # })
  
  output$plottime <- renderPlotly({
    
    # Get data ----
    dtime <- timedata()
    dtime_xaxis_nticks <- dtime %>% distinct(time) %>% count()
    dtime_filter <- addComparecountries()
    # Plot options ----
    title <- list(text = "Net electricity generation by type of fuel",
                  x = 0,
                  xanchor = "left",
                  y = 1,
                  yanchor = "top",
                  pad = list(
                    l = 5,
                    t = 5
                    )
                  )
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  rangeslider = list(
                    visible = T),
                  rangeselector=list(
                    buttons=list(
                      list(count=1, label="1m",  step="month", stepmode="backward"),
                      list(count=6, label="6m",  step="month", stepmode="backward"),
                      list(count=1, label="YTD", step="year",  stepmode="todate"),
                      list(count=1, label="1y",  step="year",  stepmode="backward"),
                      list(step="all"))
                    ),
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtime_xaxis_nticks[[1]],
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y-M%m",
                  tickfont = list(size=8))
    yaxis = list(title = "Gigawatt-hour",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    # Create plot ----
    ptime <- plot_ly(dtime, 
                     type = "scatter", 
                     mode = "lines",
                     hovertemplate = paste(
                       "%{y:,.2r} GWh"))
    # Adds the selected traces (could not get observeEvent working)
    for (row in 1:nrow(dtime_filter)) {
      ptime <- add_trace(
        ptime,
        data = dtime %>% filter(geo_code == dtime_filter$geo_code[[row]]),
        x = ~time,
        y = ~values,
        name = dtime_filter$label[[row]]
      )
    }
    
    ptime <- layout(ptime,
                    showlegend = FALSE,
                    title = title,
                    xaxis = xaxis,
                    yaxis = yaxis,
                    margin = list(
                      t = 50,
                      b = 0),
                    hovermode = 'x')
    ptime <- plotly::config(ptime, 
                            displaylogo=FALSE)
    ptime
  })
  
  output$tabletime <- renderDT({
    timedata() %>% 
      filter(geo_code %in% addComparecountries()$geo_code) %>% 
      mutate(values = round(values)) %>% 
      pivot_wider(names_from = time, values_from = values) %>% 
      datatable(
        colnames = c("Code"  = 2,
                     "Fuel"  = 3,
                     "Unit"  = 4,
                     "Label" = 5),
        filter = "top",
        extensions = 'Buttons',
        options = list(dom = "Blrtp",
                       scrollX = TRUE,
                       buttons = list(
                         list(extend = 'collection',
                              buttons = c('csv', 'excel', 'pdf'),
                              text = 'Download' )
                         )
                       )
        )
  })
  
  # Create Plots for flow tab --------------------------------------------------
  
  output$plotflow <- renderPlotly({
    
    # Get data ----
    sankey_nodes <- c("Imports", #0
                      "Primary production", #1
                      "Stock draw", #2
                      "Statistical differences - inflow", #3
                      #
                      "Available from all sources", #4
                      #
                      "Direct carry-over", #5
                      "Transformation", #6
                      "Transformation loss", #7
                      #
                      "Available after transformation", #8
                      # 
                      "Final consumption", #9
                      "Consumption of the energy branch", #10
                      "Statistical differences - outflow", #11
                      "Distribution and transmission losses", #12
                      "International aviation", #13
                      "Marine bunkers", #14
                      "Stock build", #15
                      "Exports") #16
    
    dflow <- nrg_bal_sd %>% 
      filter(geo_code == getGeocodeflow(),
             siec == input$flow_fuel,
             time == input$flow_year) %>% 
      mutate(values = values*41.868) # KTOE -> TJ
    # Create plot ----
    pflow <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = sankey_nodes,
        #x = c(NA,0.0,0.0,0.0,0.25,0.45,0.45,0.7,0.7,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0),
        #y = c(NA,0.5,0.7,0.9, 0.2, 0.2,0.70,1.0,0.3,0.2,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
        pad = 20),
      link = list(
        source = c(0,1,2,3,4,5,4,6,6,8, 8, 8, 8, 8, 8, 8, 8),
        target = c(4,4,4,4,5,8,6,8,7,9,10,11,12,13,14,15,16),
        value  = c(
          # Sources
          dflow[which(dflow$nrg_bal == "Imports"),]$values,
          dflow[which(dflow$nrg_bal == "Primary production"),]$values,
          dflow[which(dflow$nrg_bal == "Stock draw"),]$values,
          dflow[which(dflow$nrg_bal == "Statistical differences - inflow"),]$values,
          # Transformation
          dflow[which(dflow$nrg_bal == "Into direct carry-over"),]$values,
          dflow[which(dflow$nrg_bal == "From direct carry-over"),]$values,
          dflow[which(dflow$nrg_bal == "Net transformation input"),]$values,
          dflow[which(dflow$nrg_bal == "Net transformation output"),]$values,
          dflow[which(dflow$nrg_bal == "Transformation losses"),]$values,
          # Final
          dflow[which(dflow$nrg_bal == "Final consumption"),]$values,
          dflow[which(dflow$nrg_bal == "Energy sector - energy use"),]$values,
          dflow[which(dflow$nrg_bal == "Statistical differences - outflow"),]$values,
          dflow[which(dflow$nrg_bal == "Distribution losses"),]$values,
          dflow[which(dflow$nrg_bal == "International aviation"),]$values,
          dflow[which(dflow$nrg_bal == "International maritime bunkers"),]$values,
          dflow[which(dflow$nrg_bal == "Stock build"),]$values,
          dflow[which(dflow$nrg_bal == "Exports"),]$values)))
    pflow <- plotly::config(pflow, 
                            displaylogo=FALSE,
                            modeBarButtonsToRemove = c('autoScale', 
                                                       'zoomIn', 
                                                       'zoomOut'))
    pflow
  })
  
  # Create Settings ------------------------------------------------------------
  # Log-In Page ----
  credentials <- shinyauthr::loginServer(
                            "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            #sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  logout_init <- shinyauthr::logoutServer(
                              "logout", 
                              reactive(credentials()$user_auth))
  # Settings Widgets ----
  output$settings_hello <- renderText({
    "Welcome to the settings page. This page lets the user download the
    latest Eurostat datasets and upload it to the MySQL-Server."
  })
  
  # Left Box - Server Status ----
  check_Connection <- reactive({
    # Needed for server status
    conn <- tryCatch({
      list(TRUE, getSqlConnection())
    }, error=function(cond) {
      list(FALSE, cond)
    })
  })
  
  output$settings_server_status <- renderUI({
    
    shinyjs::disable(id = "btn_update_sql") # TODO: Add .py script functionality
    
    if (check_Connection()[[1]]) {
      # Checks if Connection is possible
      conn <- check_Connection()[[2]]
      db_info <- dbGetInfo(conn)
      server_status_text <- paste0(
            "<b>Connected to the server!</b><br><br>",
            "Server IP Adress: <b>", db_info$host, "</b><br>",
            "Database Name: <b>", db_info$dbname, "</b><br>",
            "MySQL-Version: <b>", db_info$serverVersion, "</b>")
      dbDisconnect(conn)
      bg_color <- "#ABEBC6"
      server_status <- list(server_status_text, bg_color)
    } else {
      # Error message for no connection
      server_status_text <- paste0(
            "<b>Server connection is not possible.</b><br><br>
            Please check the following error message:<br>",
            check_Connection()[[2]])
      bg_color <- "#F5B7B1"
      server_status <- list(server_status_text, bg_color)
    }
    # Final return for the htmlOutput
    HTML(paste0("<div style='background-color:",server_status[[2]],"'>",
                server_status[[1]],
                "</div>"))
  })
  # Right Box - Table Status ----
  servercon_status <- reactive({
    query <- "SHOW TABLE STATUS"
    data <- connectDB(query)
    data <- data %>% select(
      !c("Engine", "Version", "Row_format", "Avg_row_length", "Max_data_length", 
         "Index_length", "Auto_increment", "Check_time", "Collation", "Checksum", 
         "Create_options"))
  })
  
  output$settings_server_database <- renderDataTable(
    servercon_status(),
    options = list(dom = 't')
  )
  
  # SQL Table output ----
  sql_data <- eventReactive(input$btn_sql_send, {
    connectDB(input$sqlinput) # stores input after pressing the "Query!"-button
  })
  
  output$sql_table <- renderDT({
    sql_data() %>% 
      datatable(
        filter = "top",
        options = list(scrollX = TRUE)
      )
  }) 
  
  # Settings Layout Reactive ----
  #   Creates the layout of the settings page
  #   Syntax is similar to the ui.R layout 
  output$settings_selection <- renderUI({
    req(credentials()$user_auth) # Login request
    
    tabItem(
      tabName = "tab_overview_render",
      # Zero row (info)
      fluidRow(
        box(
          width = 12,
          textOutput("settings_hello"),
          tags$head(tags$style("#settings_hello{font-size: 20px;}"
                               )
                    )
        )
      ),
      # First row
      fluidRow(
        box(
          width = 4,
          title = "Server Status",
          htmlOutput("settings_server_status"),
          tags$div(style="display:inline-block",
                   title="Currently not working",
                   actionButton("btn_update_sql", "Update Database"),
                   style="float:center")
          ),
        box(
          width = 8,
          title = "Last Database Update",
          dataTableOutput("settings_server_database")
          )
        ),
      # Second row
      fluidRow(
        box(
          width = 4,
          column(
            width = 10,
            textAreaInput(
              inputId = "sqlinput",
              label = "SQL Query",
              resize = "vertical")
            ),
          column(
            width = 2,
            tags$div(style="display:inline-block",
                     title="Send SQL-Query to Server",
                     actionButton("btn_sql_send", "Query!"))
            )
          ),
        box(
          width = 8,
          DTOutput("sql_table")
        )
      )
    )
    
  }) # TODO: Show login
}