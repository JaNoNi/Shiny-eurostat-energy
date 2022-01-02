
server <- function(input, output) {
 
  # Get geo_code from input ----------------------------------------------------
  getGeocode <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$countryvar)]
  })
  getGeocodeflow <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$flow_countryvar)]
  })
  
  # Create reactive inputs -----------------------------------------------------
  observeEvent(input$countryvar, {
    updateSelectInput(inputId = "flow_countryvar", selected = input$countryvar)
  })
   # TODO: Show input
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
                       color = ~nrg_bal, colors = mycolors,
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
                       colors = mycolors,
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
  output$plottab13 <- renderPlotly({
    
    # Get data ----
    dtab_13_total <- nrg_bal_s %>% 
      filter(geo_code == getGeocode(),
             nrg_bal == "Final consumption - energy use",
             siec == "Total") %>% 
      pivot_wider(names_from = siec, values_from = values) %>% 
      select(c(geo_code, time, Total))
    
    dtab_13 <- nrg_bal_s %>% filter(geo_code == getGeocode(),
                                    nrg_bal == "Final consumption - energy use",
                                    siec != "Total")
    
    dtab_13 <- dtab_13 %>% left_join(dtab_13_total, by = c("geo_code", "time"))
    
    dtab_13_xaxis_nticks <- dtab_13 %>% distinct(time) %>% count()
    # Plot options ----
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab_13_xaxis_nticks[[1]]%/%2,
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y",
                  tickfont = list(size=8))
    
    yaxis = list(title = "Thousand tonnes of oil equivalent",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    # Create plot ----
    ptab_13 <- plot_ly(dtab_13, x = ~time, y = ~values, 
                       color = ~siec, colors = mycolors,
                       type = 'scatter', mode = 'lines+markers', # TODO: Adjust markers
                       line = list(
                         width = 1),
                       stackgroup = 'one',
                       text = ~Total,
                       hovertemplate = ~paste(
                         siec, "<br>",
                         "<b>%{y:,.2r} KTOE</b><br>",
                         "Total: <b>%{text:,.2r} KTOE</b><br>",
                         "Year: %{x}",
                         "<extra></extra>"),
                       hoveron = 'points+fills',
                       hoverlabel = list(
                         font = list(size=11),
                         align = 'left'))
    ptab_13 <- plotly::config(ptab_13, 
                              displaylogo=FALSE,
                              modeBarButtonsToRemove = c('autoScale', 
                                                         'zoomIn', 
                                                         'zoomOut', 
                                                         'toImage')) %>%  
      layout(legend = list(orientation = 'h'), 
             showlegend = FALSE,
             xaxis = xaxis, yaxis = yaxis,
             hovermode = 'closest',
             hoverdistance = 50,
             dragmode = 'pan')
    ptab_13
  })
  output$plottab21 <- renderPlotly({
    
    # Get data ----
    dtab_21_total <- nrg_bal_s %>% 
      filter(geo_code == getGeocode(),
             nrg_bal == "Final consumption - energy use",
             siec == "Total") %>% 
      pivot_wider(names_from = siec, values_from = values) %>% 
      select(c(geo_code, time, Total))
    
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
    
    dtab_21 <- dtab_21 %>% left_join(dtab_21_total, by = c("geo_code", "time"))
    
    dtab_21_xaxis_nticks <- dtab_21 %>% distinct(time) %>% count()
    # Plot options ----
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab_21_xaxis_nticks[[1]]%/%2,
                  ticks  = '',
                  tickangle  = 315,
                  tickformat = "%Y",
                  tickfont = list(size=8))
    
    yaxis = list(title = "Thousand tonnes of oil equivalent",
                 showgrid = TRUE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 ticks = '',
                 tickfont = list(size=8),
                 zeroline = FALSE)
    # Create plot ----
    ptab_21 <- plot_ly(dtab_21, x = ~time, y = ~values, 
                       color = ~nrg_bal, colors = mycolors,
                       type = 'scatter', mode = 'markers+lines', # TODO: adjust markers
                       #symbol = ~nrg_bal,
                       line = list(
                         width = 1),
                       stackgroup = 'one', fill = "tonexty",
                       text = ~Total,
                       hovertemplate = ~paste(
                         nrg_bal, "<br>",
                         "<b>%{y:,.2r} KTOE</b><br>",
                         "Total: <b>%{text:,.2r} KTOE</b><br>",
                         "Year: %{x}",
                         "<extra></extra>"),
                       hoveron = 'points+fills',
                       hoverlabel = list(
                         font = list(size=11),
                         align = 'left'))
    ptab_21 <- plotly::config(ptab_21, 
                              displaylogo=FALSE,
                              modeBarButtonsToRemove = c('autoScale', 
                                                         'zoomIn', 
                                                         'zoomOut', 
                                                         'toImage')) %>%  
      layout(legend = list(orientation = 'h'), 
             showlegend = FALSE,
             xaxis = xaxis, yaxis = yaxis,
             hovermode = 'closest',
             hoverdistance = 100,
             dragmode = 'pan')
    ptab_21
  }) # TODO: Improve Hover Labels
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
                       color = ~incgrp, colors = mycolors,
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
    # Plot options ----
    
    # Create plot ----
    pflow <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = sankey_nodes),
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
}