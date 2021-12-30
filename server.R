
server <- function(input, output) {
 
  # Get geo_code from input ----------------------------------------------------
  getGeocode <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$countryvar)]
  })
  
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
                       type = 'scatter', mode = 'lines+markers')
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
    ptab_12 <- plot_ly(dtab_12, x = ~time, colors = mycolors)
    
    # Create Subplots and buttons
    dtab_12_siec <- dtab_12 %>% distinct(siec) %>% arrange(siec)
    buttonlist <- vector("list", nrow(dtab_12_siec))
    for (row in 1:nrow(dtab_12_siec)) {
      visiblelist <- rep(F, nrow(dtab_12_siec))
      visiblelist[row] <- T
      # List for dropdown-buttons
      buttonlist[[row]] <- list(method = "restyle",
                                args   = list("visible", visiblelist),
                                label  = dtab_12_siec[[1]][row])
      # All possible plots
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
  })
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
                       stackgroup = 'one',
                       hovertext = ~paste0("Year: ", year(time), "<br>",
                                     siec, "<br>",
                                     "<b>", round(values), " KTOE</b><br>",
                                     "Total: <b>", round(Total)," KTOE</b><br>"),
                       hoverinfo = 'text',
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
             #hovermode = 'x',
             hoverdistance = 100,
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
    
    dtab_21 <- nrg_bal_s %>% filter(geo_code == getGeocode(),
                                    siec == "Total",
                                    nrg_bal != "Final consumption - energy use",
                                    str_detect(nrg_bal, "Final consumption"),
                                    str_detect(nrg_bal, "\\bsectors\\b")
                                    )
    
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
                       type = 'scatter', mode = 'lines+markers', # TODO: adjust markers
                       stackgroup = 'one', fill = "tonexty",
                       hovertext = ~paste0("Year: ", year(time), "<br>",
                                      nrg_bal, "<br>",
                                      "<b>", round(values), " KTOE</b><br>",
                                      "Total: <b>", round(Total)," KTOE</b><br>"),
                       hoverinfo = 'text',
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
             #hovermode = 'x',
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
                       type = 'scatter', mode = 'lines+markers')
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
}