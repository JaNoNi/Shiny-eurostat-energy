
server <- function(input, output) {
 
  # Get geo_code from input ----------------------------------------------------
  getGeocode <- reactive({
    eu_country_label$code[which(eu_country_label$name == input$countryvar)]
  })
  
  # Create Plots for Overview Tab ----------------------------------------------
  output$plottab11 <- renderPlotly({
    
    # Get data
    dtab_11 <- nrg_ind_ren %>% filter(geo_code == getGeocode())
    dtab_11_xaxis_nticks <- dtab_11 %>% distinct(time) %>% count()
    # Plot options
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
    
    # Create plot
    ptab_11 <- plot_ly(dtab_11, x = ~time, y = ~values, 
                   color = ~nrg_bal, type = 'scatter', mode = 'lines+markers')
    ptab_11 <- ptab_11 %>% layout(legend = list(orientation = 'h'), 
                          xaxis = xaxis, yaxis = yaxis,
                          hovermode = 'x')
    ptab_11
    
  })
  output$plottab12 <- renderPlotly({
    
    # Get data
    dtab12 <- nrg_ind_id %>% filter(geo_code == getGeocode())
    dtab12_xaxis_nticks <- dtab12 %>% distinct(time) %>% count()
    
    # Plot options
    xaxis <- list(title = "",
                  type = 'date',
                  showgrid = FALSE,
                  showline = TRUE,
                  linecolor  = 'rgb(204, 204, 204)',
                  automargin = TRUE,
                  showticklabels = TRUE,
                  tickmode = 'auto',
                  nticks = dtab12_xaxis_nticks[[1]]%/%2,
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
    
    # Create plot
    ptab12 <- plot_ly(dtab12, x = ~time)
    
    # Create Subplots and buttons
    dtab12_siec <- dtab12 %>% distinct(siec) %>% arrange(siec)
    buttonlist <- vector("list", nrow(dtab12_siec))
    for (row in 1:nrow(dtab12_siec)) {
      visiblelist <- rep(F, nrow(dtab12_siec))
      visiblelist[row] <- T
      # List for dropdown-buttons
      buttonlist[[row]] <- list(method = "restyle",
                                args   = list("visible", visiblelist),
                                label  = dtab12_siec[[1]][row])
      # All possible plots
      ptab12 <- ptab12 %>% add_lines(y = ~values, 
                               data = dtab12 %>% filter(siec == dtab12_siec[[1]][row]), 
                               name = dtab12_siec[[1]][row],
                               visible = visiblelist[nrow(dtab12_siec)]) # Last plot is displayed
    }
    
    ptab12 <- ptab12 %>% layout(showlegend = FALSE, 
                                xaxis  = xaxis, yaxis = yaxis,
                                hovermode = 'x',
                                updatemenus = list(
                                  list(
                                    x = 1,
                                    y = 1,
                                    active  = nrow(dtab12_siec)-1,
                                    buttons = buttonlist) # previously created list
                                  ))
    ptab12
  })
}