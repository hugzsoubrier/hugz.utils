# ---------------------------
# Script name: hugz-utils-ggplot.R
#
# Purpose of script: utils functions for ggplot
#
# Author: Hugo Soubrier
#
# Date Created: 2023-10-12
#
# Copyright (c) Hugo SOUBRIER, 2023
# Email: soubrierhugo@gmail.com
# ---------------------------
# Notes:
#   
#
#
# ---------------------------
#

# Base theme for the ggplots
  gg_base_theme <- function(gg, title_name = NULL, subtitle_name = NULL, xlab = NULL, ylab = NULL ){
    
    require(hrbrthemes)

    gg +
    #theme
    hrbrthemes::theme_ipsum(base_size = 10, 
                            strip_text_size = 6, 
                            axis_title_size = 10, 
                            plot_margin = margin(10, 10, 10, 10),
                            plot_title_size = 14,
                            subtitle_size = 12, 
                            subtitle_face = "italic", 
                            axis_text_size = 11
    ) +
      
      labs(title = title_name, 
           subtitle = subtitle_name
           ) +
      
      xlab(xlab) +
      
      ylab(ylab)+
      
      theme(
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        panel.spacing = unit(.5, "lines")
      )
    
  }