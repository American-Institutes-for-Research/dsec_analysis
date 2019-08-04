# REQUIRES: pkg name as string
# MODIFIES: nothing
# EFFECTS: loads library, installs first if needed 
install_pkgs = function(pkg){
  # Install package if it isn't already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cloud.r-project.org/')
  }
  
  library(pkg, character.only = TRUE)
  
} # end install_pkgs()

# REQUIRES: df, y_var, fill_var, text_labels needed, plot_labels vector (fill & plot title), color palette)
# MODIFIES: nothing
# EFFECTS: makes pie graph of data
make_pie_graph <- function(df, y_var, fill_var, text_labels, plot_labs,clr_palette){
  df %>% ggplot(mapping=aes(x = '', y=y_var, fill=fill_var)) +
    geom_bar(stat="identity") +
    coord_polar(theta = 'y', start = 90) +
    geom_text(mapping=aes(label=text_labels,fontface="bold"), position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = plot_labs[1], title = plot_labs[2]) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          legend.title = element_text(face="bold", size = 12),
          legend.text = element_text(face="bold", size = 10),
          plot.title = element_text(face="bold",hjust=0.5, vjust = -7, size = 14),
          text = element_text(face="bold", size = 12)
    ) +
    scale_fill_manual(values=c(clr_palette))
  
}


