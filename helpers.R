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
    coord_polar(theta = 'y', start = 0) +
    geom_text(mapping=aes(label=text_labels,fontface="bold"), position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = plot_labs[1], title = plot_labs[2]) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          legend.title = element_text(face="bold"),
          legend.text = element_text(face="bold"),
          plot.title = element_text(face="bold",hjust=0.5, vjust = -10)
    ) +
    scale_fill_manual(values=c(clr_palette))
  
}

# REQUIRES: df, x_var, y_var, plot_labels vector (x,y, & plot title), color palette)
# MODIFIES: nothing
# EFFECTS: makes bar graph of counts data
make_bar_graph_counts <- function(df, x_var, y_var, plot_labs,clr_palette){
  
  df %>% ggplot(mapping=aes(x = x_var, y=y_var,fill=x_var)) +
  geom_bar(stat="identity", position = "dodge", show.legend = FALSE) +
  geom_text(mapping=aes(label=y_var,fontface="bold",vjust=-0.5)) +
  labs(x = plot_labs[1], y = plot_labs[2], title = plot_labs[3]) +
  theme_classic() +
  theme(
    plot.title = element_text(face="bold",hjust=0.5),
    axis.text.x = element_text(face="bold",color="black"),
    axis.text.y = element_text(face="bold",color="black"),
    axis.title.x = element_text(face="bold",color="black"),
    axis.title.y = element_text(face="bold",color="black")
  ) +
  scale_fill_manual(values=c(clr_palette))

}
