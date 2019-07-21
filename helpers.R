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