setwd("Documents/DSEC/github_repo/dsec_analysis/")
source("helpers.R")
lapply(c('tidyverse','stringr', 'dplyr', "readxl","ggplot2"), install_pkgs)

input_fp = "/Users/npatel/Documents/DSEC/"

RColorBrewer::display.brewer.all()
spectral_palette = brewer.pal(11, "Spectral")
color_palette = spectral_palette[c(3,8,10)]

# tab names and partner names
tab_names = c("Structure", "Content", "Participants","Geographic Outreach","Outcomes", "Logistics&Timing")
partner_names = c("CEE","FIRST","Math Counts","NCWIT","NMSI","SSP","TGR",
                  "TIES","USASEF","Dayton STEM Center", "Morgan State CEMSE", "UC San Diego CREATE")


# start with structure sheet
structure = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = tab_names[1],skip = 2)

####### ENGAGEMENT TYPE - FIGURE 1 ###### 
engagement_type = structure[,c('X__1','ENVIRONMENT TYPE','X__2','X__3')]
colnames(engagement_type) <- c("program",'in_school','out_of_school','adult_learning')

idx_to_drop = c(1, which(is.na(engagement_type$program)), which(engagement_type$program %in% partner_names))
engagement_type = engagement_type[-unique(idx_to_drop),]
engagement_type[is.na(engagement_type)] <- 0

tot_programs <- nrow(engagement_type)
print(str_c("Total estimated programs: ", nrow(engagement_type)))

# reshape for groups
et_reshape <- gather(engagement_type, key = "engagement_type", value = "value",colnames(engagement_type)[!colnames(engagement_type) %in% c("program")])
et_final <- et_reshape %>% filter(value==1) %>% group_by(engagement_type) %>% summarize(n=n()) %>% mutate(prop_n = n/sum(n))
et_final$engagement_type[which(et_final$engagement_type == "adult_learning")] <- "Adult Learning"
et_final$engagement_type[which(et_final$engagement_type == "in_school")] <- "In School"
et_final$engagement_type[which(et_final$engagement_type == "out_of_school")] <- "Out of School"

# percentage pie plot
et_text_labs <- str_c(round(et_final$prop_n*100),"%")
et_pie_plot_labs <- c("Engagement Type","Engagement Type of DSEC Partner Programs")
et_pie <- make_pie_graph(et_final, et_final$prop_n, et_final$engagement_type, et_text_labs , et_pie_plot_labs ,color_palette)
et_pie 

# count pie plot
et_text_labs <- et_final$n
et_pie_plot_labs <- c("Engagement Type","Engagement Type of DSEC Partner Programs")
et_pie_count <- make_pie_graph(et_final, et_final$n, et_final$engagement_type, et_text_labs , et_pie_plot_labs ,color_palette)
et_pie_count 

# count bar plot
et_bar_plot_labs <- c( 'Engagement Type', 'Number of Programs', "Engagement Type of DSEC Partner Programs")
et_barplot <- make_bar_graph_counts(et_final, et_final$engagement_type, y=et_final$n, et_bar_plot_labs,color_palette)
et_barplot




