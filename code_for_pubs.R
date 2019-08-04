setwd("<<< set to folder path where this code/folder is saved >>>")

# must set working directory to source the helpers.R and load the two functions
source("helpers.R")
lapply(c('tidyverse','stringr', 'dplyr', "readxl","ggplot2","RColorBrewer"), install_pkgs)

# set input/output folder paths
input_fp = "<set to folder path where this code/folder is saved" # input file: 'Taxonomy and Snapshot Coding.xlsx'
out_fp = "out/" # output files: figures

# set up color palette for graphs
RColorBrewer::display.brewer.all()
spectral_palette = brewer.pal(11, "Spectral")
color_palette = spectral_palette[c(3,8,10)]

# relevant tab names and partner names
tab_names = c("Structure", "Content", "Participants","Geographic Outreach","Outcomes", "Logistics&Timing", "Supplementary Tracking Clean")

partner_names = c("CEE","FIRST","Math Counts","NCWIT","NMSI","SSP","TGR",
                  "TIES","USASEF","Dayton STEM Center", "Morgan State CEMSE", "UC San Diego CREATE")


# Read in the Structure Sheet for Figures 1-3
structure = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'), sheet = tab_names[1],skip = 2)

####### FIGURE 1: ENGAGEMENT TYPE ###### 
engagement_type = structure[,c('X__1','ENVIRONMENT TYPE','X__2','X__3')]
colnames(engagement_type) <- c("program",'in_school','out_of_school','adult_learning')

# cleaning
idx_to_drop = c(1, which(is.na(engagement_type$program)), which(engagement_type$program %in% partner_names))
engagement_type = engagement_type[-unique(idx_to_drop),]
engagement_type[is.na(engagement_type)] <- 0

# drop morgan state mentor program
engagement_type = engagement_type[-c(21),]

# check: total program count should be 22
print(str_c("Total estimated programs: ", nrow(engagement_type)))

# reshape for groups
et_reshape <- gather(engagement_type, key = "engagement_type", value = "value",colnames(engagement_type)[!colnames(engagement_type) %in% c("program")])
et_final <- et_reshape %>% filter(value==1) %>% group_by(engagement_type) %>% summarize(n=n()) %>% mutate(prop_n = n/sum(n))
et_final$engagement_type[which(et_final$engagement_type == "adult_learning")] <- "Adult Learning"
et_final$engagement_type[which(et_final$engagement_type == "in_school")] <- "In School"
et_final$engagement_type[which(et_final$engagement_type == "out_of_school")] <- "Out of School"

# percentage pie plot
et_text_labs <- str_c(round(et_final$prop_n*100),"%")
et_pie_plot_labs <- c("Environment","DSEC Partner Programing\n by Environment Type")
et_pie <- make_pie_graph(et_final, et_final$prop_n, et_final$engagement_type, et_text_labs , et_pie_plot_labs ,color_palette)
et_pie 

ggsave(str_c(out_fp, "fig1_et.png"), height = 4, width= 6)


####### FIGURE 2 - MECHANISM  ###### 
mechanism = structure[,c('X__1', 'MECHANISM', 'X__4','X__5')]
colnames(mechanism) <- c("program",'in-person','virtual','hybrid')

# cleaning
idx_to_drop = c(1, which(is.na(mechanism$program)), which(mechanism$program %in% partner_names))
mechanism = mechanism[-unique(idx_to_drop),]
mechanism[is.na(mechanism)] <- 0

# drop morgan state mentor program
mechanism = mechanism[-c(21),]

# check: total program count should be 22
print(str_c("Total estimated programs: ", nrow(engagement_type)))

# reshape for groups
mech_reshape <- gather(mechanism, key = "mechanism", value = "value",
                       colnames(mechanism)[!colnames(mechanism) %in% c("program")])
mech_final <- mech_reshape %>% filter(value==1) %>% group_by(mechanism) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))

mech_final$mechanism[which(mech_final$mechanism == "in-person")] <- "In-person"
mech_final$mechanism[which(mech_final$mechanism == "virtual")] <- "Virtual"
mech_final$mechanism[which(mech_final$mechanism == "hybrid")] <- "Hybrid"

# percentage pie plot
mech_text_labs <- str_c(round(mech_final$prop_n*100),"%")
mech_pie_plot_labs <- c("Mechanism","DSEC Partner Programming \nby Mechanism")
mech_pie <- make_pie_graph(mech_final, mech_final$prop_n, mech_final$mechanism, mech_text_labs , mech_pie_plot_labs ,color_palette)
mech_pie 

ggsave(str_c(out_fp, "fig2_mech.png"), height = 4, width=6)

####### FIGURE 3 - DURATION ######
duration = structure[,c('X__1','DURATION','X__6','X__7')]
colnames(duration) <- c("program",'short','medium','long_term')

# cleaning
idx_to_drop = c(1, which(is.na(duration$program)), which(duration$program %in% partner_names))
duration = duration[-unique(idx_to_drop),]
duration[is.na(duration)] <- 0


# drop morgan state mentor program
duration = duration[-c(21),]

# check: total program count should be 22
print(str_c("Total estimated programs: ", nrow(duration)))

duration_reshape <- gather(duration, key = "duration", value = "value",
                           colnames(duration)[!colnames(duration) %in% c("program")])
duration_final <- duration_reshape %>% filter(value==1) %>% group_by(duration) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))

duration_final$duration[which(duration_final$duration == "short")] <- "Short-term (1 day to 1 week)"
duration_final$duration[which(duration_final$duration == "medium")] <- "Medium-term (1 week to 1 month)"
duration_final$duration[which(duration_final$duration == "long_term")] <- "Long-term (> 1 month)"

dur_text_labs <- str_c(round(duration_final$prop_n*100),"%")
dur_pie_plot_labs <- c("Duration","DSEC Partner Programming \nby Duration")
dur_pie <- make_pie_graph(duration_final, duration_final$prop_n, duration_final$duration, dur_text_labs , dur_pie_plot_labs ,color_palette)
dur_pie

ggsave(str_c(out_fp, "fig3_duration.png"), height =4, width=6)







