setwd("Documents/DSEC/github_repo/dsec_analysis/")
# must set working directory to source the helpers.R and load the two functions
source("helpers.R")
lapply(c('tidyverse','stringr', 'dplyr', "readxl","ggplot2","RColorBrewer"), install_pkgs)

# set input/output folder paths
input_fp = "/Users/npatel/Documents/DSEC/" # input file: 'Taxonomy and Snapshot Coding.xlsx'
out_fp = "out/" # output files: figures

RColorBrewer::display.brewer.all()
spectral_palette = brewer.pal(11, "Spectral")
color_palette = spectral_palette[c(3,8,10)]

# Relevant tab names and partner names
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

####### FIGURE 4 - LOGIC MODEL Y/N ######
# read in the Supplementary tracking clean sheet (has logic model info)
logic_model = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = tab_names[length(tab_names)])

logic_model = logic_model[-c(grep("Hubs", logic_model$Partner)),c('Partner', 'Logic_Model_Exists')]

# check: should have 12 partners
print(str_c("total partners: ", nrow(logic_model)))

# drop hubs
hubs = c("UCSD", "Morgan State", "Dayton")
logic_model = logic_model[!logic_model$Partner %in% hubs, ]

logic_final <- logic_model %>% group_by(Logic_Model_Exists) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))

logic_final$Logic_Model_Exists[which(logic_final$Logic_Model_Exists == 'No')] <- "No current logic model(s)"
logic_final$Logic_Model_Exists[which(logic_final$Logic_Model_Exists == 'Yes')] <- "Has current logic model(s)"

lm_text_labs <- str_c(round(logic_final$prop_n*100),"%")
lm_pie_plot_labs <- c("Logic Model(s)"," Whether DSEC Partners Have \nExisting Logic Models")
lm_pie <- make_pie_graph(logic_final, logic_final$prop_n, logic_final$Logic_Model_Exists, lm_text_labs , lm_pie_plot_labs ,color_palette)
lm_pie 

ggsave(str_c(out_fp, "fig4_logicmodels.png"), height = 4, width=6)


####### FIGURE 5 - Evaluation ########
eval = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = 'current evaluation practices')
eval[is.na(eval)] <- 0

eval_final <- eval %>% select(partner, `external evaluation`) %>% group_by(`external evaluation`) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))


eval_final$`external evaluation`[which(eval_final$`external evaluation` == 0)] <- "No"
eval_final$`external evaluation`[which(eval_final$`external evaluation` == 1)] <- "Yes"
names(eval_final)[which(names(eval_final) == "external evaluation")] <- "External Evaluation"

eval_text_labs <- str_c(round(eval_final$prop_n*100),"%")
eval_pie_plot_labs <- c("External Evaluation Experience"," Whether DSEC Partners Have Experience \nwith External Evaluation")
eval_pie <- make_pie_graph(eval_final, eval_final$prop_n, eval_final$`External Evaluation`, eval_text_labs , eval_pie_plot_labs ,color_palette)
eval_pie


ggsave(str_c(out_fp, "fig5_eval.png"), height = 4, width=6)

####### Figure 4 Intentional Military Connections PSC ########## 
# military = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = 'military psc')
# 
# military[is.na(military)] <- 0
# 
# military = military[-c(grep("Hubs",military$partner)),]
# 
# # connection either to DoDEA School or Military connected students
# military_final <- military  %>% group_by(military_connection) %>% summarize(n = n()) %>%
#   mutate(prop_n = n / sum(n))
# 
# military_final$military_connection[which(military_final$military_connection == "0")] <- "No"
# military_final$military_connection[which(military_final$military_connection == "1")] <- "Yes"
# 
# mil_text_labs <- str_c(round(military_final$prop_n*100),"%")
# mil_pie_plot_labs <- c("Intentional Military Connections",
#                        "Intentional Military Connections by \nDSEC Partners")
# mil_pie <- make_pie_graph(military_final, military_final$prop_n, military_final$military_connection, mil_text_labs ,mil_pie_plot_labs ,color_palette)
# mil_pie
# 
# ggsave(str_c(out_fp, "military_connections_psc.png"), height = 6, width=6)
# 

####### Figure 4 Intentional Military Connections ########## 
# military = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = 'Hub Survey Military Connections')
# 
# military[is.na(military)] <- 0
# 
# military$connection = apply(military[,c("DoDEA Schools", "Military-Connected Students")], 1, function(x) max(x))
# 
# # connection either to DoDEA School or Military connected students
# military_final <- military %>% group_by(connection) %>% summarize(n = n()) %>%
#   mutate(prop_n = n / sum(n))
# 
# military_final$connection[which(military_final$connection == "0")] <- "No"
# military_final$connection[which(military_final$connection == "1")] <- "Yes"
# 
# mil_text_labs <- str_c(round(military_final$prop_n*100),"%")
# mil_pie_plot_labs <- c("Intentional Military Connections",
#                        "Intentional Military Connections by \nDSEC Partners \n(Either DoDEA Schools or \nMilitary-Connected Students)")
# mil_pie <- make_pie_graph(military_final, military_final$prop_n, military_final$connection, mil_text_labs ,mil_pie_plot_labs ,color_palette)
# mil_pie
# 
# ggsave(str_c(out_fp, "military_connections.png"), height = 6, width=6)
# 








