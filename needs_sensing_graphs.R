setwd("Documents/DSEC/github_repo/dsec_analysis/")
source("helpers.R")
lapply(c('tidyverse','stringr', 'dplyr', "readxl","ggplot2"), install_pkgs)

input_fp = "/Users/npatel/Documents/DSEC/"

RColorBrewer::display.brewer.all()
#spectral_palette = brewer.pal(11, "Spectral")
#color_palette = spectral_palette[c(3,8,10)]
blues_palette = brewer.pal(9, 'Blues')
color_palette = blues_palette[c(2,5, 7)]

# tab names and partner names
tab_names = c("Structure", "Content", "Participants","Geographic Outreach","Outcomes", "Logistics&Timing", "Supplementary Tracking Clean")
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

# # count pie plot
# et_text_labs <- et_final$n
# et_pie_plot_labs <- c("Engagement Type","Engagement Type of DSEC Partner Programs")
# et_pie_count <- make_pie_graph(et_final, et_final$n, et_final$engagement_type, et_text_labs , et_pie_plot_labs ,color_palette)
# et_pie_count 
# 
# # count bar plot
# et_bar_plot_labs <- c( 'Engagement Type', 'Number of Programs', "Engagement Type of DSEC Partner Programs")
# et_barplot <- make_bar_graph_counts(et_final, et_final$engagement_type, y=et_final$n, et_bar_plot_labs,color_palette)
# et_barplot



####### MECHANISM - FIGURE 2 ###### 
mechanism = structure[,c('X__1', 'MECHANISM', 'X__4','X__5')]
colnames(mechanism) <- c("program",'in-person','virtual','hybrid')

idx_to_drop = c(1, which(is.na(mechanism$program)), which(mechanism$program %in% partner_names))
mechanism = mechanism[-unique(idx_to_drop),]
mechanism[is.na(mechanism)] <- 0

tot_programs <- nrow(mechanism)
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
mech_pie_plot_labs <- c("Mechanism","Mechanism of DSEC Partner Programs")
mech_pie <- make_pie_graph(mech_final, mech_final$prop_n, mech_final$mechanism, mech_text_labs , mech_pie_plot_labs ,color_palette)
mech_pie 

# # count pie plot
# mech_text_labs <- mech_final$n
# mech_pie_plot_labs <-  c("Mechanism","Mechanism of DSEC Partner Programs")
# mech_pie_count <- make_pie_graph(mech_final, mech_final$n, mech_final$mechanism, mech_text_labs ,mech_pie_plot_labs ,color_palette)
# mech_pie_count 
# 
# # count bar plot
# mech_bar_plot_labs <- c( "Mechanism", 'Number of Programs', "Mechanism of DSEC Partner Programs")
# mech_barplot <- make_bar_graph_counts(mech_final, mech_final$mechanism, y=mech_final$n, mech_bar_plot_labs,color_palette)
# mech_barplot


####### DURATION - FIGURE 3 ######
duration = structure[,c('X__1','DURATION','X__6','X__7')]
colnames(duration) <- c("program",'short','medium','long_term')

idx_to_drop = c(1, which(is.na(duration$program)), which(duration$program %in% partner_names))
duration = duration[-unique(idx_to_drop),]
duration[is.na(duration)] <- 0

tot_programs <- nrow(duration)
print(str_c("Total estimated programs: ", nrow(duration)))

duration_reshape <- gather(duration, key = "duration", value = "value",
                       colnames(duration)[!colnames(duration) %in% c("program")])
duration_final <- duration_reshape %>% filter(value==1) %>% group_by(duration) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))

duration_final$duration[which(duration_final$duration == "short")] <- "Short-term (1 day to 1 week)"
duration_final$duration[which(duration_final$duration == "medium")] <- "Medium-term (1 week to 1 month)"
duration_final$duration[which(duration_final$duration == "long_term")] <- "Long-term (> 1 month)"

dur_text_labs <- str_c(round(duration_final$prop_n*100),"%")
dur_pie_plot_labs <- c("Duration","Duration of DSEC Partner Programs")
dur_pie <- make_pie_graph(duration_final, duration_final$prop_n, duration_final$duration, dur_text_labs , dur_pie_plot_labs ,color_palette)
dur_pie


####### Figure 4 Intentional Military Connections ########## 
military = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = 'Hub Survey Military Connections')

military[is.na(military)] <- 0

military$connection = apply(military[,c("DoDEA Schools", "Military-Connected Students")], 1, function(x) max(x))

# connection either to DoDEA School or Military connected students
military_final <- military %>% group_by(connection) %>% summarize(n = n()) %>%
  mutate(prop_n = n / sum(n))

military_final$connection[which(military_final$connection == "0")] <- "No"
military_final$connection[which(military_final$connection == "1")] <- "Yes"

mil_text_labs <- str_c(round(military_final$prop_n*100),"%")
mil_pie_plot_labs <- c("Intentional Military Connections",
                       "Intentional Military Connections by \nDSEC Partners \n(Either DoDEA Schools or \nMilitary-Connected Students)")
mil_pie <- make_pie_graph(military_final, military_final$prop_n, military_final$connection, mil_text_labs ,mil_pie_plot_labs ,color_palette)
mil_pie



####### LOGIC MODEL Y/N - FIGURE 5 ######
# Note: Need to address NA vs. saying No
# start with structure sheet
logic_model = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = tab_names[length(tab_names)])

logic_model = logic_model[-c(grep("Hubs", logic_model$Partner)),c('Partner', 'Logic_Model_Exists')]

tot_partners = nrow(logic_model)
print(str_c("total partners: ", tot_partners))

# logic_reshape <- gather(logic_model, key = "Logic Model(s) Exist", value = "value",
#                        colnames(logic_model)[!colnames(logic_model) %in% c("Partner")])

logic_final <- logic_model %>% group_by(Logic_Model_Exists) %>% summarize(n=n()) %>%
  mutate(prop_n = n/sum(n))

lm_text_labs <- str_c(round(logic_final$prop_n*100),"%")
lm_pie_plot_labs <- c("Logic Model(s)"," Whether DSEC Partners Have Existing Logic Models")
lm_pie <- make_pie_graph(logic_final, logic_final$prop_n, logic_final$Logic_Model_Exists, lm_text_labs , lm_pie_plot_labs ,color_palette)
lm_pie 


####### FIGURE 6 ALUMNI ########


















