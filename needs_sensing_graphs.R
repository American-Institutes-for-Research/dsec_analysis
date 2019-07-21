setwd("Documents/DSEC/github_repo/dsec_analysis/")
source("helpers.R")
lapply(c('tidyverse','stringr', 'dplyr', "readxl","ggplot2"), install_pkgs)

input_fp = "/Users/npatel/Documents/DSEC/"

RColorBrewer::display.brewer.all()
spectral_palette = brewer.pal(11, "Spectral")
color_palette = spectral_palette[c(3,8,10)]

# tab names
tab_names = c("Structure", "Content", "Participants","Geographic Outreach","Outcomes", "Logistics&Timing")
partner_names = c("CEE","FIRST","Math Counts","NCWIT","NMSI","SSP","TGR",
                  "TIES","USASEF","Dayton STEM Center", "Morgan State CEMSE", "UC San Diego CREATE")
# start with structure sheet
structure = read_excel(str_c(input_fp, 'Taxonomy and Snapshot Coding.xlsx'),sheet = tab_names[1],skip = 2)


####### ENGAGEMENT TYPE ###### 
engagement_type = structure[,c('X__1','ENVIRONMENT TYPE','X__2','X__3')]
colnames(engagement_type) <- c("program",'in_school','out_of_school','adult_learning')


idx_to_drop = c(1, which(is.na(engagement_type$program)), which(engagement_type$program %in% partner_names))
engagement_type = engagement_type[-unique(idx_to_drop),]
engagement_type[is.na(engagement_type)] <- 0

tot_programs = nrow(engagement_type)
print(str_c("Total estimated programs: ", nrow(engagement_type)))

# reshape for groups
et_reshape = gather(engagement_type, key = "engagement_type", value = "value",colnames(engagement_type)[!colnames(engagement_type) %in% c("program")])
et_final = et_reshape %>% filter(value==1) %>% group_by(engagement_type) %>% summarize(n=n()) %>% mutate(prop_n = n/sum(n))

et_final$engagement_type[which(et_final$engagement_type == "adult_learning")] <- "Adult Learning"
et_final$engagement_type[which(et_final$engagement_type == "in_school")] <- "In School"
et_final$engagement_type[which(et_final$engagement_type == "out_of_school")] <- "Out of School"


et_pie <- et_final %>% ggplot(mapping=aes(x = '', y=prop_n, fill=engagement_type)) +
  geom_bar(stat="identity") +
  coord_polar(theta = 'y', start = 0) +
  geom_text(mapping=aes(label=str_c(round(prop_n*100),"%"),fontface="bold"), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Engagement Type", title = "Engagement Type of DSEC Partner Programs") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        legend.title = element_text(face="bold"),
        legend.text = element_text(face="bold"),
        plot.title = element_text(face="bold",hjust=0.5, vjust = -10)
        ) +
  scale_fill_manual(values=c(color_palette))

et_pie 

et_bar_counts <- et_final %>% ggplot(mapping=aes(x = engagement_type, y=n,fill=engagement_type)) +
  geom_bar(stat="identity", position = "dodge", show.legend = FALSE) +
  geom_text(mapping=aes(label=n,fontface="bold",vjust=-0.5)) +
  labs(x = 'Engagement Type', y = 'Number of Programs', title = "Engagement Type of DSEC Partner Programs") +
  theme_classic() +
  theme(
        plot.title = element_text(face="bold",hjust=0.5),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")
  ) +
  scale_fill_manual(values=c(color_palette))

et_bar_counts

