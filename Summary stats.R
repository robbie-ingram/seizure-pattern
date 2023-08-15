# import libraries
library(dplyr)
library(ggplot2)
library(magrittr)

# create theme
theme <- theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
               panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
               panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
               panel.border=element_blank(), #gets rid of square going around the entire graph
               axis.line = element_line(colour = 'black', linewidth = 0.5),#sets the axis line size
               axis.ticks=element_line(colour = 'black', linewidth = 0.5), #sets the tick lines
               #axis.ticks.x=element_blank(), #sets the tick lines
               axis.title.x = element_text(size=16, color="black", face="bold"), #size of x-axis title
               axis.title.y = element_text(size=16, color="black", face="bold"), #size of y-axis title
               axis.text.x = element_text(size=16, color="black"),
               axis.text.y = element_text(size=14, color="black"), #size of y-axis text
               legend.position = "right",
               strip.text = element_text(size=16, color = "black", face="bold"),
               strip.background = element_rect(fill = "white"),
               plot.title = element_text(size=16, color="black", face="bold", hjust = 0.5))

# read in data
df <- read.csv("/Users/robbiei2/Library/CloudStorage/Box-Box/Robbie's Shared Folder/Projects/Seizure Pattern/Data Tables/eeg_in_progress.csv")
head(df)

# data cleaning and preparation
df_females <- df %>% 
                  select(start_hr, duration_sec, mouse_id, sex, injection, cycle_group, cycle_stage, date) %>%
                    subset(sex == "female" & duration_sec > 5 & duration_sec < 200)

df_duration <- df_females %>%
  group_by(start_hr, mouse_id, sex, injection, cycle_group, cycle_stage, date) %>%
  summarise(mean_duration_sec = mean(duration_sec)) %>%
  as.data.frame()

df_count <- df_females %>%
  select(start_hr, mouse_id, sex, injection, cycle_group, cycle_stage, date) %>%
  group_by(start_hr, mouse_id, sex, injection, cycle_group, cycle_stage, date) %>%
  count(name = "seizure_count") %>%
  as.data.frame()

df_percent <- df_females %>%
  group_by(start_hr, mouse_id, sex, injection, cycle_group, cycle_stage, date) %>%
  summarise(summed_seizure_duration = sum(duration_sec)) %>%
  as.data.frame() 

df_percent$percent_time <- df_percent$summed_seizure_duration / 3600 * 100

# merge the dataframes into one single dataframe
df_merged <- df_duration %>%
  left_join(df_count, by = c("start_hr", "mouse_id", "sex", "injection", "cycle_group", "cycle_stage", "date")) %>%
  left_join(df_percent[, c("start_hr", "mouse_id", "sex", "injection", "cycle_group", "cycle_stage", "date", "percent_time")], 
            by = c("start_hr", "mouse_id", "sex", "injection", "cycle_group", "cycle_stage", "date"))

df_merged$injection <- factor(df_merged$injection, levels = c("KA_left", "KA_right"))
df_merged$cycle_stage <- factor(df_merged$cycle_stage, levels = c("proestrus", "estrus",
                                                                  "diestrus", "unlabeled"))

# plot data
ggplot(df_merged, aes(x=start_hr, y=mean_duration_sec, group=cycle_group, color=cycle_group)) +
  stat_summary(fun="mean", geom='line') + 
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = "identity", width = 0,
               size=0.75) +
  scale_color_manual(values=c("red", "blue")) +
  ylab("mean_seizure_duration") +
  theme +
  scale_x_discrete(breaks=c(0,4,8,12,16,20,24)) +
  annotate("rect", xmin = 0, xmax = 5.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey") +
  annotate("rect", xmin = 19.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey")
  facet_grid(cycle_stage~injection)

ggplot(df_merged, aes(x=start_hr, y=seizure_count, group=cycle_group, color=cycle_group)) +
  stat_summary(fun="mean", geom='line') + 
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = "identity", width = 0,
               size=0.75) +
  scale_color_manual(values=c("red", "blue")) +
  ylab("seizure_count") +
  theme +
  scale_x_discrete(breaks=c(0,4,8,12,16,20,24)) +
  annotate("rect", xmin = 0, xmax = 5.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey") +
  annotate("rect", xmin = 19.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey")
  facet_grid(cycle_stage~injection)

ggplot(df_merged, aes(x=start_hr, y=percent_time, group=cycle_group, color=cycle_group)) +
  stat_summary(fun="mean", geom='line') + 
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = "identity", width = 0,
               size=0.75) +
  scale_color_manual(values=c("red", "blue")) +
  ylab("percent_time") +
  theme +
  scale_x_discrete(breaks=c(0,4,8,12,16,20,24)) +
  annotate("rect", xmin = 0, xmax = 5.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey") +
  annotate("rect", xmin = 19.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey") +
  facet_grid(.~cycle_stage)

# modify df to average across all days
df_summary <- df_merged %>%
  group_by(start_hr, mouse_id, injection, cycle_group, cycle_stage) %>%
  summarise(
    mean_duration_sec = mean(mean_duration_sec),
    mean_seizure_count = mean(seizure_count),
    mean_percent_time = mean(percent_time)) %>%
  as.data.frame()
  

# write merged_df to csv file
write.csv(df_summary, file = "/Users/robbiei2/Library/CloudStorage/Box-Box/Robbie's Shared Folder/Projects/Seizure Pattern/Data Tables/df_summary.csv", row.names= FALSE)

head(df_summary)



