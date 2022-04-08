library(readr)
library(psych)
library(reshape)
library(ggplot2)
library(Hmisc)
library(tikzDevice)
library(alluvial)
library(car)
library(dplyr)
library(pgirmess)
library(ca)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(data.table)
library(nnet)
library(stargazer)


survey_data <- read_csv("survey_data.csv", col_types = cols(time_choose_a_number = col_integer(), time_end = col_datetime(format = "%Y-%m-%d-%H-%M"), time_start = col_datetime(format = "%Y-%m-%d-%H-%M")))

time_data <- survey_data[c(47:64,67)]
time_data <- lapply(time_data, as.numeric)
time_data <- as.data.frame(time_data)

opinion_data <- survey_data[c(13:22,24:33,37:46)] 
describe(opinion_data)

spider <- opinion_data$`spider_opinion:1`+opinion_data$`spider_opinion:2`+opinion_data$`spider_opinion:3`+opinion_data$`spider_opinion:4`+opinion_data$`spider_opinion:5`+opinion_data$`spider_opinion:6`+opinion_data$`spider_opinion:7`+opinion_data$`spider_opinion:8`+opinion_data$`spider_opinion:9`+opinion_data$`spider_opinion:10`
bar <- opinion_data$`bar_opinion:1`+opinion_data$`bar_opinion:2`+opinion_data$`bar_opinion:3`+opinion_data$`bar_opinion:4`+opinion_data$`bar_opinion:5`+opinion_data$`bar_opinion:6`+opinion_data$`bar_opinion:7`+opinion_data$`bar_opinion:8`+opinion_data$`bar_opinion:9`+opinion_data$`bar_opinion:10`
plot <- opinion_data$`plot_opinion:1`+opinion_data$`plot_opinion:2`+opinion_data$`plot_opinion:3`+opinion_data$`plot_opinion:4`+opinion_data$`plot_opinion:5`+opinion_data$`plot_opinion:6`+opinion_data$`plot_opinion:7`+opinion_data$`plot_opinion:8`+opinion_data$`plot_opinion:9`+opinion_data$`plot_opinion:10`
spider <- spider/10
bar <- bar/10
plot <- plot/10


# Differences between Spider and Bar Plot

spider_bar <- survey_data[c(11,12,23)] 

spider_bar$ag1 <- as.numeric(spider_bar$`spider_test_1:1`==spider_bar$`bar_test:1`) 
spider_bar$ag2 <- as.numeric(spider_bar$`spider_test_2:1`==spider_bar$`bar_test:1`)
spider_bar$ag[is.na(spider_bar$ag1)] <- spider_bar$ag2[is.na(spider_bar$ag1)] 
spider_bar$ag[is.na(spider_bar$ag)] <- spider_bar$ag1[is.na(spider_bar$ag)] 

table(spider_bar$ag)
table(spider_bar$ag1)
table(spider_bar$ag2)

table(spider_bar$`spider_test_1:1`,spider_bar$`bar_test:1`)
table(spider_bar$`spider_test_2:1`,spider_bar$`bar_test:1`)

table(survey_data$`plot_a_test:1`)
table(survey_data$`plot_b_test:1`)

time_data$time_spider_test_1 <- time_data$time_spider_test_1 / 1000
time_data$time_spider_test_2 <- time_data$time_spider_test_2 / 1000
time_data$time_bar_test <- time_data$time_bar_test / 1000
time_data$time_plot_a_test <- time_data$time_plot_a_test / 1000
time_data$time_plot_b_test <- time_data$time_plot_b_test / 1000

########################## Statistics ######################################## 

mean(time_data$time_spider_test_1, na.rm = TRUE)
mean(time_data$time_spider_test_2, na.rm = TRUE)
mean(time_data$time_bar_test, na.rm = TRUE)

sd(time_data$time_bar_test, na.rm = TRUE)
sd(time_data$time_spider_test_1, na.rm = TRUE)
sd(time_data$time_spider_test_2, na.rm = TRUE)

var.test(time_data$time_spider_test_1,time_data$time_spider_test_2)
var.test(time_data$time_spider_test_1,time_data$time_bar_test)
var.test(time_data$time_spider_test_2,time_data$time_bar_test)

t.test(time_data$time_spider_test_1,time_data$time_spider_test_2, var.equal = FALSE)
t.test(time_data$time_spider_test_1,time_data$time_bar_test, var.equal = TRUE)
t.test(time_data$time_spider_test_2,time_data$time_bar_test, var.equal = TRUE)

##############################################################################  


time_boxplot <-  time_data[c(5,6,9,12,14)] 
time_boxplot <- melt(time_boxplot) 
time_boxplot <- na.omit(time_boxplot)
levels(time_boxplot$variable)[levels(time_boxplot$variable)=="time_spider_test_1"] <- "Open Spider"
levels(time_boxplot$variable)[levels(time_boxplot$variable)=="time_spider_test_2"] <- "Closed Spider"
levels(time_boxplot$variable)[levels(time_boxplot$variable)=="time_bar_test"] <- "Bar"
levels(time_boxplot$variable)[levels(time_boxplot$variable)=="time_plot_a_test"] <- "Map 3 Points"
levels(time_boxplot$variable)[levels(time_boxplot$variable)=="time_plot_b_test"] <- "Map 6 Points"

tikz(file = "scatter_time.tex", width = 5, height = 5)
ggplot(time_boxplot, aes(x=variable, y=value)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Response time (Seconds)") +
  theme_classic()+
  theme(axis.title.x = element_blank())
dev.off()


# Flow Chart Three points vs. Six points

plot_differences$`plot_b_test:1` <- as.numeric(plot_differences$`plot_b_test:1`)
plot_differences$difference = ifelse(plot_differences$`plot_a_test:1`==plot_differences$`plot_b_test:1`,1,0)

plot_differences <- survey_data[c(34,35)]
plot_differences$`plot_a_test:1`[plot_differences$`plot_a_test:1` == 1] <- 5
plot_differences$`plot_a_test:1`[plot_differences$`plot_a_test:1` == 3] <- 6
plot_differences$`plot_b_test:1` <- as.numeric(plot_differences$`plot_b_test:1`)

plot_differences <- as.data.frame(table(plot_differences))
plot_differences$Freq <- as.numeric(plot_differences$Freq)
plot_differences$plot_a_test.1 <- as.numeric(levels(plot_differences$plot_a_test.1))[plot_differences$plot_a_test.1]
plot_differences$plot_b_test.1 <- as.numeric(levels(plot_differences$plot_b_test.1))[plot_differences$plot_b_test.1]
plot_differences$plot_a_test.1[plot_differences$plot_a_test.1 == 5] <- 1
plot_differences$plot_a_test.1[plot_differences$plot_a_test.1 == 6] <- 3

plot_differences$plot_a_test.1<-recode(plot_differences$plot_a_test.1,"1=3;3=1")
plot_differences$plot_b_test.1<-recode(plot_differences$plot_b_test.1,"1=6;2=5;3=4;4=3;5=2;6=1")

plot_differences$plot_a_test.1 <- as.factor(plot_differences$plot_a_test.1)
plot_differences$plot_b_test.1 <- as.factor(plot_differences$plot_b_test.1)
levels(plot_differences$plot_a_test.1) <- c("C","B","A")
levels(plot_differences$plot_b_test.1) <- c("F","E","D","C","B","A")
names(plot_differences)[names(plot_differences)=="plot_a_test.1"] <- "Three Points"
names(plot_differences)[names(plot_differences)=="plot_b_test.1"] <- "Six Points"

tikz(file = "flow_plot.tex", width = 5, height = 5)
alluvial(plot_differences[,1:2], freq=plot_differences$Freq, border=NA, blocks = FALSE, alpha = 0.5)
dev.off()


# Flow Chart Spider-Bar

spider_open_bar <- survey_data[c(11,23)]
spider_closed_bar <- survey_data[c(12,23)]

spider_open_bar$`spider_test_1:1` <- as.numeric(spider_open_bar$`spider_test_1:1`)
spider_open_bar$`bar_test:1` <- as.numeric(spider_open_bar$`bar_test:1`)
spider_closed_bar$`spider_test_2:1` <- as.numeric(spider_closed_bar$`spider_test_2:1`)
spider_closed_bar$`bar_test:1` <- as.numeric(spider_closed_bar$`bar_test:1`)

spider_open_bar <- as.data.frame(table(spider_open_bar))
spider_closed_bar <- as.data.frame(table(spider_closed_bar))

spider_open_bar$spider_test_1.1 <- as.numeric(levels(spider_open_bar$spider_test_1.1))[spider_open_bar$spider_test_1.1]
spider_open_bar$bar_test.1 <- as.numeric(levels(spider_open_bar$bar_test.1))[spider_open_bar$bar_test.1]
spider_closed_bar$spider_test_2.1 <- as.numeric(levels(spider_closed_bar$spider_test_2.1))[spider_closed_bar$spider_test_2.1]
spider_closed_bar$bar_test.1 <- as.numeric(levels(spider_closed_bar$bar_test.1))[spider_closed_bar$bar_test.1]

spider_open_bar$spider_test_1.1<-recode(spider_open_bar$spider_test_1.1,"1=3;3=1")
spider_open_bar$bar_test.1<-recode(spider_open_bar$bar_test.1,"1=3;3=1")
spider_closed_bar$spider_test_2.1<-recode(spider_closed_bar$spider_test_2.1,"1=3;3=1")
spider_closed_bar$bar_test.1<-recode(spider_closed_bar$bar_test.1,"1=3;3=1")

spider_open_bar$spider_test_1.1<-as.factor(spider_open_bar$spider_test_1.1)
spider_open_bar$bar_test.1<-as.factor(spider_open_bar$bar_test.1)
spider_closed_bar$spider_test_2.1<-as.factor(spider_closed_bar$spider_test_2.1)
spider_closed_bar$bar_test.1<-as.factor(spider_closed_bar$bar_test.1)

levels(spider_open_bar$spider_test_1.1) <- c("III","II","I")
levels(spider_open_bar$bar_test.1) <- c("III","II","I")
levels(spider_closed_bar$spider_test_2.1) <- c("III","II","I")
levels(spider_closed_bar$bar_test.1) <- c("III","II","I")

names(spider_open_bar)[names(spider_open_bar)=="spider_test_1.1"] <- "Open Spider"
names(spider_open_bar)[names(spider_open_bar)=="bar_test.1"] <- "Bar Graph"
names(spider_closed_bar)[names(spider_closed_bar)=="spider_test_2.1"] <- "Closed Spider"
names(spider_closed_bar)[names(spider_closed_bar)=="bar_test.1"] <- "Bar Graph"

tikz(file = "open_spider_bar.tex", width = 5, height = 5)
alluvial(spider_open_bar[,1:2], freq=spider_open_bar$Freq, border=NA, blocks = FALSE, alpha = 0.5)
dev.off()

tikz(file = "closed_spider_bar.tex", width = 5, height = 5)
alluvial(spider_closed_bar[,1:2], freq=spider_closed_bar$Freq, border=NA, blocks = FALSE, alpha = 0.5)
dev.off()


# Response time Six and Three Point Plots

plot_time <- survey_data[c(58,60)]

plot_time$time_plot_a_test <- as.numeric(plot_time$time_plot_a_test)
plot_time$time_plot_b_test <- as.numeric(plot_time$time_plot_b_test)
plot_time$time_plot_a_test <- plot_time$time_plot_a_test/1000
plot_time$time_plot_b_test <- plot_time$time_plot_b_test/1000

mean(plot_time$time_plot_a_test, na.rm = TRUE)
sd(plot_time$time_plot_a_test, na.rm = TRUE)

mean(plot_time$time_plot_b_test, na.rm = TRUE)
sd(plot_time$time_plot_b_test, na.rm = TRUE)

var.test(plot_time$time_plot_a_test,plot_time$time_plot_b_test)
t.test(plot_time$time_plot_a_test,plot_time$time_plot_b_test,var.equal=TRUE)

time_plot_a_test <- as.data.frame(as.numeric(plot_time$time_plot_a_test))
names(time_plot_a_test)[names(time_plot_a_test)=="as.numeric(plot_time$time_plot_a_test)"] <- "value"
time_plot_a_test$var <- "Three Points"
time_plot_b_test <- as.data.frame(as.numeric(plot_time$time_plot_b_test))
names(time_plot_b_test)[names(time_plot_b_test)=="as.numeric(plot_time$time_plot_b_test)"] <- "value"
time_plot_b_test$var <- "Six Points"
rm(plot_time)
plot_time <- rbind(time_plot_a_test,time_plot_b_test)
plot_time$var <- as.factor(plot_time$var)

tikz(file = "plot_response_time.tex", width = 5, height = 5)
ggplot(plot_time, aes(x=var, y=value)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Response time (Seconds)") +
  scale_x_discrete(limits=c("Three Points", "Six Points"))+
  scale_y_continuous(limits=c(0, 80), expand = c(0,0), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80))+
  theme_classic()+
  theme(axis.title.x = element_blank())
dev.off()


# Visual and Political Knowledge

# Spider Graphs

knowledge_spider <- survey_data[c(4:9,11,12)]
knowledge_spider[1:8] <- lapply(knowledge_spider[1:8], as.numeric)
knowledge_spider$visual_skills <- knowledge_spider$`visual_test:1` + knowledge_spider$`visual_test:2` + knowledge_spider$`visual_test:3` + knowledge_spider$`visual_test:4` + knowledge_spider$`visual_test:5`
knowledge_spider$visual_skills <- knowledge_spider$visual_skills/5
knowledge_spider_1 <- knowledge_spider[c(7,9)]
knowledge_spider_2 <- knowledge_spider[c(8,9)]
knowledge_spider_1$type <- "Open"
knowledge_spider_2$type <- "Closed"
knowledge_spider_1 <- knowledge_spider_1[complete.cases(knowledge_spider_1), ]
knowledge_spider_2 <- knowledge_spider_2[complete.cases(knowledge_spider_2), ]
names(knowledge_spider_1)[names(knowledge_spider_1)=="spider_test_1:1"] <- "spider"
names(knowledge_spider_2)[names(knowledge_spider_2)=="spider_test_2:1"] <- "spider"
knowledge_spider <- rbind(knowledge_spider_1,knowledge_spider_2)
knowledge_spider$spider <- as.factor(knowledge_spider$spider)
knowledge_spider$type <- as.factor(knowledge_spider$type)
levels(knowledge_spider$spider) <- c("I","II","III")

tikz(file = "visual_skills_spider.tex", width = 5, height = 5)
ggplot(knowledge_spider, aes(x=spider, y=visual_skills)) +
geom_jitter(aes(color = type),position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),  size = 1.2) +
  stat_summary(aes(color = type),fun.data="mean_sdl",  fun.args = list(mult=1), geom = "pointrange",  size = 0.4, position = position_dodge(0.8))+
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

knowledge_spider <- survey_data[c(4:9,11,12)]
knowledge_spider_1 <- knowledge_spider[c(7,6)]
knowledge_spider_2 <- knowledge_spider[c(8,6)]
knowledge_spider_1$type <- "Open"
knowledge_spider_2$type <- "Closed"
knowledge_spider_1 <- knowledge_spider_1[complete.cases(knowledge_spider_1), ]
knowledge_spider_2 <- knowledge_spider_2[complete.cases(knowledge_spider_2), ]
names(knowledge_spider_1)[names(knowledge_spider_1)=="spider_test_1:1"] <- "spider"
names(knowledge_spider_2)[names(knowledge_spider_2)=="spider_test_2:1"] <- "spider"
knowledge_spider <- rbind(knowledge_spider_1,knowledge_spider_2)
knowledge_spider$spider <- as.factor(knowledge_spider$spider)
knowledge_spider$type <- as.factor(knowledge_spider$type)
levels(knowledge_spider$spider) <- c("I","II","III")

tikz(file = "political_skills_spider.tex", width = 5, height = 5)
ggplot(knowledge_spider, aes(x=spider, y=`political_test:1`)) +
  geom_jitter(aes(color = type),position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),  size = 1.2) +
  stat_summary(aes(color = type),fun.data="mean_sdl",  fun.args = list(mult=1), geom = "pointrange",  size = 0.4, position = position_dodge(0.8))+
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))+
  ylab("Political Interest") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

# Switching bar and spider (no distinction open vs. closed) - not used

knowledge_spider_bar <- survey_data[c(4:9,11,12,23)]
knowledge_spider_bar [1:9] <- lapply(knowledge_spider_bar[1:9], as.numeric)
knowledge_spider_bar$visual_skills <- knowledge_spider_bar$`visual_test:1` + knowledge_spider_bar$`visual_test:2` + knowledge_spider_bar$`visual_test:3` + knowledge_spider_bar$`visual_test:4` + knowledge_spider_bar$`visual_test:5`
knowledge_spider_bar$visual_skills <- knowledge_spider_bar$visual_skills/5
knowledge_spider_bar[is.na(knowledge_spider_bar)] <- 0
knowledge_spider_bar$spider <- knowledge_spider_bar$`spider_test_1:1` + knowledge_spider_bar$`spider_test_2:1`
knowledge_spider_bar <- knowledge_spider_bar[c(-1:-5,-7,-8)]
names(knowledge_spider_bar)[names(knowledge_spider_bar)=="political_test:1"] <- "political_interest"
names(knowledge_spider_bar)[names(knowledge_spider_bar)=="bar_test:1"] <- "bar"
knowledge_spider_bar$switch <- knowledge_spider_bar$bar != knowledge_spider_bar$spider
knowledge_spider_bar$switch <- as.factor(knowledge_spider_bar$switch)
levels(knowledge_spider_bar$switch) <- c("No Switch","Switch")

ggplot(knowledge_spider_bar, aes(x=switch, y=visual_skills)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())

# Switching bar and spider (distinction open vs. closed)

knowledge_spider_bar <- survey_data[c(4:9,11,12,23)]
knowledge_spider_bar [1:9] <- lapply(knowledge_spider_bar[1:9], as.numeric)
knowledge_spider_bar$visual_skills <- knowledge_spider_bar$`visual_test:1` + knowledge_spider_bar$`visual_test:2` + knowledge_spider_bar$`visual_test:3` + knowledge_spider_bar$`visual_test:4` + knowledge_spider_bar$`visual_test:5`
knowledge_spider_bar$visual_skills <- knowledge_spider_bar$visual_skills/5
knowledge_spider_bar <- knowledge_spider_bar[c(-1:-5)]
names(knowledge_spider_bar)[names(knowledge_spider_bar)=="political_test:1"] <- "political_interest"
names(knowledge_spider_bar)[names(knowledge_spider_bar)=="bar_test:1"] <- "bar"
knowledge_spider_bar_1 <- knowledge_spider_bar[c(1,3,4,5)]
knowledge_spider_bar_2 <- knowledge_spider_bar[c(1,2,4,5)]

names(knowledge_spider_bar_1)[names(knowledge_spider_bar_1)=="spider_test_2:1"] <- "spider"
names(knowledge_spider_bar_2)[names(knowledge_spider_bar_2)=="spider_test_1:1"] <- "spider"

knowledge_spider_bar_1 <- knowledge_spider_bar_1[complete.cases(knowledge_spider_bar_1), ]
knowledge_spider_bar_2 <- knowledge_spider_bar_2[complete.cases(knowledge_spider_bar_2), ]

knowledge_spider_bar_1$switch <- knowledge_spider_bar_1$bar != knowledge_spider_bar_1$spider
knowledge_spider_bar_2$switch <- knowledge_spider_bar_2$bar != knowledge_spider_bar_2$spider
knowledge_spider_bar_1$type <- "Open"
knowledge_spider_bar_2$type <- "Closed"
knowledge_spider_bar <- rbind(knowledge_spider_bar_1,knowledge_spider_bar_2)
knowledge_spider_bar <- knowledge_spider_bar[c(1,4,5,6)]
knowledge_spider_bar$switch <- as.factor(knowledge_spider_bar$switch)
knowledge_spider_bar$type <- as.factor(knowledge_spider_bar$type)
levels(knowledge_spider_bar$switch) <- c("No Switch","Switch")

tikz(file = "visual_skills_bar_spider.tex", width = 5, height = 5)
ggplot(knowledge_spider_bar, aes(x=type, y=visual_skills)) +
  geom_jitter(aes(color = switch),position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),  size = 1.2) +
  stat_summary(aes(color = switch),fun.data="mean_sdl",  fun.args = list(mult=1), geom = "pointrange",  size = 0.4, position = position_dodge(0.8))+
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

tikz(file = "political_skills_bar_spider.tex", width = 5, height = 5)
ggplot(knowledge_spider_bar, aes(x=type, y=political_interest)) +
  geom_jitter(aes(color = switch),position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),  size = 1.2) +
  stat_summary(aes(color = switch),fun.data="mean_sdl",  fun.args = list(mult=1), geom = "pointrange",  size = 0.4, position = position_dodge(0.8))+
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))+
  ylab("Political Interest") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

# Knowledge and Plots

knowledge_plot <- survey_data[c(4:9,34,35)]
knowledge_plot[1:8] <- lapply(knowledge_plot[1:8], as.numeric)
knowledge_plot$visual_skills <- knowledge_plot$`visual_test:1` + knowledge_plot$`visual_test:2` + knowledge_plot$`visual_test:3` + knowledge_plot$`visual_test:4` + knowledge_plot$`visual_test:5`
knowledge_plot$visual_skills <- knowledge_plot$visual_skills/5
knowledge_plot <- knowledge_plot[c(6:9)]

knowledge_plot$plot_a_factor <- as.factor(knowledge_plot$`plot_a_test:1`)
knowledge_plot$plot_b_factor <- as.factor(knowledge_plot$`plot_b_test:1`)

levels(knowledge_plot$plot_a_factor) <- c("A","B","C")
levels(knowledge_plot$plot_b_factor) <- c("A","B","C","D","E","F")

tikz(file = "visual_skills_plot_a.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=plot_a_factor, y=visual_skills)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

tikz(file = "political_skills_plot_a.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=plot_a_factor, y=`political_test:1`)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Political Interest") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

tikz(file = "visual_skills_plot_b.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=plot_b_factor, y=visual_skills)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

tikz(file = "political_skills_plot_b.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=plot_b_factor, y=`political_test:1`)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Political Interest") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

knowledge_plot <- knowledge_plot[c(1:4)]
knowledge_plot$`plot_a_test:1`[knowledge_plot$`plot_a_test:1` == 1] <- 5
knowledge_plot$`plot_a_test:1`[knowledge_plot$`plot_a_test:1` == 3] <- 6
knowledge_plot$switch <- knowledge_plot$`plot_a_test:1` != knowledge_plot$`plot_b_test:1`
knowledge_plot$switch <- as.factor(knowledge_plot$switch)
levels(knowledge_plot$switch) <- c("No Switch","Switch")

tikz(file = "visual_skills_plot_switch.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=switch, y=visual_skills)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()

tikz(file = "political_skills_plot_switch.tex", width = 5, height = 5)
ggplot(knowledge_plot, aes(x=switch, y=`political_test:1`)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Political Interest") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())
dev.off()



# Opinions

opinions <- survey_data[c(13:22,24:33,37:46)]
opinions[1:30] <- lapply(opinions[1:30], as.numeric)
opinions$spider <- (opinions$`spider_opinion:1` + opinions$`spider_opinion:2` + opinions$`spider_opinion:3` + opinions$`spider_opinion:4` + opinions$`spider_opinion:5` + opinions$`spider_opinion:6` + opinions$`spider_opinion:7` + opinions$`spider_opinion:8` + opinions$`spider_opinion:9` + opinions$`spider_opinion:10`)/10
opinions$bar <- (opinions$`bar_opinion:1` + opinions$`bar_opinion:2` + opinions$`bar_opinion:3` + opinions$`bar_opinion:4` + opinions$`bar_opinion:5` + opinions$`bar_opinion:6` + opinions$`bar_opinion:7` + opinions$`bar_opinion:8` + opinions$`bar_opinion:9` + opinions$`bar_opinion:10`)/10
opinions$plot <- (opinions$`plot_opinion:1` + opinions$`plot_opinion:2` + opinions$`plot_opinion:3` + opinions$`plot_opinion:4` + opinions$`plot_opinion:5` + opinions$`plot_opinion:6` + opinions$`plot_opinion:7` + opinions$`plot_opinion:8` + opinions$`plot_opinion:9` + opinions$`plot_opinion:10`)/10 

opinions_anova <- opinions[c(31,32,33)]
opinions_anova <- as.data.frame(opinions_anova)
opinions_anova <- melt(opinions_anova)

group_by(opinions_anova, variable) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

leveneTest(value ~ variable, data = opinions_anova)
aov_opinions <- aov(value ~ variable, data = opinions_anova)
summary(aov_opinions)
TukeyHSD(aov_opinions)

plot(aov_opinions, 1)
plot(aov_opinions, 2)

aov_residuals <- residuals(object = aov_opinions)
shapiro.test(aov_residuals)

# Differences between the individual questions

opinions <- survey_data[c(13:22,24:33,37:46)]
opinions[1:30] <- lapply(opinions[1:30], as.numeric)

opinion_1 <- opinions[c(1,11,21)]
opinion_2 <- opinions[c(2,12,22)]
opinion_3 <- opinions[c(3,13,23)]
opinion_4 <- opinions[c(4,14,24)]
opinion_5 <- opinions[c(5,15,25)]
opinion_6 <- opinions[c(6,16,26)]
opinion_7 <- opinions[c(7,17,27)]
opinion_8 <- opinions[c(8,18,28)]
opinion_9 <- opinions[c(9,19,29)]
opinion_10 <- opinions[c(10,20,30)]

opinion_1 <- as.data.frame(opinion_1)
opinion_2 <- as.data.frame(opinion_2)
opinion_3 <- as.data.frame(opinion_3)
opinion_4 <- as.data.frame(opinion_4)
opinion_5 <- as.data.frame(opinion_5)
opinion_6 <- as.data.frame(opinion_6)
opinion_7 <- as.data.frame(opinion_7)
opinion_8 <- as.data.frame(opinion_8)
opinion_9 <- as.data.frame(opinion_9)
opinion_10 <- as.data.frame(opinion_10)

dfs <- c("opinion_1", "opinion_2", "opinion_3","opinion_4", "opinion_5", "opinion_6","opinion_7", "opinion_8", "opinion_9","opinion_10")

for(df in dfs) {
  df.tmp <- get(df)
  names(df.tmp) <- c("spider", "bar", "plot" ) 
  assign(df, df.tmp)
}

opinion_1 <- melt(opinion_1)
opinion_2 <- melt(opinion_2)
opinion_3 <- melt(opinion_3)
opinion_4 <- melt(opinion_4)
opinion_5 <- melt(opinion_5)
opinion_6 <- melt(opinion_6)
opinion_7 <- melt(opinion_7)
opinion_8 <- melt(opinion_8)
opinion_9 <- melt(opinion_9)
opinion_10 <- melt(opinion_10)

kruskal.test(value ~ variable, data = opinion_1)
kruskal.test(value ~ variable, data = opinion_2)
kruskal.test(value ~ variable, data = opinion_3)
kruskal.test(value ~ variable, data = opinion_4)
kruskal.test(value ~ variable, data = opinion_5)
kruskal.test(value ~ variable, data = opinion_6)
kruskal.test(value ~ variable, data = opinion_7)
kruskal.test(value ~ variable, data = opinion_8)
kruskal.test(value ~ variable, data = opinion_9)
kruskal.test(value ~ variable, data = opinion_10)

kruskalmc(value ~ variable, data = opinion_1, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_2, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_3, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_4, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_5, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_6, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_7, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_8, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_9, cont='two-tailed')
kruskalmc(value ~ variable, data = opinion_10, cont='two-tailed')


# Relation between city-block and opinion

describe_data <- survey_data[c(4:8,34,35,36)]
describe_data [1:5] <- lapply(describe_data[1:5], as.numeric)
describe_data$visual_skills <- describe_data$`visual_test:1` + describe_data$`visual_test:2` + describe_data$`visual_test:3` + describe_data$`visual_test:4` + describe_data$`visual_test:5`
describe_data$visual_skills <- describe_data$visual_skills/5
describe_data <- describe_data[c(-1:-5)]

describe_data$plot_a_desc <- NA
describe_data$plot_b_desc <- NA

describe_data$plot_a_desc[describe_data $`plot_a_test:1` == 2] <- "city"
describe_data$plot_a_desc[describe_data $`plot_a_test:1` == 1] <- "euclidean"
describe_data$plot_b_desc[describe_data $`plot_b_test:1` == 2] <- "city"
describe_data$plot_b_desc[describe_data $`plot_b_test:1` == 5] <- "euclidean"

describe_data$plot_c_desc_relab <- NA
describe_data$plot_c_desc_pointgraph <- NA

describe_data$plot_c_desc_relab <- c("absolute","relative","relative","relative","absolute",NA,"relative","relative","relative","absolute",NA,"absolute","relative","relative","absolute","relative","relative","relative","relative","relative","relative",NA,"relative","relative","relative","relative","relative","absolute","relative","relative","relative","relative","relative","relative","relative","relative","relative","relative","relative","relative","absolute","relative","relative","relative","absolute","absolute","absolute","relative","absolute","absolute","relative","relative","relative","relative","relative","relative","relative","relative","absolute","absolute","relative","absolute","relative","relative","relative","absolute","relative")                                      
describe_data$plot_c_desc_pointgraph <- c("whole","point","whole","whole","whole",NA,"whole","whole","whole","point",NA,"whole","point","whole","whole","point","point","whole","point","whole","point",NA,"whole","point","whole","point","point","whole","point","whole","whole","whole","whole","point","point","whole","point","whole","whole","point","whole","whole","point","whole","whole","point","point","point","whole","point","whole","whole","point","point","point","point","whole","whole","point","whole","point","whole","point","point","point","whole","point")
  
describe_data[5:8] <- lapply(describe_data[5:8], as.factor)
describe_data[1:2] <- lapply(describe_data[1:2], as.numeric)

table(describe_data$plot_c_desc_relab,describe_data$plot_a_desc)
table(describe_data$plot_c_desc_pointgraph,describe_data$plot_a_desc)
table(describe_data$plot_c_desc_relab,describe_data$plot_b_desc)
table(describe_data$plot_c_desc_pointgraph,describe_data$plot_b_desc)

chisq.test(describe_data$plot_c_desc_relab,describe_data$plot_a_desc)
chisq.test(describe_data$plot_c_desc_pointgraph,describe_data$plot_a_desc)
chisq.test(describe_data$plot_c_desc_relab,describe_data$plot_b_desc)
chisq.test(describe_data$plot_c_desc_pointgraph,describe_data$plot_b_desc)

ggplot(describe_data, aes(x=plot_c_desc_pointgraph, y=visual_skills)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), geom="pointrange", color = "red")+
  ylab("Visual Skills Scale") +
  scale_y_continuous(limits=c(0, 7.5), expand = c(0,0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title=element_blank())

# Correlation visual knowledge and political interest

interest <- survey_data[c(4:9)]
interest[1:6] <- lapply(interest[1:6], as.numeric)
interest$visual_skills <- interest$`visual_test:1` + interest$`visual_test:2` + interest$`visual_test:3` + interest$`visual_test:4` + interest$`visual_test:5`
interest$visual_skills <- interest$visual_skills/5
interest <- interest[c(-1:-5)]
names(interest)[names(interest)=="political_test:1"] <- "political_interest"
cor.test(interest$visual_skills,interest$political_interest,method = "kendall")

sd(interest$political_interest)
mean(interest$political_interest)
sd(interest$visual_skills)
mean(interest$visual_skills)

# Total time

hist(survey_data$time_total_minutes)
summary(survey_data$time_total_minutes)
boxplot(survey_data$time_total_minutes)



