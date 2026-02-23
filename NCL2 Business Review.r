##LOAD LIBRARIES##
############################################################################################################################################
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
install.packages("dplyr")
installed.packages("lubridate")
install.packages("openxlsx")
install.packages("gghighlight")
install.packages("scales")
############################################################################################################################################

library("tidyr")
library("ggplot2")
library("skimr")
library("dplyr")
library("lubridate")
library("openxlsx")
library("scales")
library("hrbrthemes")
library("patchwork")

#Formatting and Standardising!
###########################################################################################################################################

ncl2_oob <- read.csv("C:/Users/aboyadec/Documents/Business Review/oob.csv")
ncl2_trb <- read.csv("C:/Users/aboyadec/Documents/Business Review/trb.csv")
ncl2_late_slams <- read.csv("C:/Users/aboyadec/Documents/Business Review/late_slams.csv")
ncl2_tso <- read.csv("C:/Users/aboyadec/Documents/Business Review/tso.csv")
ncl2_oob_rc <- read.csv("C:/Users/aboyadec/Documents/Business Review/oob_root_cause.csv")

colnames(ncl2_oob)
colnames(ncl2_trb)
colnames(ncl2_late_slams)
colnames(ncl2_tso)
colnames(ncl2_oob_rc)

colnames(ncl2_oob) <- c("fc", "region",'week',"oob")
colnames(ncl2_trb) <- c("fc","hub","region","target","week_number", "week", "date","trb_hours", "trb_hours_last_year","x_yoy")
colnames(ncl2_late_slams) <- c("fc","hub2","region","target_dpmo", "week_number", "week", "date", "late_slams","dpmo", "vs_target", "wow","yoy")
colnames(ncl2_tso) <- c("fc","hub","region","target_dpmo", "week_number", "week", "full_date", "units","dpmo", "vs_target","yoy")
colnames(ncl2_oob_rc) <- c("fc","start","end","pp","family","family.1", "root_cause","explanation","oob_bps")

View(ncl2_oob)
View(ncl2_trb)
View(ncl2_late_slams)
View(ncl2_tso)
View(ncl2_oob_rc)

View(ncl2_oob_wide)
head(ncl2_oob_total)
################################################################################################################################################################
##CREAT TABLES##
################################################################################################################################################################
## FOR OOB ##
#ncl2_oob <- separate(ncl2_oob, snapshot_local, c("date","time"), sep=" ") columns do not have dates

#ncl2_oob$date <- ymd(ncl2_oob$date) columns do not have dates

#ncl2_oob$weekday <- weekdays(ncl2_oob$date) columns do not have dates

ncl2_oob_wide <- ncl2_oob %>%
pivot_wider(
    names_from = week,
    values_from = oob
  )


ncl2_oob_wide <- ncl2_oob_wide %>% select(fc, all_of(as.character(sort(as.integer(setdiff(names(.),"fc"))))))


ncl2_oob_total <- ncl2_oob %>% 
group_by(fc)%>%
summarise(avg_oob = mean(oob, na.rm = TRUE))

ncl2_oob$oob <- percent(ncl2_oob$oob, accuracy=1)
ncl2_oob_wide[2:5] <- sapply(ncl2_oob_wide[2:5], function(x) percent(x, accuracy=1))
ncl2_oob_total$avg_oob <- percent(ncl2_oob_total$avg_oob, accuracy=1)


write.xlsx( 
  list( 
    dashboard = ncl2_oob, 
    wide =  ncl2_oob_wide, 
    total = ncl2_oob_total
    ), file = "ncl2_oob2.xlsx")

################################################################################################################################################################



################################################################################################################################################################
## FOR TRB ##
ncl2_trb <- separate(ncl2_trb, date,c("date","time"), sep=" ")

ncl2_trb$date <- ymd(ncl2_trb$date)

ncl2_trb$weekday <- weekdays(ncl2_trb$date)

# Fix unnamed columns by giving them placeholder names
colnames(ncl2_trb)[is.na(colnames(ncl2_trb)) | colnames(ncl2_trb) == ""] <-
  paste0("temp_col_", seq_along(which(is.na(colnames(ncl2_trb)) | colnames(ncl2_trb) == "")))

# Now proceed with your transformation
ncl2_trb_wide <- ncl2_trb %>%
  mutate(trb_hours = as.numeric(trb_hours)) %>%
  filter(!is.na(trb_hours), !is.na(week_number)) %>%
  select(fc, week_number, trb_hours) %>%
  pivot_wider(
    names_from = week_number,
    values_from = trb_hours,
    values_fill = 0,
    values_fn = sum
  )

ncl2_trb_wide <- ncl2_trb_wide %>% select(fc, all_of(as.character(sort(as.integer(setdiff(names(.),"fc"))))))


ncl2_trb_total <- ncl2_trb %>% 
group_by(fc)%>%
summarise(total_trb_hrs = sum(trb_hours, na.rm = TRUE))

write.xlsx( 
  list( 
    dashboard = ncl2_trb, 
    wide = ncl2_trb_wide,
    total = ncl2_trb_total
    ), file = "ncl2_trb.xlsx")
################################################################################################################################################################


################################################################################################################################################################
## FOR LATE SLAMS ##
ncl2_late_slams <- separate(ncl2_late_slams, balancedate,c("date","time"), sep="T")

ncl2_late_slams$date <- ymd(ncl2_late_slams$date)
ncl2_late_slams$weekday <- weekdays(ncl2_late_slams$date)

# Fix unnamed columns by giving them placeholder names
colnames(ncl2_late_slams)[is.na(colnames(ncl2_late_slams)) | colnames(ncl2_late_slams) == ""] <-
  paste0("temp_col_", seq_along(which(is.na(colnames(ncl2_late_slams)) | colnames(ncl2_late_slams) == "")))

# Now proceed with your transformation
ncl2_late_slams_wide <- ncl2_late_slams %>%
  mutate(late_slams = as.numeric(late_slams)) %>%
  filter(!is.na(late_slams), !is.na(week_number)) %>%
  select(fc, week_number, late_slams) %>%
  pivot_wider(
    names_from = week_number,
    values_from = late_slams,
    values_fill = 0,
    values_fn = sum
  )

  ncl2_late_slams_wide <- ncl2_late_slams_wide %>% select(fc, all_of(as.character(sort(as.integer(setdiff(names(.),"fc"))))))

ncl2_late_slams_total <- ncl2_late_slams %>% 
group_by(fc)%>%
summarise(total_late_slams = sum(late_slams, na.rm = TRUE))

write.xlsx( 
  list( 
    dashboard = ncl2_late_slams, 
    wide = ncl2_late_slams_wide,
    total = ncl2_late_slams_total
    ), file = "ncl2_late_slams.xlsx")
################################################################################################################################################################

################################################################################################################################################################
## FOR TSO ##
ncl2_tso <- separate(ncl2_tso, full_date,c("date","time"), sep=" ")

ncl2_tso$date <- ymd(ncl2_tso$date)
ncl2_tso$weekday <- weekdays(ncl2_tso$date)

# Fix unnamed columns by giving them placeholder names
colnames(ncl2_tso)[is.na(colnames(ncl2_tso)) | colnames(ncl2_tso) == ""] <-
  paste0("temp_col_", seq_along(which(is.na(colnames(ncl2_tso)) | colnames(ncl2_tso) == "")))

# Now proceed with your transformation
ncl2_tso_wide <- ncl2_tso %>%
  mutate(late_slams = as.numeric(units)) %>%
  filter(!is.na(units), !is.na(week_number)) %>%
  select(fc, week_number, units) %>%
  pivot_wider(
    names_from = week_number,
    values_from = units,
    values_fill = 0,
    values_fn = sum
  )

ncl2_tso_wide <- ncl2_tso_wide %>% select(fc, all_of(as.character(sort(as.integer(setdiff(names(.),"fc"))))))


  ncl2_tso_total <- ncl2_tso %>% 
group_by(fc)%>%
summarise(total_tso_cancellations = sum(units, na.rm = TRUE))

write.xlsx( 
  list( 
    dashboard = ncl2_tso, 
    wide = ncl2_tso_wide,
    total = ncl2_tso_total
    ), file = "ncl2_tso.xlsx")
################################################################################################################################################################
##OOB ROOT CAUSE ##
ncl2_oob_rc <- separate(ncl2_oob_rc, start, c("start_date","start_time"), sep=" ")
ncl2_oob_rc <- separate(ncl2_oob_rc, end, c("end_date","end_time"), sep=" ")

ncl2_oob_rc$start_date <- ymd(ncl2_oob_rc$start_date)
ncl2_oob_rc$end_date <- ymd(ncl2_oob_rc$end_date)


ncl2_oob_rc$start_weekday <- weekdays(ncl2_oob_rc$start_date)
ncl2_oob_rc$end_weekday <- weekdays(ncl2_oob_rc$end_date)


ncl2_oob_rc_pp <- ncl2_oob_rc %>%
  group_by(pp, root_cause) %>%
  summarise(bps = sum(oob_bps, na.rm = TRUE), .groups = 'drop')


ncl2_oob_ttl_rc <- ncl2_oob_rc %>% 
group_by(root_cause)%>%
summarise(bps = sum(oob_bps, na.rm = TRUE))

write.xlsx( 
  list( 
    dashboard = ncl2_oob_rc, 
    by_pp =  ncl2_oob_rc_pp, 
    ttl_rc = ncl2_oob_ttl_rc
    ), file = "ncl2_oob_rc.xlsx")



#################################################################################################################################################################
##VISUALISATION##

View(ncl2_oob_ttl_rc)

ggplot(data = ncl2_oob_ttl_rc) +
  geom_col(mapping = aes(x = root_cause, y = bps), stat = "identity")


root_cause_bps <- ggplot(data = ncl2_oob_ttl_rc)+
geom_col(
  mapping = aes(x=root_cause, y = bps), stat = "identity", 
  fill = "#86b7f0",
  colour = "black",
  width = 0.45)+
  geom_text(aes(x=root_cause, y = bps, label = bps),vjust = -0.5, size = 8)+
theme_dark()+
theme(
  plot.background = element_rect(fill = "grey", colour = "black"),
  plot.title = element_text (size = 22),
  axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
  axis.text.y = element_text(size = 15),
  axis.title = element_text(size = 18))+
labs(
  title = "Root Cause by Bps",
  x = "Root Cause",
  y = "BPS"
)

root_cause_bps


#for late Slams

ncl2_late_slams_graph_ncl2 <- ncl2_late_slams %>% 
filter(fc == 'NCL2')%>%group_by(week)%>%summarise(late_slams = sum(late_slams, na.rm = TRUE))

ggplot(data = ncl2_late_slams_graph_ncl2, aes(x = week , y= late_slams,  fill  = week)) + 
geom_col(alpha = 0.7) + theme_dark() + theme(panel.background = element_rect(colour = 'black'),
plot.title = element_text (size = 22, hjust = 0.5),
axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
axis.text.y = element_text(size = 15),
axis.title = element_text(size = 18))+
geom_text(aes(label = late_slams),vjust = -1.5, size = 5)+
labs(
  x = 'Week number',
  y = 'Number of Late Slams',
  title = 'NCL2 Lates Slams by WOW'
)


#for OOB 
ncl2_oob_graph <- ncl2_oob %>% 
filter(fc == 'NCL2')%>%group_by(week)%>%summarise(oob = median(oob, na.rm = TRUE))

ggplot(data = ncl2_oob_graph, aes(x = week , y= oob,  fill  = week)) + 
geom_col(alpha = 0.7) + theme_dark() + theme(panel.background = element_rect(colour = 'black'),
plot.title = element_text (size = 22, hjust = 0.5),
axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
axis.text.y = element_text(size = 15),
axis.title = element_text(size = 18))+
geom_text(aes(label = oob),vjust = -1.5, size = 5)+
labs(
  x = 'Week number',
  y = 'Average OOB',
  title = 'NCL2 Average Out Of buffer WoW'
)

# for TRB
ncl2_trb_graph <- ncl2_trb %>% 
filter(fc == 'NCL2')%>%group_by(week)%>%summarise(trb = round(sum(trb_hours, na.rm = TRUE), 3))

ggplot(data = ncl2_trb_graph, aes(x = week , y= trb,  fill  = week)) + 
geom_col(alpha = 0.7) + theme_dark() + theme(panel.background = element_rect(colour = 'black'),
plot.title = element_text (size = 22, hjust = 0.5),
axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
axis.text.y = element_text(size = 15),
axis.title = element_text(size = 18))+
geom_text(aes(label = trb),vjust = -1.5, size = 5)+
labs(
  x = 'Week number',
  y = 'TRB hours',
  title = 'NCL2 TRB Hours WoW'
)

# for TSO

ncl2_tso_graph <- ncl2_tso %>% 
filter(fc == 'NCL2')%>%group_by(week)%>%summarise(units = sum(units, na.rm = TRUE))

ggplot(data = ncl2_tso_graph, aes(x = week , y= units,  fill  = week)) + 
geom_col(alpha = 0.7) + theme_dark() + theme(panel.background = element_rect(colour = 'black'),
plot.title = element_text (size = 22, hjust = 0.5),
axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
axis.text.y = element_text(size = 15),
axis.title = element_text(size = 18))+
geom_text(aes(label = units),vjust = -1.5, size = 5)+
labs(
  x = 'Week number',
  y = 'Units',
  title = 'NCL2 TSO misses WoW'
)
##################################################################################################################################################################