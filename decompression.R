####################################################################################################################################################
###################################################### LIBRARY #####################################################################################
####################################################################################################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(xlsx)
library(lubridate)
library(outliers)
library(Hmisc)
library(tidyr)

####################################################################################################################################################
######################################################## SPREADSHEET INPUT OUTPUT ##################################################################
####################################################################################################################################################
setwd("C:\\Users\\Ung Lik Teng\\Desktop\\UCLA Research\\DCS")
diving <- read_excel("diving_data.xlsx", sheet= 1 )
diving <- read.csv("diving_data.csv", stringsAsFactors =  F)

write.csv(diving, "diving_data.csv", row.names =F)

write.xlsx(diving, "diving_data_new.xlsx", row.names = F, sheetName = "sumamry_table")
write.xlsx(dive_table_less_24, "diving_summary.xlsx", row.names = F, sheetName = "less_24_summary")
write.xlsx(dive_table_more_24, "diving_summary.xlsx", row.names = F, sheetName = "more_24_summary", append = T)

####################################################################################################################################################
######################################################## FUNCTION ############### ##################################################################
####################################################################################################################################################

depth_range <- function(x)
{
  "
  This is a function that return the depth range of a fishermen
  "
  if(x < 20) return("<20")
  else if(x >= 20 & x < 40) return("20-40")
  else if(x >= 40 & x < 60) return("40-60")
  else if(x >= 60 & x < 80) return("60-80")
  else return(">80")
}


summary_time <- function(df)
{
  df["depth_range"] <- character(nrow(df))
  for(i in 1:nrow(df))
  {
    df$depth_range[i] <- depth_range(df$meand[i])
  }
  
  df %>% 
    filter(controlling >0) %>% 
    group_by(depth_range) %>% 
    summarise(mean_control = mean(controlling), median_control = median(controlling), min_control = min(controlling), max_control = max(controlling))
  
  df["stop_depth"] <- numeric(nrow(df))
  for(i in 1:nrow(df))
  {
    df$stop_depth[i] <- 10
  }
  
  df <- df %>% 
    filter(controlling >0)
  
  #Calculate average depth during linear travel
  df["avg_depth"] <- (df$meand - df$stop_depth)/2 + df$stop_depth
  
  #Calculate new gas pressue in each tissue on arrival at stop_depth 
  thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
  dim(thalf) <- c(1,9)
  thalf_1 <- 1 / thalf
  
  Po <- df %>% select(d_5, d_10, d_20, d_40, d_80, d_120, d_160, d_200, d_240)
  Pa <- 0.79 * (as.numeric(df$avg_depth) + 33 ) 
  dim(Pa) <- c(nrow(df), 1)
  neg0.693_t <- -0.693 * (df$meand - df$stop_depth) /35 ##assuming travel speed of 35 fsw/min
  dim(neg0.693_t) <- c(nrow(df), 1)
  
  e <- exp(neg0.693_t %*% thalf_1)
  Pt_avgdepth <- Po + (Pa - Po) * (1-e)
  names(Pt_avgdepth) <- c("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
  df <- data.frame(df, Pt_avgdepth)
  
  #Calculate the required stop time before ascent to surface
  Pa <- 0.79 * (df$stop_depth + 33)
  Po <- df %>% select("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
  thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
  Mnaught #M-value at surface level 
  dim(Mnaught) <- NULL
  
  Mo_Po <- t(Mnaught - t(Po))
  Pa_Po <- Pa - Po 
  ln <- log(1 - (Mo_Po/Pa_Po))
  time <- t(t(ln) * thalf) / -0.693
  names(time) <- c("t_5", "t_10", "t_20", "t_40", "t_80", "t_120", "t_160", "t_200","t_240")
  df <- data.frame(df, time)
  
  df["final_t"] <- numeric(nrow(df))
  for(i in 1:nrow(df))
  {
    df[i,"final_t"] <- max(df[i, 24:33], na.rm = T) 
  }
  
  summary_df <- 
    df %>% 
    group_by(depth_range) %>% 
    filter(final_t >0) %>% 
    summarise(mean_t = mean(final_t), stop_depth = 10)
  
  return(summary_df)
}



####################################################################################################################################################
#################################### ONE DIVE AND MORE THAN 24 HOURS AFTER PREVIOUS DIVE ###########################################################
####################################################################################################################################################
onedive <- 
  diving %>% 
  filter(dives_per_day == 1, lag_diving_days > 1 ) %>% 
  select(Meanfsw1, d1_5, d1_10, d1_20, d1_40, d1_80, d1_120, d1_160, d1_200, d1_240, d1_controlling)

onedive["depth_range"] <- character(nrow(onedive))
for(i in 1:nrow(onedive))
{
  onedive$depth_range[i] <- depth_range(onedive$Meanfsw1[i])
}

onedive %>% 
  filter(d1_controlling >0) %>% 
  summarise(mean_control = mean(d1_controlling), median_control = median(d1_controlling), min_control = min(d1_controlling), max_control = max(d1_controlling))

onedive["stop_depth"] <- numeric(nrow(onedive))
for(i in 1:nrow(onedive))
{
  onedive$stop_depth[i] <- 10
}

#Filter out those who doesn't need to stop
onedive2 <- onedive %>% filter(d1_controlling > 0)

#Calculate average depth during linear travel
onedive2["avg_depth"] <- (onedive2$Meanfsw1 - onedive2$stop_depth)/2 + onedive2$stop_depth

#Calculate new gas pressue in each tissue on arrival at stop_depth 
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- onedive2 %>% select(d1_5, d1_10, d1_20, d1_40, d1_80, d1_120, d1_160, d1_200, d1_240)
Pa <- 0.79 * (as.numeric(onedive2$avg_depth) + 33 ) 
dim(Pa) <- c(nrow(onedive2), 1)
neg0.693_t <- -0.693 * (onedive2$Meanfsw1 - onedive2$stop_depth) /35 ##assuming travel speed of 35 fsw/min
dim(neg0.693_t) <- c(nrow(onedive2), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_avgdepth <- Po + (Pa - Po) * (1-e)
names(Pt_avgdepth) <- c("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
onedive2 <- data.frame(onedive2, Pt_avgdepth)

#Calculate the required stop time before ascent to surface
Pa <- 0.79 * (onedive2$stop_depth + 33)
Po <- onedive2 %>% select("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
Mnaught #M-value at surface level 
dim(Mnaught) <- NULL

Mo_Po <- t(Mnaught - t(Po))
Pa_Po <- Pa - Po 
ln <- log(1 - (Mo_Po/Pa_Po))
t <- t(t(ln) * thalf) / -0.693
names(t) <- c("t_5", "t_10", "t_20", "t_40", "t_80", "t_120", "t_160", "t_200","t_240")
onedive2 <- data.frame(onedive2, t)

onedive2["final_t"] <- numeric(nrow(onedive2))
for(i in 1:nrow(onedive2))
{
  onedive2[i,"final_t"] <- max(onedive2[i, 24:33], na.rm = T) 
}

summary_onedive <- 
  onedive2 %>% 
  group_by(depth_range) %>% 
  filter(final_t >0) %>% 
  summarise(mean_t = mean(final_t), stop_depth = 10)

t(t(log(1 - (Mnaught - onedive2[2,15:23]) / (33.97 - onedive2[2,15:23]))) * thalf) / -0.693

####################################################################################################################################################
#################################### TWO DIVES AND MORE THAN 24 HOURS AFTER PREVIOUS DIVE ##########################################################
####################################################################################################################################################
twodive <- 
  diving %>% 
  filter(dives_per_day == 2, lag_diving_days > 1 ) %>% 
  select(Meanfsw2, d2_5, d2_10, d2_20, d2_40, d2_80, d2_120, d2_160, d2_200, d2_240, d2_controlling)

twodive["depth_range"] <- character(nrow(twodive))
for(i in 1:nrow(twodive))
{
  twodive$depth_range[i] <- depth_range(twodive$Meanfsw2[i])
}

twodive %>% 
  filter(d2_controlling >0) %>% 
  group_by(depth_range) %>% 
  summarise(mean_control = mean(d2_controlling), median_control = median(d2_controlling), min_control = min(d2_controlling), max_control = max(d2_controlling))

twodive["stop_depth"] <- numeric(nrow(twodive))
for(i in 1:nrow(twodive))
{
  twodive$stop_depth[i] <- 10
}

twodive <- twodive %>% 
  filter(d2_controlling >0)

#Calculate average depth during linear travel
twodive["avg_depth"] <- (twodive$Meanfsw2 - twodive$stop_depth)/2 + twodive$stop_depth

#Calculate new gas pressue in each tissue on arrival at stop_depth 
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- twodive %>% select(d2_5, d2_10, d2_20, d2_40, d2_80, d2_120, d2_160, d2_200, d2_240)
Pa <- 0.79 * (as.numeric(twodive$avg_depth) + 33 ) 
dim(Pa) <- c(nrow(twodive), 1)
neg0.693_t <- -0.693 * (twodive$Meanfsw2 - twodive$stop_depth) /35 ##assuming travel speed of 35 fsw/min
dim(neg0.693_t) <- c(nrow(twodive), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_avgdepth <- Po + (Pa - Po) * (1-e)
names(Pt_avgdepth) <- c("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
twodive <- data.frame(twodive, Pt_avgdepth)

#Calculate the required stop time before ascent to surface
Pa <- 0.79 * (twodive$stop_depth + 33)
Po <- twodive %>% select("t5", "t10", "t20", "t40", "t80", "t120", "t160", "t200","t240")
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
Mnaught #M-value at surface level 
dim(Mnaught) <- NULL

Mo_Po <- t(Mnaught - t(Po))
Pa_Po <- Pa - Po 
ln <- log(1 - (Mo_Po/Pa_Po))
time <- t(t(ln) * thalf) / -0.693
names(time) <- c("t_5", "t_10", "t_20", "t_40", "t_80", "t_120", "t_160", "t_200","t_240")
twodive <- data.frame(twodive, time)

twodive["final_t"] <- numeric(nrow(twodive))
for(i in 1:nrow(twodive))
{
  twodive[i,"final_t"] <- max(twodive[i, 24:33], na.rm = T) 
}

summary_twodive <- 
  twodive %>% 
  group_by(depth_range) %>% 
  filter(final_t >0) %>% 
  summarise(mean_t = mean(final_t), stop_depth = 10)

####################################################################################################################################################
#################################### THREE DIVES AND MORE THAN 24 HOURS AFTER PREVIOUS DIVE ########################################################
####################################################################################################################################################
threedive <- 
  diving %>% 
  filter(dives_per_day == 3, lag_diving_days > 1, meand3 >= 0  ) %>% 
  select(meand3, d3_5, d3_10, d3_20, d3_40, d3_80, d3_120, d3_160, d3_200, d3_240, d3_controlling)

names(threedive) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(threedive)

threedive$meand <- as.numeric(threedive$meand)

summary_threedive <- summary_time(threedive)
summary_threedive

####################################################################################################################################################
#################################### FOUR DIVES AND MORE THAN 24 HOURS AFTER PREVIOUS DIVE #########################################################
####################################################################################################################################################
fourdive <- 
  diving %>% 
  filter(dives_per_day == 4, lag_diving_days > 1, meand4 >= 0  ) %>% 
  select(meand4, d4_5, d4_10, d4_20, d4_40, d4_80, d4_120, d4_160, d4_200, d4_240, d4_controlling)

names(fourdive) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(fourdive)

fourdive$meand <- as.numeric(fourdive$meand)

summary_fourdive <- summary_time(fourdive)
summary_fourdive

####################################################################################################################################################
#################################### FIVE DIVES AND MORE THAN 24 HOURS AFTER PREVIOUS DIVE #########################################################
####################################################################################################################################################
fivedive <- 
  diving %>% 
  filter(dives_per_day == 5, lag_diving_days > 1, meand5 >= 0  ) %>% 
  select(meand5, d5_5, d5_10, d5_20, d5_40, d5_80, d5_120, d5_160, d5_200, d5_240, d5_controlling)

names(fivedive) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(fivedive)

fivedive$meand <- as.numeric(fivedive$meand)

summary_fivedive <- summary_time(fivedive)
summary_fivedive

###FULL TABLE
twenty_fourty <- list(depth_range = "20-40", mean_t = 0, stop_depth = 10)

summary_onedive <- rbind(summary_onedive[1,], twenty_fourty, summary_onedive[2:3,])
summary_twodive <- rbind(summary_twodive[1,], twenty_fourty, summary_twodive[2:3,])


dive_table_more_24 <- 
  summary_onedive %>% 
  select(depth_range, 'stop_depth', mean_t) %>% 
  cbind(summary_twodive[2], summary_threedive[2], summary_fourdive[2],summary_fivedive[2])

names(dive_table_more_24)[3:7] <- c("onedive", "twodive", "threedive", "fourdive", "fivedive")

####################################################################################################################################################
############################################### SUMMARY TABLE FOr < 24 HOURS  ######################################################################
####################################################################################################################################################
onedive_less_24 <- 
  diving %>% 
  filter(dives_per_day == 1, lag_diving_days == 1, Meanfsw1 >= 0) %>% 
  select(Meanfsw1, d1_5, d1_10, d1_20, d1_40, d1_80, d1_120, d1_160, d1_200, d1_240, d1_controlling)

twodive_less_24 <- 
  diving %>% 
  filter(dives_per_day == 2, lag_diving_days == 1, Meanfsw2 >= 0) %>% 
  select(Meanfsw2, d2_5, d2_10, d2_20, d2_40, d2_80, d2_120, d2_160, d2_200, d2_240, d2_controlling)

threedive_less_24 <- 
  diving %>% 
  filter(dives_per_day == 3, lag_diving_days == 1, meand3 >= 0) %>% 
  select(meand3, d3_5, d3_10, d3_20, d3_40, d3_80, d3_120, d3_160, d3_200, d3_240, d3_controlling)

fourdive_less_24 <- 
  diving %>% 
  filter(dives_per_day == 4, lag_diving_days == 1, meand4 >= 0  ) %>% 
  select(meand4, d4_5, d4_10, d4_20, d4_40, d4_80, d4_120, d4_160, d4_200, d4_240, d4_controlling)

fivedive_less_24 <- 
  diving %>% 
  filter(dives_per_day == 5, lag_diving_days == 1, meand5 >= 0  ) %>% 
  select(meand5, d5_5, d5_10, d5_20, d5_40, d5_80, d5_120, d5_160, d5_200, d5_240, d5_controlling)

names(onedive_less_24) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(twodive_less_24) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(threedive_less_24) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(fourdive_less_24) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")
names(fivedive_less_24) <- c("meand", "d_5", "d_10", "d_20", "d_40", "d_80", "d_120", "d_160", "d_200", "d_240", "controlling")

onedive_less_24$meand <- as.numeric(onedive_less_24$meand)
twodive_less_24$meand <- as.numeric(twodive_less_24$meand)
threedive_less_24$meand <- as.numeric(threedive_less_24$meand)
fourdive_less_24$meand <- as.numeric(fourdive_less_24$meand)
fivedive_less_24$meand <- as.numeric(fivedive_less_24$meand)

summary_onedive_less_24 <- summary_time(onedive_less_24)
summary_twodive_less_24 <- summary_time(twodive_less_24)
summary_threedive_less_24 <- summary_time(threedive_less_24)
summary_fourdive_less_24 <- summary_time(fourdive_less_24)
summary_fivedive_less_24 <- summary_time(fivedive_less_24)

dive_table_less_24 <- 
  summary_onedive_less_24 %>% 
  select(depth_range, 'stop_depth', mean_t) %>% 
  cbind(summary_twodive_less_24[2], summary_threedive_less_24[2], summary_fourdive_less_24[2],summary_fivedive_less_24[2])

names(dive_table_less_24)[3:7] <- c("onedive", "twodive", "threedive", "fourdive", "fivedive")



###TIME
par(mfrow= c(2,3))
hist(diving$tbt1, main= "time at bottom 1")
hist(as.numeric(diving$tbt2), main= "time at bottom 2")
hist(as.numeric(diving$tbt3), main= "time at bottom 3", freq = F)
hist(as.numeric(diving$tbt4), main= "time at bottom 4", freq = F)
hist(as.numeric(diving$tbt5), main= "time at bottom 5", freq = F)
hist(diving$d1_controlling, main = "controlling 1")
hist(diving$d2_controlling, main = "controlling 2")
hist(diving$d3_controlling, main = "controlling 3")
hist(diving$d4_controlling, main = "controlling 4")
hist(diving$d5_controlling, main = "controlling 5")


####################################################################################################################################################
#################################### TREATING OUTLIERS AND MISSING VALUES ##########################################################################
####################################################################################################################################################

boxplot.stats(diving$tbt1)
points(diving$tbt1)

outlier(diving$tbt1)
diving$tbt1[scores(diving$tbt1, type = "z", prob = 0.95)]

diving_post <- diving

#One Dive
tbt1_lower <- quantile(diving_post$tbt1, prob = c(0.05, 0.95), na.rm= T)[1]
tbt1_upper <- quantile(diving_post$tbt1, prob = c(0.05, 0.95), na.rm= T)[2]

summary(diving$tbt1)
diving_post$tbt1[which(diving$tbt1 < tbt1_lower | diving$tbt1 > tbt1_upper)] <- NA
diving_post$tbt1 <- impute(diving_post$tbt1, summary(diving$tbt1)[3])
diving$tbt1 <- diving_post$tbt1

#Two Dives
tbt2_lower <- quantile(diving_post$tbt2, prob = c(0.05, 0.95), na.rm= T)[1]
tbt2_upper <- quantile(diving_post$tbt2, prob = c(0.05, 0.95), na.rm= T)[2]

summary(diving$tbt2)
diving_post$tbt2[which(diving$tbt2 < tbt2_lower | diving$tbt2 > tbt2_upper)] <- NA
diving_post$tbt2 <- impute(diving_post$tbt2, summary(diving$tbt2)[3])
diving$tbt2 <- diving_post$tbt2

#Three Dives
tbt3_lower <- quantile(diving_post$tbt3, prob = c(0.05, 0.95), na.rm= T)[1]
tbt3_upper <- quantile(diving_post$tbt3, prob = c(0.05, 0.95), na.rm= T)[2]

summary(diving$tbt3)
diving_post$tbt3[which(diving$tbt3 < tbt3_lower | diving$tbt3 > tbt3_upper)] <- NA
diving_post$tbt3 <- impute(diving_post$tbt3, summary(diving$tbt3)[3])
diving$tbt3 <- diving_post$tbt3

#Four Dives
tbt4_lower <- quantile(diving_post$tbt4, prob = c(0.05, 0.95), na.rm= T)[1]
tbt4_upper <- quantile(diving_post$tbt4, prob = c(0.05, 0.95), na.rm= T)[2]

summary(diving$tbt4)
diving_post$tbt4[which(diving$tbt4 < tbt4_lower | diving$tbt4 > tbt4_upper)] <- NA
diving_post$tbt4 <- impute(diving_post$tbt4, summary(diving$tbt4)[3])
diving$tbt4 <- diving_post$tbt4

#Five Dives
tbt5_lower <- quantile(diving_post$tbt5, prob = c(0.05, 0.95), na.rm= T)[1]
tbt5_upper <- quantile(diving_post$tbt5, prob = c(0.05, 0.95), na.rm= T)[2]

summary(diving$tbt5)
diving_post$tbt5[which(diving$tbt5 < tbt5_lower | diving$tbt5 > tbt5_upper)] <- NA
diving_post$tbt5 <- impute(diving_post$tbt5, summary(diving$tbt5)[3])
diving$tbt5 <- diving_post$tbt5

####################################################################################################################################################
######################################################## < 24 HOURS DIVES ##########################################################################
####################################################################################################################################################

##################CLEANING##########################
(diving$startdate[2] - diving$startdate[1]) == 1
a <- diving$st1[1] - diving$st1[2]
setwd("C:\\Users\\Ung Lik Teng\\Desktop\\DCS")
diving_time <- read_excel("diving2.xlsx", sheet = 1)

for(i in 1:nrow(diving))
{
  if(diving$dives_per_day[i] < 5)
  {
    diving$st5[i] <- NA
  }
}


diving <- read_excel("diving_data.xlsx", sheet = 1)

day(diving$end5) <- day(diving$startdate)
month(diving$end5) <- month(diving$startdate)
year(diving$end5) <- year(diving$startdate)


names(diving)[13] <- "timediff_consecutive_day_dive"
diving$timediff_consecutive_day_dive <- 0

for(i in 2:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$dives_per_day[i-1] == 1)
      diving$timediff_consecutive_day_dive[i] <- diving$st1[i] - diving$end1[i-1]
    else if(diving$dives_per_day[i-1] == 2)
      diving$timediff_consecutive_day_dive[i] <- diving$st1[i] - diving$end2[i-1]
    else if(diving$dives_per_day[i-1] == 3)
      diving$timediff_consecutive_day_dive[i] <- diving$st1[i] - diving$end3[i-1]
    else if(diving$dives_per_day[i-1] == 4)
      diving$timediff_consecutive_day_dive[i] <- diving$st1[i] - diving$end4[i-1]
    else if(diving$dives_per_day[i-1] == 5)
      diving$timediff_consecutive_day_dive[i] <- diving$st1[i] - diving$end5[i-1]
    else diving$timediff_consecutive_day_dive[i] <- NA
      
  }
  else
    diving$timediff_consecutive_day_dive[i] <- NA
}

#Conversion to minutes
diving$timediff_consecutive_day_dive <- diving$timediff_consecutive_day_dive * 60
sum(diving$timediff_consecutive_day_dive > 24*60, na.rm = T) #sanity check - all time < 24 hours

##extracting 
diving[c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <- numeric(nrow(diving))
for(i in 7:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] ==1 && diving$lag_diving_days[i-2] ==1 && diving$lag_diving_days[i-3] ==1 && diving$lag_diving_days[i-4] ==1
       && diving$lag_diving_days[i-5] ==1 && diving$lag_diving_days[i-6] > 1)
    {
      if(diving$dives_per_day[i-1] == 1)
      {
        diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
          cbind(diving[i-1, c("d1_5", "d1_10", "d1_20", "d1_40", "d1_80", "d1_120", "d1_160", "d1_200","d1_240")],1)
      }
      else if(diving$dives_per_day[i-1] == 2)
      {
        diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
          cbind(diving[i-1, c("d2_5", "d2_10", "d2_20", "d2_40", "d2_80", "d2_120", "d2_160", "d2_200","d2_240")],2)
      }
      else if(diving$dives_per_day[i-1] == 3)
      {
        diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
          cbind(diving[i-1, c("d3_5", "d3_10", "d3_20", "d3_40", "d3_80", "d3_120", "d3_160", "d3_200","d3_240")],3)
      }
      else if(diving$dives_per_day[i-1] == 4)
      {
        diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
          cbind(diving[i-1, c("d4_5", "d4_10", "d4_20", "d4_40", "d4_80", "d4_120", "d4_160", "d4_200","d4_240")],4)
      }
      else if(diving$dives_per_day[i-1] >= 5)
      {
        diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
          cbind(diving[i-1, c("d5_5", "d5_10", "d5_20", "d5_40", "d5_80", "d5_120", "d5_160", "d5_200","d5_240")],5)
      }
    }
    else
    {
      diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
        rep(NA, 10)
    }
  }
  else
  {
    diving[i,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
      rep(NA, 10)
  }
}

diving[1:7,  c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240", "prev_which_dive")] <-
  rep(NA, 10)

sum(!is.na(diving[240]))
####################################################################################################################################################
############################################### CALCULATION FOR < 24 HOURS DIVE  ###################################################################
####################################################################################################################################################
diving$Minutes <- as.numeric(diving$Minutes)


##PRESSURE BEFORE FIRST DIVE
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

#-0.693t as a column vector with dim nx1
neg0.693_t <- -0.693 * as.numeric(diving$Minutes)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)

Po <- diving[c("prev_d5", "prev_d10", "prev_d20", "prev_d40", "prev_d80", "prev_d120", "prev_d160", "prev_d200", "prev_d240")]
Pa <- 0.79 * (0+33)

Pt0 <- Po + (Pa - Po) *(1- e)
names(Pt0) <- c("prev_b1_5", "prev_b1_10", "prev_b1_20", "prev_b1_40", "prev_b1_80", "prev_b1_120", "prev_b1_160", "prev_b1_200","prev_b1_240")

diving <- data.frame(diving, Pt0)

##PRESSURE DURING FIRST DIVE 
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- Pt0
Pa <- 0.79 * (as.numeric(diving$Meanfsw1) + 33 ) 
dim(Pa) <- c(nrow(diving), 1)
neg0.693_t <- -0.693 * as.numeric(diving$tbt1)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pao <- Pa - Po

Pt_d1 <- Po + (Pao) * (1 - e)
names(Pt_d1) <- c("prev_d1_5", "prev_d1_10", "prev_d1_20", "prev_d1_40", "prev_d1_80", "prev_d1_120", "prev_d1_160", "prev_d1_200","prev_d1_240")
diving <- data.frame(diving, Pt_d1)

##FIRST TRIAL STOP FOR FIRST DIVE 
Mnaught <- c(104,88,72,56,54,52, 51, 51, 50)
gradient <- c(1.8,1.6,1.5,1.4,1.3,1.2, 1.15, 1.1, 1.1)

M <- Pt_d1 

FTS_1st <- t((t(M) - Mnaught) / gradient)
names(FTS_1st) <- c("p_d1_FTS5", "p_d1_FTS10", "p_d1_FTS20", "p_d1_FTS40", "p_d1_FTS80", "p_d1_FTS120", "p_d1_FTS160", "p_d1_FTS200", "p_d1_FTS240")
diving <- data.frame(diving, FTS_1st)
names(diving)[268:276] <- 
  c("p_d1_FTS5", "p_d1_FTS10", "p_d1_FTS20", "p_d1_FTS40", "p_d1_FTS80", "p_d1_FTS120", "p_d1_FTS160", "p_d1_FTS200", "p_d1_FTS240")

##First Dive: Distribution of Controlling Tissue
diving["p_d1_controlling"] <- numeric(nrow(diving))
for(i in 1:nrow(diving))
{
    diving[i, "p_d1_controlling"] <- 
      max(diving[i, c("p_d1_FTS5", "p_d1_FTS10", "p_d1_FTS20", "p_d1_FTS40", "p_d1_FTS80", "p_d1_FTS120", "p_d1_FTS160", "p_d1_FTS200", "p_d1_FTS240")])
}

##PRESSURE BEFORE SECOND DIVE 
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

#-0.693t as a column vector with dim nx1
neg0.693_t <- -0.693 * as.numeric(diving$lag1)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)

Po <- diving[c("prev_d1_5", "prev_d1_10", "prev_d1_20", "prev_d1_40", "prev_d1_80", "prev_d1_120", "prev_d1_160", "prev_d1_200","prev_d1_240")]
Pa <- 0.79 * (0+33)

Pt_b2 <- Po + (Pa - Po) *(1- e)
names(Pt_b2) <- c("p_b2_5", "p_b2_10", "p_b2_20", "p_b2_40", "p_b2_80", "p_b2_120", "p_b2_160", "p_b2_200","p_b2_240")

diving <- data.frame(diving, Pt_b2)

##PRESSURE DURING SECOND DIVE 
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- Pt_b2
Pa <- 0.79 * (as.numeric(diving$Meanfsw2) + 33 ) 
dim(Pa) <- c(nrow(diving), 1)
neg0.693_t <- -0.693 * as.numeric(diving$tbt2)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_d2 <- Po + (Pa - Po) * (1-e)
names(Pt_d2) <- c("p_d2_5", "p_d2_10", "p_d2_20", "p_d2_40", "p_d2_80", "p_d2_120", "p_d2_160", "p_d2_200","p_d2_240")
diving <- data.frame(diving, Pt_d2)

##FIRST TRIAL STOP FOR SECOND DIVE 
Mnaught <- c(104,88,72,56,54,52, 51, 51, 50)
gradient <- c(1.8,1.6,1.5,1.4,1.3,1.2, 1.15, 1.1, 1.1)

M <- Pt_d2

FTS_2nd <- t((t(M) - Mnaught) / gradient)
names(FTS_2nd) <- c("p_d2_FTS5", "p_d2_FTS10", "p_d2_FTS20", "p_d2_FTS40", "p_d2_FTS80", "p_d2_FTS120", "p_d2_FTS160", "p_d2_FTS200", "p_d2_FTS240")
diving <- data.frame(diving, FTS_2nd)
names(diving)[296:304] <- 
  c("p_d2_FTS5", "p_d2_FTS10", "p_d2_FTS20", "p_d2_FTS40", "p_d2_FTS80", "p_d2_FTS120", "p_d2_FTS160", "p_d2_FTS200", "p_d2_FTS240")

##SECOND DIVE:DISTRIBUTION OF CONTROLLING TISSUE 
diving["p_d2_controlling"] <- numeric(nrow(diving))
for(i in 1:nrow(diving))
{
  diving[i, "p_d2_controlling"] <- 
    max(diving[i, c("p_d2_FTS5", "p_d2_FTS10", "p_d2_FTS20", "p_d2_FTS40", "p_d2_FTS80", "p_d2_FTS120", "p_d2_FTS160", "p_d2_FTS200", "p_d2_FTS240")])
}

##PRESSURE BEFORE THIRD DIVE 
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

#-0.693t as a column vector with dim nx1
neg0.693_t <- -0.693 * as.numeric(diving$lag2)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)

Po <- diving[c("p_d2_5", "p_d2_10", "p_d2_20", "p_d2_40", "p_d2_80", "p_d2_120", "p_d2_160", "p_d2_200","p_d2_240")]
Pa <- 0.79 * (0+33)

Pt_b3 <- Po + (Pa - Po) * (1-e)
names(Pt_b3) <- c("p_b3_5", "p_b3_10", "p_b3_20", "p_b3_40", "p_b3_80", "p_b3_120", "p_b3_160", "p_b3_200","p_b3_240")

diving <- data.frame(diving, Pt_b3)

##PRESSURE DURING THIRD DIVE 
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- Pt_b3
Pa <- 0.79 * (as.numeric(diving$meand3) + 33 ) 
dim(Pa) <- c(nrow(diving), 1)
neg0.693_t <- -0.693 * as.numeric(diving$tbt3)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_d3 <- Po + (Pa - Po) * (1-e)
names(Pt_d3) <- c("p_d3_5", "p_d3_10", "p_d3_20", "p_d3_40", "p_d3_80", "p_d3_120", "p_d3_160", "p_d3_200","p_d3_240")
diving <- data.frame(diving, Pt_d3)

##FIRST TRIAL STOP FOR THIRD DIVE 
Mnaught <- c(104,88,72,56,54,52, 51, 51, 50)
gradient <- c(1.8,1.6,1.5,1.4,1.3,1.2, 1.15, 1.1, 1.1)

M <- Pt_d3 

FTS_3rd <- t((t(M) - Mnaught) / gradient)
names(FTS_3rd) <- c("p_d3_FTS5", "p_d3_FTS10", "p_d3_FTS20", "p_d3_FTS40", "p_d3_FTS80", "p_d3_FTS120", "p_d3_FTS160", "p_d3_FTS200", "p_d3_FTS240")
diving <- data.frame(diving, FTS_3rd)
names(diving)[324:332] <- 
  c("p_d3_FTS5", "p_d3_FTS10", "p_d3_FTS20", "p_d3_FTS40", "p_d3_FTS80", "p_d3_FTS120", "p_d3_FTS160", "p_d3_FTS200", "p_d3_FTS240")

##THIRD DIVE: DISTRIBUTION OF CONTROLLING TISSUE
diving["p_d3_controlling"] <- numeric(nrow(diving))
for(i in 1:nrow(diving))
{
  diving[i, "p_d3_controlling"] <- 
    max(diving[i, c("p_d3_FTS5", "p_d3_FTS10", "p_d3_FTS20", "p_d3_FTS40", "p_d3_FTS80", "p_d3_FTS120", "p_d3_FTS160", "p_d3_FTS200", "p_d3_FTS240")])
}

##PRESSUSRE BEFORE FOURTH DIVE

#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

#-0.693t as a column vector with dim nx1
neg0.693_t <- -0.693 * as.numeric(diving$lag3)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)

Po <- diving[ c("p_d3_5", "p_d3_10", "p_d3_20", "p_d3_40", "p_d3_80", "p_d3_120", "p_d3_160", "p_d3_200","p_d3_240")]
Pa <- 0.79 * (0+33)

Pt_b4 <- Po + (Pa - Po) * (1- e)
names(Pt_b4) <- c("p_b4_5", "p_b4_10", "p_b4_20", "p_b4_40", "p_b4_80", "p_b4_120", "p_b4_160", "p_b4_200","p_b4_240")

diving <- data.frame(diving, Pt_b4)

##PRESSURE DURING FOURTH DIVE 
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200,240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- Pt_b4
Pa <- 0.79 * (as.numeric(diving$meand4) + 33 ) 
dim(Pa) <- c(nrow(diving), 1)
neg0.693_t <- -0.693 * as.numeric(diving$tbt4)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_d4 <- Po + (Pa - Po) * (1 - e)
names(Pt_d4) <- c("p_d4_5", "p_d4_10", "p_d4_20", "p_d4_40", "p_d4_80", "p_d4_120", "p_d4_160", "p_d4_200","p_d4_240")
diving <- data.frame(diving, Pt_d4)

##FIRST TRIAL STOP FOR FOURTH DIVE
Mnaught <- c(104,88,72,56,54,52, 51, 51, 50)
gradient <- c(1.8,1.6,1.5,1.4,1.3,1.2, 1.15, 1.1, 1.1)

M <- Pt_d4

FTS_4th <- t((t(M) - Mnaught) / gradient)
names(FTS_4th) <- c("p_d4_FTS5", "p_d4_FTS10", "p_d4_FTS20", "p_d4_FTS40", "p_d4_FTS80", "p_d4_FTS120", "p_d4_FTS160", "p_d4_FTS200", "p_d4_FTS240")
diving <- data.frame(diving, FTS_4th)
names(diving)[352:360] <-
  c("p_d4_FTS5", "p_d4_FTS10", "p_d4_FTS20", "p_d4_FTS40", "p_d4_FTS80", "p_d4_FTS120", "p_d4_FTS160", "p_d4_FTS200", "p_d4_FTS240")

##FOURTH DIVE: DISTRIBUTION OF CONTROLLING TISSUE 
diving["p_d4_controlling"] <- numeric(nrow(diving))
for(i in 1:nrow(diving))
{
  diving[i, "p_d4_controlling"] <- 
    max(diving[i, c("p_d4_FTS5", "p_d4_FTS10", "p_d4_FTS20", "p_d4_FTS40", "p_d4_FTS80", "p_d4_FTS120", "p_d4_FTS160", "p_d4_FTS200", "p_d4_FTS240")])
}

##PRESSURE BEFORE FIFTH DIVE
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200, 240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

#-0.693t as a column vector with dim nx1
neg0.693_t <- -0.693 * as.numeric(diving$lag4)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)

Po <- diving[c("p_d4_5", "p_d4_10", "p_d4_20", "p_d4_40", "p_d4_80", "p_d4_120", "p_d4_160", "p_d4_200","p_d4_240")]
Pa <- 0.79 * (0+33)

Pt_b5 <- Po + (Pa - Po) * (1- e)
names(Pt_b5) <- c("p_b5_5", "p_b5_10", "p_b5_20", "p_b5_40", "p_b5_80", "p_b5_120", "p_b5_160", "p_b5_200","p_b5_240")

diving <- data.frame(diving, Pt_b5)

##PRESSURE DURING FIFTH DIVE
#1/thalf as a row vector with dim 1x9
thalf <- c(5, 10, 20, 40, 80, 120, 160, 200, 240)
dim(thalf) <- c(1,9)
thalf_1 <- 1 / thalf

Po <- Pt_b5
Pa <- 0.79 * (as.numeric(diving$meand5) + 33 ) 
dim(Pa) <- c(nrow(diving), 1)
neg0.693_t <- -0.693 * as.numeric(diving$tbt5)
dim(neg0.693_t) <- c(nrow(diving), 1)

e <- exp(neg0.693_t %*% thalf_1)
Pt_d5 <- Po + (Pa - Po) * (1-e)
names(Pt_d5) <- c("p_d5_5", "p_d5_10", "p_d5_20", "p_d5_40", "p_d5_80", "p_d5_120", "p_d5_160", "p_d5_200","p_d5_240")
diving <- data.frame(diving, Pt_d5)

##FIRST TRIAL STOP FOR FIFTH DIVE
Mnaught <- c(104,88,72,56,54,52, 51, 51, 50)
gradient <- c(1.8,1.6,1.5,1.4,1.3,1.2, 1.15, 1.1, 1.1)

M <- Pt_d5

FTS_5th <- t((t(M) - Mnaught) / gradient)
names(FTS_5th) <- c("p_d5_FTS5", "p_d5_FTS10", "p_d5_FTS20", "p_d5_FTS40", "p_d5_FTS80", "p_d5_FTS120", "p_d5_FTS160", "p_d5_FTS200", "p_d5_FTS240")
diving <- data.frame(diving, FTS_5th)
names(diving)[380:388]<-
  c("p_d5_FTS5", "p_d5_FTS10", "p_d5_FTS20", "p_d5_FTS40", "p_d5_FTS80", "p_d5_FTS120", "p_d5_FTS160", "p_d5_FTS200", "p_d5_FTS240")

##FIFTH DIVE: DISTRIBUTION OF CONTROLLING TISSUE 
diving["p_d5_controlling"] <- numeric(nrow(diving))
for(i in 1:nrow(diving))
{
  diving[i, "p_d5_controlling"] <- 
    max(diving[i, c("p_d5_FTS5", "p_d5_FTS10", "p_d5_FTS20", "p_d5_FTS40", "p_d5_FTS80", "p_d5_FTS120", "p_d5_FTS160", "p_d5_FTS200", "p_d5_FTS240")])
}

##Filling in the blank 

for(i in 2:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] > 1)
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

for(i in 3:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] > 1)
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

for(i in 4:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] > 1 )
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

for(i in 5:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] != 1)
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

for(i in 6:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] == 1 && diving$lag_diving_days[i-5] != 1)
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

for(i in 7:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] == 1 && diving$lag_diving_days[i-5] == 1 && diving$lag_diving_days[i-6] != 1)
      diving[i, 109:239] <- diving[i, 259:389]
  }
}

before_first_dive <- diving[250:258]

for(i in 3:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] > 1)
      before_first_dive[i,] <- diving[i, 250:258]
  }
}

for(i in 4:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] > 1 )
      before_first_dive[i,] <- diving[i, 250:258]
  }
}

for(i in 5:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] != 1)
      before_first_dive[i,] <- diving[i, 250:258]
  }
}

for(i in 6:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] == 1 && diving$lag_diving_days[i-5] != 1)
      before_first_dive[i,] <- diving[i, 250:258]
  }
}

for(i in 7:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
  {
    if(diving$lag_diving_days[i-1] == 1 && diving$lag_diving_days[i-2] == 1 && diving$lag_diving_days[i-3] == 1  && diving$lag_diving_days[i-4] == 1 && diving$lag_diving_days[i-5] == 1 && diving$lag_diving_days[i-6] != 1)
      before_first_dive[i,] <- diving[i, 250:258]
  }
}

diving[250:389] <- NULL

for(i in 1:nrow(diving))
{
  if(diving$lag_diving_days[i] == 1)
    diving[i,109:117] <- NA
}

sum(!is.na(diving$d1_5)) ##Sanity Check
sum(!is.na(before_first_dive[1])) ##Sanity Check
diving <- cbind(diving, before_first_dive)

####################################################################################################################################################
######################################################## DATA VISUALIZATION ########################################################################
####################################################################################################################################################
##Distribution of controlling tissue 
library(ggplot2)

##CASE STUDIES 
fivedive_case <- diving[8,]
names(fivedive_case) <- 
five_dive_case <- rbind(rep(26.07,9),fivedive_case[c(110:118)], fivedive_case[c(129:137)], fivedive_case[c(138:146)], fivedive_case[c(157:165)],
                       fivedive_case[c(166:174)], fivedive_case[c(185:193)], fivedive_case[c(194:202)], fivedive_case[c(213:221)],
                       fivedive_case[c(222:230)])

Time <- c(fivedive_case[17], fivedive_case[18], fivedive_case[26],fivedive_case[27], fivedive_case[35], fivedive_case[36], fivedive_case[44], 
          fivedive_case[45], fivedive_case[53],fivedive_case[54])
for(i in 1:10)
{
  five_dive_case[i,"Time"] <- Time[i]
}
names(five_dive_case) <- c("h5", "h10", "h20", "h40", "h80", "h120", "h160", "h200", "h240", "Time")

five_dive_case_tidy <- gather(five_dive_case, key = "half_life", value = "pressure", 1:9)

ggplot(five_dive_case_tidy, aes(x = Time, y = pressure, color = half_life)) + 
  geom_line() + 
  geom_vline(xintercept = 12)

five_FTS_Case <- rbind(fivedive_case[119:128], fivedive_case[147:156], fivedive_case[175:184], fivedive_case[203:212], fivedive_case[231:240])
five_FTS_Case["Dive"] <- c(1:5)
names(five_FTS_Case) <- c("h5", "h10", "h20", "h40", "h80", "h120", "h160", "h200", "h240", "controlling", "Dive")
FTS_tidy <- gather(five_FTS_Case, key = "half_life", value = "first_trial_stop", 1:9)
FTS_tidy$half_life <-factor(FTS_tidy$half_life, level =c("h5", "h10", "h20", "h40", "h80", "h120", "h160", "h200", "h240") ) 


FTS_five_dive <- 
  ggplot(FTS_tidy, aes(x = Dive, y = first_trial_stop, color = half_life))+
  geom_line()+
  geom_hline(yintercept = five_FTS_Case$controlling, lty = "dotted")+
  scale_y_reverse(breaks = five_FTS_Case$controlling) +
  ggtitle("First Trial Stops for Diver who dives 5 times a day")

ggsave(filename = "fts_fivedive.png", plot = FTS_five_dive, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")

diving$d2_controlT <- factor(diving$d2_controlT, levels = c("d2_FTS20", "d2_FTS40", "d2_FTS80", "d2_FTS120", "d2_FTS240"))
diving$d3_controlT <- factor(diving$d3_controlT, levels = c("d3_FTS40", "d3_FTS80", "d3_FTS120"))
diving$d4_controlT <- factor(diving$d4_controlT, levels = c("d4_FTS40", "d4_FTS80", "d4_FTS120"))
diving$d5_controlT <- factor(diving$d5_controlT, levels = c("d5_FTS40", "d5_FTS80", "d5_FTS120"))

controlling_first <-
  ggplot(data = diving, aes(x = d1_controlling, fill = d1_controlT), na.rm = T) + 
  geom_histogram(position = "stack") +
  xlab("Dive 1: Controlling Tissue (fsw)") +
  ggtitle("Distribution of Controlling Tissue for First Dive")

controlling_second <-
  ggplot(data = diving, aes(x = d2_controlling, fill = d2_controlT), na.rm = T) + 
  geom_histogram(position = "stack") +
  xlab("Dive 2: Controlling Tissue (fsw)") +
  ggtitle("Distribution of Controlling Tissue for Second Dive")

controlling_third <-
  ggplot(data = diving, aes(x = d3_controlling, fill = d3_controlT), na.rm = T) + 
  geom_histogram(position = "stack") +
  xlab("Dive 3: Controlling Tissue (fsw)") +
  ggtitle("Distribution of Controlling Tissue for Third Dive")

controlling_fourth <-
  ggplot(data = diving, aes(x = d4_controlling, fill = d4_controlT), na.rm = T) + 
  geom_histogram(position = "stack") +
  xlab("Dive 4: Controlling Tissue (fsw)") +
  ggtitle("Distribution of Controlling Tissue for Fourth Dive")

controlling_fifth <-
  ggplot(data = diving, aes(x = d5_controlling, fill = d5_controlT), na.rm = T) + 
  geom_histogram(position = "stack") +
  xlab("Dive 5: Controlling Tissue (fsw)") +
  ggtitle("Distribution of Controlling Tissue for Fifth Dive")


t_1 <- c(table(diving$d1_controlT), rep(0,3))
t_2 <- c(0,table(diving$d2_controlT))
t_3 <- c(rep(0,2),table(diving$d3_controlT),0) 
t_4 <- c(rep(0,2),table(diving$d4_controlT),0) 
t_5 <- c(rep(0,2),table(diving$d5_controlT),0) 

names(t_1) <- c("h_10", "h_20", "h_40", "h_80", "h_120", "h_240")
names(t_2) <- c("h_10", "h_20", "h_40", "h_80", "h_120", "h_240")
names(t_3) <- c("h_10", "h_20", "h_40", "h_80", "h_120", "h_240")
names(t_4) <- c("h_10", "h_20", "h_40", "h_80", "h_120", "h_240")
names(t_5) <- c("h_10", "h_20", "h_40", "h_80", "h_120", "h_240")

ggsave(filename = "tissue_first_dive.png", plot = controlling_first, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "tissue_second_dive.png", plot = controlling_second, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "tissue_third_dive.png", plot = controlling_third, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "tissue_fourth_dive.png", plot = controlling_fourth, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "tissue_fifth_dive.png", plot = controlling_fifth, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")

write.xlsx(tissue_table, "tissue_table.xlsx", row.names = F)

tissue_table <- rbind(t_1,t_2,t_3,t_4,t_5)
rownames(tissue_table) <- c("first_dive", "second_dive", "third_dive", "fourth_dive", "fifth_dive")

diving_controlling <- diving %>%  filter(controlling >= 0)
dist1 <- ggplot(data = diving_controlling, aes(x = controlling), na.rm = T) + geom_histogram(bins = 100, aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(diving_controlling$controlling), sd = sd(diving_controlling$controlling)), 
                lwd = 2, 
                col = 'red')
dist2 <-  ggplot(data = diving_controlling, aes(x = controlling, fill = ControlT), na.rm = T) + geom_histogram(bins = 100) + facet_grid(ControlT~.)

diving_controlT <- diving %>%  filter(!is.na(ControlT))
dist3 <- ggplot(data =diving_controlT, aes(x= ControlT, na.rm = TRUE, fill = ControlT)) +geom_bar()+ geom_text(stat='count',aes(label=..count..),vjust=-1)

#save image
ggsave(filename = "chart1.png", plot = dist1, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "chart2.png", plot = dist2, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")
ggsave(filename = "chart3.png", plot = dist3, path = "C:\\Users\\Ung Lik Teng\\Desktop\\DCS", device = "png")


####################################################################################################################################################
############################################### INVESTIGATING DAYS OF WEEK / MONTHS ################################################################
####################################################################################################################################################
diving["days_of_week"] <- character(nrow(diving))
diving["days_of_week"] <- weekdays(diving$startdate)

diving <- diving[c(1:11, 249, 12:248)]

table(diving$days_of_week)

diving["Monthts"] <- months(ymd(diving$startdate))

####################################################################################################################################################
######################################################## dummy code violation ######################################################################
####################################################################################################################################################
diving["violation"] <- numeric(nrow(diving))
diving["violation_whichdive"] <- character(nrow(diving))
diving["violation_controlling"] <- numeric(nrow(diving))
diving["violation_whichtissue"] <- character(nrow(diving))

for(i in 1:nrow(diving))
{
  if(!is.na(diving$d5_controlling[i]))
  {
    ifelse(diving$d5_controlling[i] > 0, diving$violation[i] <- 1, diving$violation[i] <- 0 )
    diving$violation_whichdive[i] <- "5th"
    diving$violation_controlling[i] <- diving$d5_controlling[i]
    diving$violation_whichtissue[i] <- diving$d5_controlT[i]
  }
  else if(!is.na(diving$d4_controlling[i]))
  {
    ifelse(diving$d4_controlling[i] > 0, diving$violation[i] <- 1, diving$violation[i] <- 0 )
    diving$violation_whichdive[i] <- "4th"
    diving$violation_controlling[i] <- diving$d4_controlling[i]
    diving$violation_whichtissue[i] <- diving$d4_controlT[i]
  }
  else if(!is.na(diving$d3_controlling[i]))
  {
    ifelse(diving$d3_controlling[i] > 0, diving$violation[i] <- 1, diving$violation[i] <- 0 )
    diving$violation_whichdive[i] <- "3rd"
    diving$violation_controlling[i] <- diving$d3_controlling[i]
    diving$violation_whichtissue[i] <- diving$d3_controlT[i]
  }
  else if(!is.na(diving$d2_controlling[i]))
  {
    ifelse(diving$d2_controlling[i] > 0, diving$violation[i] <- 1, diving$violation[i] <- 0 )
    diving$violation_whichdive[i] <- "2nd"
    diving$violation_controlling[i] <- diving$d2_controlling[i]
    diving$violation_whichtissue[i] <- diving$d2_controlT[i]
  }
  else if(!is.na(diving$d1_controlling[i]))
  {
    ifelse(diving$d1_controlling[i] > 0, diving$violation[i] <- 1, diving$violation[i] <- 0 )
    diving$violation_whichdive[i] <- "1st"
    diving$violation_controlling[i] <- diving$d1_controlling[i]
    diving$violation_whichtissue[i] <- diving$d1_controlT[i]
  }
  else 
  {
    diving$violation[i] <- NA
    diving$violation_whichdive[i] <- NA
    diving$violation_controlling[i] <- NA
    diving$violation_whichtissue[i] <- NA
  }
}

table(diving$violation)
table(diving$violation_whichdive)
table(diving$violation_whichdive, diving$violation)


