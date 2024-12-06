library(tidyverse)
library(psych)
library(patchwork)

#Dataset input
FA95 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Fayetteville-AR-1995-v4-Final.csv")
FA95$DATE <- as.Date(FA95$DATE, format = "%Y-%m-%d")
FA95$MONTH <- format(FA95$DATE, "%m")
FA95$YEAR <- "1995"

FA05 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Fayetteville-AR-2005-v4-Final.csv")
FA05$DATE <- as.Date(FA05$DATE, format = "%Y-%m-%d")
FA05$MONTH <- format(FA05$DATE, "%m")
FA05$YEAR <- "2005"

FA15 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Fayetteville-AR-2015-v4-Final.csv")
FA15$DATE <- as.Date(FA15$DATE, format = "%Y-%m-%d")
FA15$MONTH <- format(FA15$DATE, "%m")
FA15$YEAR <- "2015"

FA_combined <- rbind(FA95, FA05, FA15)

SB95 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Sofia-BU-1995-v4-Final.csv")
SB95$DATE <- as.Date(SB95$DATE, format = "%Y-%m-%d")
SB95$MONTH <- format(SB95$DATE, "%m")
SB95$YEAR <- "1995"

SB05 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Sofia-BU-2005-v4-Final.csv")
SB05$DATE <- as.Date(SB05$DATE, format = "%Y-%m-%d")
SB05$MONTH <- format(SB05$DATE, "%m")
SB05$YEAR <- "2005"

SB15 <- read.csv("C:/Users/jimbo/Desktop/Computer Science Louisville/Fall2024/Data Mining/Data Preprocessing/Sofia-BU-2015-v4-Final.csv")
SB15$DATE <- as.Date(SB15$DATE, format = "%Y-%m-%d")
SB15$MONTH <- format(SB15$DATE, "%m")
SB15$YEAR <- "2015"

SB_combined <- rbind(SB95, SB05, SB15)

###########################################################################################################
#####Replacing missing data################################################################################
###########################################################################################################

###Histogram showing data before replacement of missing values###

ggplot(FA_combined, aes(x = WND)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Wind Speed Across Years - Fayetteville, AR",
       x = "Wind Speeds",
       y = "Frequency")

ggplot(SB_combined, aes(x=WND)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Wind Speed Across Years - Sofia, BU",
       x = "Wind Speeds",
       y = "Frequency")

ggplot(subset(FA_combined, CIG <= 5000), aes(x = CIG)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ceiling Height Across Years - Fayetteville, AR",
       x = "Ceiling Height",
       y = "Frequency")

ggplot(subset(SB_combined, CIG <= 5000), aes(x = CIG)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ceiling Height Across Years - Sofia, BU",
       x = "Ceiling Height",
       y = "Frequency")

ggplot(FA_combined, aes(x = TMP)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Temperatures Across Years - Fayetteville, AR",
       x = "Temperature",
       y = "Frequency")

ggplot(SB_combined, aes(x = TMP)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Temperatures Across Years - Sofia, BU",
       x = "Temperature",
       y = "Frequency")

###Replacement of missing values feature###

FA95_monthly_medians <- FA95 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
FA95 <- FA95 %>% left_join(FA95_monthly_medians, by = "MONTH")
FA95$WND <- ifelse(is.na(FA95$WND), FA95$median_WND, FA95$WND)
FA95$CIG <- ifelse(is.na(FA95$CIG), FA95$median_CIG, FA95$CIG)
FA95$TMP <- ifelse(is.na(FA95$TMP), FA95$median_TMP, FA95$TMP)

FA05_monthly_medians <- FA05 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
FA05 <- FA05 %>% left_join(FA05_monthly_medians, by = 'MONTH')
FA05$WND <- ifelse(is.na(FA05$WND), FA05$median_WND, FA05$WND)
FA05$CIG <- ifelse(is.na(FA05$CIG), FA05$median_CIG, FA05$CIG)
FA05$TMP <- ifelse(is.na(FA05$TMP), FA05$median_TMP, FA05$TMP)

FA15_monthly_medians <- FA15 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
FA15 <- FA15 %>% left_join(FA15_monthly_medians, by = 'MONTH')
FA15$WND <- ifelse(is.na(FA15$WND), FA15$median_WND, FA15$WND)
FA15$CIG <- ifelse(is.na(FA15$CIG), FA15$median_CIG, FA15$CIG)
FA15$TMP <- ifelse(is.na(FA15$TMP), FA15$median_TMP, FA15$TMP)

FA_combined_v2 <- rbind(FA95, FA05, FA15)

SB95_monthly_medians <- SB95 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
SB95 <- SB95 %>% left_join(SB95_monthly_medians, by = "MONTH")
SB95$WND <- ifelse(is.na(SB95$WND), SB95$median_WND, SB95$WND)
SB95$CIG <- ifelse(is.na(SB95$CIG), SB95$median_CIG, SB95$CIG)
SB95$TMP <- ifelse(is.na(SB95$TMP), SB95$median_TMP, SB95$TMP)

SB05_monthly_medians <- SB05 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
SB05 <- SB05 %>% left_join(SB05_monthly_medians, by = "MONTH")
SB05$WND <- ifelse(is.na(SB05$WND), SB05$median_WND, SB05$WND)
SB05$CIG <- ifelse(is.na(SB05$CIG), SB05$median_CIG, SB05$CIG)
SB05$TMP <- ifelse(is.na(SB05$TMP), SB05$median_TMP, SB05$TMP)

SB15_monthly_medians <- SB15 %>% group_by(MONTH) %>% summarise(
  median_WND = median(WND, na.rm = TRUE),
  median_CIG = median(CIG[CIG != 22000], na.rm = TRUE),
  median_TMP = median(TMP, na.rm = TRUE))
SB15 <- SB15 %>% left_join(SB15_monthly_medians, by = "MONTH")
SB15$WND <- ifelse(is.na(SB15$WND), SB15$median_WND, SB15$WND)
SB15$CIG <- ifelse(is.na(SB15$CIG), SB15$median_CIG, SB15$CIG)
SB15$TMP <- ifelse(is.na(SB15$TMP), SB15$median_TMP, SB15$TMP)

SB_combined_v2 <- rbind(SB95, SB05, SB15)

###Histogram showing data after replacement of missing values###

ggplot(FA_combined_v2, aes(x = WND)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Wind Speed Across Years (Data Filled) - Fayetteville, AR",
       x = "Wind Speeds",
       y = "Frequency")

ggplot(SB_combined_v2, aes(x=WND)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Wind Speeds Across Years (Data Filled) - Sofia, BU",
       x = "Wind Speeds",
       y = "Frequency")

ggplot(subset(FA_combined_v2, CIG <= 5000), aes(x = CIG)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ceiling Height Across Years (Data Filled) - Fayetteville, AR",
       x = "Ceiling Height",
       y = "Frequency")

ggplot(subset(SB_combined_v2, CIG <= 5000), aes(x = CIG)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ceiling Height Across Years (Data Filled) - Sofia, BU",
       x = "Ceiling Height",
       y = "Frequency")

ggplot(FA_combined_v2, aes(x = TMP)) +
  geom_histogram(binwidth = 10, fill = 'blue', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Temperatures Across Years (Data Filled) - Fayetteville, AR",
       x = "Temperature",
       y = "Frequency")

ggplot(SB_combined_v2, aes(x = TMP)) +
  geom_histogram(binwidth = 10, fill = 'red', alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Temperatures Across Years (Data Filled) - Sofia, BU",
       x = "Temperature",
       y = "Frequency")

###Removal of median columns###

FA95 <- FA95[-c(7:9)]
FA05 <- FA05[-c(7:9)]
FA15 <- FA15[-c(7:9)]
SB95 <- SB95[-c(7:9)]
SB05 <- SB05[-c(7:9)]
SB15 <- SB15[-c(7:9)]

###Removal of '22000' value indiciating no clouds in CIG column###
FA95$CIG[FA95$CIG == 22000] <- NA
FA05$CIG[FA05$CIG == 22000] <- NA
FA15$CIG[FA15$CIG == 22000] <- NA
SB95$CIG[SB95$CIG == 22000] <- NA
SB05$CIG[SB05$CIG == 22000] <- NA
SB15$CIG[SB15$CIG == 22000] <- NA



############################################################################################################
#####Outlier removal using Z-score##########################################################################
############################################################################################################

FA_combined_v3 <- rbind(FA95, FA05, FA15)
SB_combined_v3 <- rbind(SB95, SB05, SB15)

###Scatterplot showing distribution of data before outlier removal###

ggplot(FA_combined_v3, aes(x = MONTH, y = WND)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of WND before outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(FA_combined_v3, aes(x = MONTH, y = CIG)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of CIG before outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(FA_combined_v3, aes(x = MONTH, y = TMP)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of TMP before outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v3, aes(x = MONTH, y = WND)) +
  geom_point(color = "red") +
  labs(title = "Distribution of WND before outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v3, aes(x = MONTH, y = CIG)) +
  geom_point(color = "red") +
  labs(title = "Distribution of CIG before outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v3, aes(x = MONTH, y = TMP)) +
  geom_point(color = "red") +
  labs(title = "Distribution of TMP before outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

###Removal of outliers (replacing with NA)##

FA95_stats <- FA95 %>% group_by(MONTH) %>% summarise (
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
FA95 <- FA95 %>% left_join(FA95_stats, by = 'MONTH')
FA95 <- FA95 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

FA95$WND[FA95$Z_WND >= 3] <- NA
FA95$WND[FA95$Z_WND <= -3] <- NA
FA95$CIG[FA95$Z_CIG >= 3] <- NA
FA95$CIG[FA95$Z_CIG <= -3] <- NA
FA95$TMP[FA95$Z_TMP >= 3] <- NA
FA95$TMP[FA95$Z_TMP <= -3] <- NA

FA05_stats <- FA05 %>% group_by(MONTH) %>% summarise (
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
FA05 <- FA05 %>% left_join(FA05_stats, by = 'MONTH')
FA05 <- FA05 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

FA05$WND[FA05$Z_WND >= 3] <- NA
FA05$WND[FA05$Z_WND <= -3] <- NA
FA05$CIG[FA05$Z_CIG >= 3] <- NA
FA05$CIG[FA05$Z_CIG <= -3] <- NA
FA05$TMP[FA05$Z_TMP >= 3] <- NA
FA05$TMP[FA05$Z_TMP <= -3] <- NA

FA15_stats <- FA15 %>% group_by(MONTH) %>% summarise(
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
FA15 <- FA15 %>% left_join(FA15_stats, by = 'MONTH')
FA15 <- FA15 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

FA15$WND[FA15$Z_WND >= 3] <- NA
FA15$WND[FA15$Z_WND <= -3] <- NA
FA15$CIG[FA15$Z_CIG >= 3] <- NA
FA15$CIG[FA15$Z_CIG <= -3] <- NA
FA15$TMP[FA15$Z_TMP >= 3] <- NA
FA15$TMP[FA15$Z_TMP <= -3] <- NA

SB95_stats <- SB95 %>% group_by(MONTH) %>% summarise(
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
SB95 <- SB95 %>% left_join(SB95_stats, by = 'MONTH')
SB95 <- SB95 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

SB95$WND[SB95$Z_WND >= 3] <- NA
SB95$WND[SB95$Z_WND <= -3] <- NA
SB95$CIG[SB95$Z_CIG >= 3] <- NA
SB95$CIG[SB95$Z_CIG <= -3] <- NA
SB95$TMP[SB95$Z_TMP >= 3] <- NA
SB95$TMP[SB95$Z_TMP <= -3] <- NA

SB05_stats <- SB05 %>% group_by(MONTH) %>% summarise(
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
SB05 <- SB05 %>% left_join(SB05_stats, by = 'MONTH')
SB05 <- SB05 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

SB05$WND[SB05$Z_WND >= 3] <- NA
SB05$WND[SB05$Z_WND <= -3] <- NA
SB05$CIG[SB05$Z_CIG >= 3] <- NA
SB05$CIG[SB05$Z_CIG <= -3] <- NA
SB05$TMP[SB05$Z_TMP >= 3] <- NA
SB05$TMP[SB05$Z_TMP <= -3] <- NA

SB15_stats <- SB15 %>% group_by(MONTH) %>% summarise(
  mean_WND = mean(WND, na.rm = TRUE),
  sd_WND = sd(WND, na.rm = TRUE),
  mean_CIG = mean(CIG, na.rm = TRUE),
  sd_CIG = sd(CIG, na.rm = TRUE),
  mean_TMP = mean(TMP, na.rm = TRUE),
  sd_TMP = sd(TMP, na.rm = TRUE))
SB15 <- SB15 %>% left_join(SB15_stats, by = 'MONTH')
SB15 <- SB15 %>% mutate(
  Z_WND = (WND - mean_WND) / sd_WND,
  Z_CIG = (CIG - mean_CIG) / sd_CIG,
  Z_TMP = (TMP - mean_TMP) / sd_TMP)

SB15$WND[SB15$Z_WND >= 3] <- NA
SB15$WND[SB15$Z_WND <= -3] <- NA
SB15$CIG[SB15$Z_CIG >= 3] <- NA
SB15$CIG[SB15$Z_CIG <= -3] <- NA
SB15$TMP[SB15$Z_TMP >= 3] <- NA
SB15$TMP[SB15$Z_TMP <= -3] <- NA

FA95 <- FA95[-c(7:15)]
FA05 <- FA05[-c(7:15)]
FA15 <- FA15[-c(7:15)]
SB95 <- SB95[-c(7:15)]
SB05 <- SB05[-c(7:15)]
SB15 <- SB15[-c(7:15)]

FA_combined_v4 <- rbind(FA95, FA05, FA15)
SB_combined_v4 <- rbind(SB95, SB05, SB15)

###Scatterplot showing distribution of date after outlier removal###

ggplot(FA_combined_v4, aes(x = MONTH, y = WND)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of WND after outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(FA_combined_v4, aes(x = MONTH, y = CIG)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of CIG after outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(FA_combined_v4, aes(x = MONTH, y = TMP)) +
  geom_point(color = "blue") +
  labs(title = "Distribution of TMP after outlier removal - Fayetteville, AR", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v4, aes(x = MONTH, y = WND)) +
  geom_point(color = "red") +
  labs(title = "Distribution of WND after outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v4, aes(x = MONTH, y = CIG)) +
  geom_point(color = "red") +
  labs(title = "Distribution of CIG after outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

ggplot(SB_combined_v4, aes(x = MONTH, y = TMP)) +
  geom_point(color = "red") +
  labs(title = "Distribution of TMP after outlier removal - Sofia, BU", x = "Month", y = "Wind") +
  theme_minimal()

############################################################################################################
#####Min Max Normalization##################################################################################
############################################################################################################

###

FA95_WND_norm_plot <- ggplot() +
  geom_line(data = FA95, aes(x = DATE, y = WND), color = 'blue') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA95_CIG_norm_plot <- ggplot() +
  geom_line(data = FA95, aes(x = DATE, y = CIG), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA95_TMP_norm_plot <- ggplot() +
  geom_line(data = FA95, aes(x = DATE, y = TMP), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA95_WND_norm_plot + FA95_CIG_norm_plot + FA95_TMP_norm_plot + plot_layout(ncol = 1)

FA05_WND_norm_plot <- ggplot() +
  geom_line(data = FA05, aes(x = DATE, y = WND), color = 'blue') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA05_CIG_norm_plot <- ggplot() +
  geom_line(data = FA05, aes(x = DATE, y = CIG), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA05_TMP_norm_plot <- ggplot() +
  geom_line(data = FA05, aes(x = DATE, y = TMP), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA05_WND_norm_plot + FA05_CIG_norm_plot + FA05_TMP_norm_plot + plot_layout(ncol = 1)

FA15_WND_norm_plot <- ggplot() +
  geom_line(data = FA15, aes(x = DATE, y = WND), color = 'blue') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA15_CIG_norm_plot <- ggplot() +
  geom_line(data = FA15, aes(x = DATE, y = CIG), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA15_TMP_norm_plot <- ggplot() +
  geom_line(data = FA15, aes(x = DATE, y = TMP), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA15_WND_norm_plot + FA15_CIG_norm_plot + FA15_TMP_norm_plot + plot_layout(ncol = 1)

SB95_WND_norm_plot <- ggplot() +
  geom_line(data = SB95, aes(x = DATE, y = WND), color = 'red') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB95_CIG_norm_plot <- ggplot() +
  geom_line(data = SB95, aes(x = DATE, y = CIG), color = 'red') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB95_TMP_norm_plot <- ggplot() +
  geom_line(data = SB95, aes(x = DATE, y = TMP), color = 'red') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB95_WND_norm_plot + SB95_CIG_norm_plot + SB95_TMP_norm_plot + plot_layout(ncol = 1)

SB05_WND_norm_plot <- ggplot() +
  geom_line(data = SB05, aes(x = DATE, y = WND), color = 'red') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB05_CIG_norm_plot <- ggplot() +
  geom_line(data = SB05, aes(x = DATE, y = CIG), color = 'red') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB05_TMP_norm_plot <- ggplot() +
  geom_line(data = SB05, aes(x = DATE, y = TMP), color = 'red') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB05_WND_norm_plot + SB05_CIG_norm_plot + SB05_TMP_norm_plot + plot_layout(ncol = 1)

SB15_WND_norm_plot <- ggplot() +
  geom_line(data = SB15, aes(x = DATE, y = WND), color = 'red') +
  ggtitle("Line Plot of WND Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB15_CIG_norm_plot <- ggplot() +
  geom_line(data = SB15, aes(x = DATE, y = CIG), color = 'red') +
  ggtitle("Line Plot of CIG Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB15_TMP_norm_plot <- ggplot() +
  geom_line(data = SB15, aes(x = DATE, y = TMP), color = 'red') +
  ggtitle("Line Plot of TMP Over Time Before Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB15_WND_norm_plot + SB15_CIG_norm_plot + SB15_TMP_norm_plot + plot_layout(ncol = 1)

###Normalization function###

min_max_normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

###Normalizations###

FA95_normalized <- FA95 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

FA05_normalized <- FA05 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

FA15_normalized <- FA15 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

SB95_normalized <- SB95 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

SB05_normalized <- SB05 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

SB15_normalized <- SB15 %>% mutate(
  WND_normalized = min_max_normalize(WND),
  CIG_normalized = min_max_normalize(CIG),
  TMP_normalized = min_max_normalize(TMP))

###

FA95_WND_norm_plot <- ggplot() +
  geom_line(data = FA95_normalized, aes(x = DATE, y = WND_normalized), color = 'blue') +
  ggtitle("Line Plot of WND Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA95_CIG_norm_plot <- ggplot() +
  geom_line(data = FA95_normalized, aes(x = DATE, y = CIG_normalized), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA95_TMP_norm_plot <- ggplot() +
  geom_line(data = FA95_normalized, aes(x = DATE, y = TMP_normalized), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA95_WND_norm_plot + FA95_CIG_norm_plot + FA95_TMP_norm_plot + plot_layout(ncol = 1)

FA05_WND_norm_plot <- ggplot() +
  geom_line(data = FA05_normalized, aes(x = DATE, y = WND_normalized), color = 'blue') +
  ggtitle("Line Plot of WND Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA05_CIG_norm_plot <- ggplot() +
  geom_line(data = FA05_normalized, aes(x = DATE, y = CIG_normalized), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA05_TMP_norm_plot <- ggplot() +
  geom_line(data = FA05_normalized, aes(x = DATE, y = TMP_normalized), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA05_WND_norm_plot + FA05_CIG_norm_plot + FA05_TMP_norm_plot + plot_layout(ncol = 1)

FA15_WND_norm_plot <- ggplot() +
  geom_line(data = FA15_normalized, aes(x = DATE, y = WND_normalized), color = 'blue') +
  ggtitle("Line Plot of WND Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

FA15_CIG_norm_plot <- ggplot() +
  geom_line(data = FA15_normalized, aes(x = DATE, y = CIG_normalized), color = 'blue') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

FA15_TMP_norm_plot <- ggplot() +
  geom_line(data = FA15_normalized, aes(x = DATE, y = TMP_normalized), color = 'blue') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Fayetteville, AR") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

FA15_WND_norm_plot + FA15_CIG_norm_plot + FA15_TMP_norm_plot + plot_layout(ncol = 1)

SB95_WND_norm_plot <- ggplot() +
  geom_line(data = SB95_normalized, aes(x = DATE, y = WND_normalized), color = 'red') +
  ggtitle("Line Plot of WND Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB95_CIG_norm_plot <- ggplot() +
  geom_line(data = SB95_normalized, aes(x = DATE, y = CIG_normalized), color = 'red') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB95_TMP_norm_plot <- ggplot() +
  geom_line(data = SB95_normalized, aes(x = DATE, y = TMP_normalized), color = 'red') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB95_WND_norm_plot + SB95_CIG_norm_plot + SB95_TMP_norm_plot + plot_layout(ncol = 1)

SB05_WND_norm_plot <- ggplot() +
  geom_line(data = SB05_normalized, aes(x = DATE, y = WND_normalized), color = 'red') +
  ggtitle("Line Plot of WND Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB05_CIG_norm_plot <- ggplot() +
  geom_line(data = SB05_normalized, aes(x = DATE, y = CIG_normalized), color = 'red') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB05_TMP_norm_plot <- ggplot() +
  geom_line(data = SB05_normalized, aes(x = DATE, y = TMP_normalized), color = 'red') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB05_WND_norm_plot + SB05_CIG_norm_plot + SB05_TMP_norm_plot + plot_layout(ncol = 1)

SB15_WND_norm_plot <- ggplot() +
  geom_line(data = SB15_normalized, aes(x = DATE, y = WND_normalized), color = 'red') +
  ggtitle("Line Plot of WND Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Wind Speed") +
  theme_minimal()

SB15_CIG_norm_plot <- ggplot() +
  geom_line(data = SB15_normalized, aes(x = DATE, y = CIG_normalized), color = 'red') +
  ggtitle("Line Plot of CIG Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Cloud Height") +
  theme_minimal()

SB15_TMP_norm_plot <- ggplot() +
  geom_line(data = SB15_normalized, aes(x = DATE, y = TMP_normalized), color = 'red') +
  ggtitle("Line Plot of TMP Over Time After Normalization for Sofia, BU") +
  labs(x = "Date", y = "Temperature") +
  theme_minimal()

SB15_WND_norm_plot + SB15_CIG_norm_plot + SB15_TMP_norm_plot + plot_layout(ncol = 1)

############################################################################################################
#####Data Aggregation#######################################################################################
############################################################################################################

FA95_monthly_aggregates <- FA95_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),

  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

FA05_monthly_aggregates <- FA05_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),
  
  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

FA15_monthly_aggregates <- FA15_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),
  
  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

SB95_monthly_aggregates <- SB95_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),
  
  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

SB05_monthly_aggregates <- SB05_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),
  
  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

SB15_monthly_aggregates <- SB15_normalized %>% group_by(MONTH) %>% summarise(
  avg_WND = mean(WND_normalized, na.rm = TRUE),
  sd_WND = sd(WND_normalized, na.rm = TRUE),
  var_WND = var(WND_normalized, na.rm = TRUE),
  
  avg_CIG = mean(CIG_normalized, na.rm = TRUE),
  sd_CIG = sd(CIG_normalized, na.rm = TRUE),
  var_CIG = var(CIG_normalized, na.rm = TRUE),
  
  avg_TMP = mean(TMP_normalized, na.rm = TRUE),
  sd_TMP = sd(TMP_normalized, na.rm = TRUE),
  var_TMP = var(TMP_normalized, na.rm = TRUE))

FA95_monthly_aggregates$YEAR <- 1995
FA05_monthly_aggregates$YEAR <- 2005
FA15_monthly_aggregates$YEAR <- 2015
SB95_monthly_aggregates$YEAR <- 1995
SB05_monthly_aggregates$YEAR <- 2005
SB15_monthly_aggregates$YEAR <- 2015

FA_aggregated_data <- bind_rows(FA95_monthly_aggregates, FA05_monthly_aggregates, FA15_monthly_aggregates)
aggregated_data$MONTH <- factor(FA_aggregated_data$MONTH, levels = 1:12)

SB_aggregated_data <- bind_rows(SB95_monthly_aggregates, SB05_monthly_aggregates, SB15_monthly_aggregates)
aggregated_data$MONTH <- factor(SB_aggregated_data$MONTH, levels = 1:12)

###Graphs showing aggregated data###

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_WND, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_WND - sd_WND, ymax = avg_WND + sd_WND), width = 0.2) +
  geom_text(aes(label = round(var_WND, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of WND (Wind) with Standard Deviation and Variance by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Wind', color = 'Year') +
  theme_minimal()

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_CIG, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_CIG - sd_CIG, ymax = avg_CIG + sd_CIG), width = 0.2) +
  geom_text(aes(label = round(var_CIG, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of CIG (Ceiling Height) with Standard Deviation and Variance by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Height', color = 'Year') +
  theme_minimal()

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_TMP, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_TMP - sd_TMP, ymax = avg_TMP + sd_TMP), width = 0.2) +
  geom_text(aes(label = round(var_TMP, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of TMP (Temperature) with Standard Deviation and Variance by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Temperature', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_WND, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_WND - sd_WND, ymax = avg_WND + sd_WND), width = 0.2) +
  geom_text(aes(label = round(var_WND, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of WND (Wind) with Standard Deviation and Variance by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Wind', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_CIG, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_CIG - sd_CIG, ymax = avg_CIG + sd_CIG), width = 0.2) +
  geom_text(aes(label = round(var_CIG, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of CIG (Ceiling Height) with Standard Deviation and Variance by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Height', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_TMP, group = YEAR, color = as.factor(YEAR))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = avg_TMP - sd_TMP, ymax = avg_TMP + sd_TMP), width = 0.2) +
  geom_text(aes(label = round(var_TMP, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of TMP (Temperature) with Standard Deviation and Variance by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Temperature', color = 'Year') +
  theme_minimal()

############################################################################################################
#####Exponential Smoothing##################################################################################
############################################################################################################

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_WND, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_WND - sd_WND, ymax = avg_WND + sd_WND), width = 0.2) +
  geom_text(aes(label = round(var_WND, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of WND by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Wind', color = 'Year') +
  theme_minimal()

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_CIG, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_CIG - sd_CIG, ymax = avg_CIG + sd_CIG), width = 0.2) +
  geom_text(aes(label = round(var_CIG, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of CIG by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Cloud Height', color = 'Year') +
  theme_minimal()

ggplot(FA_aggregated_data, aes(x = MONTH, y = avg_TMP, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_TMP - sd_TMP, ymax = avg_TMP + sd_TMP), width = 0.2) +
  geom_text(aes(label = round(var_TMP, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of TMP by Year for Fayetteville, AR") +
  labs(x = 'Month', y = 'Average Temperature', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_WND, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_WND - sd_WND, ymax = avg_WND + sd_WND), width = 0.2) +
  geom_text(aes(label = round(var_WND, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of WND by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Wind', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_CIG, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_CIG - sd_CIG, ymax = avg_CIG + sd_CIG), width = 0.2) +
  geom_text(aes(label = round(var_CIG, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of CIG by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Cloud Height', color = 'Year') +
  theme_minimal()

ggplot(SB_aggregated_data, aes(x = MONTH, y = avg_TMP, group = YEAR, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, span = 0.3) +
  geom_errorbar(aes(ymin = avg_TMP - sd_TMP, ymax = avg_TMP + sd_TMP), width = 0.2) +
  geom_text(aes(label = round(var_TMP, 2)), vjust = -1, size = 3) +
  facet_wrap(~ YEAR) +
  ggtitle("Monthly Averages of TMP by Year for Sofia, BU") +
  labs(x = 'Month', y = 'Average Temperature', color = 'Year') +
  theme_minimal()
