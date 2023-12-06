library(dplyr)

# Uploaded 'waterdata_ecoli_nutrients.csv'

# MORE DF CLEANING 
# change column names for e. coli and nutrients
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'NITRATE/NITRITE AS N'] <- 'NITRATE_NITRITE'
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'E COLI BACTERIA'] <- 'E_COLI'
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'AMMONIA AS N'] <- 'AMMONIA'
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'ORTHOPHOSPHORUS AS P'] <- 'ORTHOPHOS'
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'TOTAL KJELDAHL NITROGEN AS N'] <- 'totalnitrogen'
colnames(waterdata_ecoli_nutrients)[colnames(waterdata_ecoli_nutrients) == 'PHOSPHORUS AS P'] <- 'phosphorus'
# Remove rows where columns where e. coli or nutrients of interest have NA values
cols_to_check <- c('NITRATE_NITRITE', 'E_COLI', 'AMMONIA', 'ORTHOPHOS')
waterdata_nutrients_new <- waterdata_ecoli_nutrients[rowSums(is.na(waterdata_ecoli_nutrients[cols_to_check])) == 0, ]
waterdata_nutrients_new <- waterdata_nutrients_new %>% dplyr::select(-totalnitrogen, -phosphorus)

# Determining outliers in E. coli data
plot(waterdata_nutrients_new$E_COLI, main = "Scatter Plot")
waterdata_nutrients_new %>% dplyr::select(E_COLI) %>% arrange(E_COLI) %>% pull(E_COLI)
waterdata_nutrients_new %>% summarize(quantile(E_COLI, .975), quantile(E_COLI, .025))
# 97.5% of the data in the E_COLI column falls below the value 2400 (output of code directly above)

# messing around to determine distribution 
waterdata_nutrients_new %>% dplyr::select(E_COLI) %>% filter(E_COLI > 0) %>% filter(E_COLI < 2400) %>% arrange(desc(E_COLI)) %>% pull(E_COLI)
waterdata_nutrients_new1 <- waterdata_nutrients_new %>% filter(E_COLI > 0) %>% filter(E_COLI < 2400)
waterdata_nutrients_new1 %>% arrange(E_COLI) %>% pull(E_COLI)
hist(waterdata_nutrients_new1$E_COLI, main = "Hist") #appears to have exponential dist.
waterdata_nutrients_new1 %>% mutate(log_ecoli = log(E_COLI)) %>% dplyr::select(log_ecoli) %>% arrange(log_ecoli) %>% pull(log_ecoli)
waterdata_nutrients_new1 %>% mutate(log_ecoli = log(E_COLI)) %>% dplyr::select(log_ecoli) %>% ggplot(aes(x=log_ecoli)) + geom_histogram()

# determining outliers and distributions for ammonia, nitrate/nitrate, and orthophosphorus
# ammonia 
plot(waterdata_nutrients_new1$AMMONIA, main = "Scatter Plot")
waterdata_nutrients_new1 %>% summarize(quantile(AMMONIA, .90), quantile(AMMONIA, .1))
waterdata_nutrients_new1 %>% filter(AMMONIA < 0.05) %>% filter(AMMONIA > 0.008) %>% ggplot(aes(x=AMMONIA)) + geom_histogram() 
# nitrate/nitrite
plot(waterdata_nutrients_new1$NITRATE_NITRITE, main = "Scatter Plot")
waterdata_nutrients_new1 %>% summarize(quantile(NITRATE_NITRITE, .975), quantile(NITRATE_NITRITE, .025))
waterdata_nutrients_new1 %>% filter(NITRATE_NITRITE < 4.14) %>% filter(NITRATE_NITRITE > 0.008) %>% ggplot(aes(x=NITRATE_NITRITE)) + geom_histogram() 
# orthophosphorus 
plot(waterdata_nutrients_new1$ORTHOPHOS, main = "Scatter Plot")
waterdata_nutrients_new1 %>% summarize(quantile(ORTHOPHOS, .95), quantile(ORTHOPHOS, .05))
waterdata_nutrients_new1 %>% filter(ORTHOPHOS < 0.079) %>% filter(ORTHOPHOS > 0.002) %>% ggplot(aes(x=ORTHOPHOS)) + geom_histogram() 

# Create new df, only keeping rows that do not have outliers for ammonia, nitrate/nitrite, or orthophosphorus concentration 
waterdata_nutrients_new2 <- waterdata_nutrients_new1 %>% filter(AMMONIA < 0.05) %>% filter(AMMONIA > 0.008) %>% filter(NITRATE_NITRITE < 4.14) %>% filter(NITRATE_NITRITE > 0.008) %>% filter(ORTHOPHOS < 0.079) %>% filter(ORTHOPHOS > 0.002)
# histograms of data (w/o outliers) does not pass normality assumption...
# add rows for transformed data
waterdata_nutrients_new2 <- waterdata_nutrients_new2 %>% mutate(log_ecoli = log(E_COLI)) %>% mutate(log_ammonia = log(AMMONIA)) %>% mutate(log_nit = log(NITRATE_NITRITE)) %>% mutate(log_orthophos = log(ORTHOPHOS))
# make sure transformed data appears normal 
hist(waterdata_nutrients_new2$log_ecoli, main = "Log E. coli Histogram") 
hist(waterdata_nutrients_new2$log_ammonia, main = "Log Ammonia Histogram") # normal enough
hist(waterdata_nutrients_new2$log_nit, main = "Log Nitrate/Nitrite Histogram") # normal enough 
hist(waterdata_nutrients_new2$log_orthophos, main = "Log Orthophosphorus Histogram") #normal enough

waterdata_clean <- waterdata_nutrients_new2

# PLOTTING THE DATA (SCATTERPLOTS)

# E.coli vs Ammonia
waterdata_clean %>% ggplot(aes(x = AMMONIA, y = E_COLI)) +
  geom_point() +
  labs(title = "Scatter Plot of E. coli vs Ammonia", x = "Ammonia (Mg/L)", y = "E. coli (Colonies/100mL)")
# log E. coli vs log Ammonia
waterdata_clean %>% ggplot(aes(x = log_ammonia, y = log_ecoli)) +
  geom_point() +
  labs(title = "Scatter Plot of log E. coli vs log Ammonia", x = "log Ammonia", y = "log transformed E. coli")
# log E. coli vs log Ammonia, colored by site_type
waterdata_clean %>% ggplot(aes(x = log_ammonia, y = log_ecoli)) +
  geom_point(aes(color = SITE_TYPE)) +
  labs(title = "Scatter Plot", x = "log Ammonia", y = "log transformed E. coli")

# E. coli vs. Nitrate/Nitrite
waterdata_clean %>% ggplot(aes(x = NITRATE_NITRITE, y = E_COLI)) +
  geom_point() +
  labs(title = "Scatter Plot of E. coli vs Nitrate/Nitrite", x = "Nitrate/Nitrite (Mg/L)", y = "E. coli (Colonies/100mL)")
# log E. coli vs log Nitrate/Nitrite
waterdata_clean %>% ggplot(aes(x = log_nit, y = log_ecoli)) +
  geom_point() +
  labs(title = "Scatter Plot of log E. coli vs log Nitrate/Nitrite", x = "log Nitrate/Nitrite", y = "log E. coli")
# log E. coli vs log Nitrate/Nitrite, colored by site_type
waterdata_clean %>% ggplot(aes(x = log_nit, y = log_ecoli)) +
  geom_point(aes(color = SITE_TYPE)) +
  labs(title = "Scatter Plot", x = "log Nitrate/Nitrite", y = "log E. coli")

# E. coli vs. Orthophosphorus
waterdata_clean %>% ggplot(aes(x = ORTHOPHOS, y = E_COLI)) +
  geom_point() +
  labs(title = "Scatter Plot of E. coli vs Orthophosphorus", x = "Orthophosphorus (Mg/L)", y = "E. coli (Colonies/100mL)")
# log E. coli vs log Orthophosphorus
waterdata_clean %>% ggplot(aes(x = log_orthophos, y = log_ecoli)) +
  geom_point() +
  labs(title = "Scatter Plot of log E. coli vs log Orthophosphorus", x = "log Orthophosphorus", y = "log E. coli")
# log E. coli vs log Orthophosphorus, colored by site_type
waterdata_clean %>% ggplot(aes(x = log_orthophos, y = log_ecoli)) +
  geom_point(aes(color = SITE_TYPE)) +
  labs(title = "Scatter Plot", x = "log Orthophosphorus", y = "log E. coli")

write.csv(waterdata_clean, file = "waterdata_clean.csv", row.names = TRUE)
