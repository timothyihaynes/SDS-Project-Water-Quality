# SDS-Project-Water-Quality
Analyzing the relationship between pathogenic bacteria and organic nutrients present in freshwater samples taken in Austin.
#select data
Water_Quality <- read.csv("Water_Quality.csv")
# Separate month, date, year, and time
Water_Quality_new <- Water_Quality %>% separate(SAMPLE_DATE, into = c("Date", "Time", "Meridium"), sep = " ")
Water_Quality_new <- Water_Quality_1 %>% separate(Date, into = c("Month", "Day", "Year"), sep = "/")
#Visualization 1
Water_Quality_new %>% group_by(PARAMETER)%>% filter(PARAMETER == "E COLI BACTERIA") %>% ggplot(aes(x=WATERSHED)) + geom_bar(alpha=1, size=2)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
