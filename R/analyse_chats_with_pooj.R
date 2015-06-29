# library(xml2)
library(XLConnect)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)
library(lubridate)
#### read xml data, preferrably using the xsl ####
# doc = xmlParse(file = "data/sms-20150627233547.xml")
# ls = xmlToList()

# xsl = read_xml("data/sms.xsl")
# xsl_ls = as_list(xsl)
# doc = read_xml("data/sms-20150627233547.xml")
# ls = as_list(doc)

d_in1 = XLConnect::readWorksheetFromFile(file = "data/sms-20150627233547.xlsx", sheet = "Sheet1")

d_in1$date_ft = as.POSIXct(x = (d_in1$date/1000), tz = "America/New_York", origin = "1970-01-01")
d_in1$type_ft = ifelse(d_in1$type == 1, "Pooj", "Dru")
d_in1$week1 = d_in1$date_ft %>% week
d_in1.1 = d_in1 %>%
  group_by(week1) %>%
  mutate(week2 = min(date_ft))
d_in1.1$week_ft =  d_in1.1$week2 %>% 
  round("days" ) %>%
  ymd(tz = "America/New_York")

week_vec = as.matrix(unique(d_in1.1$week_ft))

plot_comparison_cloud = function(week_cond, d_in){
  print(week_cond)
  d_in1 = d_in %>%
    filter(week_ft %in% week_cond)
  
  d_in2.1 = d_in1 %>%
    filter(type_ft == "Pooj") %>% 
    select(body) 
  
  d_in2.2 = d_in1 %>%
    filter(type_ft == "Dru") %>% 
    select(body) 
  
  d_in2 = d_in2.1 %>%
    list(d_in2.2) %>%
    VectorSource() %>%
    Corpus()
  
  d_in3 = d_in2 %>%
    tm_map(stripWhitespace) %>%
    tm_map(tolower) %>%
    tm_map(removeWords, stopwords("SMART")) %>%
    tm_map(removeWords, words = c("hey", "haha", "yes", "yep", "yup", "yea", "yeah", "hmm", "ohh", "che", "hai")) %>%
    tm_map(removePunctuation ) %>%
    tm_map(stemDocument) %>%
    tm_map(PlainTextDocument)
  
  d_in4 = d_in3 %>%
    TermDocumentMatrix() %>%
    as.matrix
  week_cond = as.POSIXct(x = (week_cond), tz = "America/New_York", origin = "1970-01-01")
  colnames(d_in4) <- c(paste0(week_cond,"\n\nPooj"), "Dru")
  f_name = paste0(week_cond,"_PoojDru.png")
  
  change_wd_flag = FALSE
  if(length(grep("plots", getwd())) == 0) change_wd_flag = TRUE
  
  if(change_wd_flag) setwd("plots")
  
  png(f_name, width=6, height=6, units="in", res=500)
  comparison.cloud(d_in4, title.size = 1.5)
  dev.off()
  
  if(change_wd_flag)  setwd("..")
}

a = apply(X = week_vec, MARGIN = 1, FUN = plot_comparison_cloud, d_in1.1)
plot_comparison_cloud(tail(d_in1.1$week_ft, 1), d_in1.1)



#### wordcloud time line ####
