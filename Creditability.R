
#Load relevant Libraries
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)


#Load data
d1 <- as_tibble(read_xlsx("C:/Users/GetInnotized/Downloads/Dataset 1.xlsx"))
head(d1)


# Add the Creditability  and Reason_for_non_creditability fields
d2 <- d1 %>%
  
  mutate(
         
         Creditable = ifelse(!is.na(default_phone) & !duplicated(default_phone) & nchar(default_phone) == 12 & !is.na(SN) & !duplicated(SN),
                             yes = "Creditable",
                             no = "Not Creditable"
                             ),
                missing_phone = ifelse(is.na(default_phone), "Missing Phone",""),
           
                duplicated_phone = ifelse(!is.na(default_phone), 
                                yes = ifelse(duplicated(default_phone), "Duplicated Phone",""),
                                no = ""
                                ),
         
                valid_phone = ifelse(!is.na(default_phone) & nchar(default_phone) != 12, "Valid Phone",""),
         
                missing_SN =  ifelse(is.na(SN), "Missing SN",""),
         
                duplicated_SN = ifelse(!is.na(SN), 
                                yes = ifelse(duplicated(SN), "Duplicated SN",""),
                                no = ""
                                ),
         
         Reason_for_non_creditability = paste(missing_phone, duplicated_phone, valid_phone, missing_SN, duplicated_SN
                                             )
        
         ) %>% 
  
  select(case_id, default_phone, SN, Product, original_sale_date, valid, Creditable, Reason_for_non_creditability)


write.csv(d2, file = "C:/Users/GetInnotized/Downloads/BURN CASE STUDY/d2.csv", row.names = FALSE)

# Plot 1

d1_summary_Creditable <- d2 %>% filter(Creditable == "Creditable") %>% 
  group_by(original_sale_date) %>% 
  summarise(count = n())

d1_summary_Not_Creditable <- d2 %>% filter(Creditable == "Not Creditable") %>% 
  group_by(original_sale_date) %>% 
  summarise(count = n())

 ggplot() + 
  geom_line(d1_summary_Creditable, mapping = aes(x = original_sale_date, y = count), color = "blue") +
  geom_line(d1_summary_Not_Creditable, mapping = aes(x = original_sale_date, y = count), color = "red") +
  xlab('original_sale_date') +
  ylab('Creditability')
  



#Plot 2

d2_summary <- d2 %>% 
  group_by(Reason_for_non_creditability) %>% 
  summarise(count = n())

ggplot(d2_summary, aes(x = reorder(Reason_for_non_creditability, -count), y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Reason for Non Creditability") +
  xlab("Reason")
 

#render markdown:

rmarkdown::render(
  "C:/Users/GetInnotized/Downloads/BURN CASE STUDY/Exercise 1/Creditability.R", 
  output_format = rmarkdown::html_document(
   
  )
)
