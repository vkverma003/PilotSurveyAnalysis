rm(list = ls())
cat("\014")
graphics.off()

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

#1. Locating file location and reading them ====

jai_data <- "E:/Other computers/Dept. Desktop/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/1. Jaipur PT Data.xlsx"
del_data <- "E:/Other computers/Dept. Desktop/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/2a. Pilot Survey Analysis.xlsx"
del_QRdata <- "E:/Other computers/Dept. Desktop/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/Delhi QR Code Data.xlsx"

#2. Reading Bus and Metro dataset for both Jaipur and Metro Transit ====

jai_bus <- read_xlsx(jai_data, sheet = "Bus", range = "A1:BE318", col_names = TRUE)
jai_metro <- read_xlsx(jai_data, sheet = "Metro", range = "A1:BD78", col_names = TRUE)

del_bus <- read_xlsx(del_data, sheet = "Bus-Unified", range = "A1:BQ63", col_names = TRUE)
del_metro <- read_xlsx(del_data, sheet = "Metro-Unified", range = "A1:BO62", col_names = TRUE)
del_busQRCode <- read_xlsx(del_QRdata, sheet = "2. Bus Pilot Survey- Phase II", range = "A1:BN2", col_names = TRUE)

#3. Creating consistent entries in dataset ====

#3.1 Survey Mode Information ====
unique <- sort(unique(jai_bus$Mode))
unique

jai_bus$SurMode[jai_bus$Mode %in% unique[1:21]] <- "QR Code"
jai_bus$SurMode[jai_bus$Mode %in% unique[22]] <- "Paper"

unique <- sort(unique(jai_metro$Mode))
unique

jai_metro$SurMode[jai_metro$Mode %in% unique[1]] <- "Paper"
jai_metro$SurMode[jai_metro$Mode %in% unique[2]] <- "QR Code"

del_metro$SurMode <- "Paper"
del_bus$SurMode <- "Paper"
del_busQRCode$SurMode <- "QR Code"

#3.2 Socioeconomic Information ====

#AgeGroup
unique <- sort(unique(del_busQRCode$`Age Group`))
unique

del_busQRCode$`Age Group`[del_busQRCode$`Age Group` %in% unique[1]] <- "15-25"

#Education Level
unique <- sort(unique(jai_bus$`Education level`))
unique

jai_bus$`Education level`[jai_bus$`Education level` %in% unique[2]] <- "Illiterate"

#INcome Category
unique <- sort(unique(jai_bus$`Income Category`))
unique

jai_bus$`Income Category`[jai_bus$`Income Category` %in% unique[2]] <- "10001-25000"
jai_bus$`Income Category`[jai_bus$`Income Category` %in% unique[3]] <- "25001-50000"

unique <- sort(unique(jai_metro$`Income Category (per month)`))
unique

jai_metro$`Income Category (per month)`[jai_metro$`Income Category (per month)` %in% unique[2]] <- "10001-25000"    
jai_metro$`Income Category (per month)`[jai_metro$`Income Category (per month)` %in% unique[3]] <- "25001-50000"

unique <- sort(unique(del_metro$`Income Category`))
unique

del_metro$`Income Category`[del_metro$`Income Category` %in% unique[5]] <- "Nil"

unique <- sort(unique(del_bus$`Income Category`))
unique

del_bus$`Income Category`[del_bus$`Income Category` %in% unique[1]] <- "10000 or Below"

unique <- sort(unique(del_busQRCode$`Income Category (per month)`))
unique

del_busQRCode$`Income Category (per month)`[del_busQRCode$`Income Category (per month)` %in% unique[1]] <- "Nil"

#Renaming socio columns for consistent entries
colnames(del_bus)[63:70] <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")
del_bus$Transit <- "Bus"
del_bus$City <- "New Delhi"

colnames(del_busQRCode)[60:67] <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")
del_busQRCode$Transit <- "Bus"
del_busQRCode$City <- "New Delhi"

colnames(del_metro)[61:68] <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")
del_metro$Transit <- "Metro"
del_metro$City <- "New Delhi"

colnames(jai_bus)[54:58] <- c("Gender", "AgeGroup", "Education", "Income", "SurveyMode")
jai_bus$Transit <- "Bus"
jai_bus$City <- "Jaipur"

colnames(jai_metro)[53:57] <- c("Gender", "AgeGroup", "Education", "Income", "SurveyMode")
jai_metro$Transit <- "Metro"
jai_metro$City <- "Jaipur"


#3.3 Survey Formats Information ====
del_bus$Format <- "Format 2"
del_busQRCode$Format <- "Format 2"

del_metro$Format <- NA
del_metro$Format[c(1:3, 9, 10)] <- "Format 3"
del_metro$Format[c(4:8, 11:61)] <- "Format 2"

jai_bus$Format<- "Format 1"
jai_metro$Format<- "Format 1"

#4. Survey Sampling Information ====

#Combinig both Jaipur and New Delhi sample infor
Comb_Samdata <- bind_rows(del_bus[ , c(1, 70:73)], del_busQRCode[1 ,67:70], del_metro[ , c(1, 68:71)],
                          jai_bus[ , c(58:61)], jai_metro[ , c(57:60)])

Comb_Samdata %>%
  select(-c(`Sample No`)) %>% 
  table() %>% 
  data.frame() %>%
  filter(Freq > 0) %>% 
  ggplot(aes(x = Format, y = Freq, fill = SurveyMode)) +
  geom_col() +
  facet_grid(City ~ Transit) +
  geom_text(aes(label = Freq), vjust = -0.4) + # Adjust vjust as needed
  labs(x = "Questionnaire Formats", y = "No. of Sample Collected") +
  guides(fill = guide_legend(title = "Survey Mode"))


#5. Plotting socioeconomic dataset ====

#Combinig both Jaipur and New Delhi socioeconomic dataset
Comb_Sociodata <- bind_rows(del_bus[, 63:72], del_busQRCode[, 60:69], del_metro[, 61:70],
               jai_bus[, 54:60], jai_metro[, 53:59])

#4.1 With respect to Transit wise ====

Gender <- Comb_Sociodata %>%
    select(Gender, SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill = Gender)) +
    geom_col() +
    guides(fill=guide_legend(title="Gender")) +
    theme_classic()


AgeGroup <- Comb_Sociodata %>%
    select(AgeGroup, SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill=factor(AgeGroup, levels = c("Above 60", "46-60", "26-45", "15-25", "Under 15")))) +
    geom_col() +
    guides(fill=guide_legend(title="Age Group")) +
    theme_classic()


Education <- Comb_Sociodata %>%
    select(Education, SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill = factor(Education, levels = c("PG or Higher", "Graduate", "Up to 12th", "Illiterate")))) +
    geom_col() +
    guides(fill=guide_legend(title="Education")) +
    theme_classic()


Income <- Comb_Sociodata %>%
    select(Income , SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill = factor(Income, levels = c("50001-1 Lakh", "25001-50000", "10001-25000", "10000 or Below", "Nil")))) +
    geom_col() +
    guides(fill=guide_legend(title="Income")) +
    theme_classic()

Ownership <- Comb_Sociodata %>%
    select(Ownership, SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill = factor(Ownership, levels = c( "Both", "Car", "2 Wheeler", "None")))) +
    geom_col() +
    guides(fill=guide_legend(title="Ownership")) +
    theme_classic()


License <- Comb_Sociodata %>%
    select(License, SurveyMode, Transit, City) %>% 
    table() %>% 
    data.frame() %>% 
    group_by(Transit) %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=Transit, y=Percent, fill = factor(License, levels = c( "Both", "Car", "2 Wheeler", "None")))) +
    geom_col() +
    guides(fill=guide_legend(title="License")) +
    theme_classic()


#Arranging these plots in single plot by ggpubr package
ggarrange(Gender, AgeGroup, Education, Income, Ownership, License, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3)

#4.2 With respect to Survey Mode wise ====

Gender2 <- Comb_Sociodata %>% 
    select(SurveyMode, Gender) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = Gender)) +
    geom_col()

AgeGroup2 <- Comb_Sociodata %>% 
    select(SurveyMode, AgeGroup) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = factor(AgeGroup, levels = c("Above 60", "46-60", "26-45", "15-25", "Under 15")))) +
    geom_col() +
    guides(fill=guide_legend(title="Age Group")) +
    theme_classic()

Education2 <- Comb_Sociodata %>% 
    select(SurveyMode, Education) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = factor(Education, levels = c("PG or Higher", "Graduate", "Up to 12th", "Illiterate")))) +
    geom_col() +
    guides(fill=guide_legend(title="Education")) +
    theme_classic()

Income2 <- Comb_Sociodata %>% 
    select(SurveyMode, Income) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = factor(Income, levels = c("50001-1 Lakh", "25001-50000", "10001-25000", "10000 or Below", "Nil")))) +
    geom_col() +
    guides(fill=guide_legend(title="Income")) +
    theme_classic()

Ownership2 <- Comb_Sociodata %>% 
    select(SurveyMode, Ownership) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = factor(Ownership, levels = c("Both", "Car", "2 Wheeler", "None")))) +
    geom_col() +
    guides(fill=guide_legend(title="Ownership")) +
    theme_classic()

License2 <- Comb_Sociodata %>% 
    select(SurveyMode, License) %>%
    table() %>% 
    data.frame() %>% 
    group_by(SurveyMode) %>%
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(x=SurveyMode, y=Percent, fill = factor(License, levels = c("Both", "Car", "2 Wheeler", "None")))) +
    geom_col() +
    guides(fill=guide_legend(title="License")) +
    theme_classic()

#Arranging these plots in single plot by ggpubr package
ggarrange(Gender2, AgeGroup2, Education2, Income2, Ownership2, License2, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3)


#4.2.1. Mann-Whitney U Test to examine the Difference in Sample population ====
a <- Comb_Sociodata %>% 
    filter(SurveyMode == "Paper") %>% 
    select(Income) %>%
    na.omit() %>% 
    unlist()

b <- Comb_Sociodata %>% 
    filter(SurveyMode == "QR Code") %>% 
    select(Income) %>% 
    na.omit() %>% 
    unlist()

# Convert the categorical data into numerical data
sample1_numeric <- as.numeric(factor(a))
sample2_numeric <- as.numeric(factor(b))

# Perform the Mann-Whitney U test
result <- wilcox.test(sample1_numeric, sample2_numeric)

# View the result
print(result)


#5. Satisfaction and Importance Scale Presentation and Information Plots =====

del_metro$`System Infra: Parking- Space Adeqacy` <- as.numeric(del_metro$`System Infra: Parking- Space Adeqacy`)
del_metro$`System Infra: Parking- Dist to the parking bay` <- as.numeric(del_metro$`System Infra: Parking- Dist to the parking bay`)
del_metro$`System Infra: Parking- cost` <- as.numeric(del_metro$`System Infra: Parking- cost`)

#5.1 With respect to the Formats ====

del_satis_bus <- del_bus %>% 
  filter(Format == "Format 2") %>%
  select(`Overall Satisfaction`, Frequency:`Overall transport service`, SurveyMode) %>% 
  pivot_longer(`Overall Satisfaction`:`Overall transport service`, names_to = "Attributes", values_to = "Rating") %>% 
  table() %>% 
  data.frame() %>% 
  select(SurveyMode, Rating, Freq) %>% 
  filter(Rating != 15) %>% 
  group_by(SurveyMode, Rating) %>%
  summarise(Count = sum(Freq))

del_satis_qrcode = del_busQRCode %>% 
  filter(Format == 'Format 2') %>% 
  select(`overall satisfaction`, frequency:`Overall Satisfaction (After)`, SurveyMode) %>% 
  pivot_longer(`overall satisfaction`:`Overall Satisfaction (After)`, names_to = 'Attributes', 
               values_to = 'Rating') %>% 
  table() %>% 
  data.frame() %>% 
  select(SurveyMode, Rating, Freq) %>% 
  filter(Freq != 0) %>% 
  group_by(SurveyMode, Rating) %>% 
  summarise(Count = sum(Freq))
  

del_satis_metro <- del_metro %>% 
  filter(Format == 'Format 2') %>%
  select(`Overall Satisfaction`, Frequency:`Overall transport service`, SurveyMode) %>% 
  pivot_longer(`Overall Satisfaction`:`Overall transport service`, names_to = "Attributes", values_to = "Rating") %>% 
  table() %>% 
  data.frame() %>% 
  select(SurveyMode, Rating, Freq) %>% 
  group_by(SurveyMode, Rating) %>%
  summarise(Count = sum(Freq))

del_format2 <- bind_rows(del_satis_bus, del_satis_metro, del_satis_qrcode) %>% 
  group_by(SurveyMode, Rating) %>% 
  summarise(Total = sum(Count)) %>% 
  mutate(Percent = round(Total/sum(Total), 2), Format = "Format 2") %>% 
  ggplot(aes(x=Rating, y=Percent)) +
  geom_col() +
  facet_grid(~SurveyMode)+
  ggtitle("Format 2") +
  labs(y= "Total Responses (Relative)")

jai_import_bus <- jai_bus %>% 
  filter(Format == 'Format 1') %>% 
  select(`Schedule Reliability`:`Posting of security staff inside the bus or at the bus stop`, SurveyMode) %>% 
  pivot_longer(cols = -SurveyMode, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(SurveyMode, Rating, Freq) %>% 
    group_by(SurveyMode, Rating) %>%
    summarise(Count = sum(Freq))

jai_import_metro <- jai_metro %>% 
  filter(Format == 'Format 1') %>% 
  select(`Schedule Reliability`:`Posting of security staff inside the metro or at the metro station`, SurveyMode)%>% 
    pivot_longer(cols = -SurveyMode, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(SurveyMode, Rating, Freq) %>% 
    group_by(SurveyMode, Rating) %>%
    summarise(Count = sum(Freq))

jai_format1 <- full_join(jai_import_bus, jai_import_metro) %>% 
    group_by(SurveyMode, Rating) %>% 
    summarise(Total = sum(Count)) %>% 
    mutate(Percent = round(Total/sum(Total), 2),
           Format = "Format 1") %>% 
    ggplot(aes(x=Rating, y=Percent)) +
    geom_col()+
  facet_grid(~SurveyMode)+
    ggtitle("Format 1") +
    labs(y= "Total Responses (Relative)")

library(ggpubr)
ggarrange(jai_format1,
          del_format2,
          nrow = 2,
          ncol = 1)

#6. Fractional and Discrete Values Scale ====

library(purrr)

del_metro %>% 
  filter(Format == 'Format 3') %>% 
  select(`Overall Satisfaction`, Frequency:`Overall transport service`)

