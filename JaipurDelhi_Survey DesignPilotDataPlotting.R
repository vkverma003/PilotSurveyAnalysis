rm(list = ls())
cat("\014")
graphics.off()


library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

#1. Locating file location and reading them ====

jai_data <- "D:/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/1. Jaipur PT Data.xlsx"
del_data <- "D:/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/2a. Pilot Survey Analysis.xlsx"
del_QRdata <- "D:/4. IITR/5. Data Collection/1. Until Pilot Level Survey/8. Preliminary Analysis/3. Combined/Delhi QR Code Data.xlsx"


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
    
#4. Slicing the dataset for socioeconomic information ====

#For Delhi
delsocio <- del_bus[ , 63:70]
colnames(delsocio) <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")

delsocio2 <- del_busQRCode[ , 60:67]
colnames(delsocio2) <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")

Delhi <- rbind(delsocio, delsocio2)
Delhi$Transit <- "Bus"

delsocio3 <- del_metro[ , 61:68]
colnames(delsocio3) <- c("Gender", "AgeGroup", "Education", "Occupation", "Income", "Ownership", "License", "SurveyMode")
delsocio3$Transit <- "Metro"

#For jaipur

jaisocio <- jai_bus[ , 54:58]
colnames(jaisocio) <- c("Gender", "AgeGroup", "Education", "Income", "SurveyMode")
jaisocio$Transit <- "Bus"

jaisocio2 <- jai_metro[ , 53:57]
colnames(jaisocio2) <- c("Gender", "AgeGroup", "Education", "Income", "SurveyMode")
jaisocio2$Transit <- "Metro"

#Combinig both Jaipur and New Delhi socioeconomic dataset
Delhi <- rbind(Delhi, delsocio3) #combine Delhi socio data
Delhi$City <- "New Delhi"
Jaipur <- rbind(jaisocio, jaisocio2) #combine jaipur socio data
Jaipur$City <- "Jaipur"

Comb_Sociodata <- bind_rows(Delhi, Jaipur) #final socioeconomic dataset of pilot survey

#5. Plotting socioeconomic dataset ====

#5.1 With respect to Transit wise ====

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

#5.2 With respect to Survey Mode wise ====

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


#6. Mann-Whitney U Test to examine the Difference in Sample population
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


#7. Survey Sampling Information ====

#For jaipur

jai_bus_sam <- jai_bus[ , c(1, 58)]
jai_bus_sam$Transit <- "Bus"
jai_bus_sam$Format<- "Format 1"

jai_metro_sam <- jai_metro[ , c(1, 57)]
jai_metro_sam$Transit <- "Metro"
jai_metro_sam$Format<- "Format 1"


#For Delhi
del_sam_bus <- del_bus[ , c(1, 70)]
del_sam_bus_QRCode <- del_busQRCode[1 ,67]

del_sam_bus2 <- bind_rows(del_sam_bus, del_sam_bus_QRCode)
del_sam_bus2$Transit <- "Bus"
del_sam_bus2$Format <- "Format 2"

del_sam_metro <- del_metro[ , c(1, 68)]
del_sam_metro$Format <- NA
del_sam_metro$Format[c(1:3, 9, 10)] <- "Format 3"
del_sam_metro$Format[c(4:8, 11:61)] <- "Format 2"
del_sam_metro$Transit <- "Metro"


#Combinig both Jaipur and New Delhi socioeconomic dataset
Delhi <- bind_rows(del_sam_bus2, del_sam_metro) #combine Delhi socio data
Delhi$City <- "New Delhi"
Jaipur <- bind_rows(jai_bus_sam, jai_metro_sam) #combine jaipur socio data
Jaipur$City <- "Jaipur"

Comb_Samdata <- bind_rows(Delhi, Jaipur) #final socioeconomic dataset of pilot survey

Comb_Samdata %>%
    select(-c(`Sample No`, Mode)) %>% 
    table() %>% 
    data.frame() %>%
    filter(Freq > 0) %>% 
    ggplot(aes(x = Format, y = Freq, fill = SurMode)) +
    geom_col() +
    facet_grid(City ~ Transit) +
    geom_text(aes(label = Freq), vjust = -0.4) + # Adjust vjust as needed
    labs(x = "Questionnaire Formats", y = "No. of Sample Collected") +
    guides(fill = guide_legend(title = "Survey Mode"))

#7. Satisfaction and Importance Scale Presentation and Information Plots =====

del_satis_bus <- del_bus[, c(32, 36:62)]

del_satis_metro <- del_metro[c(4:8, 11:61) , c(30, 34:60)]
del_satis_metro$`Overall Satisfaction` <- as.numeric(del_satis_metro$`Overall Satisfaction`)
del_satis_metro$`System Infra: Parking- Space Adeqacy` <- as.numeric(del_satis_metro$`System Infra: Parking- Space Adeqacy`)

del_satis_metro$`System Infra: Parking- Dist to the parking bay` <- as.numeric(del_satis_metro$`System Infra: Parking- Dist to the parking bay`)

del_satis_metro$`System Infra: Parking- cost` <- as.numeric(del_satis_metro$`System Infra: Parking- cost`)


jai_import_bus <- jai_bus[ , 9:38]

jai_import_metro <- jai_metro[ , 8:37]


del_satis_bus <- del_satis_bus %>% 
    pivot_longer(1:28, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(Rating, Freq) %>% 
    group_by(Rating) %>%
    summarise(Count = sum(Freq)) %>% 
    filter(Rating != 15)

del_satis_metro <- del_satis_metro %>% 
    pivot_longer(1:28, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(Rating, Freq) %>% 
    group_by(Rating) %>%
    summarise(Count = sum(Freq))

del_format2 <- full_join(del_satis_bus, del_satis_metro) %>% 
    group_by(Rating) %>% 
    summarise(Total = sum(Count)) %>% 
    mutate(Percent = round(Total/sum(Total), 2),
           Format = "Format 2") %>% 
    ggplot(aes(x=Rating, y=Percent)) +
    geom_col() +
    ggtitle("Format 2 - Tested through Paper-based Survey") +
    labs(y= "Total Responses (Relative)")

jai_import_bus <- jai_import_bus %>% 
    pivot_longer(1:30, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(Rating, Freq) %>% 
    group_by(Rating) %>%
    summarise(Count = sum(Freq))

jai_import_metro <- jai_import_metro %>% 
    pivot_longer(1:30, names_to = "Attributes", values_to = "Rating") %>% 
    table() %>% 
    data.frame() %>% 
    select(Rating, Freq) %>% 
    group_by(Rating) %>%
    summarise(Count = sum(Freq))

jai_format1 <- full_join(jai_import_bus, jai_import_metro) %>% 
    group_by(Rating) %>% 
    summarise(Total = sum(Count)) %>% 
    mutate(Percent = round(Total/sum(Total), 2),
           Format = "Format 1") %>% 
    ggplot(aes(x=Rating, y=Percent)) +
    geom_col()+
    ggtitle("Format 1 - Tested through Paper-based Survey") +
    labs(y= "Total Responses (Relative)")

library(ggpubr)
ggarrange(jai_format1,
          del_format2,
          nrow = 1,
          ncol = 2)

