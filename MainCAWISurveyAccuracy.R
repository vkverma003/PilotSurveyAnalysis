rm(list = ls())

cat("\014")
graphics.off()

#1. Sourcing all responses data from Combined Code ====
combined_all <- "D:/4. IITR/6. Data Analysis/Satisfaction Data Analysis/1. Cleaning/WaitTimeCleaning.R"
source(combined_all, local = TRUE, echo = FALSE)

#2. Response Completeness ====

total_data %>%
    select(Remarks) %>%
    filter(Remarks == "Correct" | Remarks == "Incomplete") %>% 
    group_by(Remarks) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(Per = round(Freq/sum(Freq), 2))
    
prun <- total_data %>% 
    filter(Remarks == "Correct" | Remarks == "Incomplete")

travel <- prun[, c(5:10, 12:13, 15:16, 18:20, 22:25, 27:29, 31:32, 34:36)]
satisfaction <- prun[ , c(37:47, 52:63, 67)]
socio <- prun[ , 68:74]

rowSums(is.na(travel)) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(Per = round((Freq/2276)*100, 2))

rowSums(is.na(satisfaction)) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(Per = round((Freq/2276)*100, 2))

rowSums(is.na(socio)) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(Per = round((Freq/2276)*100, 2))

n <- sapply(prun[ , -c(1:5, 75:78)], function(x) sum(is.na(x))) %>% 
    data.frame() %>% 
    rownames_to_column()

n[1:31, 3] <- "Travel"
n[32:62, 3] <- "Satisfaction"
n[63:69, 3] <- "Personal"

colnames(n) <- c("Attribute", "Blank", "Questionnaire Part")

n <- n %>% 
    mutate( Percent = round(Blank/nrow(total_data), 2))

n <- n[-c(6, 9, 12, 28), ]

n %>% 
    filter(`Questionnaire Part` == "Travel") %>% 
    select(Blank) %>%
    group_by(Blank) %>% 
    table()



n$Attribute <- factor(n$Attribute, levels = n$Attribute)


ggplot(n, aes(x=Attribute, y = Percent, fill=factor(`Questionnaire Part`, levels = c("Travel", "Satisfaction", "Personal")))) +
    geom_col() +
    guides(fill = guide_legend("Questionnaire Part"))+
    scale_x_discrete(guide = guide_axis(angle = 90))


#test
