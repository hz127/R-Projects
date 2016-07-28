data_in <- read.csv("NEISS2014.csv")
head(data_in)
str(data_in)

# Question 1: 
# What are the top three body parts most frequently represented in this dataset?
# What are the top three body parts that are least frequently represented?
# To find top/least three body parts, I first grouped body parts and counted the number of incidences in 
# each group, then select top/least three.
library(dplyr)
top3_bodypart <- data_in %>%
  group_by(body_part) %>%
  summarise(count = n()) %>%
  top_n(3, count)
# Top three body part: 1. 75 head; 2. 76 Face; 3. Finger.
low3_bodypart <- data_in %>%
  group_by(body_part) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  slice(1:3)
# Least three body part: 1. 25-50% of body; 2.Pubic region; 3. Not Recorded.

# Question 2:
# How many injuries in this dataset involve a skateboard?
# Of those injuries, what percentage were male and what percentage were female?
# What was the average age of someone injured in an incident involving a skateboard?
# Extract "SKATEBOARD" in narrative column
n_match <- grepl('SKATEBOARD', data_in$narrative)
n_skateboard <- sum(n_match)
# There are 466 incidences involved in a skateboard.
# Count the number of injuries by sex
male_female <- as.list(tapply(n_match, data_in$sex, sum))
male_p <- male_female$Male / n_skateboard
female_p <- 1 - male_p
# Of those injuries, 82.4% are male, 17.6% are female.
# Sum up the total ages involved in a skateborad 
age_sum <- as.list(tapply(data_in$age, n_match, sum))
mean_age <- age_sum$'TRUE' / n_skateboard   
# The average age of injury in a skateboard is 18.

# Question 3:
# What diagnosis had the highest hospitalization rate? 
# What diagnosis most often concluded with the individual leaving without being seen?
# Briefly discuss your findings and any caveats you'd mention when discussing this data
# Count the number of incidences by grouping diage and disposition.
hospitalization_max <- data_in %>% 
  count(diag, disposition) %>%
  mutate(total = sum(n)) %>% 
  filter(disposition == 4) %>%
  mutate(prop = n/total) %>%
  ungroup() %>%
  top_n(1, prop)
# Diagnosis 69 (Submersion (including Drowning)) had the highest hospitalization rate with 42.6%.
leaving_max <- data_in %>% 
  count(diag, disposition) %>%
  mutate(total = sum(n)) %>% 
  filter(disposition == 6) %>%
  mutate(prop = n/total) %>%
  ungroup() %>%
  top_n(1, prop)
# Diagnosis 68 (Poisoning) most often concluded with the individual leaving without being seen, with rate 3.3%.

# Question 4:
# Visualize any existing relationship between age and reported injuries
library(ggplot2)
# Ages under 2 were coded 200+, I first converted these ages into a numeric value between 0 and 2. 
for (i in 1:nrow(data_in)) {
  if (data_in[i,]$age > 200) {
    data_in[i,]$age <- (data_in[i,]$age - 200) / 24.0
  }
}
# View the number of incidences for different diagnosis at each age.
age_1 <- data_in %>% count(age, diag) 
ggplot(age_1, aes(x = age, y = n, color = factor(diag))) + geom_point(shape = 1, size = 2.5) + theme(legend.position="top") 
# View the number of incidences for different age at each kind of diagonosis. 
p1 <- ggplot(age_1, aes(x = age, y = n)) + theme(legend.position="top", axis.text=element_text(size = 5))
p1 + geom_line() + facet_wrap(~diag, ncol=10)
# Group age into small groups, view the number of incidences for different diagnosis at each age group
data_in$AgeGrp <- as.factor(cut(data_in$age, breaks=c(0,5,25,50,65,Inf),
                                labels=c('0-5', '6-25', '26-50', '51-65', 'Over 65')))
age_2 <- data_in %>% count(AgeGrp, diag)
ggplot(age_2, aes(x = AgeGrp, y = n, color = factor(diag))) + geom_point(shape = 1, size = 5) + theme(legend.position="top") 
# View the number of incidences for different diagnosis at each age group.
p2 <- ggplot(age_2, aes(x = diag, y = n)) + theme(legend.position="top", axis.text=element_text(size = 5))
p2 + geom_line() + facet_wrap(~AgeGrp, ncol=3)

# Question 5:
# Investigate the data however you like and discuss any interesting insights you can find in the data
# Find the relationship between injury location and diagnosis.
location_1 <- data_in %>%
  count(location, diag) 
ggplot(location_1, aes(x = factor(location), y = n, color = factor(diag))) + geom_point(size = 5) + theme(legend.position="top") + geom_text(aes(label=diag), col="Black")
ggplot(location_1, aes(x = diag, y = n)) + geom_line() + facet_wrap(~location, ncol = 5)
# According to the plot, location 1 (Hone), 0 (Not recorded), 9 (Place of recreation or sports) are top three
# places for a injury to take place. The top three injuries at location 1 (home) are 59 (Laceration), 53(Contusions, Abrasions),
# 57 (Fracture), we can also read the diagnosis rankings from each location.

# Find the relationship between sex and injury location.
sex_1 <- data_in %>% count(sex, location)
ggplot(sex_1, aes(x = factor(location), y = n, color = factor(sex))) + geom_point(size = 5) + theme(legend.position="top")
# According to the plot, there are only male injuries at location 2(Farm/Ranch), 6(Manufactured home), 7(Industrial Place), it's 
# probably these places were work locations dominated by males. The places where female injuries were higher than male
# injuries were location 1(home), 5(other public property), it can be infered that women tend to stay at home (housewives) and often
# go shopping outside. 

# Find the relationship between disposition and diagnosis.
disp_1 <- data_in %>% count(disposition, diag)
p3 <- ggplot(disp_1, aes(x = factor(disposition), y = n, color = factor(diag))) + geom_point(size = 5) + theme(legend.position="top") + geom_text(aes(label=diag), col="Black") 
p3 + scale_y_continuous(limits = c(0, 2500))
p3 + scale_y_continuous(limits = c(0, 500))
p3 + scale_y_continuous(limits = c(0, 50))
# According to the plot, disposition 1 (Treated and released, or examined and released) was most common one with highest total counts.
# 59 (Laceration), 53(Contusions, Abrasions) were mostly treated as disposition 1. 
# Disposition 8 (Fatality, including DOA, died in the ED) was least occured. 




