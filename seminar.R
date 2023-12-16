rm(list=ls())
# tell R not to use Scientific notation
options(scipen=999)
options(digits = 3) # controls how many digits are printed by default
options(na.action = "na.exclude")
#loading packages
#install.packages("pacman")
library(pacman)

pacman::p_load(
  data.table,
  tidyverse,
  openxlsx,
  DT,
  shiny,
  visdat,esquisse)
pacman::p_load(ggplot2)
pacman::p_load(lubridate)
pacman::p_load(scales)
pacman::p_load(stargazer)
table(seminar$F2)


# install.packages("contrast")
library(contrast)
#להגדיר משתנים כפקטורים, כל אחד מהם
#contrasts(data$Category) <- contr.sum(levels(data$Category))
#אחוזים, לעשות בטבלה לכל משתנה בלתי תלוי


seminar <- X2022_STATA
#gender
seminar <- as.data.table(seminar)
seminar$v104
unique(seminar$gender)
esquisse::esquisser()

class(seminar$F2)
seminar$F2 <- (as.numeric(seminar$F2))
seminar_sex <-seminar[,c("F2","gender")]
seminar_sex <- as.data.table(seminar_sex)
seminar_sex <- seminar_sex[seminar_sex$F2 <= 13, ]
seminar_sex$gender <- factor(seminar_sex$gender)
name_vector <- c("likud", "yesh", "mahene", "rzion", "shas", "torah", "betinu",
                 "labor", "merez", "yehudi", "hadash", "raam", "ballad")



seminar_sex %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = gender) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "Gender by Party") +
  theme_minimal() +
  xlim(1, 13) +
  scale_x_continuous(breaks = 1:13,labels = name_vector) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#religion
seminar$v144 %>% class()
seminar$v144 <- as.numeric(seminar$v144) 
seminar_religion <-seminar[,c("F2","v144")]
summary(seminar$v144)
seminar_religion <- seminar_religion[seminar_religion$v144 <= 5, ]
seminar_religion$v144 <- factor(as.numeric(seminar_religion$v144))
seminar_religion %>% class()
seminar_religion <- seminar_religion[seminar_religion$F2 <= 13, ]
seminar_religion$F2 %>% class()

seminar_religion %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = v144) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "religious by Party") +
  theme_minimal() +
  xlim(1, 13) +
  scale_x_continuous(breaks = 1:13) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#education
seminar$educ %>% class()
seminar_educ <-seminar[,c("F2","educ")]
seminar_educ$educ <- as.numeric(seminar_educ$educ)
seminar_educ$educ %>% class()
seminar_educ <- seminar_educ[seminar_educ$educ <= 9, ]
seminar_educ$educ <- factor(as.numeric(seminar_educ$educ)) 


seminar_educ <- seminar_educ[seminar_educ$F2 <= 13, ]



seminar_educ %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = educ) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "educ by Party") +
  theme_minimal() +
  xlim(1, 13) +
  scale_x_continuous(breaks = 1:13,labels = name_vector) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

#INCOME
seminar$v139
seminar_inc <-seminar[,c("F2","v139")]
seminar_inc %>% class()
seminar_inc <- as.data.table(seminar_inc)
seminar_inc <- seminar_inc[seminar_inc$F2 <= 13, ]
seminar_inc$v139 <- as.numeric(seminar_inc$v139)
seminar_inc$v139 %>% class()
seminar_inc <- seminar_inc[seminar_inc$v139 <= 5, ]
seminar_inc$v139  <- factor(as.numeric(seminar_inc$v139)) 


seminar_inc %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = v139) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "income by Party") +
  theme_minimal() +
  xlim(1, 13) +
  scale_x_continuous(breaks = 1:13,labels = name_vector) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#political view
unique(seminar$v111)
class(seminar$v111)
seminar_poli <-seminar[,c("F2","v111")]
seminar_poli <- as.data.table(seminar_poli)
unique(seminar_poli$v111)
seminar_poli$v111 <- as.numeric(seminar_poli$v111)
seminar_poli <- seminar_poli[seminar_poli$v111 <=7, ]
seminar_poli <- seminar_poli[seminar_poli$F2 <= 13, ]
seminar_poli$v111 <- factor(seminar_poli$v111)
class()

seminar_poli %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = v111) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "view by Party") +
  theme_minimal() +
  xlim(1, 13) +
  scale_x_continuous(breaks = 1:13) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))



seminar_poli %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = v111) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "party", y = "Percentage", title = "view by Party") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:13, labels = name_vector) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))



#goood one
name_vector <- c("likud", "yesh", "mahene", "rzion", "shas", "torah", "betinu",
                 "labor", "merez", "yehudi", "hadash", "raam", "ballad")





#########question 20

#party
unique(seminar$v20)
unique(seminar$v111)
class(seminar$v111)
seminar_eco <-seminar[,c("F2","v20")]
seminar_eco <- as.data.table(seminar_eco)
seminar_eco$v20 <- as.numeric(seminar_eco$v20)
seminar_eco <- seminar_eco[seminar_eco$v20 <=4, ]
seminar_eco <- seminar_eco[seminar_eco$F2 <= 13, ]
seminar_eco$v20 <- factor(seminar_eco$v20)
class(seminar_eco$v20)



seminar_eco %>%
  filter(!is.na(F2)) %>%
  ggplot() +
  aes(x = F2, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Party", y = "Percentage", title = "Eco by Party") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:13, labels = name_vector) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))



#sex
class(seminar$F2)
seminar$F2 <- (as.numeric(seminar$F2))
seminar_sex1 <-seminar[,c("v20","sex")]
seminar_sex1 <- as.data.table(seminar_sex1)
seminar_sex1$v20 <- as.numeric(seminar_sex1$v20)
seminar_sex1 <- seminar_sex1[seminar_sex1$v20 <=4, ]
seminar_sex1$v20 <- factor(seminar_sex1$v20)
seminar_sex1$sex <- as.numeric(seminar_sex1$sex)
seminar_sex1 <- seminar_sex1[seminar_sex1$sex <= 2, ]

class(seminar_sex1$v20)
class(seminar_sex1$gender)

seminar_sex1 %>%
  ggplot() +
  aes(x = sex, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Gender", y = "Percentage", title = "Gender by Eco") +
  theme_minimal() +
  xlim(1, 2) +
  scale_x_continuous(breaks = 1:13) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))



unique(seminar$v20)

#INCOME-change it
seminar$v139
seminar_inc1 <-seminar[,c("v20","v139")]
seminar_inc1 %>% class()
seminar_inc1 <- as.data.table(seminar_inc1)
seminar_inc1 <- seminar_inc1[seminar_inc1$v139 <= 5, ]
seminar_inc1$v139 <- as.numeric(seminar_inc1$v139)
seminar_inc1$v139 %>% class()
seminar_inc1 <- seminar_inc1[seminar_inc1$v20 <= 4, ]
seminar_inc1$v20  <- (as.numeric(seminar_inc1$v20)) 


seminar_inc1 %>%
  filter(!is.na(v20)) %>%
  ggplot() +
  aes(x = v139, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "income", y = "Percentage", title = "view by income") +
  theme_minimal() +
  xlim(1, 5) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#income

ggplot(seminar_inc1, aes(x =v139, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "income ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs income")+
  ylim(1,4)+
  xlim(1,5)+scale_x_continuous(breaks = 1:5)+theme_minimal()





#religious- linear line
unique(seminar$v144)
seminar_rel1 <-seminar[,c("v20","v144")]
seminar_rel1 %>% class()
seminar_rel1 <- as.data.table(seminar_rel1)
seminar_rel1 <- seminar_rel1[seminar_rel1$v144 <= 5, ]
seminar_rel1$v144 <- as.numeric(seminar_rel1$v144)
seminar_rel1 <- seminar_rel1[seminar_rel1$v20 <= 4, ]
seminar_rel1$v20  <- factor(as.numeric(seminar_rel1$v20)) 





seminar_rel1 %>%
  filter(!is.na(v20)) %>%
  ggplot() +
  aes(x = v144, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "religious", y = "Percentage", title = "religious by view") +
  theme_minimal() +
  xlim(1, 5) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#age-linear line
unique(seminar$age_group)
unique(seminar$age_group)
seminar_age <-seminar[,c("v20","age_group")]
seminar_age %>% class()
seminar_age <- as.data.table(seminar_age)
seminar_age <- seminar_age[seminar_age$age_group <= 8, ]
seminar_age$age_group <- as.numeric(seminar_age$age_group)
seminar_age <- seminar_age[seminar_age$v20 <= 4, ]
seminar_age$v20  <- factor(as.numeric(seminar_age$v20)) 
table(seminar_age$age_group)
seminar_age$age_group <- ifelse(seminar_age$age_group==2,1,seminar_age$age_group)
seminar_age$age_group <- ifelse(seminar_age$age_group==4,3,seminar_age$age_group)
seminar_age$age_group <- ifelse(seminar_age$age_group==6,5,seminar_age$age_group)
seminar_age$age_group <- ifelse(seminar_age$age_group==8,7,seminar_age$age_group)


seminar_age %>%
  filter(!is.na(v20)) %>%
  ggplot() +
  aes(x = age_group, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "age group", y = "Percentage", title = "age groups by view") +
  theme_minimal() +
  xlim(1, 8) +
  scale_x_continuous(breaks = c(1:8)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))
#summary(seminar$age)

#educ-linear

unique(seminar$educ)
seminar_educ1 <-seminar[,c("v20","educ")]
seminar_age %>% class()
seminar_educ1 <- as.data.table(seminar_educ1)
seminar_educ1 <- seminar_educ1[seminar_educ1$educ <= 9, ]
seminar_educ1$educ <- as.numeric(seminar_educ1$educ)
seminar_educ1 <- seminar_educ1[seminar_educ1$v20 <= 4, ]
seminar_educ1$v20  <- factor(as.numeric(seminar_educ1$v20)) 

table(seminar$educ)/1585
#bind 1-3
seminar_educ1 %>%
  filter(!is.na(v20)) %>%
  ggplot() +
  aes(x = educ, fill = v20) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "edcu", y = "Percentage", title = "educ by view") +
  theme_minimal() +
  xlim(1, 9) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

#reggresion
seminar1 <- seminar
seminar1$v139
seminar1$v20 <- as.numeric(seminar1$v20)  # Convert v20 to numeric

# Converting other variables to numeric (if needed)
seminar1$educ <- as.numeric(seminar1$educ)
seminar1$v144 <- as.numeric(seminar1$v144)
seminar1$v139 <- as.numeric(seminar$v139)
seminar1$sex <- as.numeric(seminar1$sex)
seminar1$age_group <- as.numeric(seminar1$age_group)
dt <- seminar1
unique(dt$sex)
dt <- dt[dt$v20 <= 4, ]
dt <- dt[dt$educ <= 9, ]
dt <- dt[dt$v139 <= 5, ]
dt <- dt[dt$F2 <= 13, ]
dt <- dt[dt$sex <= 2, ]
unique(dt$v131)
#check this variable  
dt$v131 %>% class()
table(dt$v131)
362+193+212



# Create a scatter plot with the regression line
ggplot(dt, aes(x = F2, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "educ ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs educ")+
  ylim(1,4)+
  xlim(1,13)+theme_minimal()


sd(dt$age_group,na.rm=TRUE)
summary(dt$age_group)
table(dt$age_group)/(1300-346)


# install.packages("contrast")
library(contrast)
#להגדיר משתנים כפקטורים, כל אחד מהם
#contrasts(data$Category) <- contr.sum(levels(data$Category))
#אחוזים, לעשות בטבלה לכל משתנה בלתי תלוי



# Create a scatter plot with the regression line
ggplot(dt, aes(x =sex, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_jitter(size = 0.5)+
  labs(x = "educ ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs educ")+
  ylim(1,4)+
  xlim(1,2)+theme_minimal()
stargazer(model1, type = "text")





#descriptive statistics 
#educ
dt$educ <- ifelse(dt$educ==2,1,dt$educ)
dt$educ <- ifelse(dt$educ==3,1,dt$educ)
dt$educ <- ifelse(dt$educ==4,1,dt$educ)
unique(dt$educ)
dt$educ <- ifelse(dt$educ==5,2,dt$educ)
dt$educ <- ifelse(dt$educ== 6,3,dt$educ)
dt$educ <- ifelse(dt$educ== 7,4,dt$educ)
dt$educ <- ifelse(dt$educ== 8,5,dt$educ)
dt$educ <- ifelse(dt$educ== 9,6,dt$educ)
prop.table(
  table(dt$educ)
) 

#sex
prop.table(
  table(dt$sex)
) 


#income
prop.table(
  table(dt$v139)
) 

#religious 
prop.table(
  table(dt$v144)
) 

#age_group

prop.table(
  table(dt$age_group)
) 

#party
prop.table(
  table(dt$F2)
) 
#view

prop.table(
  table(dt$v20)
) 

##########
probtable <- as.data.table(prop.table(
  table(dt$v20)
) )

probtable <- as.data.table(prop.table(
  table(dt$F2)
) )
#PLOTTING

ggplot(probtable, aes(x = V1, y = N)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.2f%%", N)), vjust = -0.3)+ 
  labs(title = "Distribution Of Economic View ",
       x = "Economic View (v20)",
       y = "Percentage (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +ylim(0,0.6)+
  theme_minimal()
#for f2###

name_vector <- c("likud", "yesh", "mahene", "rzion", "shas", "torah", "betinu",
                 "labor", "merez", "yehudi", "hadash", "raam", "ballad")

# Your ggplot code with x-axis labels added
ggplot(probtable, aes(reorder(x = V1,N), y = N)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.2f%%", N)), vjust = -0.3) + 
  labs(title = "Distribution Of Parties ",
       x = "Party(F2)",
       y = "Percentage (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0.1)) +
  scale_x_discrete(breaks = probtable$V1, labels = name_vector) +
  ylim(0, 0.25) +
  theme_minimal()


####fixing f2-my preferencce
#saving on 99
dt5 <- dt
dt5$F2 %>% unique()
#likud and zionut datit
dt5$F2 <- ifelse(dt5$F2==1,99,dt5$F2)
dt5$F2 <- ifelse(dt5$F2==4,1,dt5$F2)
dt5$F2 <- ifelse(dt5$F2==99,4,dt5$F2)
####habiet hayudi, yesh atid,havoda
dt5$F2 <- ifelse(dt5$F2==2,99,dt5$F2)
dt5$F2 <- ifelse(dt5$F2==10,2,dt5$F2)
dt5$F2 <- ifelse(dt5$F2==8,98,dt5$F2)#havoda
dt5$F2 <- ifelse(dt5$F2==99,8,dt5$F2)
###havoda, merez
dt5$F2 <- ifelse(dt5$F2==9,10,dt5$F2)#merez
dt5$F2 <- ifelse(dt5$F2==98,9,dt5$F2)#havoda
#gantz,beitenu
dt5$F2 <- ifelse(dt5$F2==3,99,dt5$F2)#gantz
dt5$F2 <- ifelse(dt5$F2==7,3,dt5$F2)#betinu
dt5$F2 <- ifelse(dt5$F2==99,7,dt5$F2)#gantz
#raam and hadash
dt5$F2 <- ifelse(dt5$F2==12,99,dt5$F2)#raam
dt5$F2 <- ifelse(dt5$F2==11,12,dt5$F2)#hadash
dt5$F2 <- ifelse(dt5$F2==99,11,dt5$F2)#raam
####the other way around####
dt52 <- dt5
unique(dt52$F2)
###1 and 13
dt52$F2 <- ifelse(dt52$F2==13,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==1,13,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,1,dt52$F2)
###2 and 12
dt52$F2 <- ifelse(dt52$F2==12,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==2,12,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,2,dt52$F2)
###3 and 11
dt52$F2 <- ifelse(dt52$F2==11,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==3,11,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,3,dt52$F2)
###4 and 10
dt52$F2 <- ifelse(dt52$F2==10,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==4,10,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,4,dt52$F2)
###5 and 9
dt52$F2 <- ifelse(dt52$F2==9,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==5,9,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,5,dt52$F2)
###6 and 8
dt52$F2 <- ifelse(dt52$F2==8,99,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==6,8,dt52$F2)
dt52$F2 <- ifelse(dt52$F2==99,6,dt52$F2)


######################coaliation
dt6 <- dt
dt6 %>% class()
dt6$F2 <- ifelse(dt6$F2==4,1,dt6$F2)#zionutdatit
dt6$F2 <- ifelse(dt6$F2==5,1,dt6$F2)#shas
dt6$F2 <- ifelse(dt6$F2==6,1,dt6$F2)#torah
dt6$F2 <- ifelse(dt6$F2==10,1,dt6$F2)#beityudi

######################opotion
dt6$F2 <- ifelse(dt6$F2==3,2,dt6$F2)#ganyz
dt6$F2 <- ifelse(dt6$F2==7,2,dt6$F2)#israelbetuinu
dt6$F2 <- ifelse(dt6$F2==8,2,dt6$F2)#labor
dt6$F2 <- ifelse(dt6$F2==9,2,dt6$F2)#merez
dt6$F2 <- ifelse(dt6$F2==11,2,dt6$F2)#arabs
dt6$F2 <- ifelse(dt6$F2==12,2,dt6$F2)#arabs
dt6$F2 <- ifelse(dt6$F2==13,2,dt6$F2)#arabs
unique(dt6$F2)
#####diferrent kvutzat yihus
dt62 <- dt6
#left to hold
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#ganyz
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#israelbetuinu
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#labor
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#merez
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#arabs
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#arabs
dt62$F2 <- ifelse(dt62$F2==2,99,dt62$F2)#arabs
#right to 2
dt62$F2 <- ifelse(dt62$F2==1,2,dt62$F2)#zionutdatit
dt62$F2 <- ifelse(dt62$F2==1,2,dt62$F2)#shas
dt62$F2 <- ifelse(dt62$F2==1,2,dt62$F2)#torah
dt62$F2 <- ifelse(dt62$F2==1,2,dt62$F2)#beityudi
#left to 1
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#ganyz
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#israelbetuinu
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#labor
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#merez
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#arabs
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#arabs
dt62$F2 <- ifelse(dt62$F2==99,1,dt62$F2)#arabs
unique(dt62$F2)



##############סדר עולה מימין לשמאל
class(dt)
dt5 <- as.data.table(dt5)
dt_c5 <- dt5
dt_c5 <- dt5[, .(educ, v20,F2,age_group,v144,v139,sex)]
dt_c5 %>% class()
dt_c5$F2 %>%class() 
dt_c5 <- na.omit(dt_c5)
# Use setnames to rename the columns
setnames(dt_c, old = names(dt_c), new = new_column_names)

cor_matrix5 <- cor(dt_c5)
cor_matrix %>% class()
###############cor-test
#age group
cor.test(dt_c5$age_group,dt_c5$educ)
cor.test(dt_c5$age_group,dt_c5$v20)
cor.test(dt_c5$age_group,dt_c5$F2)
cor.test(dt_c5$age_group,dt_c5$v139)
cor.test(dt_c5$age_group,dt_c5$v144)
cor.test(dt_c5$age_group,dt_c5$sex)
#educ
cor.test(dt_c5$educ,dt_c5$v20)
cor.test(dt_c5$educ,dt_c5$F2)
cor.test(dt_c5$educ,dt_c5$v139)
cor.test(dt_c5$educ,dt_c5$v144)
cor.test(dt_c5$educ,dt_c5$sex)
#F2
cor.test(dt_c5$F2,dt_c5$v20)
cor.test(dt_c5$F2,dt_c5$v139)
cor.test(dt_c5$F2,dt_c5$v144)
cor.test(dt_c5$F2,dt_c5$sex)
#v20
cor.test(dt_c5$v20,dt_c5$v139)
cor.test(dt_c5$v20,dt_c5$v144)
cor.test(dt_c5$v20,dt_c5$sex)

#v139
cor.test(dt_c5$v139,dt_c5$v144)
cor.test(dt_c5$v139,dt_c5$sex)

#v144
cor.test(dt_c5$v144,dt_c5$sex)

table(dt5$v144)
table(dt5$v513)

###קאולציה אופוזיציזיה

class(dt)
dt_c62 <- dt62
dt_c62 <- dt62[, .(educ, v20,F2,age_group,v144,v139,sex)]
dt_c62 <- na.omit(dt_c62)
# Use setnames to rename the columns
setnames(dt_c, old = names(dt_c), new = new_column_names)

cor_matrix62 <- cor(dt_c62)
cor_matrix %>% class()

#####קואלציה אופוזיציה הפוך

class(dt)
dt6 <- as.data.table(dt6)
dt_c6 <- dt6
dt_c6 <- dt6[, .(educ, v20,F2,age_group,v144,v139,sex)]
dt_c6 <- na.omit(dt_c6)
# Use setnames to rename the columns
setnames(dt_c, old = names(dt_c), new = new_column_names)

cor_matrix6 <- cor(dt_c6)
cor_matrix %>% class()

##############סדר עולה משמאל לימין
dt_c52 <- dt52
dt_c52 <- dt52[, .(educ, v20,F2,age_group,v144,v139,sex)]
dt_c52 <- na.omit(dt_c52)
new_column_names <- c("educ", "view", "party", "age_group", "religious", "income", "sex")

# Use setnames to rename the columns
setnames(dt_c52, old = names(dt_c52), new = new_column_names)

cor_matrix52 <- cor(dt_c52)






############
#correlation
class(dt)
dt <- as.data.table(dt)
dt_c <- dt
dt_c <- dt[, .(educ, v20,F2,age_group,v144,v139,sex)]
dt_c %>% class()
dt_c$F2 %>%class() 
dt_c <-dt_c[, (names(dt_c)) := lapply(.SD, as.numeric), .SDcols = names(dt_c)]
dt_c <- na.omit(dt_c)
new_column_names <- c("educ", "view", "party", "age_group", "religious", "income", "sex")

# Use setnames to rename the columns
setnames(dt_c, old = names(dt_c), new = new_column_names)

cor_matrix <- cor(dt_c)
cor_matrix %>% class()

#if theres a problem
# Example imputation with mean
#dt_c[is.na(dt_c)] <- mean(dt_c, na.rm = TRUE)








#plots








# Create a scatter plot with the regression line
#need to think
ggplot(dt, aes(x = F2, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Party ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs Party")+
  ylim(1,4)+
  xlim(1,13)+scale_x_continuous(breaks = 1:13)+theme_minimal()




# Create a scatter plot with the regression line
#sex

ggplot(dt, aes(x =sex, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_jitter(size = 0.5)+
  labs(x = "Sex ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs Sex")+
  ylim(1,4)+
  xlim(1,2)+theme_minimal()





#educ

ggplot(dt, aes(x =educ, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Educ ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs educ")+
  ylim(1,4)+
  xlim(1,6)+scale_x_continuous(breaks = 1:6)+theme_minimal()



#religious

ggplot(dt, aes(x =v144, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Religious ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs religious")+
  ylim(1,4)+
  xlim(1,5)+scale_x_continuous(breaks = 1:5)+theme_minimal()


#age

ggplot(dt, aes(x =age_group, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age Group ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs religious")+
  ylim(1,4)+
  xlim(1,8)+scale_x_continuous(breaks = 1:8)+theme_minimal()


#income

ggplot(dt, aes(x =v139, y = v20)) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "income ", y = "Economic Preferences (v20)",
       title = "Linear Regression: Economic Preferences vs income")+
  ylim(1,4)+
  xlim(1,5)+scale_x_continuous(breaks = 1:5)+theme_minimal()




#regression


model1 <- lm(v20 ~sex  + F2 + educ + v144 + v139 + age_group+0, data = dt)
summary(model1)




# install.packages("contrast")
library(contrast)

dt1 <- dt
#setting leveles

levels(dt1$v20)


dt1$educ <- factor(dt1$educ, levels = unique(dt1$educ))
# Set up Helmert contrasts for educ variable
contrasts(dt1$educ) <- contr.sum(levels(dt1$educ))

dt1$sex <- factor(dt1$sex, levels = unique(dt1$sex))
# Set up Helmert contrasts for educ variable
contrasts(dt1$sex) <- contr.sum(levels(dt1$sex))

dt1$F2 <- factor(dt1$F2, levels = unique(dt1$F2))
# Set up Helmert contrasts for educ variable
contrasts(dt1$F2) <- contr.sum(levels(dt1$F2))


dt1$age_group <- factor(dt1$age_group, levels = unique(dt1$age_group))
# Set up Helmert contrasts for educ variable
contrasts(dt1$age_group) <- contr.sum(levels(dt1$age_group))



dt1$v144 <- factor(dt1$v144, levels = unique(dt1$v144))
# Set up Helmert contrasts for educ variable
contrasts(dt1$v144) <- contr.sum(levels(dt1$v144))



dt1$v139 <- factor(dt1$v139, levels = unique(dt1$v139))
# Set up Helmert contrasts for educ variable
contrasts(dt1$v139) <- contr.sum(levels(dt1$v139))




dt1$v20 <- factor(dt1$v20, levels = unique(dt1$v20))
# Set up Helmert contrasts for educ variable
contrasts(dt1$v20) <- contr.sum(levels(dt1$v20))
contrasts(AllData$Description)[1,]=-1

dt1$educ <- as.numeric(dt1$educ)
dt1$sex <- as.numeric(dt1$sex)
dt1$F2 <- as.numeric(dt1$F2)
dt1$v144 <- as.numeric(dt1$v144)
dt1$v139 <- as.numeric(dt1$v139)
dt1$age_group <- as.numeric(dt1$age_group)
dt1$v20 <- as.numeric(dt1$v20)

model2 <- lm(v20 ~sex  + F2 + educ + v144 + v139 + age_group+0, data = dt1)
summary(model2)



model3 <- lm(v20~ factor(sex)  + factor(F2) + factor(educ) + factor(v144) + factor(v139) + factor(age_group)+0, data = dt)
summary(model3)





stargazer(model3,type = "text")



unique(dt$v12)
####check out this variable  
dt$v12 <- as.numeric(dt$v12)
dt <- dt[dt$v12 <=4, ]

model33 <- lm(v12~factor(F2)+0,data=dt)
summary(model33)
model44 <- lm(v20~factor(F2)+0,data=dt)



model5 <- lm(v20~ factor(sex)  + factor(F2) + factor(educ) + factor(v144) + factor(v139) + factor(age_group), data = dt5)
coef(summary(model5))
coef(model5)
summary(dt5$v20)

table(dt5$F2)
model6 <- lm(v20~ factor(sex)  + factor(F2) + factor(educ) + factor(v144) + factor(v139) + factor(age_group), data = dt6)
summary(model6)
levels(factor(dt6$F2))

dt8 <- dt6

dt7$educ <- factor(dt7$educ, levels = unique(dt7$educ))
# Set up Helmert contrasts for educ variable
contrasts(dt7$educ) <- contr.sum(levels(dt7$educ))

dt7$sex <- factor(dt7$sex, levels = unique(dt7$sex))
# Set up Helmert contrasts for educ variable
contrasts(dt7$sex) <- contr.sum(levels(dt7$sex))

dt7$F2 <- factor(dt7$F2, levels = unique(dt7$F2))
# Set up Helmert contrasts for educ variable
contrasts(dt7$F2) <- contr.sum(levels(dt7$F2))


dt7$age_group <- factor(dt7$age_group, levels = unique(dt7$age_group))
# Set up Helmert contrasts for educ variable
contrasts(dt7$age_group) <- contr.sum(levels(dt7$age_group))
levels(dt7$age_group)


dt7$v144 <- factor(dt7$v144, levels = unique(dt7$v144))
# Set up Helmert contrasts for educ variable
contrasts(dt7$v144) <- contr.sum(levels(dt7$v144))



dt7$v139 <- factor(dt7$v139, levels = unique(dt7$v139))
# Set up Helmert contrasts for educ variable
contrasts(dt7$v139) <- contr.sum(levels(dt7$v139))

model7 <- lm(v20 ~sex  + F2 + educ + v144 + v139 + age_group, data = dt7)
summary(model7)


unique(dt7$age_group)




dt8$educ <- factor(dt8$educ)
dt8$sex <- factor(dt8$sex)
dt8$F2 <- factor(dt8$F2)
dt8$age_group <- factor(dt8$age_group)
dt8$v144 <- factor(dt8$v144)
dt8$v139 <- factor(dt8$v139)

# Create dummy variables for each categorical variable
dt7_dummies <- model.matrix(~educ + sex + F2 + age_group + v144 + v139 - 1, data = dt7)

# Fit the linear regression model using all levels
model7 <- lm(v20 ~ educ + sex + F2 + age_group + v144 + v139+0, data = cbind(dt7, dt7_dummies))
summary(model7)

model7 <- lm(v20 ~ sex + F2 + educ + v144 + v139 + age_group+0, data = dt7)
summary(model7)

levels(dt7$F2)
coefficients(model8)


model8 <- lm(v20 ~ sex + F2 + educ + v144 + v139 + age_group+0, data = dt8)
summary(model8)




model62 <- lm(formula = v20 ~ factor(sex)+factor(F2) +factor(educ) + factor(v144) + factor(v139) + factor(age_group), data = dt62)
summary(model62)
(dt62$F2)




model52 <- lm(formula = v20 ~ factor(sex)+factor(F2) +factor(educ) + factor(v144) + factor(v139) + factor(age_group), data = dt52)
summary(model52)

model99 <- lm(formula = v20 ~factor(F2) , data =dt5)
summary(model99)

table(dt5$F2)



#v413 , place of birth
prop.table(
  table(dt$v513)
) 
0.3941+0.2297+0.1644+0.0428
0.0687+0.0664+0.0338
  
#v143 , religion
100*prop.table(
  table(dt$v143_code)
) 


table(dt$v143_code)

90.46+4.82+0.21 
100-4.82
  