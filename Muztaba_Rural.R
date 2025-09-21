#Blocks Which are of use for my Analysis Purpose
# Block-3 HH_Size,  Religion ,Social_Group, Land_Owned, weight
# Block-4  Sex, Age, Marital_Status, General_Education,Sector, weight
# Block-5_1 Usual_Principal_Activity_Status,Social_Security_Benefits, weight


install.packages("haven")
install.packages("stargazer")
install.packages("dplyr")

library(haven)
library(stargazer)
library(dplyr)

Block_3<- read_dta("C:\\Users\\muzta\\OneDrive\\Desktop\\nsso68_schedule_10_emplyment_and_unemployment\\Block_3_Household characteristics.dta")
Block_4<- read_dta("C:\\Users\\muzta\\OneDrive\\Desktop\\nsso68_schedule_10_emplyment_and_unemployment\\Block_4_Demographic particulars of household members.dta")
Block_5_1<- read_dta("C:\\Users\\muzta\\OneDrive\\Desktop\\nsso68_schedule_10_emplyment_and_unemployment\\Block_5_1_Usual principal activity particulars of household members.dta")


#Since Block3  Doesn't have HHID let's create one
Block_3$HHID <-paste0(  Block_3$FSU_Serial_No,Block_3$Hamlet_Group_Sub_Block_No,Block_3$Second_Stage_Stratum_No,
                        Block_3$Sample_Hhld_No)

#Filtering Out Variables from each Block
Block_3_data <- Block_3%>%dplyr :: select( State, Religion ,Social_Group, Land_Owned, HHID)
Block_4_data <- Block_4%>%dplyr :: select(Sex, Age, Marital_Status, General_Education,Sector, HHID)
Block_5_1_data <- Block_5_1%>%dplyr :: select(Usual_Principal_Activity_Status, HHID)

#Merging Data and making to individual level using HHID
m1<-merge.data.frame(Block_3_data,Block_4_data,by="HHID",all.x = F) 
m2<-merge.data.frame(Block_5_1_data,m1 , by="HHID" ,all.x= F)

#Renamed and removed HHID variable
final_data<-m2%>%select(Usual_Principal_Activity_Status,General_Education,Sex,Age,Sector,Religion,Social_Group,Land_Owned,Marital_Status,State)

#Removing NA Entries
final_data<-na.omit(final_data)


##Creating Dependent Variable in two categories
# Assuming in Labor Force=1, Out of labor force=0
Labor_Force <- final_data %>%
  mutate(Usual_Principal_Activity_Status = case_when(
    Usual_Principal_Activity_Status %in% c(11, 12, 21, 31, 41, 51, 81) ~ as.numeric(1),
    Usual_Principal_Activity_Status %in% c(91, 92, 93, 94, 95, 96, 97, 99) ~ as.numeric(0),
    TRUE ~ as.numeric(Usual_Principal_Activity_Status)
  ))

na.omit(Labor_Force)



#Category in General Educations
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$General_Education %in% c("01","06","07","08","10","11","12","13"))


#Category in Religion
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$Religion %in% c("1","2","3","4","5","6","7","9"))

#Only Females ,coz only female participation
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$Sex %in% c("2"))

#Category in Social Group
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$Social_Group %in% c("1","2","3","9"))

#Category in Sector  
#ONLY RURAL
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$Sector %in% c("1"))

#Category in Marital Status
Labor_Force <- Labor_Force %>%
  filter(Labor_Force$Marital_Status %in% c("1","2","3","4"))



Labor_Force<-Labor_Force%>%select(Usual_Principal_Activity_Status,General_Education,Sex,Age,Religion,Social_Group,Land_Owned,Marital_Status)
Labor_Force$Usual_Principal_Activity_Status<-as.numeric(Labor_Force$Usual_Principal_Activity_Status)

#Seeing Structure
str(Labor_Force)
Labor_Force<-na.omit(Labor_Force)

#Convert the "General_Education" variable to an unordered factor
Labor_Force$General_Education <- factor(Labor_Force$General_Education, ordered = FALSE)

#use the relevel function to set "01" as the reference category
Labor_Force$General_Education <- relevel(Labor_Force$General_Education, ref = "01")


#NOT CREATING THIS BECAUSE Filted out Only RURAL AREA 
#Convert the "Sector" variable to an unordered factor
#Labor_Force$Sector <- factor(Labor_Force$Sector, ordered = FALSE)
#use the relevel function to set "1" as the reference category
#Labor_Force$Sector <- relevel(Labor_Force$Sector, ref = "1")

#Convert the "Religion" variable to an unordered factor
Labor_Force$Religion <- factor(Labor_Force$Religion, ordered = FALSE)
#use the relevel function to set "1" as the reference category
Labor_Force$Religion <- relevel(Labor_Force$Religion, ref = "1")

#Convert the "Social_Group" variable to an unordered factor
Labor_Force$Social_Group <- factor(Labor_Force$Social_Group, ordered = FALSE)
#use the relevel function to set "9" as the reference category
Labor_Force$Social_Group <- relevel(Labor_Force$Social_Group, ref = "9")

Labor_Force$Marital_Status <- factor(Labor_Force$Marital_Status, ordered = FALSE)
#use the relevel function to set "1" as the reference category
Labor_Force$Marital_Status <- relevel(Labor_Force$Marital_Status, ref = "1")


#Logistic regression to check effect of Education Age Sector religion social group, land owned, marital status
# On Female labor force participation On "NATIONAL LEVEL"
str(Labor_Force)


#Changing Column Names
colnames(Labor_Force)<-c("Female_Labor_Force_Participation","General_Education","Sex","Age","Religion","Social_Group","Land_Owned","Marital_Status" )

#C-LOG-LOG MODEL
cloglog_reg_Rural <- glm(Female_Labor_Force_Participation ~ General_Education + Age + Age^2 + 
                           Religion + Social_Group + Land_Owned + Marital_Status,
                         data = Labor_Force, family = binomial(link = "cloglog"))

#LOGIT MODEL
logistic_reg_Rural <- glm(Labor_Force$Female_Labor_Force_Participation ~ Labor_Force$General_Education + Labor_Force$Age  + Labor_Force$Religion + Labor_Force$Social_Group + Labor_Force$Land_Owned + Labor_Force$Marital_Status,
                          data = Labor_Force,  family = "binomial" (link="logit"))


#PROBIT MODEL
probit_reg_Rural <- glm(Labor_Force$Female_Labor_Force_Participation ~ Labor_Force$General_Education + Labor_Force$Age + Labor_Force$Religion + Labor_Force$Social_Group + Labor_Force$Land_Owned + Labor_Force$Marital_Status,
                        data = Labor_Force,  family = "binomial" (link="probit"))



stargazer(logistic_reg_Rural,type="text",out="models.txt")
stargazer(probit_reg_Rural,type="text",out="models.txt")
options(scipen=999)
cat("\nCloglog Regression - Rural Sector\n")
summary(cloglog_reg_Rural)
summary(logistic_reg_Rural)
summary(probit_reg_Rural)




##PLOTTING
xtabs(~Labor_Force$General_Education, data = Labor_Force)
count <- c(286022, 107392, 103402 , 67990  ,41516  , 3153 , 17421 ,  4244)
barplot(count, names=c("illiterate","Primary", "middle", "secondary","higher secondary","diploma", "graduate","post graduate"),
        main="General Education in Rural Area ", xlab = "Education level",
        las=3,ylab="Number of people", cex.axis =0.5,
        col=c("orange","pink","blue","grey","green","red","cyan","yellow"), cex.names=0.75, las =2 ,
        cex.main = 1,cex.lab = 0.90, border ="red", density = 145)


##GENDER
xtabs(~final_data$Sex, data = final_data)
count <- c(1192084 ,1154434)
barplot(count, names=c("Male","Female"),
        main="Gender",ylab="Number of people", 
        col=c("orange","blue"), cex.axis =0.7,      
        cex.main = 1, border ="red", density = 145)


##Religion
xtabs(~Labor_Force$Religion, data = Labor_Force)
count <- c(467752 , 90457,  43195,  15760,    620,   5820,      4,   7532)
barplot(count, names=c("Hinduism","Islam","Christanity","Sikhism","Jainism","Buddhism","Zoroastrianism","others"),
        main="Religion in Rural Area ", xlab = "Religion",ylab="Number of people", 
        col=c("orange","pink","blue","grey","green","red","cyan","yellow","purple"), cex.names=0.75, las = 2 ,
        cex.main = 1,cex.axis = 0.75, border ="red", density = 145)


#Social Group
xtabs(~Labor_Force$Social_Group, data = Labor_Force)
count <- c(172242 , 99060 , 99235 ,260603)
barplot(count, names=c("Others","ST","SC","OBC"),
        main="Social Group in Rural Area ", xlab = "Social Group",ylab="Number of people", 
        col=c("orange","blue","grey","green "), cex.names=0.75, las = 1 ,
        cex.main = 1, cex.axis = 0.70, border ="red", density = 145)


##Marital Status
xtabs(~Labor_Force$Marital_Status, data = Labor_Force)
count <- c(246040, 339895 , 43153 ,  2052)
barplot(count, names=c("Never Married","Currently Married","Widowed","Divorced"),
        main="Marital Status in Rural Area ", xlab = "Marital Status",ylab="Number of people", 
        col=c("orange","pink","blue","grey","green"), cex.names=0.75, las = 1 ,
        cex.main = 1,cex.axis = .70, border ="red", density = 145)