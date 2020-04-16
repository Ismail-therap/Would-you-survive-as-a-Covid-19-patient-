library(shiny)
library(randomForest)
library(mlbench)
library(caret)
require(ggplot2)
require(pscl)
require(boot)
library(readr)
library(e1071)
##################################

dat1 <- read_csv("COVID19_line_list_data.csv")



##########################
#### Data management #####
##########################

no_outcome_yet <- "0"
'%!in%' <- function(x,y)!('%in%'(x,y))
death_dat <- dat1[dat1$death %!in% no_outcome_yet,]
recover_dat <- dat1[dat1$recovered %!in% no_outcome_yet,]



dat <- rbind(recover_dat,death_dat)

dat$case_outcome <- ifelse(dat$death == 0, dat$recovered,
                           ifelse(dat$recovered == 0, dat$death,NA))

dat$dead_or_alive <- ifelse(dat$death == 0,"Alive",
                            ifelse(dat$recovered == 0,"Dead",NA))


dat$symptom_onset <- as.Date(dat$symptom_onset,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$hosp_visit_date <- as.Date(dat$hosp_visit_date,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$exposure_start <- as.Date(dat$exposure_start,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$exposure_end <- as.Date(dat$exposure_end,c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))


dat$case_outcome <- ifelse(dat$case_outcome == "1",NA,dat$case_outcome)
dat$case_outcome_date <- as.Date(dat$case_outcome,c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y")) 


dat$symptom_to_hospital <- as.numeric(dat$hosp_visit_date-dat$symptom_onset)
dat$hosp_visit_to_outcome <- as.numeric(dat$case_outcome_date-dat$hosp_visit_date)

dat$symptom_to_hospital <- ifelse(dat$symptom_to_hospital < 0,NA,dat$symptom_to_hospital)
dat$hosp_visit_to_outcome <- ifelse(dat$hosp_visit_to_outcome < 0,NA,dat$hosp_visit_to_outcome)

##### Health complicacy:

dat$chills <- as.factor(ifelse(sapply("chills", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$cough <- as.factor(ifelse(sapply("cough", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$fever <- as.factor(ifelse(sapply("fever", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$pneumonia <- as.factor(ifelse(sapply("pneumonia", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$cold <- as.factor(ifelse(sapply("cold", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$diarrhea <- as.factor(ifelse(sapply("diarrhea", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$runny_nose <- as.factor(ifelse(sapply("runny nose", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))

dat$diff_breath1 <- as.numeric(ifelse(sapply(c("difficulty breathing"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$diff_breath2 <- as.numeric(ifelse(sapply(c("shortness of breath"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$diff_breath3 <- as.numeric(ifelse(sapply(c("difficult in breathing"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$diff_breath4 <- as.numeric(ifelse(sapply(c("breathlessness"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$diff_breath <- dat$diff_breath1+dat$diff_breath2+ dat$diff_breath3+ dat$diff_breath4
dat$diff_breath <- as.factor(ifelse(dat$diff_breath > 0, "1","0"))

dat$fatigue1 <- as.numeric(ifelse(sapply(c("fatigue"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$fatigue2 <- as.numeric(ifelse(sapply(c("malaise"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$fatigue <- dat$fatigue1+dat$fatigue2
dat$fatigue <- as.factor(ifelse(dat$fatigue > 0, "1","0"))


dat$loss_appetit <- as.factor(ifelse(sapply("loss of appetite", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$sore_thrt <- as.factor(ifelse(sapply("sore throat", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))
dat$headache <- as.factor(ifelse(sapply("headache", function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,"1","0"))


dat$muscle_pain1 <- as.numeric(ifelse(sapply(c("muscle pain"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$muscle_pain2 <- as.numeric(ifelse(sapply(c("myalgia"), function(x) grepl(tolower(x), tolower(dat$symptom))) == TRUE,1,0))
dat$muscle_pain <- dat$muscle_pain1+dat$muscle_pain2
dat$muscle_pain <- as.factor(ifelse(dat$muscle_pain > 0, "1","0"))

#################

dat$dependent <- ifelse(dat$dead_or_alive == "Alive",1,0)
dat$dependent <- as.factor(dat$dependent)
dat$dead_or_alive <- as.factor(dat$dead_or_alive)

newdat <- dat[,c(c("dependent","gender","age","symptom_to_hospital","chills","cough","fever","pneumonia","loss_appetit","sore_thrt","diff_breath","fatigue"))]
newdat <- na.omit(newdat)

newdat$dependent <- as.factor(newdat$dependent)

##################################


control <- trainControl(method="repeatedcv", number=10, repeats=3)
#metric <- "Accuracy"
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)

fit <- train(dependent~gender+age+symptom_to_hospital+chills+cough+fever+
               pneumonia+loss_appetit+sore_thrt+diff_breath+fatigue, data = newdat, method="rf", tuneGrid=tunegrid, trControl=control)




Probability <- function(gender,age,symptom_to_hospital,fever,chills,cough,pneumonia,loss_appetit,sore_thrt,diff_breath,fatigue){
  as.numeric(c(age,symptom_to_hospital))
  as.factor(c(gender,fever,chills,cough,pneumonia,loss_appetit,sore_thrt,diff_breath,fatigue))
  
  data2 <- data.frame(gender,age,symptom_to_hospital,fever,chills,cough,pneumonia,loss_appetit,sore_thrt,diff_breath,fatigue)
  paste0("Survival chance is:"," ",predict(fit,newdata=data2, "prob")[2]*100,"%")}

shinyServer(
  function(input,output){
    output$inputValue<-renderPrint({c(input$gender,input$age,input$symptom_to_hospital,
                                      input$fever,input$chills,input$cough,input$pneumonia,input$loss_appetit,
                                      input$sore_thrt,input$diff_breath,input$fatigue)})
    output$prediction<-renderPrint({Probability(input$gender,input$age,input$symptom_to_hospital,
                                                input$fever,input$chills,input$cough,input$pneumonia,input$loss_appetit,
                                                input$sore_thrt,input$diff_breath,input$fatigue)})
  }
)




