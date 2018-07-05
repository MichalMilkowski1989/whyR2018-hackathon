library(mlr)
library(dplyr)
listLearners()$type %>% table
listLearners('surv') %>% View

hard <- read.csv('harddrive.csv', nrows = 2)
hard <- read.csv('harddrive.csv')
hard$date <- hard$date %>% as.character() %>% as.Date()

hard$serial_number %>% unique %>% length()

hard %>% group_by(serial_number) %>% summarize(count = n()) %>% select(count)%>% table

number_of_days <-
hard %>% group_by(serial_number) %>% 
  summarize(count = n())



fail <- hard %>% filter(failure == 1) %>% select(serial_number, failure)

number_of_days %>% full_join(fail) %>% select(-serial_number) %>% table(useNA = 'ifany')


hard %>% filter(serial_number  == 'MJ0351YNG9Z0XA') %>% head(1000) %>% View

harddrive<- hard

harddrive %>% filter(failure == 1) %>% tail(50) %>% select(date, serial_number, failure)

harddrive %>% filter(serial_number  == 'W0Q6KWV8') %>% head(1000) %>% View
harddrive %>% filter(serial_number  == 'Z300XNXP') %>% head(1000) %>% View


  
harddrive %>% data.frame %>% unique %>% dim  

harddrive2 <- 
harddrive[!duplicated(harddrive[,c(1,2)]),] %>% group_by(serial_number) %>% 
  mutate(first_date = min(date), time = date - first_date)
harddrive2$time <- harddrive2$time %>% as.numeric

str(harddrive2)

harddrive2$model %>% table %>% data.frame %>% arrange(Freq)

harddrive2 <- harddrive2 %>% as.data.frame

harddrive3 <- 
harddrive2 %>% filter(model == 'ST4000DM000') %>% arrange(serial_number)

no_NAs <- colnames(harddrive3[c(1,1000),])[colSums(is.na(harddrive3[c(1,1000),])) == 0]
cols <- no_NAs[sapply(harddrive3[,no_NAs], class) == 'integer']

task = makeSurvTask(id = "hard", 
                    data = harddrive3[,c('time',cols)], 
                    target = c('time','failure'))

lrn = makeLearner("surv.coxph", predict.type = 'response')
mod = train(lrn, task)
pred = predict(mod, task = task)
performance(pred, measures = list(mlr::auc))
colnames(harddrive3)[apply(is.na(harddrive3), 2, any)]
