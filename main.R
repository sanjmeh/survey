# survey analysis of students who took a self evaluation questionnaire on 4 major categories:
# "Communication", "Confidence", "Competence", "Collaboration"
# 
source("fns.R")
# read the file that contains question class serial number borders
qclass <- fread("question_class.txt",header = T)
glink <- readLines("links.txt")[1]
# read the sentences that have to be affixed in recommendation - one time read.
sndt <- read_excel("SoME Communication Profile Descriptions (1) (1).xlsx",sheet = "Final") %>% as.data.table()

##### modify the number of files to be picked up (sorted in descending order of modified time)
##### uncomment the next line and source the file again

# run_sent_join(nfiles = 4, "Survey.*xlsx", date_strt = "2023-03-25")
# 
# smry2()

proc.survey("~/Downloads/2023-05-12_export.csv",newlms = T) %>% fwrite("testlms6.csv")




