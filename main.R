# survey analysis of students who took a self evaluation questionaire on 4 major categories:
# "Communication", "Confidence", "Competence", "Collaboration"
# 
pacman::p_load(shiny,shinydashboard,readxl, shinydashboardPlus,googlesheets4,tidyverse,janitor,data.table)
# read the file that contains question class serial number borders
qclass <- fread("question_class.txt",header = T)
glink <- readLines("links.txt")[1]
# read the sentences that have to be affixed in recommentation - one time read.
sndt <- read_excel("SoME Communication Profile Descriptions (1) (1).xlsx",sheet = "Final") %>% as.data.table()

##### modify the survey result file name here only
survey_result_file = "Survey Responses-SoMECommunicationProfileSMB1.xlsx"
sheet_name = str_sub(survey_result_file,-9) %>% str_remove(".xlsx")
strt <- "2023-02-07"

# read and melt survey results - both sheets 1 & 2 separately and then join
x1 <- read_excel(survey_result_file,sheet = 1,skip = 0) %>% 
        as.data.table() %>% clean_names()
x2 <- read_excel(survey_result_file,sheet = 2,skip = 1)%>% 
        as.data.table() %>% 
        row_to_names(1) %>% 
        clean_names() %>% 
        select(1,matches("x\\d+_")) %>% tail(-1) %>% 
        select(-starts_with(c("x31","x32"))) %>% # ignoring the two columns after 30, If there are more just add x33, x34 etc.
        rename(name = question) %>% 
        left_join(x1,by = "name")
scoredt <- melt.data.table(x2,id.vars = c("name","email","submitted_on"))
scoredt[, seq := seq_len(.N), by = name]
scoredt[,qtype := case_when(
        seq <= qclass[[1]][1] ~ qclass[[2]][1],
        seq <= qclass[[1]][2] ~ qclass[[2]][2],
        seq <= qclass[[1]][3] ~ qclass[[2]][3],
        seq <= qclass[[1]][4] ~ qclass[[2]][4],
        seq <= qclass[[1]][5] ~ qclass[[2]][5],
        seq <= qclass[[1]][6] ~ qclass[[2]][6],
        T ~ "no class"
        ),name]
#scoredt[,qcompr:=variable  %>% str_remove("^x\\d+_") %>% str_replace_all("_"," ") %>% str_remove("[:punct:]")]

# read database of sentennces
sndt2 <- sndt %>% clean_names()  %>%  select(1:5) %>% filter(!is.na(advanced_scores_5))
setDT(sndt2)
sndt2[, seq := seq_len(.N)]
sndt2[,qtype := case_when(
        seq <= qclass[[1]][1] ~ qclass[[2]][1],
        seq <= qclass[[1]][2] ~ qclass[[2]][2],
        seq <= qclass[[1]][3] ~ qclass[[2]][3],
        seq <= qclass[[1]][4] ~ qclass[[2]][4],
        seq <= qclass[[1]][5] ~ qclass[[2]][5],
        seq <= qclass[[1]][6] ~ qclass[[2]][6],
        T ~ "no class")
]

master <- sndt2[scoredt,on = "seq",nomatch = NA]
#if(master[is.na(x1),.N] > 0) message("Warning: Atleast one question string did not match")
master[,reco:=case_when(
        value==5 ~ advanced_scores_5,
        value == 4 ~ intermediate_scores_4,
        value == 3 ~ intermediate_scores_3,
        value %in% c(1,2) ~ low_scores_1_2,
        T ~ "NOT FOUND"
)]
master[,reco:=str_squish(reco) %>% str_remove("\\.$")]

final <- master[,.(name,email,submitted_on,qtype,reco)] %>% 
        unique() %>% 
        group_by(qtype,name) %>% 
        mutate(para=paste(reco,collapse = ".\n*  "), para = paste0("*  ",para,".")) %>% 
        ungroup() %>% 
        select(name,email,submitted_on,qtype,para) %>% unique() %>% 
        as.data.table()
        
final2 <- 
        final %>% 
        dcast.data.table(name + email + submitted_on ~ qtype,value.var = "para") %>% 
        rename(Learner = name) %>% 
        mutate(date = lubridate::dmy(submitted_on)) %>% 
        select(-submitted_on) %>% 
        filter(date >= strt)

write_sheet(final2,ss = glink,sheet = sheet_name)

message(paste("Loaded a total of ",nrow(final)," results across ",final[,uniqueN(name)]," students"))
print(final[,unique(name)])
