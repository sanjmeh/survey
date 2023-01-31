# survey analysis of SOME students
pacman::p_load(shiny,shinydashboard,readxl, shinydashboardPlus,googlesheets4,tidyverse,janitor,data.table)
# read the file that contains question class serial number borders
qclass <- fread("question_class.txt")
glink <- readLines("links.txt")[1]

# read and melt survey results
x1 <- read_excel("Survey Responses-SoMECommunicationProfileVersion10.xlsx",sheet = 1,skip = 0) %>% 
        as.data.table() %>% clean_names()
x2 <- read_excel("Survey Responses-SoMECommunicationProfileVersion10.xlsx",sheet = 2,skip = 1)%>% 
        as.data.table() %>% 
        row_to_names(1) %>% 
        clean_names() %>% 
        select(1,matches("x\\d+_")) %>% tail(-1) %>% 
        rename(name = question) %>% 
        left_join(x1,by = "name")
scoredt <- melt.data.table(x2,id.vars = c("name","email","submitted_on"))
scoredt[, seq := seq_len(.N), by = name]
scoredt[,ques_type := case_when(
        seq <= qclass[[1]][1] ~ qclass[[2]][1],
        seq <= qclass[[1]][2] ~ qclass[[2]][2],
        seq <= qclass[[1]][3] ~ qclass[[2]][3],
        seq <= qclass[[1]][4] ~ qclass[[2]][4],
        T ~ "no class"
        ),name]
scoredt[,qcompr:=variable  %>% str_remove("^x\\d+_") %>% str_replace_all("_"," ")]

# read database of sentennces
sntdt <- read_sheet(glink,col_names = F) %>% as.data.table() 
sndt2 <- sntdt[-1] %>% clean_names()  %>% filter(!is.na(x2)) %>% select(x1:x4)
sndt2[, seq := seq_len(.N)]
sndt2[,ques_type := case_when(
        seq <= qclass[[1]][1] ~ qclass[[2]][1],
        seq <= qclass[[1]][2] ~ qclass[[2]][2],
        seq <= qclass[[1]][3] ~ qclass[[2]][3],
        seq <= qclass[[1]][4] ~ qclass[[2]][4],
        T ~ "no class")
]

master <- sndt2[scoredt,on = "qcompr",nomatch = NA]
if(master[is.na(x1),.N] > 0) message("Warning: Atleast one question string did not match")