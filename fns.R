# functions that work on some data files
# read and melt survey results - both sheets 1 & 2 separately and then join
pacman::p_load(gt,ggplot2,janitor,googlesheets4,readxl,tidyverse,purrr,lubridate,magrittr,data.table)
proc.survey <- function(filename,profiles = sndt,strt = "2023-03-15",newlms = F){
        
        if(newlms==T){
                x1 <- fread(filename) %>% clean_names()
                x1[,name:=str_extract(user,".{2,25}(?=\\()")]
                x1[,email:=str_extract(user,"\\(.{3,40}\\)") %>% str_remove_all("[()]")]
                x2 <- x1[,submitted_on:=parse_date_time(date_time,orders = "HMdmY",tz = "Asia/Kolkata")] %>% select(-user,-date_time) %>% select(c(1:30),name,email,submitted_on)
        } else {
        x1 <- read_excel(filename,sheet = 1,skip = 0,.name_repair = "unique_quiet") %>% 
                as.data.table() %>% clean_names()
        x2 <- read_excel(filename,sheet = 2,skip = 1,.name_repair = "unique_quiet")%>% 
                as.data.table() %>% 
                row_to_names(1) %>% 
                clean_names() %>% 
                select(1,matches("x\\d+_")) %>% tail(-1) %>% 
                select(-starts_with(c("x31","x32"))) %>% # ignoring the two columns after 30, If there are more just add x33, x34 etc.
                rename(name = question) %>% 
                left_join(x1,by = "name")
        }
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
        sndt2 <- profiles %>% clean_names()  %>%  select(1:5) %>% filter(!is.na(advanced_scores_5))
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
                #mutate(date = lubridate::dmy(submitted_on)) %>%  # the date format needs unification.
                mutate(date = as_date(submitted_on)) %>%  # the date format needs unification.
                select(-submitted_on) %>% 
                filter(date >= strt)
        return(final2)
        
}

# run last n files with pattern in patfiles and filter all responses that start at date_strt
run_sent_join <- function(nfiles = 4, patfiles = "Survey.*xlsx", date_strt = "2023-03-15"){
        list.files(pattern = patfiles,full.names = T) %>% 
                file.info() %>% 
                data.table::as.data.table(keep.rownames = T) %>% 
                filter(mtime >= Sys.Date() - days(2)) %>% 
                extract2("rn") %>% 
                head(nfiles) %>% 
                map_dfr(proc.survey,strt = date_strt) %>% 
                fwrite(file = "output.csv",dateTimeAs = "write.csv")
}

scores <- function(filename){
        x1 <- read_excel(filename,sheet = 1,skip = 0,.name_repair = "unique_quiet") %>% 
                as.data.table() %>% clean_names()
        x2 <- read_excel(filename,sheet = 2,skip = 1,.name_repair = "unique_quiet") %>% 
                row_to_names(1) %>% 
                as.data.table() %>% 
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
        scoredt[,value:=as.integer(value)][]
}

smry1 <- function(filename,title = NA){
        x1 <- suppressMessages(smry_scores(filename))
        x1[,.(tot_score = sum(value),
              qcount = .N,
              people = uniqueN(name),
              ppl_fives = .SD[value==5,uniqueN(name)],
              q_fives = .SD[value==5,uniqueN(variable)],
              ppl_ones = .SD[value==1,uniqueN(name)],
                q_ones = .SD[value==1,uniqueN(variable)]
              ),qtype][,mean :=round(tot_score/qcount,2)][order(-mean)] %>% 
                gt() %>% 
                tab_header(coalesce(title,filename))  %>% 
                cols_label(
                        qtype = "Ques.Category",
                        qcount = "Total Questions",
                        ppl_fives = md("People who scored <br> atleast one 5"),
                        ppl_ones = md("People who scored <br>atleast one 1"),
                        q_ones = md("Questions that scored<br> atleast one 1"),
                        q_fives = md("Questions that scored<br> atleast one 5"),
                )
                
}

# bar chart showing sum of scores
bar_mean_qtype <- function(filename,title = NA){
        x1 <- scores(filename)
        x1 %>% ggplot() + 
                geom_bar(aes(qtype,value),stat = "summary",fun = "mean") +
                ggtitle(coalesce(title,filename)) +
                xlab("Question Category") +
                ylab("Total score")
}

barpercent <- function(filename,title = NA){
        x1 <- suppressMessages(scores(filename))
        x1 %>% mutate(value = factor(value)) %>%
                ggplot() + 
                geom_bar(aes(qtype,value,fill = value),position = "stack",stat = "identity") +
                ggtitle(coalesce(title,filename)) +
                xlab("Question Category") +
                ylab("Total score")
}

crossrank <- function(filename,title = NA){
        x1 <- scores(filename)
        x1[,avgscore:=round(mean(value),2),name]
        x1[,.(value = mean(value)), .(name, qtype,avgscore)
           ][order(qtype,-value)
             ][,rank:=seq_len(.N),qtype] %>% 
                dcast.data.table(name + avgscore ~ qtype,value.var = "rank") %>% 
                arrange(desc(avgscore)) %>% 
                gt() %>% 
                tab_header(coalesce(title,filename)) %>% 
                tab_spanner("Rank in batch",columns = c(3:8)) %>% 
                cols_label(
                        "avgscore" = "Average Score"
                )
}

# this is the tall scores output from all files. It will extract batch string
# from file name
smry2 <- function(){
        batch <- 
                list.files(path = ".",pattern = "Survey",ignore.case = T)  %>% 
                str_extract(".{15}(?=.xlsx)") %>% 
                str_remove("\\(.*\\)") %>% 
                str_extract("(?<=(le)).{1,10}")
        
        x1 <- 
                list.files(path = ".",pattern = "Survey",ignore.case = T)
        names(x1) <- batch
        x2 <- 
                x1 %>%         
                map(scores) %>% 
                rbindlist(idcol = "batch")
        
        x2[,.(score_mean = mean(value)), .(name, qtype,batch)][order(name)] %>% fwrite("scores.tall.csv")
}
######################### NEW LMS DATA ############

x1 <- fread("~/Downloads/2023-05-12_export.csv")

