library("dplyr")
library("ggplot2"); theme_set(theme_bw())
library("dbplyr")
library("RSQLite")
#connect to SQLite
dataframe = "lending-club-loan-data/database.sqlite"
loan.sqlite = dbConnect(SQLite(),dbname=dataframe)
loan_tbl <- (loan.sqlite
    %>% tbl("loan")
)
## get total number of rows
n_tot <- (loan_tbl
    %>% summarise(count=n())
    %>% pull(count)
)
all_id <- (loan_tbl
    %>% pull(id)
)
set.seed(101)
keep_id <- sample(all_id,size=floor(n_tot/2))

loan_train <- (loan_tbl
    ## already decided we don't want these?
    %>% select(-c(index,
                  url,
                  desc,
                  verification_status_joint,
                  pymnt_plan,
                  application_type,member_id,
                  inq_last_12m,
                  total_cu_tl,
                  inq_fi,
                  all_util,
                  max_bal_bc,
                  open_rv_24m,
                  open_rv_12m,
                  il_util,
                  total_bal_il,
                  mths_since_rcnt_il,
                  open_il_24m,
                  open_il_12m,
                  open_il_6m,
                  open_acc_6m,
                  dti_joint,
                  annual_inc_joint,
                  policy_code
                  ))
    %>% inner_join(tibble(id=keep_id),copy=TRUE,by="id")
)

loan_train_cc <- collect(loan_train)

names(loan_train_cc)[sapply(loan_train_cc,is.character)]
## look at first few rows of char vars:
loan_train_cc %>% select_if(is.character) %>% slice(1:25) %>% View()
bad_vars <- c("id",
              "int_rate","revol_util",  ## misclassified numbers
              "emp_title","title")

loan_train_cc %>% select_if(is.character) %>%
    purrr::map_int(~length(unique(.))) %>% sort()

## summarise
fac_list <- (loan_train_cc
    %>% select_if(~length(unique(.))<100)
    %>% purrr::map(~as.data.frame(sort(table(.))))
    %>% bind_rows(.id="var")
    %>% rename(value=".",count="Freq")
)


fac_list2 <- (fac_list
    %>% mutate(var=reorder(var,count,min))
    %>% group_by(var)
    %>% mutate(value=reorder(value,count))
)

g1 <- ggplot(fac_list2, aes(var,count,fill=value)) + 
    geom_bar(stat="identity") +
    coord_flip()+
    theme(legend.position="none")

library(plotly)
ggplotly(g1)
