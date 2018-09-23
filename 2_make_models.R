library(tidyverse)
library(Hmisc)
library(lubridate)
library(gdata)

'load dataframe'
full_df = readRDS(file = "./post_clean.rds")

#split into pre- and post- cutoff date datasets
cutoff_date             <- '2017-09-01'
logfile_path            <- './logfile_2.txt'
write('MODELS\n',logfile_path)
df_post                 <- full_df[which(full_df$salesdate >= as.Date(cutoff_date, format = "%Y-%m-%d")),]
'number rows in post cutoff dataframe'
nrow(df_post)
df_pre                  <- full_df[which(full_df$salesdate < as.Date(cutoff_date, format = "%Y-%m-%d")),]
'number rows in pre cutoff dataframe'
nrow(df_pre)

create_model            <- function(df_pre,df_post,target_col_name,noisy,logfilepath) {
    model = lm(df_pre[[target_col_name]] ~ weekday+week+month+lastdayofmonth+day_tot+holiday+misc+(week*weekday*month),data=df_pre,weights=day_tot)
    r_sq_rpt  <- paste('Model for ',target_col_name,'\nR-squared',summary(model)$r.squared,'\nAdjusted R-squared',summary(model)$adj.r.squared)
    write(r_sq_rpt,file = logfilepath,append=TRUE)
    saveRDS(model, file = paste("./models/lm",target_col_name,".rds", sep = ""))
    suppressWarnings(df_post$pred_val            <- predict(model, df_post))
    pred_r_sq_header    <- paste('R-sq of predictions to actual for data after ',cutoff_date)
    write(pred_r_sq_header,file=logfilepath,append=TRUE)
    pred_r_sq           <- cor(df_post[[target_col_name]],df_post$pred_val   )^2
    write(pred_r_sq,file=logfilepath,append=TRUE)
    print(paste(target_col_name,summary(model)$r.squared,summary(model)$adj.r.squared,pred_r_sq))
    if (noisy) {
        #this part is to show us just the variables and interactions at 0.05 level
        modelcoeff      <- round(summary(model)$coef, 4)
        modelcoeff[modelcoeff[, 4] < 0.05, ]
        print(modelcoeff)
    }
    return(model)
}


noisy                   <- FALSE
ts_model                <- create_model(df_pre,df_post,'total_sales',noisy,logfile_path)
bv_model                <- create_model(df_pre,df_post,'beverage_all',noisy,logfile_path)
br_model                <- create_model(df_pre,df_post,'breads_togo',noisy,logfile_path)
ca_model                <- create_model(df_pre,df_post,'cakes_togo',noisy,logfile_path)
ck_model                <- create_model(df_pre,df_post,'cookie_all',noisy,logfile_path)
dl_model                <- create_model(df_pre,df_post,'deli',noisy,logfile_path)
dn_model                <- create_model(df_pre,df_post,'donut_all',noisy,logfile_path)
fr_model                <- create_model(df_pre,df_post,'freezer_togo',noisy,logfile_path)
nf_model                <- create_model(df_pre,df_post,'nonfood',noisy,logfile_path)
ps_model                <- create_model(df_pre,df_post,'pastry_all',noisy,logfile_path)


