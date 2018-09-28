library(tidyverse)
library(Hmisc)
library(lubridate)
library(gdata)
library(car)

cutoff_date             <- '2017-09-01'
logfile_path            <- './logfile_2.txt'
write('MODELS\n',logfile_path)

full_df                 <- readRDS(file = "./post_clean.rds")
df_post                 <- full_df[which(full_df$salesdate >= as.Date(cutoff_date, format = "%Y-%m-%d")),]
'number rows in post cutoff dataframe'
nrow(df_post)
df_pre                  <- full_df[which(full_df$salesdate < as.Date(cutoff_date, format = "%Y-%m-%d")),]
'number rows in pre cutoff dataframe'
nrow(df_pre)

make_boxcox_plot        <- function(target_col_name,target,model,lambda,df_post,logfilepath) {
    #create boxcox plot to see what transform should be
    png(paste('./plots/',target_col_name,'_box_cox.png',sep=""), width = 1000, height = 600)
    myplot              <- boxCox(model,plotit=TRUE,family="yjPower")
    print(myplot)
    garbage             <- dev.off()

    #create new model based on boxCox plot
    new_resp            <- yjPower(target, lambda)
    newmodel = lm(new_resp ~ weekday+week+month+lastdayofmonth+day_tot+holiday+misc+(week*weekday*month),data=df_pre,weights=day_tot, y=TRUE, qr=TRUE)
    newm_rpt            <- paste('new resp model',target_col_name,summary(newmodel)$r.squared,summary(newmodel)$adj.r.squared)
    write(newm_rpt,file = logfilepath,append=TRUE)

    #how well do we predict with this model
    suppressWarnings(df_post$pred_val            <- predict(newmodel, df_post))
    pred_r_sq_header    <- paste('newmodel R-sq of predictions to actual for data after ',cutoff_date)
    write(pred_r_sq_header,file=logfilepath,append=TRUE)
    pred_r_sq           <- cor(df_post[[target_col_name]],df_post$pred_val   )^2
    write(pred_r_sq,file=logfilepath,append=TRUE)
    write('\n')
    return
}

create_model            <- function(df_pre,df_post,target_col_name,noisy,logfilepath,lambda) {
    target              <- df_pre[[target_col_name]]
    model = lm(target ~ weekday+week+month+lastdayofmonth+day_tot+holiday+misc+(week*weekday*month),data=df_pre,weights=day_tot, y=TRUE, qr=TRUE)

    #print out some logging about how things went
    r_sq_rpt            <- paste('Model for ',target_col_name,'\nR-squared',summary(model)$r.squared,'\nAdjusted R-squared',summary(model)$adj.r.squared)
    write(r_sq_rpt,file = logfilepath,append=TRUE)

    #save the model and then use it to predict some later dates
    saveRDS(model, file = paste("./models/lm",target_col_name,".rds", sep = ""))
    suppressWarnings(df_post$pred_val            <- predict(model, df_post))
    pred_r_sq_header    <- paste('R-sq of predictions to actual for data after ',cutoff_date)
    write(pred_r_sq_header,file=logfilepath,append=TRUE)
    pred_r_sq           <- cor(df_post[[target_col_name]],df_post$pred_val   )^2
    write(pred_r_sq,file=logfilepath,append=TRUE)
    print(paste(target_col_name,summary(model)$r.squared,summary(model)$adj.r.squared,pred_r_sq))

    sink("/dev/null")
    make_boxcox_plot(target_col_name,target,model,lambda,df_post,logfilepath)
    sink()

    #only do this part if we feel like being chatty
    if (noisy) {
        #this part is to show us just the variables and interactions at 0.05 level
        modelcoeff      <- round(summary(model)$coef, 4)
        modelcoeff[modelcoeff[, 4] < 0.05, ]
        print(modelcoeff)
    }
    return(model)
}


noisy                   <- FALSE
ts_model                <- create_model(df_pre,df_post,'total_sales',noisy,logfile_path,0)
bv_model                <- create_model(df_pre,df_post,'beverage_all',noisy,logfile_path,-2)
br_model                <- create_model(df_pre,df_post,'breads_togo',noisy,logfile_path,-2)
ca_model                <- create_model(df_pre,df_post,'cakes_togo',noisy,logfile_path,-2)
ck_model                <- create_model(df_pre,df_post,'cookie_all',noisy,logfile_path,-2)
dl_model                <- create_model(df_pre,df_post,'deli',noisy,logfile_path,-2)
dn_model                <- create_model(df_pre,df_post,'donut_all',noisy,logfile_path,2)
fr_model                <- create_model(df_pre,df_post,'freezer_togo',noisy,logfile_path,-2)
nf_model                <- create_model(df_pre,df_post,'nonfood',noisy,logfile_path,-2)
ps_model                <- create_model(df_pre,df_post,'pastry_all',noisy,logfile_path,-2)


