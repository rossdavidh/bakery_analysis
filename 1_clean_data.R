library(tidyverse)
library(Hmisc)
library(lubridate)
library(gdata)

file_location                                    <- "scaled_data_food.csv"
#file_location                                    <- "data_food.csv"

val_num_col <- function(df,c_name) {
    df[[c_name]]                                 <- as.numeric(df[[c_name]])
    df[[c_name]][is.na(df[[c_name]])]            <- 0
    print(paste(c_name,' nbr nas: ',sum(is.na(df[[c_name]])),' sum: ',sum(df[[c_name]])))
    return(df[[c_name]])
}

check_cross_corr <- function(df) {
    'correlation between here and togo for donuts'
    cor(c_data['donut_here'],c_data['donuts_togo'])
    'correlation between here and togo for cookies'
    cor(c_data['cookie_here'],c_data['cookies_togo'])
    'correlation between here and togo for pastries'
    cor(c_data['pastry_here'],c_data['pastries_togo'])
    num_col <- subset(c_data,select = c('beerdrinks','nonfood','deli','beverage_all','donut_here','cookie_here','pastry_here','freezer_togo','cakes_togo','breads_togo','donuts_togo','cookies_togo','pastries_togo'))
    rcorr(as.matrix(num_col),type="pearson")
    sum_col <- subset(c_data,select = c('total_sales','total_food_sales','deli','beverage_all','donut_all','cookie_all','pastry_all','nonfood'))
    rcorr(as.matrix(sum_col),type="pearson")
}

process_date     <- function(df) {
    df$day                                       <- as.Date(df$salesdate, format = "%m/%d/%y")
    df$lastdayofmonth                            <- ceiling_date(df$day, "month") - 1
    df$dotm                                      <- as.numeric(format(as.Date(df$salesdate, format = "%m/%d/%y"), "%d"))
    df$weekday                                   <- weekdays(df$salesdate)
    #not enough Sunday or Monday data to really make an estimate, especially since what we have are special occasions
    df                                           <- subset(df, weekday %in% c("Tuesday", "Wednesday","Thursday","Friday","Saturday"))
    df$weekday                                   <- factor(df$weekday,
                                                     levels=c("Tuesday", "Wednesday","Thursday","Friday","Saturday"),
                                                     labels=c("2Tue", "3Wed","4Thu","5Fri","6Sat"))
    df$week             <- '1first'
    df$week[df$dotm>7]  <- '2second'
    df$week[df$dotm>14] <- '3third'
    df$week[df$dotm>21] <- '4fourth'
    df$week[df$dotm>28] <- '5fifth'
    df$week             <- as.factor(df$week)
    contrasts(df$week)  <- contr.treatment(5)
    df$month            <- format(as.Date(df$salesdate, format = "%m/%d/%y"), "%m")
    df$month            <- factor(df$month,
                           levels=c("01","02","03","04","05","06","07","08","09","10","11","12"),
                           labels=c("01Jan","02Feb","03Mar","04Apr","05May","06Jun","07Jul","08Aug","09Sep","10Oct","11Nov","12Dec"))
    contrasts(df$month) <- contr.treatment(12)
    #total days since store opening
    df$day_tot          <- as.numeric(difftime(as.Date(df$salesdate,format="%m/%d/%y"),as.Date("10/28/11",format="%m/%d/%y"),units=c('days')))
    return(df)
}

holidays                <- function(df) {
    df$holiday                                                                                     <- 'none'
    df$holiday[df$month=='01Jan' & df$dotm==1]                                                     <- 'NewYearsDay'
    df$holiday[df$month=='02Feb' & df$dotm==13]                                                    <- 'ValentinesEve'
    df$holiday[df$month=='02Feb' & df$dotm==14]                                                    <- 'ValentinesDay'
    df$holiday[grepl('Easter', df$notes)]                                                          <- 'Easter'
    #the check for 'EasterEve' must come after the check for 'Easter', because both contain the word 'Easter' and grepl will pick up both
    df$holiday[grepl('EasterEve', df$notes)]                                                       <- 'EasterEve'
    df$holiday[df$month=='07Jul' & df$dotm==3]                                                     <- 'July3rd'
    df$holiday[df$month=='07Jul' & df$dotm==4]                                                     <- 'July4th'
    df$holiday[df$month=='10Oct' & df$dotm==30]                                                    <- 'HalloweenEve'
    df$holiday[df$month=='10Oct' & df$dotm==31]                                                    <- 'Halloween'
    df$holiday[df$month=='11Nov' & df$weekday=='Wednesday' & df$week=='4fourth']                   <- 'ThanksgivingEve'
    df$holiday[df$month=='11Nov' & df$weekday=='Thursday' & df$week=='4fourth']                    <- 'Thanksgiving'
    df$holiday[df$month=='11Nov' & df$weekday=='Friday'   & df$week=='4fourth']                    <- 'BlackFriday'
    df$holiday[df$month=='12Dec' & df$dotm==24]                                                    <- 'ChristmasEve'
    df$holiday[df$month=='12Dec' & df$dotm==25]                                                    <- 'Christmas'
    df$holiday[df$month=='12Dec' & df$dotm==26]                                                    <- 'BoxingDay'
    df$holiday[df$month=='12Dec' & df$dotm==31]                                                    <- 'NewYearsEve'
    df$holiday                                                                                     <- as.factor(df$holiday)
    return(df)
}

process_notes           <- function(df) {
    df$notes[is.na(c_data$notes)]                <- ""
    df$misc                                      <- 'none'
    df$misc[grepl('closed_at_noon',df$notes)]    <- 'closed_at_noon'
    df$misc[grepl('eclipse_day',df$notes)]       <- 'eclipse_day'
    df$misc[grepl('eclipse_week',df$notes)]      <- 'eclipse_week'
    df$misc[grepl('two_days_averaged',df$notes)] <- 'two_days_averaged'
    df$misc                                      <- as.factor(df$misc)
    return(df)
}

create_combo_columns    <- function(df) {
    df$total_sales      <- (df$nonfood+df$deli+df$beverage_all+df$donut_here+df$cookie_here+df$pastry_here+df$freezer_togo+df$cakes_togo+df$donuts_togo+df$cookies_togo+df$breads_togo+df$pastries_togo+df$breads_togo)
    df$total_food_sales <- (df$deli+df$beverage_all+df$donut_here+df$cookie_here+df$pastry_here+df$freezer_togo+df$cakes_togo+df$donuts_togo+df$cookies_togo+df$breads_togo+df$pastries_togo+df$breads_togo)
    df$donut_all        <- (df$donut_here+df$donuts_togo)
    df$cookie_all       <- (df$cookie_here+df$cookies_togo)
    df$pastry_all       <- (df$pastry_here+df$pastries_togo)
    return(df)
}

factor_levels_check     <- function(df) {
    print('unique levels of factors')
    print(lapply(df[c("weekday", "week", "month", "holiday","misc")], unique))
    nbr_rows_before     <- nrow(c_data)
    df                  <- drop.levels(df)
    nbr_rows_after      <- nrow(c_data)
    if (nbr_rows_before != nbr_rows_after) {
        print(paste('before dropping levels number of rows was: ',nbr_rows_before))
        print(paste('after dropping levels it was: ',nbr_rows_after))
    } else {
        print(paste('number rows unchanged after dropping levels: ',nbr_rows_after))
    }
}

set_contrasts           <- function(df) {
    holidays_found                               <- levels(df$holiday)
    int_for_none                                 <- grep("none",holidays_found)
    contrasts(df$holiday)                        <- contr.treatment(holidays_found,base=int_for_none)
    misc_found                                   <- levels(df$misc)
    int_for_none_misc                            <- grep("none",misc_found)
    contrasts(df$misc)                           <- contr.treatment(misc_found,base=int_for_none_misc)
    return(df)
}

#MAIN SCRIPT BEGINS HERE

c_data                  <- read_csv(file_location, na = "")
c_data$salesdate        <- as.Date(c_data$salesdate, format = "%m/%d/%y")
c_data                  <- dplyr::arrange(c_data, salesdate)
#numeric columns
for (num_col_name in c('beerdrinks','nonfood','deli','beverage_all','donut_here','cookie_here','pastry_here','freezer_togo','cakes_togo','breads_togo','donuts_togo','cookies_togo','pastries_togo')) {
    c_data[[num_col_name]]                       <- val_num_col(c_data,num_col_name)
}
#'combination of existing columns'
c_data                  <- create_combo_columns(c_data)
#'categorical and date columns'
c_data                  <- process_notes(c_data)
c_data                  <- process_date(c_data)
c_data                  <- holidays(c_data)
#sanity check of what we did
factor_levels_check(c_data)
#set contrasts after factor_levels because we might drop levels there
c_data                  <- set_contrasts(c_data)

#save your work
write.csv(c_data,'./post_clean.csv', row.names=FALSE)
saveRDS(c_data, file = "./post_clean.rds")

