library(tidyverse)
library(Hmisc)
library(lubridate)
library(gdata)

y_limits                <- c(0,.5)
#'define colors'
tsc                     <- "#e6194B"
bvc                     <- "#3cb44b"
brc                     <- "#9A6324"
cac                     <- "#4363d8"
ckc                     <- "#f58231"
dlc                     <- "#911eb4"
dnc                     <- "#42d4f4"
frc                     <- "#f032e6"
nfc                     <- "#000000"
psc                     <- "#000075"
#'load dataframe'
c_data = readRDS(file = "./post_clean.rds")

barplot_all_cats        <- function(c_data) {
    image_pathname      <- './plots/barplot_of_mix.png'
    png(image_pathname, width = 1000, height = 600)
    c_data$year         <- substr(c_data$salesdate,1,4)
    df_12on             <- c_data[which(c_data$salesdate >= as.Date("2012-01-01", format = "%Y-%m-%d")),]
    df_12to17           <- df_12on[which(c_data$salesdate < as.Date("2018-01-01", format = "%Y-%m-%d")),]
    tbl                 <- t(as.table(as.matrix(aggregate(cbind(beverage_all,breads_togo,cakes_togo,cookie_all,deli,donut_all,freezer_togo,nonfood,pastry_all) ~ year,data=df_12to17,sum))))
    myplot              <- barplot(tbl[2:10,],col=rainbow(9),names.arg=c(tbl[1,]),legend=c("bev","bread","cake","cookie","deli","donut","freezer","nonfood","pastry"))
    print(myplot)
    garbage <- dev.off()
}

barplot_all_cats(c_data)
#'load models'
ts_model                <- readRDS(file = "./models/lmtotal_sales.rds")
bv_model                <- readRDS(file = "./models/lmbeverage_all.rds")
br_model                <- readRDS(file = "./models/lmbreads_togo.rds")
ca_model                <- readRDS(file = "./models/lmcakes_togo.rds")
ck_model                <- readRDS(file = "./models/lmcookie_all.rds")
dl_model                <- readRDS(file = "./models/lmdeli.rds")
dn_model                <- readRDS(file = "./models/lmdonut_all.rds")
fr_model                <- readRDS(file = "./models/lmfreezer_togo.rds")
nf_model                <- readRDS(file = "./models/lmnonfood.rds")
ps_model                <- readRDS(file = "./models/lmpastry_all.rds")

plot_all_cats_vs_day_tot                         <- function(df) {
    image_pathname      <- './plots/all_cats_sales_v_day_tot.png'
    png(image_pathname, width = 1500, height = 600)
    myplot              <- ggplot(data = df) +
      labs(x = 'days since store opened',y = 'sales by category') +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = beverage_all,colour = "bev")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = breads_togo,colour = "bread")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = cakes_togo,colour = "cakes")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = cookie_all,colour = "cookie")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = deli,colour = "deli")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = donut_all,colour = "donut")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = freezer_togo,colour = "freezer")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = nonfood,colour = "nonfood")) +
      geom_smooth(method='loess',mapping = aes(x = day_tot,y = pastry_all,colour = "pastry")) +
      scale_colour_manual(name="legend", values=c(bvc,brc,cac,ckc,dlc,dnc,frc,nfc,psc))
    print(myplot)
    garbage <- dev.off()
}
plot_all_cats_vs_day_tot(c_data)

plot_all_cats_dotm_vs_month                      <- function(cutoff_date) {
    image_pathname      <- paste('./plots/all_cats_sales_dotm_v_month_since_',cutoff_date,'.png',sep="")
    png(image_pathname, width = 1500, height = 600)
    if (cutoff_date == 'ever') {
        df              <- c_data
    } else {
        df              <- c_data[which(c_data$salesdate >= as.Date(cutoff_date, format = "%Y-%m-%d")),]
    }
    myplot              <- ggplot(data = df) +
      labs(x = 'day of the month',y = 'sales by category') +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = beverage_all,colour = "bev")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = breads_togo,colour = "bread")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = cakes_togo,colour = "cakes")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = cookie_all,colour = "cookie")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = deli,colour = "deli")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = donut_all,colour = "donut")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = freezer_togo,colour = "freezer")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = nonfood,colour = "nonfood")) +
      geom_smooth(method='loess',mapping = aes(x = dotm,y = pastry_all,colour = "pastry")) +
      scale_colour_manual(name="legend", values=c(bvc,brc,cac,ckc,dlc,dnc,frc,nfc,psc)) +
      coord_cartesian(ylim=y_limits) +
      facet_wrap(~ month, nrow=2)
    print(myplot)
    garbage <- dev.off()
}

plot_all_cats_dotm_vs_month('ever')
plot_all_cats_dotm_vs_month('2016-02-01')

png('./plots/all_cats_sales_dotw_v_dotm_faceted.png', width = 1000, height = 600)
ggplot(data = c_data) +
  labs(x = 'day of the month',y = 'category sales') +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=beverage_all,colour="bev")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=breads_togo,colour="breads")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=cakes_togo,colour="cakes")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=cookie_all,colour="cookie")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=deli,colour="deli")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=donut_all,colour="donut")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=freezer_togo,colour="freezer")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=nonfood,colour="nonfood")) +
  geom_smooth(method='loess',mapping = aes(x=dotm,y=pastry_all,colour="pastry")) +
  scale_colour_manual(name="legend", values=c(bvc,brc,cac,ckc,dlc,dnc,frc,nfc,psc)) +
  facet_wrap(~ weekday)
garbage <- dev.off()


plot_sales_dotw_dotm    <- function(c_data,target_output,model,y_lim) {
    image_pathname      <- paste('./plots/',target_output,'_dotw_and_dotm.png',sep="")
    png(image_pathname, width = 1000, height = 600)
    myplot              <- ggplot(data = c_data) +
      labs(x = 'day of the month',y = target_output) +
      geom_smooth(method='loess',mapping = aes(x=dotm,y=suppressWarnings(predict(model,c_data)), color=weekday))
    print(myplot)
    garbage <- dev.off()
}

dotw_v_dotm             <- function(c_data,target_output) {
    image_path          <- paste('./plots/',target_output,'_dotw_v_dotm_faceted.png',sep="")
    png(image_path, width = 1000, height = 600)
    myplot              <- ggplot(c_data,aes_string('dotm',target_output)) +
      labs(x = 'day of the month',y = target_output) +
      geom_smooth(method='loess') +
      facet_wrap(~ weekday)
    print(myplot)
    garbage <- dev.off()
}

dotm_v_month_faceted    <- function(c_data,target_output,cc) {
    df_last12           <- c_data[which(c_data$salesdate >= as.Date('2017-02-16', format = "%Y-%m-%d")),]
    image_path          <- paste('./plots/',target_output,'_dotm_v_month.png',sep="")
    png(image_path, width=1000, height=600)
    myplot              <- ggplot(data = c_data) +
        labs(x = 'day of the month',y = target_output) +
        geom_smooth(data=c_data,method='loess',mapping = aes_string(x='dotm',y=target_output),color=cc) +
        geom_smooth(data=df_last12,method='loess',mapping = aes_string(x='dotm',y=target_output),color=cc,linetype="dotted") +
        facet_wrap(~ month, nrow=2)
    print(myplot)
    garbage             <- dev.off()
}

act_v_pred_by_day_tot   <- function(c_data,target_output,model) {
    image_path          <- paste('./plots/',target_output,'_act_v_pred_by_total_days.png',sep="")
    png(image_path, width = 1000, height = 600)
    myplot              <- ggplot(data = c_data) +
      labs(x = 'days since store opened',y = target_output) +
      geom_smooth(method='loess',mapping = aes(x=day_tot,y=predict(model,c_data),colour="predicted")) +
      geom_smooth(method='loess',mapping = aes(x=day_tot,y=c_data[[target_output]],colour="act")) +
      scale_colour_manual(name="legend", values=c("red","blue"))
    print(myplot)
    garbage             <- dev.off()
}

plot_model_analysis     <- function(c_data,target_output,model) {
    image_pathname      <- paste('./plots/',target_output,'_model_analysis.png',sep="")
    png(image_pathname, width = 1000, height = 600)
    par(mfrow = c(2, 2))
    print(suppressWarnings(plot(model,labels.id=c_data$salesdate)))
    garbage             <- dev.off()
    
    image_pathname_2    <- paste('./plots/',target_output,'_model_analysis_2.png',sep="")
    png(image_pathname_2, width = 1000, height = 600)
    par(mfrow=c(1,3));   #Make a new 1-by-3 plot
    print(plot(residuals(model),labels.id=c_data$salesdate))
    title("Simple Residual Plot")
    print(acf(residuals(model), main = ""))
    title("Residual Autocorrelation Plot");
    print(plot(fitted(model), residuals(model),labels.id=c_data$salesdate))
    title("Residual vs Fit. value");
    garbage             <- dev.off()
}

category_plots          <- function(c_data,target_output,model,clr) {
    plot_model_analysis(c_data,target_output,model)
    act_v_pred_by_day_tot(c_data,target_output,model)
    dotm_v_month_faceted(c_data,target_output,clr)
    dotw_v_dotm(c_data,target_output)
    plot_sales_dotw_dotm(c_data,target_output,model)
}

category_plots(c_data,'total_sales',ts_model,tsc)
category_plots(c_data,'beverage_all',bv_model,bvc)
category_plots(c_data,'breads_togo',br_model,brc)
category_plots(c_data,'cakes_togo',ca_model,cac)
category_plots(c_data,'cookie_all',ck_model,ckc)
category_plots(c_data,'deli',dl_model,dlc)
category_plots(c_data,'donut_all',dn_model,dnc)
category_plots(c_data,'freezer_togo',fr_model,frc)
category_plots(c_data,'nonfood',nf_model,nfc)
category_plots(c_data,'pastry_all',ps_model,psc)

