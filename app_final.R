library(ggplot2)
library(dplyr)
library(reshape2)
library(dplyr)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shiny.semantic)
library(DT)
library(formattable)
library(shinyBS)
library(formattable)
library(jsonlite)
library(rjson)
library(rpivotTable)
library(stringr)
library(treemap)
library(RColorBrewer)
library(rlang)
library(tidyr)
library(reshape2)
library(reshape)
library(newsanchor)
library(httr)
library(tidyverse)
library(deSolve)
library(shinydashboardPlus)
library(wppExplorer)
library(wpp2019)


# setwd("C:/Corona_App")

#functions

logit <- function(x){ log(x/(1-x)) }
logit.inv <- function(x) { 1/(1 + exp(-x)) }

list <- c('HKG',
          'MAC',
          'FRO',
          'GRL',
          'GUF',
          'PYF',
          'GLP',
          'MTQ',
          'MYT',
          'NCL',
          'REU',
          'BLM',
          'SPM',
          'MAF',
          'ABW',
          'BES',
          'CUW',
          'SXM',
          'AIA',
          'BMU',
          'VGB',
          'CYM',
          'FLK',
          'GIB',
          'IMN',
          'MSR',
          'TCA')

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

create.delta <- function(x){
    x <- x %>%
        group_by(Country.Region) %>%
        mutate(Delta_Value = Value - lag(Value))
}

roll.up.region.dates <- function(x){
    x <- x%>%
        group_by(Country.Region, Date) %>%
        summarise(Value = sum(Value, na.rm = T),
                  iso3 = max(iso3))
}

roll.up <- function(x){
    data.frame(Country.Region = "World",
               x %>% group_by(Date) %>% 
                   summarise(Value = sum(Value, na.rm = T), Delta_Value = sum(Delta_Value, na.rm = T)),iso3 = "NULL")
}

modify.data <- function(x){
    x$Date <- as.Date(x$Date)
    x <- x[,c(1,2,5,6,7)]
    x$Value <- as.numeric(x$Value)
    colnames(x)[length(colnames(x))] <-"iso3"
    x[x$iso3 %in% list,"Country.Region"] <- x[x$iso3 %in% list,"Province.State"]
    x <- create.delta(roll.up.region.dates(x))
    x <- x[with(x, order(Country.Region, Date)),]
    x <- rbind(roll.up(x),data.frame(x))
    x[which(!is.na(x$Date)),]
}

wide_to_long <- function(x) {
    x <- x[-1,]
    x <- reshape2::melt(x, id.vars = c("Province.State","Country.Region","Lat","Long","ISO.3166.1.Alpha.3.Codes","Region.Code","Sub.region.Code","Intermediate.Region.Code"))
    x$variable <- gsub("X","",x$variable)
    x$variable <- as.Date(x$variable, format = "%m.%d.%y")
    colnames(x) <- c("Province.State","Country.Region","Lat","Long","ISO.3166.1.Alpha.3.Codes","Region.Code","Sub.region.Code","Intermediate.Region.Code",
                     "Date","Value")
    x <- x[c("Province.State","Country.Region","Lat","Long","Date", "Value", "ISO.3166.1.Alpha.3.Codes","Region.Code","Sub.region.Code","Intermediate.Region.Code")]
    x <- x[which(!is.na(x$Value)),]
    return(x)
}

linechart <- function(x, text, type){
    highcharter::hchart(x, type = 'line', 
                        hcaes(y = Value, group = Country.Region, x = Date))  %>%
        hc_yAxis(title = list(text = text), type = type) %>% hc_xAxis(type = "datetime") %>% 
        hc_legend(align = "right", layout = "proximate") %>%
        hc_chart(zoomType = 'xy') %>%
        hc_rangeSelector(enabled = TRUE,
                         buttons = list(
                             list(type = 'all', text = 'All'),
                             list(type = 'week', count = 1, text = '1w'),
                             list(type = 'week', count = 2, text = '2w'),
                             list(type = 'month', count = 1, text = '1m'),
                             list(type = 'month', count = 2, text = '2m'),
                             list(type = 'month', count = 3, text = '3m'),
                             list(type = 'month', count = 6, text = '6m'),
                             list(type = 'ytd', text = 'YTD'),
                             list(type = 'year', count = 1, text = '1y')
                         ),
                         verticalAlign = "top",
                         selected = 0
        ) %>% hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(shared = TRUE)
    
}

mapchart <- function(x, text, type, minColor, maxColor, allowNegativeLog = TRUE){
    hcmap(data = x ,
          joinBy = "iso-a3", value = "Value", name = text, dataLabels = list(enabled = TRUE, format = '{point.name}'),
          download_map_data = F) %>%
        hc_colorAxis(type = type, minColor = minColor, maxColor =  maxColor, allowNegativeLog = allowNegativeLog) %>%
        hc_mapNavigation(enabled = TRUE)  %>% hc_exporting(enabled = TRUE)
}

regression <- function(x, country_list){
    do.call("rbind",sapply(country_list, function(c){
        y <- x[x$Country.Region == c,"Value"]
        y[y < 1] <- 1
        reg_data <- data.frame(x = c(1:length(y)),y)
        reg <- lm(log10(y) ~ x, data= reg_data)
        d <- data.frame(Country = c, 
                        growth_rate_log10 = reg$coefficients[2], 
                        exponential_growth_rate = 10^reg$coefficients[2])
        colnames(d)[2:3] <- c("log10 Growth Factor", "Exponential Growth Factor")
        d
    }, simplify = F))
}

box_function <- function(title, type, status, plot_name, table_name, width) {
    box(title = title, solidheader = F, width = width, collapsible = T, status = status,
        column(4, radioGroupButtons(
            inputId = paste("choice",type,sep="_"),
            label = NULL,
            choices = c("Cumulative", "New"),
            selected = "Cumulative"
        )),
        column(4,offset = 3, radioGroupButtons(
            inputId = paste("axis_type",type,sep="_"),
            label = NULL,
            choices = c("Linear", "Log10"),
            selected = "Linear"
        )),
        highchartOutput(plot_name),
        h3("Average Daily Growth Rate"),
        DT::dataTableOutput(table_name)
    )
}

box_function2 <- function(title, type, status, plot_name, width) {
    box(title = title, solidheader = F, width = width, collapsible = T, status = status,
        radioGroupButtons(
            inputId = paste("choice2",type,sep="_"),
            label = NULL,
            choices = c("Cumulative", "New"),
            selected = "Cumulative"
        ),
        highchartOutput(plot_name)
    )
}

#extract data
confirmed_0 <- wide_to_long(read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_iso3_regions.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&merge-replace02=on&merge-overwrite02=on&tagger-match-all=on&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv", stringsAsFactors = F))
deaths_0 <- wide_to_long(read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_iso3_regions.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&merge-replace02=on&merge-overwrite02=on&tagger-match-all=on&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv", stringsAsFactors = F))
recovered_0 <- wide_to_long(read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_recovered_global_iso3_regions.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&merge-replace02=on&merge-overwrite02=on&tagger-match-all=on&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv", stringsAsFactors = F))

confirmed <- modify.data(confirmed_0)
deaths <- modify.data(deaths_0)
recovered <- modify.data(recovered_0)

active <- data.frame(Country.Region = confirmed$Country.Region, 
                     Date = confirmed$Date, Value = confirmed$Value - deaths$Value - recovered$Value, iso3 = confirmed$iso3)
#Ration of first derivative
confirmed <- confirmed %>%
    group_by(Country.Region) %>%
    mutate(Ratio_Value = Delta_Value/lag(Delta_Value)) %>% as.data.frame

all <- cbind(confirmed[which(!colnames(confirmed) == "iso3")], deaths[which(!colnames(deaths) == "iso3")], 
             recovered[which(!colnames(recovered) == "iso3")], active[which(!colnames(active) == "iso3")])
all <- all[,c(1,2,3,4,5,8,9,12,13,16)]
colnames(all) <- c("Country","Date","Confirmed(#)","New Confirmed(#)","Growth Factor",
                   "Deaths(#)","New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")
rownames(all) <- NULL

all$Death_pct <- all[["Deaths(#)"]]/all[["Confirmed(#)"]]*100
colnames(all)[length(colnames(all))] <- "Overall Death Rate(%)"
all$Fnl_Death_pct <- all[["Deaths(#)"]]/rowSums(all[c("Deaths(#)","Recovered(#)")], na.rm = T)*100
colnames(all)[length(colnames(all))] <- "Finalised Death Rate(%)"
all$Recovery_pct <- all[["Recovered(#)"]]/all[["Confirmed(#)"]]*100
colnames(all)[length(colnames(all))] <- "Overall Recovery Rate(%)"
all$Fnl_Recovery_pct <- 100 - all[["Finalised Death Rate(%)"]]
colnames(all)[length(colnames(all))] <- "Finalised Recovery Rate(%)"
all$Active_pct <- 100 - all[["Overall Death Rate(%)"]] - all[["Overall Recovery Rate(%)"]]
colnames(all)[length(colnames(all))] <- "Active Rate(%)"

all[mapply(is.infinite, all)] <- NA
all[mapply(is.na, all)] <- NA
all[mapply(is.nan, all)] <- NA

all <- all[,c(1:6,11:12,7:8,13:14,9:10,15)]

country.list <- as.character(sort(unique(confirmed$Country.Region)))
date.range <- range(confirmed$Date, na.rm = T)

#india overall data extraction
dyn_data <- function(){
    a <- jsonlite::fromJSON("https://api.covid19india.org/data.json", simplifyVector = TRUE)
    india <- a$cases_time_series[c("dailyconfirmed","dailydeceased","dailyrecovered","date","totalconfirmed","totaldeceased","totalrecovered")]
    india$date <- paste0(india$date,"2020")
    india$date <- as.Date(india$date, format = "%d %B %Y")
    india[which(!colnames(india) == "date")] <- apply(india[which(!colnames(india) == "date")], 2, as.numeric)     
    colnames(india) <- c("New Confirmed(#)","New Deaths(#)","New Recovered(#)","Date","Confirmed(#)","Deaths(#)","Recovered(#)")
    india$Active <- india[["Confirmed(#)"]] - india[["Deaths(#)"]] - india[["Recovered(#)"]]
    colnames(india)[length(colnames(india))] <- "Active(#)"
    
    india$Death_pct <- india[["Deaths(#)"]]/india[["Confirmed(#)"]]*100
    colnames(india)[length(colnames(india))] <- "Overall Death Rate(%)"
    india$Fnl_Death_pct <- india[["Deaths(#)"]]/rowSums(india[c("Deaths(#)","Recovered(#)")], na.rm = T)*100
    colnames(india)[length(colnames(india))] <- "Finalised Death Rate(%)"
    india$Recovery_pct <- india[["Recovered(#)"]]/india[["Confirmed(#)"]]*100
    colnames(india)[length(colnames(india))] <- "Overall Recovery Rate(%)"
    india$Fnl_Recovery_pct <- 100 - india[["Finalised Death Rate(%)"]]
    colnames(india)[length(colnames(india))] <- "Finalised Recovery Rate(%)"
    india$Active_pct <- 100 - india[["Overall Death Rate(%)"]] - india[["Overall Recovery Rate(%)"]]
    colnames(india)[length(colnames(india))] <- "Active Rate(%)"
    
    india <- india[order(india$Date),]
    
    india$Growth_Factor <- india[["New Confirmed(#)"]]/lag(india[["New Confirmed(#)"]])
    colnames(india)[length(colnames(india))] <- "Growth Factor"
    
    india[mapply(is.infinite, india)] <- NA
    india[mapply(is.na, india)] <- NA
    india[mapply(is.nan, india)] <- NA
    
    india$Country.Region <- "India"
    india <- india[,c(15,4,5,1,14,6,9:10,2,7,11:12,3,8,13)]
    
    #india statewise data extraction
    
    #delta wrong so adjusting for that
    u <- jsonlite::fromJSON("https://api.covid19india.org/states_daily.json", simplifyVector = T)
    u1 <- reshape::melt(u$states_daily,id.vars = c("date","status"))
    u1$value <- as.numeric(as.character(u1$value))
    u1$date <- as.Date(u1$date, format = "%d-%b-%y")
    u1 <-  spread(u1,"status","value")
    u1 <- u1 %>% group_by(variable) %>% 
        mutate(Growth_Factor = Confirmed/lag(Confirmed)) 
    u1 <- u1[u1$date == as.Date(last(sort(u1$date))),]
    colnames(u1) <- c("date","state","confirmed","death","recovered","growth_factor")
    u1$state <- tolower(trimws(u1$state ,whitespace = "[ \t\n\r\v\f]"))
    u1 <- u1[order(u1$state),]
    
    
    india_statewise <- data.frame(a$statewise[c("active","confirmed","deaths","lastupdatedtime","recovered","state","deltaconfirmed",
                                                "deltadeaths","deltarecovered","statecode")])
    india_statewise$state1 <- tolower(trimws(gsub(" ", "", india_statewise$statecode, fixed = TRUE), whitespace = "[ \t\n\r\v\f]")) 
    india_statewise <- india_statewise[order(india_statewise$state1),]
    
    india_statewise[which(!colnames(india_statewise) %in% c("lastupdatedtime","state","state1","statecode"))] <- apply(india_statewise[which(!colnames(india_statewise) %in% c("lastupdatedtime","state","state1","statecode"))], 2, as.numeric)
    
    v <- c("state","deltaconfirmed","deltadeaths","deltarecovered","lastupdatedtime","confirmed","deaths","recovered")
    india_statewise <- india_statewise[v]
    colnames(india_statewise) <- c("Country.Region","New Confirmed(#)","New Deaths(#)","New Recovered(#)","Date","Confirmed(#)","Deaths(#)","Recovered(#)")
    india_statewise$Active <- india_statewise[["Confirmed(#)"]] - india_statewise[["Deaths(#)"]] - india_statewise[["Recovered(#)"]]
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Active(#)"
    
    india_statewise$Death_pct <- india_statewise[["Deaths(#)"]]/india_statewise[["Confirmed(#)"]]*100
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Overall Death Rate(%)"
    india_statewise$Fnl_Death_pct <- india_statewise[["Deaths(#)"]]/rowSums(india_statewise[c("Deaths(#)","Recovered(#)")], na.rm = T)*100
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Finalised Death Rate(%)"
    india_statewise$Recovery_pct <- india_statewise[["Recovered(#)"]]/india_statewise[["Confirmed(#)"]]*100
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Overall Recovery Rate(%)"
    india_statewise$Fnl_Recovery_pct <- 100 - india_statewise[["Finalised Death Rate(%)"]]
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Finalised Recovery Rate(%)"
    india_statewise$Active_pct <- 100 - india_statewise[["Overall Death Rate(%)"]] - india_statewise[["Overall Recovery Rate(%)"]]
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Active Rate(%)"
    
    india_statewise$Date <- as.Date(india_statewise$Date, format='%d/%m/%Y')
    india_statewise$Growth_Factor <- u1$growth_factor
    colnames(india_statewise)[length(colnames(india_statewise))] <- "Growth Factor"
    
    india_statewise <- india_statewise[order(india_statewise$`Confirmed(#)`, decreasing = T),]
    
    india_statewise[mapply(is.infinite, india_statewise)] <- NA
    india_statewise[mapply(is.na, india_statewise)] <- NA
    india_statewise[mapply(is.nan, india_statewise)] <- NA
    
    india_statewise <- india_statewise[,c(1,5,6,2,15,7,10:11,3,8,12:13,4,9,14)]
    india_statewise[india_statewise$Country.Region == "Total","Country.Region"] <- "India"
    
    #appending the latest value with india
    india <- rbind(india,india_statewise[1,])
    
    #india raw data
    d <- jsonlite::fromJSON("https://api.covid19india.org/raw_data.json", simplifyVector = T)
    india_raw_data <- d$raw_data[c("agebracket","currentstatus","detectedcity","detecteddistrict","detectedstate","gender","nationality","typeoftransmission")]
    india_raw_data$agebracket <- cut(as.numeric(india_raw_data$agebracket), breaks  = c(5, 10, 15, 20, 30, 40, 50, 60, 70 ,80), right = F, ordered_result = T)
    
    #district data
    b <- jsonlite::fromJSON("https://api.covid19india.org/state_district_wise.json", simplifyVector = T)
    
    district_data <- do.call('rbind',sapply(names(b), function(x){
        do.call('rbind',sapply(names(b[[x]][["districtData"]]), function(y){
            data.frame(State = x, District = y, Confirmed = b[[x]][["districtData"]][[y]][["confirmed"]],
                       New_Confirmed = b[[x]][["districtData"]][[y]][["delta"]][["confirmed"]])
            
        }, simplify = F))
    }, simplify = F))
    
    district_data[c("State","District")] <- apply(district_data[c("State","District")], 2, as.character) 
    district_data[district_data$District == "Unknown","District"] <- "Unknown District"
    
    district_data[district_data$State == district_data$District, "District"] <- paste(
        district_data[district_data$State == district_data$District, "State"],"District"
    ) 
    
    #state time series 
    u1 <- reshape::melt(u$states_daily,id.vars = c("date","status"))
    u1$value <- as.numeric(as.character(u1$value))
    u1$date <- as.Date(u1$date, format = "%d-%b-%y")
    u1 <-  spread(u1,"status","value")
    u1 <- u1 %>% group_by(variable) %>% 
        mutate(Growth_Factor = Confirmed/lag(Confirmed)) 
    u1 <- u1[u1$date < Sys.Date(),]
    colnames(u1) <- c("date","state","confirmed","death","recovered","growth_factor")
    
    f_na <- function(x) {x[is.na(x)] <- 0; x}
    
    u1[c("confirmed","death","recovered")] <- apply(u1[c("confirmed","death","recovered")], 2, f_na)
    
    u1 <- u1 %>% group_by(state) %>%
        mutate(total_confirmed = cumsum(confirmed),
               total_deaths = cumsum(death),
               total_recovered = cumsum(recovered))
    statecode <- a$statewise[c("state","statecode")]
    statecode$statecode <- tolower(statecode$statecode)
    state_daily <- merge.data.frame(u1,statecode, by.x = "state", by.y = "statecode")
    
    v <- c("state.y","confirmed","death","recovered","date","total_confirmed","total_deaths","total_recovered", "growth_factor")
    state_daily <- state_daily[v]
    colnames(state_daily) <- c("Country.Region","New Confirmed(#)","New Deaths(#)","New Recovered(#)","Date","Confirmed(#)","Deaths(#)","Recovered(#)", "Growth Factor")
    state_daily$Active <- state_daily[["Confirmed(#)"]] - state_daily[["Deaths(#)"]] - state_daily[["Recovered(#)"]]
    colnames(state_daily)[length(colnames(state_daily))] <- "Active(#)"
    
    state_daily$Death_pct <- state_daily[["Deaths(#)"]]/state_daily[["Confirmed(#)"]]*100
    colnames(state_daily)[length(colnames(state_daily))] <- "Overall Death Rate(%)"
    state_daily$Fnl_Death_pct <- state_daily[["Deaths(#)"]]/rowSums(state_daily[c("Deaths(#)","Recovered(#)")], na.rm = T)*100
    colnames(state_daily)[length(colnames(state_daily))] <- "Finalised Death Rate(%)"
    state_daily$Recovery_pct <- state_daily[["Recovered(#)"]]/state_daily[["Confirmed(#)"]]*100
    colnames(state_daily)[length(colnames(state_daily))] <- "Overall Recovery Rate(%)"
    state_daily$Fnl_Recovery_pct <- 100 - state_daily[["Finalised Death Rate(%)"]]
    colnames(state_daily)[length(colnames(state_daily))] <- "Finalised Recovery Rate(%)"
    state_daily$Active_pct <- 100 - state_daily[["Overall Death Rate(%)"]] - state_daily[["Overall Recovery Rate(%)"]]
    colnames(state_daily)[length(colnames(state_daily))] <- "Active Rate(%)"
    
    state_daily$Date <- as.Date(state_daily$Date, format='%d/%m/%Y')
    
    state_daily[mapply(is.infinite, state_daily)] <- NA
    state_daily[mapply(is.na, state_daily)] <- NA
    state_daily[mapply(is.nan, state_daily)] <- NA
    
    state_daily <- state_daily[names(india_statewise)]
    state_daily[state_daily$Country.Region == "Total","Country.Region"] <- "India"
    
    #appending the latest value with india
    
    x <- india[india$Date < Sys.Date(),]
    state_daily <- state_daily[state_daily$Country.Region != "India",]
    state_daily <- rbind(state_daily,x)
    
    if(india_statewise[india_statewise$Country.Region == "India","Date"] == Sys.Date()){
        statewise <- india_statewise
        statewise$Date <- Sys.Date()
        state_daily <- rbind(state_daily,statewise)
    }
    
    state_daily <- state_daily[order(state_daily$Date),]
    
    essentials <- jsonlite::fromJSON("https://api.covid19india.org/resources/resources.json", simplifyVector = TRUE)
    
    return(list(india = india, india_raw_data = india_raw_data, india_statewise = india_statewise, 
                district_data = district_data, a = a, state_daily = state_daily, essentials = essentials$resources))
}



time_diff <- function(x, current_time = Sys.time()){
    if(round(difftime(current_time,x, unit = "secs"),0) <= 1) {paste(round(difftime(current_time,x, unit = "secs"),0),"second ago")}
    else if(round(difftime(current_time,x, unit = "secs"),0) < 60) {paste(round(difftime(current_time,x, unit = "secs"),0),"seconds ago")}
    else if(round(difftime(current_time,x, unit = "mins"),0) <= 1) {paste(round(difftime(current_time,x, unit = "mins"),0),"minute ago")}
    else if(round(difftime(current_time,x, unit = "mins"),0) < 60) {paste(round(difftime(current_time,x, unit = "mins"),0),"minutes ago")}
    else if(round(difftime(current_time,x, unit = "hours"),0) <= 1) {paste(round(difftime(current_time,x, unit = "hours"),0),"hour ago")}
    else if(round(difftime(current_time,x, unit = "hours"),0) < 24) {paste(round(difftime(current_time,x, unit = "hours"),0),"hours ago")}
    else if(round(difftime(current_time,x, unit = "days"),0) <= 1) {paste(round(difftime(current_time,x, unit = "days"),0),"day ago")}
    else if(round(difftime(current_time,x, unit = "days"),0) < 7) {paste(round(difftime(current_time,x, unit = "days"),0),"days ago")}
    else if(round(difftime(current_time,x, unit = "weeks"),0) < 52) {paste(round(difftime(current_time,x, unit = "weeks"),0),"weeks ago")}
}

# df_world <- get_everything(query = c("COVID-19"),
#                            from = Sys.Date()-1, to = Sys.Date(), sort_by = "relevancy", language = "en")

df_india <- get_everything(query = c("COVID-19 India"),
                           from = Sys.Date()-1, to = Sys.Date(), sort_by = "relevancy", language = "en")

df_australia <- get_everything(query = c("COVID-19 Australia"),
                               from = Sys.Date()-1, to = Sys.Date(), sort_by = "relevancy", language = "en")


map.australia <- "https://code.highcharts.com/mapdata/countries/au/au-all.js"
map.india <- "https://code.highcharts.com/mapdata/countries/in/custom/in-all-disputed.js"
b <- get_data_from_map(download_map_data(map.india))
chk <- sort(b$name)
chk2 <- chk[which(!chk %in% c("NCT of Delhi"))]

create.delta.au <- function(x){
    x <- x %>%
        group_by(Country.Region) %>%
        mutate(Delta_Value = Value - lag(Value))
}

roll.up.region.dates.au <- function(x){
    x <- x%>%
        group_by(Country.Region, Date) %>%
        summarise(Value = sum(Value, na.rm = T))
}

roll.up.au <- function(x){
    data.frame(Country.Region = "Australia",
               x %>% group_by(Date) %>% 
                   summarise(Value = sum(Value, na.rm = T), Delta_Value = sum(Delta_Value, na.rm = T)))
}

modify.data.au <- function(x){
    x$Date <- as.Date(x$Date)
    x$Country.Region <- x$Province.State
    x$Value <- as.numeric(x$Value)
    x <- x[,c(2,5,6)] 
    x <- create.delta.au(roll.up.region.dates.au(x))
    x <- x[with(x, order(Country.Region, Date)),]
    x <- rbind(roll.up.au(x),data.frame(x))
    x[which(!is.na(x$Date)),]
}

confirmed_au <- modify.data.au(confirmed_0[confirmed_0$Country.Region=="Australia",])
deaths_au <- modify.data.au(deaths_0[deaths_0$Country.Region=="Australia",])
recovered_au <- modify.data.au(recovered_0[recovered_0$Country.Region=="Australia",])

active_au <- data.frame(Country.Region = confirmed_au$Country.Region, 
                        Date = confirmed_au$Date, Value = confirmed_au$Value - deaths_au$Value - recovered_au$Value)
#Ration of first derivative
confirmed_au <- confirmed_au %>%
    group_by(Country.Region) %>%
    mutate(Ratio_Value = Delta_Value/lag(Delta_Value)) %>% as.data.frame

australia <- cbind(confirmed_au, deaths_au, recovered_au, active_au)
australia <- australia[,c(1,2,3,4,5,8,9,12,13,16)]
colnames(australia) <- c("Country","Date","Confirmed(#)","New Confirmed(#)","Growth Factor",
                         "Deaths(#)","New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")
rownames(australia) <- NULL
australia$Death_pct <- australia[["Deaths(#)"]]/australia[["Confirmed(#)"]]*100
colnames(australia)[length(colnames(australia))] <- "Overall Death Rate(%)"
australia$Fnl_Death_pct <- australia[["Deaths(#)"]]/rowSums(australia[c("Deaths(#)","Recovered(#)")], na.rm = T)*100
colnames(australia)[length(colnames(australia))] <- "Finalised Death Rate(%)"
australia$Recovery_pct <- australia[["Recovered(#)"]]/australia[["Confirmed(#)"]]*100
colnames(australia)[length(colnames(australia))] <- "Overall Recovery Rate(%)"
australia$Fnl_Recovery_pct <- 100 - australia[["Finalised Death Rate(%)"]]
colnames(australia)[length(colnames(australia))] <- "Finalised Recovery Rate(%)"
australia$Active_pct <- 100 - australia[["Overall Death Rate(%)"]] - australia[["Overall Recovery Rate(%)"]]
colnames(australia)[length(colnames(australia))] <- "Active Rate(%)"

australia[mapply(is.infinite, australia)] <- NA
australia[mapply(is.na, australia)] <- NA
australia[mapply(is.nan, australia)] <- NA

australia <- australia[,c(1:6,11:12,7:8,13:14,9:10,15)]
colnames(australia)[1] <- "Country.Region"

pop <- wpp.by.year(wpp.indicator("tpop"),2020)
data(iso3166)

pop <- merge.data.frame(pop,iso3166[c("charcode","charcode3")], by.x = "charcode", by.y = "charcode")
pop$value <- pop$value*1000

cnf_dth <- merge.data.frame(confirmed,deaths, by.x = c("Date","iso3"), by.y = c("Date","iso3"))

cnf_dth_pop <- merge.data.frame(cnf_dth, pop, by.x = "iso3", by.y = "charcode3")

cnf_dth_pop <- cnf_dth_pop[,c(1,3,2,4,8,11)]
colnames(cnf_dth_pop) <- c("iso3","Country.Region","Date","Confirmed","Deaths","Population")
cnf_dth_pop$cnf_pop <- cnf_dth_pop$Confirmed/cnf_dth_pop$Population*(10^6)
cnf_dth_pop$dth_cnf <- cnf_dth_pop$Deaths/cnf_dth_pop$Confirmed*100
cnf_dth_pop <- cnf_dth_pop[c("iso3","Country.Region","Date","cnf_pop","dth_cnf","Population")]

#end of data extration

#app
ui <- dashboardPagePlus(skin = "red", enable_preloader = TRUE, sidebar_fullCollapse = F, md = F,
                        dashboardHeaderPlus(title = "COVID - 19 Daily Status", disable = F,
                                            tags$li(class = "dropdown",tags$a(href="mailto:abhijatridas@gmail.com",icon("envelope"),target="_blank")),
                                            tags$li(class = "dropdown",tags$a(href="https://www.linkedin.com/in/abhijatri-das-2800454b",icon("linkedin"),target="_blank")),
                                            tags$li(class="dropdown",tags$a(href="https://github.com/abhijatri/CoronaVirus/tree/master", icon("github"), "Source Code", target="_blank"))
                        ),
                        dashboardSidebar(collapsed = F,
                                         sidebarUserPanel(name = "Abhijatri Das", subtitle = "Developer",image = "https://i.ibb.co/vHbFPFJ/pp.jpg"),
                                         sidebarMenu(
                                             menuItem("India Report", badgeLabel = "LIVE!", badgeColor = "red", icon = icon("line-chart"), tabName = "a"),
                                             menuItem("India Essentials", icon = icon("question"), tabName = "h"),
                                             menuItem("Australia  Report", badgeLabel = "DAILY", icon = icon("line-chart"), tabName = "f"),
                                             menuItem("Across the Globe", icon = icon("globe", lib = "font-awesome"), startExpanded = F,
                                                      menuSubItem("Daily Report", tabName = "b", icon = icon("table")),
                                                      menuSubItem("Winning the War", tabName = "i", icon = icon("line-chart")),
                                                      menuSubItem("Map View", tabName = "c", icon = icon("area-chart")),
                                                      menuSubItem("Statistical Trends ", tabName = "d", icon = icon("line-chart"))
                                             ),
                                             menuItem("SIR Modelling Tool", badgeLabel = "POC", icon = icon("bar-chart-o"), tabName = "g"),
                                             menuItem("Information ", tabName = "e", icon = icon("info"))
                                         )
                        ),
                        dashboardBody(
                            includeCSS("./www/bootstrap.css"),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
                            fluidRow( 
                                tabItems(
                                    tabItem("a",
                                            column(12, offset = 0.5, h1(strong("India Live Updates (Unofficial)"))),
                                            column(12, offset = 0.5, htmlOutput("updt_time")),
                                            uiOutput("value_boxes3"),
                                            column(width = 12 ,uiOutput("axis_type_india")),
                                            box(title = "Live Cases in India", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(width = 6, offset = 0.5, uiOutput("analysis_type_india_select_input")),
                                                column(12,highchartOutput("plot_india")),
                                                fluidRow(
                                                    column(4,highchartOutput("plot_statewise", height = '700px')),
                                                    column(6, offset = 1,
                                                           uiOutput("date_input_india"),
                                                           highchartOutput("plot_statewise_india", height = '600px'))
                                                    
                                                ),
                                                column(12, offset = 0.5, h3(strong("India Forecast")), p("Forecast not available for new cases and growth factor.")),
                                                column(6,sliderTextInput(
                                                    inputId = "scenario",
                                                    label = "Choose Scenario",
                                                    choices = c("Very Optimistic", "Mildly Optimistic", "Baseline", "Severe", "Extremely Severe"),
                                                    selected = "Baseline",
                                                    grid = T, width = "100%",
                                                    force_edges =T
                                                )),
                                                column(12, offset = 0.5, radioGroupButtons(
                                                    inputId = "axis_type_fcst",
                                                    label = NULL,
                                                    choices = c("Linear", "Log10"),
                                                    selected = "Linear"
                                                )),
                                                highchartOutput("fcst_plot"),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("fcst_data_table"))
                                            ),
                                            box(title = "Live Cases in India Data", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                popify(el = ,
                                                       actionBttn(
                                                           inputId = "compare2",
                                                           label = "Compare Trends",
                                                           style = "bordered",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("line-chart")
                                                       ), title = "<b>Compare Statistical Trends</b>", placement = "right",
                                                       content = "Compare statistical trends of selected state/UT from table below."),
                                                uiOutput("modal_state"),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("data_table_india"))
                                            ),
                                            box(title = "Pivot Table from Raw Data", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                h4(strong("Use this for advanced deep dive analysis")),
                                                div(style = 'overflow-x: scroll',rpivotTableOutput("pivot_table"))
                                            ),
                                            box(title = "Testing Data", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("testing_datatable"))
                                            ),
                                            box(title = "Latest News Articles", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("news_india"))
                                            )
                                    ),
                                    tabItem("b",
                                            column(width = 12, offset = 0.5, dateInput(
                                                inputId = "date2",
                                                label = "Select Date",
                                                value = date.range[2],
                                                min = date.range[1],
                                                max = date.range[2],
                                                format = "dd/mm/yyyy"
                                            )),
                                            uiOutput("value_boxes2"),
                                            box(title = "Data Across the Globe",solidheader = F, width = 12, collapsible = T, status = "primary",
                                                popify(el = ,
                                                       actionBttn(
                                                           inputId = "compare",
                                                           label = "Compare Trends",
                                                           style = "bordered",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("line-chart")
                                                       ), title = "<b>Compare Statistical Trends</b>", placement = "right",
                                                       content = "Compare statistical trends of selected countries from table below."),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("data_table"))
                                            ),
                                            uiOutput("modal"),
                                    ),
                                    tabItem("c",
                                            column(width = 12, offset = 0.5, uiOutput("date_world_map_view")),
                                            uiOutput("value_boxes"),
                                            box_function2("Confirmed Cases","confirmed", "warning", "map_plot_confirmed", width = 6),
                                            box_function2("Death Cases","deaths", "danger", "map_plot_deaths", width = 6),
                                            box_function2("Recovered Cases","recovered", "success", "map_plot_recovered", width = 6),
                                            box(title = "Active Cases", solidheader = F, width = 6, collapsible = T, status = "primary",
                                                highchartOutput("map_plot_active")
                                            )
                                    ),
                                    tabItem("d",
                                            column(width = 12, offset = 0.5,selectizeInput(
                                                inputId = "countries",
                                                label = "Select Countries",
                                                choices = country.list,
                                                selected = "World",
                                                multiple = TRUE,
                                                options = list(
                                                    placeholder = 'Please select atleast one country',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                )
                                            )),
                                            column(width = 12, offset = 0.5,dateRangeInput("daterange", "Input Date Range",
                                                                                           start  = date.range[1],
                                                                                           end    = date.range[2],
                                                                                           min    = date.range[1],
                                                                                           max    = date.range[2],
                                                                                           format = "dd/mm/yyyy",
                                                                                           separator = "to")),
                                            box_function("Confirmed Cases","confirmed", "warning", "plot_confirmed", "growth_table_confirmed", width = 12),
                                            box(title = "Daily Cumulative Confirmed Cases Post First 'N' Cases", solidheader = F, width = 12, collapsible = T, status = "warning",
                                                column(4,numericInput("N",  label = NULL, value= 100, min = 1, step = 1)),
                                                column(3,offset = 3, radioGroupButtons(
                                                    inputId = paste("axis_type_N"),
                                                    label = NULL,
                                                    choices = c("Linear", "Log10"),
                                                    selected = "Linear"
                                                )),
                                                highchartOutput("plot_confirmed_n")
                                            ),
                                            box_function("Death Cases","deaths", "danger", "plot_deaths", "growth_table_deaths", width = 12),
                                            box(title = "Death Rate", solidheader = F, width = 12, collapsible = T, status = "danger",
                                                column(12,offset = 0.5, radioGroupButtons(
                                                    inputId = "choice3",
                                                    label = NULL,
                                                    choices = c("Overall", "Finalised"),
                                                    selected = "Overall"
                                                )),
                                                highchartOutput("plot_death_rate"),
                                            ),
                                            box_function("Recovered Cases","recovered", "success", "plot_recovered", "growth_table_recovered", width = 12),
                                            box(title = "Recovery Rate", solidheader = F, width = 12, collapsible = T, status = "success",
                                                column(12, offset = 0.5, radioGroupButtons(
                                                    inputId = "choice4",
                                                    label = NULL,
                                                    choices = c("Overall", "Finalised"),
                                                    selected = "Overall"
                                                )),
                                                highchartOutput("plot_recovery_rate"),
                                            ),
                                            box(title = "Active Cases", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(12,offset = 0.5, radioGroupButtons(
                                                    inputId = paste("axis_type","active",sep="_"),
                                                    label = NULL,
                                                    choices = c("Linear", "Log10"),
                                                    selected = "Linear"
                                                )),
                                                highchartOutput("plot_active")
                                            ),
                                            box(title = "Active Rate", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                highchartOutput("plot_active_rate"),
                                            ),
                                            box(title = "Ratio First Derivates of Cumulative Growth", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(6,selectizeInput(
                                                    inputId = "c2",
                                                    label = "Select a Country",
                                                    choices = country.list,
                                                    selected = "World",
                                                    multiple = F,
                                                    options = list(
                                                        placeholder = 'Please select a country',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                    )
                                                )),
                                                highchartOutput("plot_ratio_confirmed")
                                            ),
                                            box(title = "Forecast", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(12, offset = 0.5, p("Forecast not available for new cases and growth factor.")),
                                                fluidRow(column(4,
                                                                wellPanel(selectizeInput(
                                                                    inputId = "c3",
                                                                    label = "Select a Country",
                                                                    choices = country.list,
                                                                    selected = "World",
                                                                    multiple = F,
                                                                    options = list(
                                                                        placeholder = 'Please select a country',
                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                    )
                                                                ),
                                                                selectInput("analysis_type_world", label = "Forecast", choices = names(all)[3:15]),
                                                                sliderTextInput(
                                                                    inputId = "scenario2",
                                                                    label = "Choose Scenario",
                                                                    choices = c("Very Optimistic", "Mildly Optimistic", "Baseline", "Severe", "Extremely Severe"),
                                                                    selected = "Baseline",
                                                                    grid = T,
                                                                    force_edges =T
                                                                ))),
                                                         column(8, 
                                                                radioGroupButtons(
                                                                    inputId = "axis_type_fcst_world",
                                                                    label = NULL,
                                                                    choices = c("Linear", "Log10"),
                                                                    selected = "Linear"
                                                                ),
                                                                highchartOutput("fcst_plot_world"))),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("fcst_data_table_world"))
                                            ),
                                            box(title = "Proportion of Cases", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(6,selectizeInput(
                                                    inputId = "c",
                                                    label = "Select a Country",
                                                    choices = country.list,
                                                    selected = "World",
                                                    multiple = F,
                                                    options = list(
                                                        placeholder = 'Please select a country',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                    )
                                                )),
                                                highchartOutput("plot_proportion")
                                            ),
                                            box(title = "Latest News Articles", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(6,selectizeInput(
                                                    inputId = "news_country",
                                                    label = "Select a Country",
                                                    choices = country.list,
                                                    selected = "World",
                                                    multiple = F,
                                                    options = list(
                                                        placeholder = 'Please select a country',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                    )
                                                )),
                                                column(12,div(style = 'overflow-x: scroll',DT::dataTableOutput("news_world")))
                                            )
                                    ),
                                    tabItem("e",
                                            widgetUserBox(
                                                uiOutput("source_heading"),
                                                title = "Abhijatri Das",
                                                type = 2,
                                                subtitle = "Credit Risk Modeller, Statistician and a Data Enthusiast",
                                                color = "aqua-active",
                                                src = "https://i.ibb.co/vHbFPFJ/pp.jpg",
                                                footer_padding = FALSE),
                                            box(
                                                uiOutput("source_heading2"),
                                                title = span(icon("table"),"Data Sources"),
                                                status = "primary",
                                                solidHeader = T,
                                                collapsible = T
                                                )
                                            
                                    ),
                                    tabItem("f",
                                            column(12, offset = 0.5, h1(strong("Australia Daily Update"))),
                                            column(12, offset = 0.5, htmlOutput("australia_header")),
                                            htmlOutput("value_boxes4"),
                                            column(12, offset = 0.5, uiOutput("axis_type_aus")),
                                            box(title = "Daily Cases in Australia", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                column(6,offset = 0.5, uiOutput("analysis_type_australia_select_input")),
                                                column(12,highchartOutput("plot_australia")),
                                                column(6, uiOutput("date_input_australia")),
                                                column(12,highchartOutput("plot_statewise_australia", height = '700px')),
                                                column(12, offset = 0.5, h3(strong("Australia Forecast")),
                                                       p("Forecast not available for new cases and growth factor.")),
                                                column(6,sliderTextInput(
                                                    inputId = "scenario3",
                                                    label = "Choose Scenario",
                                                    choices = c("Very Optimistic", "Mildly Optimistic", "Baseline", "Severe", "Extremely Severe"),
                                                    selected = "Baseline",
                                                    grid = T, width = "100%",
                                                    force_edges =T
                                                )),
                                                column(12, offset = 0.5, radioGroupButtons(
                                                    inputId = "axis_type_fcst_australia",
                                                    label = NULL,
                                                    choices = c("Linear", "Log10"),
                                                    selected = "Linear"
                                                )),
                                                highchartOutput("fcst_plot_australia"),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("fcst_data_table_australia"))
                                            ),
                                            box(title = "Daily Cases in Australia Data", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                popify(el = ,
                                                       actionBttn(
                                                           inputId = "compare3",
                                                           label = "Compare Trends",
                                                           style = "bordered",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("line-chart")
                                                       ), title = "<b>Compare Statistical Trends</b>", placement = "right",
                                                       content = "Compare statistical trends of selected region/territories from table below."),
                                                uiOutput("modal_australia"),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("data_table_australia"))
                                            ),
                                            box(title = "Latest News Articles", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("news_australia"))
                                            )
                                    ),
                                    tabItem("g",
                                            column(12, offset = 0.5, h1(strong("Susceptible, Infected (Active Cases) and Removed (Deaths + Recovered) Model"))),
                                            box(title = "Toy Model", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                p("This is a toy model where you change the infection and recovery rates and analyse decrease in susceptible population,
                              increase in removed population and peak of the infected population. The results are based on the proportion of susceptible population.
                              This is not a model build exercise as it is not based on any data but more of a tool for the user to study the sensitivities/relationships 
                              associated with the model parameters before actually building the model"),
                                                fluidRow(
                                                    column(4,
                                                           
                                                           wellPanel(sliderInput(inputId = "beta2", label = "Infection Rate (Beta)", value = 0.5, min = 0, max = 1, step = 0.0001,
                                                                                 width = NULL),
                                                                     sliderInput(inputId = "gamma2", label = "Recovery Rate (Gamma)", value = 0.05, min = 0, max = 1, step = 0.0001,
                                                                                 width = NULL),
                                                                     numericInput(inputId = "nahead2", label = "Duration", value = 180, min = 0, max = NA, step = 0.01,
                                                                                  width = NULL)
                                                           )
                                                    ),
                                                    column(8, highchartOutput("plot_toy_sir")
                                                    )
                                                )
                                            ),
                                            box(title = "Model Build", solidheader = F, width = 12, collapsible = T, status = "primary",
                                                p("This is the most basic version SIR Model that minimises the sum of squares between
                                                   actuals and predicted. The user needs to select a country, a daterange for the actual data
                                                   on which the model would be built, input the volume of suceptible population,
                                                   initial parameters of infection rate (beta) and 
                                                   recovery rate (gamma) and number of days to forecast from the end of the selected data range.
                                                   Given that this is a basic SIR model, the forecasts are extremely indicative 
                                                   and should be assessed carefully. This model does not assume re-infection of a recovered 
                                                   patient.Please note that, convergence of algorithm is heavily dependant on the choice of inputs."),
                                                fluidRow(
                                                    column(4,
                                                           wellPanel(
                                                               selectInput(
                                                                   inputId = "c4",
                                                                   label = "Select a Country",
                                                                   choices = country.list
                                                               ),
                                                               dateRangeInput("daterange2", "Input Date Range",
                                                                              start  = date.range[1],
                                                                              end    = date.range[2],
                                                                              min    = date.range[1],
                                                                              max    = date.range[2],
                                                                              format = "dd/mm/yyyy",
                                                                              separator = "to"),
                                                               
                                                               numericInput(inputId = "pop_size", label = "Susceptible Population Size", value = 10^5, min = 1, max = NA, step = 1,
                                                                            width = NULL),
                                                               
                                                               numericInput(inputId = "beta", label = "Initial Value of Beta", value = 0.05, min = 0, max = 1, step = 0.01,
                                                                            width = NULL),
                                                               numericInput(inputId = "gamma", label = "Initial Value of Gamma", value = 0.1, min = 0, max = 1, step = 0.01,
                                                                            width = NULL),
                                                               numericInput(inputId = "nahead", label = "No. of Days to Forecast", value = 60, min = 0, max = NA, step = 1,
                                                                            width = NULL),
                                                               actionBttn(
                                                                   inputId = "sir_bttn",
                                                                   label = "Build SIR Model",
                                                                   style = "unite",
                                                                   color = "primary",
                                                                   size = "sm",
                                                                   icon = icon("line-chart")
                                                               )
                                                           )
                                                    ), 
                                                    column(8,
                                                           radioGroupButtons(
                                                               inputId = "axis_type_sir",
                                                               label = NULL,
                                                               choices = c("Linear", "Log10"),
                                                               selected = "Linear"
                                                           ),
                                                           highchartOutput("plot_sir", height = '425px'),
                                                           uiOutput("sir_result")
                                                    )
                                                )
                                            )
                                    ),
                                    tabItem("h",
                                            box(title = "Essentials and Resources", solidheader = F, width = 12, collapsible = T, 
                                                status = "primary",   
                                                wellPanel(fluidRow(
                                                    column(4, 
                                                           uiOutput("category_select")),
                                                    column(4, uiOutput("state_select")),
                                                    column(4, uiOutput("city_select")))),
                                                div(style = 'overflow-x: scroll',DT::dataTableOutput("essential_table")
                                                )
                                            )
                                    ),
                                    tabItem("i",
                                            column(12, offset = 0.5, h1("Winning the War Against COVID-19")),
                                            box(title = "Who is Winning the War against COVID-19?", solidheader = F, width = 12, collapsible = T, 
                                                status = "primary",
                                                p("'Winning the war' is a visual  analysis based on bubble chart between 'cases per million' and 'Deaths per 100 cases; size of the bubble is a measure of population. 
                                                The x-axis could be a measure of country's preparedness and effective controls taken against the spread of the virus, 
                                                whereas the y-axis could be the response of a country's health-care system given the infected cases. An ideal scenario 
                                                for any country would be on the lower coordinates with respect to both x and y. On the other hand, if any country 
                                                is on the top corresponding to both coordinates we might say that currently the country is in a precarious situation 
                                                and health-care system is probably getting overwhelmed. Slide the dates to observe
                                                  how the positions of countries have changed during this time period, some have flattened the curve and
                                                  sitting on the lower coordinates where as some are facing a challenging and tough situation against covid and 
                                                  probably their health care system is getting overwhelmed."),
                                                column(6, offset = 0.5, uiOutput("date_world_bubble_view")),
                                                highchartOutput("bubble_plot", height = "800px"),
                                                column(12, offset = 0.5,p("*Size of the bubble represents the population size"))
                                            )
                                    )
                                )
                            )
                        )
)


server <- function(input, output, session) { 
    
    output$map_plot_confirmed <- renderHighchart({
        x <- confirmed
        x <- x[x$Date==input$date,]
        x[x$Value < 1,"Value"] <- 1
        x[x$Delta_Value < 1 & is.na(x$Delta_Value) == FALSE,"Delta_Value"] <- 1
        m <- match("iso3",colnames(x))
        colnames(x)[m] <- "iso-a3"
        choice <- input[["choice2_confirmed"]]
        v <- ifelse(choice == "Cumulative","Value","Delta_Value")
        d <- x[,c("Country.Region","Date",v,"iso-a3")]
        colnames(d)[3] <- "Value"
        text <- ifelse(choice == "Cumulative","Cumulative","New")
        
        mapchart(d,text = text, type = "logarithmic", minColor = "#efecf3", maxColor = "#f39c12",
                 allowNegativeLog = TRUE)
    })
    
    output$map_plot_deaths <- renderHighchart({
        x <- deaths
        x <- x[x$Date==input$date,]
        x[x$Value < 1,"Value"] <- 1
        x[x$Delta_Value < 1 & is.na(x$Delta_Value) == FALSE,"Delta_Value"] <- 1
        m <- match("iso3",colnames(x))
        colnames(x)[m] <- "iso-a3"
        choice <- input[["choice2_deaths"]]
        if (choice == "Cumulative"){
            d <- x[,c("Country.Region","Date","Value","iso-a3")]
            mapchart(d,text = "Cumulative", type = "logarithmic", minColor = "#efecf3", maxColor = "#dd4b39",
                     allowNegativeLog = TRUE)
        }
        else {
            d <- x[,c("Country.Region","Date","Delta_Value","iso-a3")]
            colnames(d)[3] <- "Value"
            mapchart(d,text = "New", type = "logarithmic", minColor = "#efecf3", maxColor = "#dd4b39",
                     allowNegativeLog = TRUE)
        }
        
    })
    
    output$map_plot_recovered <- renderHighchart({
        x <- recovered
        x <- x[x$Date==input$date,]
        x[x$Value < 1,"Value"] <- 1
        x[x$Delta_Value < 1 & is.na(x$Delta_Value) == FALSE,"Delta_Value"] <- 1
        m <- match("iso3",colnames(x))
        colnames(x)[m] <- "iso-a3"
        choice <- input[["choice2_recovered"]]
        if (choice == "Cumulative"){
            d <- x[,c("Country.Region","Date","Value","iso-a3")]
            mapchart(d,text = "Cumulative", type = "logarithmic", minColor = "#efecf3", maxColor = "#00a65a",
                     allowNegativeLog = TRUE)
        }
        else {
            d <- x[,c("Country.Region","Date","Delta_Value","iso-a3")]
            colnames(d)[3] <- "Value"
            mapchart(d,text = "New", type = "logarithmic", minColor = "#efecf3", maxColor = "#00a65a",
                     allowNegativeLog = TRUE)
        }
        
    })
    
    output$map_plot_active <- renderHighchart({
        x <- active
        x <- x[x$Date==input$date,]
        x[x$Value < 1,"Value"] <- 1
        m <- match("iso3",colnames(x))
        colnames(x)[m] <- "iso-a3"
        d <- x[,c("Country.Region","Date","Value","iso-a3")]
        hcmap(data = d , dataLabels = list(enabled = TRUE, format = '{point.name}'),
              joinBy = "iso-a3", value = "Value", name = "Total", 
              download_map_data = F) %>%
            hc_colorAxis(type = "logarithmic", allowNegativeLog = TRUE, minColor = "#efecf3", maxColor = "#3c8dbc")%>%
            hc_mapNavigation(enabled = TRUE) 
        
    })
    
    filtered <- reactive({
        list(confirmed = confirmed[confirmed$Date >= input$daterange[1] & confirmed$Date <= input$daterange[2] & confirmed$Country.Region %in% input$countries,],
             deaths = deaths[deaths$Date >= input$daterange[1] & deaths$Date <= input$daterange[2] & deaths$Country.Region %in% input$countries,],
             recovered = recovered[recovered$Date >= input$daterange[1] & recovered$Date <= input$daterange[2] & recovered$Country.Region %in% input$countries,],
             active = active[active$Date >= input$daterange[1] & active$Date <= input$daterange[2] & active$Country.Region %in% input$countries,])
    })
    
    output$plot_confirmed <- renderHighchart({
        x <- filtered()[["confirmed"]]
        choice <- input[["choice_confirmed"]]
        type <- "linear"
        type <- ifelse(input[["axis_type_confirmed"]] == "Linear","linear","logarithmic")
        if (choice == "Cumulative"){
            d <- x[,c("Country.Region","Date","Value")]
            linechart(d,"Cumulative Confirmed Cases", type)
        }
        else {
            d <- x[,c("Country.Region","Date","Delta_Value")]
            colnames(d)[3] <- "Value"
            linechart(d,"New Confirmed Cases", type)  
        }
    })
    
    output$plot_confirmed_n <- renderHighchart({
        x <- filtered()[["confirmed"]]
        x <- x[x$Value >= input$N,]
        x <- x %>% group_by(Country.Region) %>% mutate(Days = Date - min(Date))
        type <- "linear"
        type <- ifelse(input[["axis_type_N"]] == "Linear","linear","logarithmic")
        highcharter::hchart(x, type = 'line', 
                            hcaes(y = Value, group = Country.Region, x = Days))  %>%
            hc_yAxis(title = list(text = "Cumulative Confirmed Cases"), type = type) %>% 
            hc_xAxis(title = list(text = paste("Days From First",input$N,"Cases"))) %>% 
            hc_legend(align = "right", layout = "proximate")%>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE) %>% hc_tooltip(shared = TRUE)
        
    })
    
    output$plot_deaths <- renderHighchart({
        x <- filtered()[["deaths"]]
        choice <- input[["choice_deaths"]]
        type <- "linear"
        type <- ifelse(input[["axis_type_deaths"]] == "Linear","linear","logarithmic")
        if (choice == "Cumulative"){
            d <- x[,c("Country.Region","Date","Value")]
            linechart(d,"Cumulative Death Cases",type)
        }
        else {
            d <- x[,c("Country.Region","Date","Delta_Value")]
            colnames(d)[3] <- "Value"
            linechart(d,"New Death Cases",type)  
        }
    })
    
    output$plot_recovered <- renderHighchart({
        x <- filtered()[["recovered"]]
        choice <- input[["choice_recovered"]]
        type <- "linear"
        type <- ifelse(input[["axis_type_recovered"]] == "Linear","linear","logarithmic")
        if (choice == "Cumulative"){
            d <- x[,c("Country.Region","Date","Value")]
            linechart(d,"Cumulative Recovered Cases",type)
        }
        else {
            d <- x[,c("Country.Region","Date","Delta_Value")]
            colnames(d)[3] <- "Value"
            linechart(d,"New Recovered Cases",type)  
        }
    })
    
    output$plot_active <- renderHighchart({
        x <- filtered()[["active"]]
        type <- "linear"
        type <- ifelse(input[["axis_type_active"]] == "Linear","linear","logarithmic")
        d <- x[,c("Country.Region","Date","Value")]
        linechart(d,"Active Cases",type)
    })
    
    output$plot_death_rate <- renderHighchart({
        name <- ifelse(input$choice3 == "Overall", "Overall Death Rate(%)", "Finalised Death Rate(%)")
        x <- all[all$Date >= input$daterange[1] & all$Date <= input$daterange[2] & all$Country %in% input$countries,c("Country","Date",name)]
        colnames(x) <- c("Country.Region","Date","Value")
        type <- "linear"
        linechart(x,name,type) %>% hc_tooltip(valueDecimals = 2) %>% hc_exporting(enabled = TRUE)
    })
    
    output$plot_recovery_rate <- renderHighchart({
        name <- ifelse(input$choice4 == "Overall", "Overall Recovery Rate(%)", "Finalised Recovery Rate(%)")
        x <- all[all$Date >= input$daterange[1] & all$Date <= input$daterange[2] & all$Country %in% input$countries,c("Country","Date",name)]
        colnames(x) <- c("Country.Region","Date","Value")
        type <- "linear"
        linechart(x,name,type) %>% hc_tooltip(valueDecimals = 2) %>% hc_exporting(enabled = TRUE)
    })
    
    output$plot_active_rate <- renderHighchart({
        name <- "Active Rate(%)"
        x <- all[all$Date >= input$daterange[1] & all$Date <= input$daterange[2] & all$Country %in% input$countries,c("Country","Date",name)]
        colnames(x) <- c("Country.Region","Date","Value")
        type <- "linear"
        linechart(x,name,type) %>% hc_tooltip(valueDecimals = 2) %>% hc_exporting(enabled = TRUE)
    })
    
    output$plot_proportion <- renderHighchart({
        c <- input$c
        cnf <- confirmed[confirmed$Country.Region == c,-match("iso3",colnames(confirmed))]
        dth <- deaths[deaths$Country.Region == c,-match("iso3",colnames(deaths))]
        rec <- recovered[recovered$Country.Region == c,-match("iso3",colnames(recovered))]
        d <- base::merge(cnf,dth,by = "Date")
        d <- base::merge(d,rec,by = "Date")
        d <- na.omit(d[,c(1,3,7,10)])
        colnames(d) <- c("Date", "Confirmed", "Deaths", "Recovered")
        d$Active <- d$Confirmed - d$Deaths - d$Recovered
        d <- reshape2::melt(d,id.vars = "Date")
        d <- d[d$variable != "Confirmed",]
        highcharter:: hchart(d, type = 'column', hcaes(y = value, group = variable, x = Date)) %>% 
            hc_xAxis(type = "datetime",title = list(text = "Date"), type = "datetime") %>%
            hc_plotOptions(column = list(stacking = "percent")) %>% hc_colors(colors = list("#dd4b39","#00a65a","#f39c12")) %>%
            hc_yAxis(title = list(text = "% Cases")) %>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE) %>%
            hc_rangeSelector(enabled = TRUE,
                             buttons = list(
                                 list(type = 'all', text = 'All'),
                                 list(type = 'week', count = 1, text = '1w'),
                                 list(type = 'week', count = 2, text = '2w'),
                                 list(type = 'month', count = 1, text = '1m'),
                                 list(type = 'month', count = 2, text = '2m'),
                                 list(type = 'month', count = 3, text = '3m'),
                                 list(type = 'month', count = 6, text = '6m'),
                                 list(type = 'ytd', text = 'YTD'),
                                 list(type = 'year', count = 1, text = '1y')
                             ),
                             verticalAlign = "top",
                             selected = 0
            ) %>% 
            hc_tooltip(shared = TRUE)
    })
    
    output$growth_table_confirmed <- DT::renderDataTable({
        choice <- input[["choice_confirmed"]]
        if(choice == "New"){ return(NULL)}
        else{
            x <- confirmed
            x <- x[x$Date >= input$daterange[1] & x$Date <= input$daterange[2],]
            c <- input$countries
            x <- x[x$Country.Region %in% c, c("Country.Region","Value")]
            d <- regression(x,c)
            DT::datatable(d, rownames = F) %>% formatRound(c(2:3), 2)
        }
    })
    
    output$growth_table_deaths <- DT::renderDataTable({
        choice <- input[["choice_deaths"]]
        if(choice == "New"){ return(NULL)}
        else{
            x <- deaths
            x <- x[x$Date >= input$daterange[1] & x$Date <= input$daterange[2],]
            c <- input$countries
            x <- x[x$Country.Region %in% c, c("Country.Region","Value")]
            d <- regression(x,c)
            DT::datatable(d, rownames = F) %>% formatRound(c(2:3), 2)
        }
    })
    
    output$growth_table_recovered <- DT::renderDataTable({
        choice <- input[["choice_recovered"]]
        if(choice == "New"){ return(NULL)}
        else{
            x <- recovered
            x <- x[x$Date >= input$daterange[1] & x$Date <= input$daterange[2],]
            c <- input$countries
            x <- x[x$Country.Region %in% c, c("Country.Region","Value")]
            d <- regression(x,c)
            DT::datatable(d, rownames = F) %>% formatRound(c(2:3), 2)
        }
    })
    
    output$plot_ratio_confirmed <- renderHighchart({
        c <- input$c2
        x <- confirmed
        x <- x[x$Country.Region %in% c & x$Date >= input$daterange[1] & x$Date <= input$daterange[2], c("Country.Region","Date","Value","Ratio_Value")]
        d <- regression(x,c)
        d <- na.omit(data.frame(x, Fitted = d[3]))
        colnames(d)[4:5] <- c("Actual","Fitted")
        highchart() %>%
            hc_add_series(name = "Actual", data = d, type = "line", hcaes(y = Actual, x = as.Date(Date))) %>%
            hc_add_series(name = "Fitted", data = d, type = "line", hcaes(y = Fitted, x = as.Date(Date)),
                          marker = list(enabled = FALSE)) %>%
            hc_tooltip(valueDecimals = 2, shared = TRUE) %>%
            hc_xAxis(type = "datetime", title = list(text= "Date")) %>% hc_yAxis(title = list(text = "New Cases(t)/New Cases(t-1)"))%>%
            hc_chart(zoomType = 'xy') %>%
            hc_rangeSelector(enabled = TRUE,
                             buttons = list(
                                 list(type = 'all', text = 'All'),
                                 list(type = 'week', count = 1, text = '1w'),
                                 list(type = 'week', count = 2, text = '2w'),
                                 list(type = 'month', count = 1, text = '1m'),
                                 list(type = 'month', count = 2, text = '2m'),
                                 list(type = 'month', count = 3, text = '3m'),
                                 list(type = 'month', count = 6, text = '6m'),
                                 list(type = 'ytd', text = 'YTD'),
                                 list(type = 'year', count = 1, text = '1y')
                             ),
                             verticalAlign = "top",
                             selected = 0
            ) %>% hc_exporting(enabled = TRUE)
    })
    
    output$value_boxes <- renderUI({
        cnf <- comma(confirmed[confirmed$Country.Region == "World" & confirmed$Date == input$date, "Value"], digits = 0)
        cnf_new <- comma(confirmed[confirmed$Country.Region == "World" & confirmed$Date == input$date, "Delta_Value"], digits = 0)
        dth <- comma(deaths[deaths$Country.Region == "World" & deaths$Date == input$date, "Value"], digits = 0)
        dth_new <- comma(deaths[deaths$Country.Region == "World" & deaths$Date == input$date, "Delta_Value"], digits = 0)
        rec <- comma(recovered[recovered$Country.Region == "World" & recovered$Date == input$date, "Value"], digits = 0)
        rec_new <- comma(recovered[recovered$Country.Region == "World" & recovered$Date == input$date, "Delta_Value"], digits = 0)
        act <- comma(active[active$Country.Region == "World" & active$Date == input$date, "Value"], digits = 0)
        list(valueBox(cnf, icon = icon("plus-square"), subtitle = paste0("Confirmed (+",cnf_new,")"), color = "yellow", width = 3),
             valueBox(dth, icon = icon("exclamation-triangle"), subtitle = paste0("Deaths"," (+",dth_new,"; ",round(dth/cnf*100,0),"% overall; ",round(dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "red", width = 3),
             valueBox(rec, icon = icon("heartbeat"),subtitle = paste0("Recovered"," (+",rec_new,"; ",round(rec/cnf*100,0),"% overall; ",round(100 - dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "green", width  = 3),
             valueBox(act, icon = icon("diagnoses"), subtitle = paste0("Active"," (",round(act/cnf*100,0),"%)"), color = "aqua", width = 3))
    })
    
    output$value_boxes2 <- renderUI({
        cnf <- comma(confirmed[confirmed$Country.Region == "World" & confirmed$Date == input$date2, "Value"], digits = 0)
        cnf_new <- comma(confirmed[confirmed$Country.Region == "World" & confirmed$Date == input$date2, "Delta_Value"], digits = 0)
        dth <- comma(deaths[deaths$Country.Region == "World" & deaths$Date == input$date2, "Value"], digits = 0)
        dth_new <- comma(deaths[deaths$Country.Region == "World" & deaths$Date == input$date2, "Delta_Value"], digits = 0)
        rec <- comma(recovered[recovered$Country.Region == "World" & recovered$Date == input$date2, "Value"], digits = 0)
        rec_new <- comma(recovered[recovered$Country.Region == "World" & recovered$Date == input$date2, "Delta_Value"], digits = 0)
        act <- comma(active[active$Country.Region == "World" & active$Date == input$date2, "Value"], digits = 0)
        list(valueBox(cnf, icon = icon("plus-square"), subtitle = paste0("Confirmed (+",cnf_new,")"), color = "yellow", width = 3),
             valueBox(dth, icon = icon("exclamation-triangle"), subtitle = paste0("Deaths"," (+",dth_new,"; ",round(dth/cnf*100,0),"% overall; ",round(dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "red", width = 3),
             valueBox(rec, icon = icon("heartbeat"),subtitle = paste0("Recovered"," (+",rec_new,"; ",round(rec/cnf*100,0),"% overall; ",round(100 - dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "green", width  = 3),
             valueBox(act, icon = icon("diagnoses"), subtitle = paste0("Active"," (",round(act/cnf*100,0),"%)"), color = "aqua", width = 3))
    })
    
    data_table <- reactive({
        all <- all[all$Date == input$date2, -2]
        all <- all[order(all$`Confirmed(#)`,decreasing = T),]
        colnames(all)[4] <- "Growth Factor"
        all
    })
    
    output$data_table <- DT::renderDataTable({
        DT::datatable(data_table(), rownames = FALSE, filter = "top", 
                      options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
            formatStyle('Overall Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Overall Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Active Rate(%)',
                        background = styleColorBar(range(na.omit(all['Active Rate(%)'])), '#3c8dbc'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Growth Factor',
                        color = styleInterval(1,c("#00a65a","#dd4b39"))) %>%
            formatStyle(c("Country","Growth Factor"), fontWeight = "bold") %>% 
            formatRound(c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                          "Finalised Recovery Rate(%)","Active Rate(%)"), 2) %>%
            formatCurrency(c("Confirmed(#)","New Confirmed(#)","Deaths(#)","New Deaths(#)",
                             "Recovered(#)","New Recovered(#)","Active(#)" ),
                           currency = "", interval = 3, mark = ",", digits = 0)
        
    })
    
    country <- reactive({
        data_table()[input$data_table_rows_selected,1]
    })
    
    output$plot_modal <- renderHighchart({
        x <- all[all$Country %in% country(),c("Country","Date",input$analysis_type)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        type <- "linear"
        if (input$analysis_type %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                       "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type2 == "Log10","logarithmic","linear")
        }
        p <- linechart(x,input$analysis_type, type) 
        if (input$analysis_type %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                       "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2)}
        p %>% hc_exporting(enabled = TRUE)
    })
    
    output$modal_axis_type <- renderUI({
        
        if (input$analysis_type %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                       "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            radioGroupButtons(
                inputId = "axis_type2",
                label = NULL,
                choices = c("Linear", "Log10"),
                selected = "Linear"
            )
        }
        else { return(NULL) }
    })
    
    
    output$modal <- renderUI({
        bsModal(id = "modal_compare", title = strong("Comparison of Trends"), trigger = "compare", size = "large",
                column(width = 12, offset = 0.5,selectInput("analysis_type", label = "Compare", choices = names(all)[3:15])),
                column(12, offset = 0.5,uiOutput("modal_axis_type")),
                highchartOutput("plot_modal")
        )
    })
    
    india <- reactiveVal()
    india_raw_data <-  reactiveVal()
    india_statewise <- reactiveVal()
    district_data <- reactiveVal()
    a <- reactiveVal()
    time <- reactiveVal()
    state_daily <- reactiveVal()
    essentials <- reactiveVal()
    
    # Observer that updates the data every 1000ms.
    observe({
        # invalidate every 10000ms
        invalidateLater(300000, session)
        isolate({    
            # fetch the new data
            new_data <- dyn_data()
            india(new_data$india)
            india_raw_data(new_data$india_raw_data)
            india_statewise(new_data$india_statewise)
            district_data(new_data$district_data)
            state_daily(new_data$state_daily)
            a(new_data$a)
            essentials(new_data$essentials)
            time(Sys.time())
        })
    })
    
    output$analysis_type_india_select_input <- renderUI({
        selectInput("analysis_type_india", label = "Select", choices = names(india())[3:15])
    })
    
    # output$state_input <- renderUI({
    #     selectInput("state", label = "Select State/UT", choices = sort(unique(district_data()$State)))
    # })
    
    observeEvent(input$hcClicked, {
        updateSelectInput(session, "state", selected = input$hcClicked)
    })
    
    observeEvent(input$hcClicked, {
        showModal(
            modalDialog(
                tagList(
                    renderHighchart({
                        x <- state_daily()[state_daily()$Country.Region == input$hcClicked,c("Country.Region","Date",input$analysis_type_india)]
                        colnames(x) <- c("Country.Region", "Date", "Value")
                        type <- "linear"
                        if (input$analysis_type_india %in% c("Confirmed(#)", "New Confirmed(#)")) {color <- "#f39c12"}
                        else if (input$analysis_type_india %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {color <- "#dd4b39"}
                        else if (input$analysis_type_india %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {color <- "#00a65a"}
                        else color <- "#3c8dbc"
                        if (input$analysis_type_india %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
                            type <- ifelse(input$axis_type3 == "Log10","logarithmic","linear")
                        }
                        p <- linechart(x,input$analysis_type_india, type)
                        if (input$analysis_type_india %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                                             "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2, shared = TRUE)}
                        p %>% hc_colors(colors  = color) %>% hc_exporting(enabled = TRUE) %>% hc_title(text =  input$hcClicked, align = 'left') %>%
                            hc_legend(enabled = FALSE)
                    }),
                    renderHighchart({
                        district_data <- district_data()
                        data <- district_data[district_data$State == input$hcClicked,]
                        
                        var <- var(data$New_Confirmed)
                        
                        if(var > 0) {
                            hctreemap2(data,group_vars = c("District"), size_var = "Confirmed", color_var = "New_Confirmed",
                                       layoutAlgorithm = "squarified") %>% 
                                hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
            Confirmed: {point.value:,.0f}<br> New Confirmed: {point.colorValue:,.0f}") %>% hc_legend(enabled = F) %>% 
                                hc_colorAxis(minColor = brewer.pal(9,"Reds")[3], maxColor = brewer.pal(9,"Reds")[9]) %>% hc_exporting(enabled = TRUE) 
                        } else{
                            
                            hctreemap2(data,group_vars = c("District"), size_var = "Confirmed", color_var = "Confirmed",
                                       layoutAlgorithm = "squarified") %>% 
                                hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
            Confirmed: {point.value:,.0f}") %>% hc_legend(enabled = F) %>% 
                                hc_colorAxis(minColor = brewer.pal(9,"Reds")[3], maxColor = brewer.pal(9,"Reds")[9]) %>% hc_exporting(enabled = TRUE) 
                        }
                        
                    })
                ),
                easyClose = TRUE,
            )
        )
    })
    
    output$axis_type_india <- renderUI({
        
        if (input$analysis_type_india %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            radioGroupButtons(
                inputId = "axis_type3",
                label = NULL,
                choices = c("Linear", "Log10"),
                selected = "Linear"
            )
        }
        else { return(NULL) }
    })
    
    output$plot_india <- renderHighchart({
        x <- india()[,c("Country.Region","Date",input$analysis_type_india)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        type <- "linear"
        if (input$analysis_type_india %in% c("Confirmed(#)", "New Confirmed(#)")) {color <- "#f39c12"}
        else if (input$analysis_type_india %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {color <- "#dd4b39"}
        else if (input$analysis_type_india %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {color <- "#00a65a"}
        else color <- "#3c8dbc"
        if (input$analysis_type_india %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type3 == "Log10","logarithmic","linear")
        }
        p <- linechart(x,input$analysis_type_india, type)
        if (input$analysis_type_india %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                             "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2, shared = TRUE)}
        p %>% hc_colors(colors  = color) %>% hc_exporting(enabled = TRUE) %>% hc_legend(enabled = FALSE)
    })
    
    
    output$plot_statewise <- renderHighchart({
        myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked', event.point.name);}")
        x <- india_statewise()[india_statewise()$Country.Region != "India",c("Country.Region","Date",input$analysis_type_india)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        colorByPoint <- FALSE
        type <- "linear"
        if (input$analysis_type_india %in% c("Confirmed(#)", "New Confirmed(#)")) {
            color <- "#f39c12"
        } else if (input$analysis_type_india %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {
            color <- "#dd4b39"
        } else if (input$analysis_type_india %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {
            color <- "#00a65a"
        } else {
            color <- "#3c8dbc"
        }
        if (input$analysis_type_india %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type3 == "Log10","logarithmic","linear")
        }
        p <- hchart(x, "bar", hcaes(x = Country.Region, y = Value), name = input$analysis_type_india) %>%
            hc_yAxis(title = list(text = input$analysis_type_india), type = type) %>% hc_xAxis(title = list(text = "State/UT"))
        if (input$analysis_type_india %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                             "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2)}
        if (input$analysis_type_india == "Growth Factor") {
            color <- ifelse(x$Value < 1, "#00a65a","#dd4b39")
            colorByPoint <- TRUE
        }
        p %>% hc_title(text = "<b>Latest State/Union Territory Split</b>", align = 'left') %>%
            hc_plotOptions(bar = list(colorByPoint = colorByPoint), series = list(stacking = FALSE, events = list(click = myClickFunc))) %>% hc_colors(colors = color)%>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE) %>% hc_subtitle(text = "Click to get the corresponding time series and latest district level numbers", align = 'left')
    })
    
    output$date_input_india <- renderUI({
        a <- state_daily()[state_daily()$Country.Region != "India",]
        min <- min(a$Date)
        max <- max(a$Date)
        sliderInput("date_input_ind","Select Date", min = min, max = max, value = max, animate = animationOptions(200), 
                    timeFormat = "%d %B %Y" )
    })
    
    output$pivot_table <- renderRpivotTable({
        rpivotTable(india_raw_data(), rows = "agebracket", cols = "typeoftransmission", subtotals = T)
    })
    
    output$data_table_india <- DT::renderDataTable({
        DT::datatable(data_table_india(), rownames = FALSE, filter = "top",
                      options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
            formatStyle('Overall Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Overall Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Active Rate(%)',
                        background = styleColorBar(range(na.omit(all['Active Rate(%)'])), '#3c8dbc'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Growth Factor',
                        color = styleInterval(1,c("#00a65a","#dd4b39"))) %>%
            formatStyle(c("Country/State/UT","Growth Factor"), fontWeight = "bold") %>%
            formatStyle('lastupdatedtime','text-align' = 'center') %>%
            formatRound(c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                          "Finalised Recovery Rate(%)","Active Rate(%)"), 2) %>%
            formatCurrency(c("Confirmed(#)","New Confirmed(#)","Deaths(#)","New Deaths(#)",
                             "Recovered(#)","New Recovered(#)","Active(#)" ),
                           currency = "", interval = 3, mark = ",", digits = 0)
        
        
    })
    
    # output$district_treemap <- renderHighchart({
    #     district_data <- district_data()
    #     data <- district_data[district_data$State == input$state,]
    #     
    #     var <- var(data$New_Confirmed)
    #     
    #     if(var > 0) {
    #         hctreemap2(data,group_vars = c("District"), size_var = "Confirmed", color_var = "New_Confirmed",
    #                    layoutAlgorithm = "squarified") %>% 
    #             hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
    #         Confirmed: {point.value:,.0f}<br> New Confirmed: {point.colorValue:,.0f}") %>% hc_legend(enabled = F) %>% 
    #             hc_title(text = paste(input$state), align = "left", style = list(fontWeight  = "bold")) %>%
    #             hc_colorAxis(minColor = brewer.pal(9,"Reds")[3], maxColor = brewer.pal(9,"Reds")[9]) %>% hc_exporting(enabled = TRUE)
    #     } else{
    #         
    #         hctreemap2(data,group_vars = c("District"), size_var = "Confirmed", color_var = "Confirmed",
    #                    layoutAlgorithm = "squarified") %>% 
    #             hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
    #         Confirmed: {point.value:,.0f}") %>% hc_legend(enabled = F) %>% 
    #             hc_title(text = paste(input$state), align = "left",style = list(fontWeight  = "bold")) %>%
    #             hc_colorAxis(minColor = brewer.pal(9,"Reds")[3], maxColor = brewer.pal(9,"Reds")[9]) %>% hc_exporting(enabled = TRUE)
    #     }
    #     
    # })
    
    output$testing_datatable <- DT::renderDataTable({
        x <- a()$tested[,c(3,1,2,4:ncol(a()$tested))]
        x$updatetimestamp <- as.POSIXct(x$updatetimestamp, format = '%d/%m/%Y %H:%M:%S')
        x <- x[order(x$updatetimestamp, decreasing = T),]
        x$updatetimestamp <- format(x$updatetimestamp, format = "%d %B %Y, %I:%M:%S %p IST")
        x[colnames(x)[2:(length(colnames(x))-1)]] <- apply(x[colnames(x)[2:(length(colnames(x))-1)]],2, as.numeric)
        x$pctpostivecases <- x$totalpositivecases/x$totalsamplestested*100
        
        n <- colnames(x)[1:(length(colnames(x)) - 2)]
        x <- x[c(n,"pctpostivecases","updatetimestamp")]
        DT::datatable(x, rownames = FALSE, filter = "top") %>%
            formatCurrency(colnames(x)[2:(length(colnames(x)) - 2)], currency = "", interval = 3, mark = ",", digits = 0) %>%
            formatRound(c("pctpostivecases"), 2) %>%
            formatStyle('updatetimestamp','text-align' = 'center') %>%
            formatStyle('pctpostivecases',
                        background = styleColorBar(range(na.omit(x['pctpostivecases'])), '#3c8dbc'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
    })
    
    output$value_boxes3 <- renderUI({
        x <- india_statewise()
        cnf <- comma(x[x$Country.Region == "India", "Confirmed(#)"], digits = 0)
        cnf_new <- comma(x[x$Country.Region == "India", "New Confirmed(#)"], digits = 0)
        dth <- comma(x[x$Country.Region == "India", "Deaths(#)"], digits = 0)
        dth_new <- comma(x[x$Country.Region == "India", "New Deaths(#)"], digits = 0)
        rec <- comma(x[x$Country.Region == "India", "Recovered(#)"], digits = 0)
        rec_new <- comma(x[x$Country.Region == "India", "New Recovered(#)"], digits = 0)
        act <- comma(x[x$Country.Region == "India", "Active(#)"], digits = 0)
        list(valueBox(cnf, icon = icon("plus-square"), subtitle = paste0("Confirmed (+",cnf_new,")"), color = "yellow", width = 3),
             valueBox(dth, icon = icon("exclamation-triangle"), subtitle = paste0("Deaths"," (+",dth_new,"; ",round(dth/cnf*100,0),"% overall; ",round(dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "red", width = 3),
             valueBox(rec, icon = icon("heartbeat"),subtitle = paste0("Recovered"," (+",rec_new,"; ",round(rec/cnf*100,0),"% overall; ",round(100 - dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "green", width  = 3),
             valueBox(act, icon = icon("diagnoses"), subtitle = paste0("Active"," (",round(act/cnf*100,0),"%)"), color = "aqua", width = 3))
    })
    
    output$updt_time <- renderText({
        y <- a()$statewise[a()$statewise$state == "Total","lastupdatedtime"]
        paste("<B>Last Updated:</B>",format(as.POSIXct(y, format = '%d/%m/%Y %H:%M:%S'), format = "%d %B %Y, %I:%M:%S %p IST"))
    })
    
    data_table_india <- reactive({
        x <- india_statewise()[,-2]
        y <- a()$statewise
        y$lastupdatedtime <- format(as.POSIXct(y$lastupdatedtime, format = '%d/%m/%Y %H:%M:%S'), format = "%d %B %Y, %I:%M:%S %p IST")
        y <- y[order(as.numeric(y$confirmed), decreasing = T),]
        x <- cbind(x,y["lastupdatedtime"])
        colnames(x)[1] <- "Country/State/UT"
        x
    })
    
    fcst_data <- reactive({
        x <- india()
        reg_data <- x[c("Date","Confirmed(#)","Deaths(#)","Recovered(#)")]
        reg_data <- reg_data[-nrow(reg_data),]
        reg_data <- reg_data[order(reg_data$Date),]
        base_data <- data.frame(Period = c("Last 3 days","Last 1 week","Last 2 weeks","Full Time Series"))
        
        if (input$scenario %in% c("Very Optimistic","Extremely Severe")){
            level <- 0.90
        } else if (input$scenario %in% c("Mildly Optimistic","Severe")) {
            level <- 0.50
        } else {
            level <- 0.95
        }
        
        pred.type <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            if(x == "Recovered(#)"){
                if(input$scenario %in% c("Very Optimistic","Mildly Optimistic")) {"upr"}
                else if (input$scenario %in% c("Severe","Extremely Severe")) {"lwr"}
                else {"fit"}
            }
            else {
                if(input$scenario %in% c("Very Optimistic","Mildly Optimistic")) {"lwr"}
                else if (input$scenario %in% c("Severe","Extremely Severe")) {"upr"}
                else {"fit"}
            }
        }, simplify = F)
        
        final_data <- list()
        
        final_data <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            a <- do.call('rbind',sapply(c(3,7,14,nrow(reg_data)),function(l){
                d <- data.frame(y = tail(reg_data[x],n = l), x = 1:l)
                colnames(d) <- c("y","x")
                d[d$y < 1,"y"] <- 1
                reg <- lm(log(y) ~ x, data = d)
                p <- as.data.frame(predict.lm(reg,data.frame(x = c(l,l + 3, l + 7, l + 14, l + 30)), level = level, interval = "prediction"))
                p <- t(exp(p[[pred.type[[x]]]]))
                data.frame(current = last(d$y), p)
            }, simplify = F))
        }, simplify = F)
        
        b <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]]
        final_data[["Deaths(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        b <- final_data[["Confirmed(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Recovered(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        
        final_data[["Active(#)"]] <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Active(#)"]][final_data[["Active(#)"]] < 0] <- 0
        final_data[["Active Rate(%)"]] <- final_data[["Active(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Overall Death Rate(%)"]] <- final_data[["Deaths(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Death Rate(%)"]] <- final_data[["Deaths(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        final_data[["Overall Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        
        
        final_data <- sapply(names(final_data), function(x){
            a <- cbind(base_data, final_data[[x]])
            colnames(a) <- c("Growth Rate Estimated From","Current Observed","Current Predicted","Forecast 3 Days","Forecast 1 Week",
                             "Forecast 2 Weeks","Forecast 1 Month")
            a
        }, simplify = F)
        final_data
    })
    
    output$fcst_data_table <- DT::renderDataTable({
        d <- fcst_data()[[input$analysis_type_india]]
        if(is.null(d)) {return(NULL)}
        if (input$analysis_type_india %in% c("Confirmed(#)","Deaths(#)","Recovered(#)","Active(#)")){
            DT::datatable(d, rownames = FALSE,
                          options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatCurrency(c(2:7),
                               currency = "", interval = 3, mark = ",", digits = 0)
        } else {
            DT::datatable(d, rownames = FALSE,
                          options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatRound(c(2:7), 2)
        }
    })
    
    output$fcst_plot <- renderHighchart({
        d <- fcst_data()[[input$analysis_type_india]]
        if(is.null(d)) {return(NULL)}
        d <- reshape::melt(d,id.vars = "Growth Rate Estimated From")
        colnames(d)[1] <-"Growth_Period_Estimated_From"
        d$Growth_Period_Estimated_From <- paste("Growth -",d$Growth_Period_Estimated_From)
        type <- "linear"
        type <- ifelse(input$axis_type_fcst == "Linear","linear","logarithmic")
        highcharter::hchart(d, type = 'column',
                            hcaes(y = value, group = Growth_Period_Estimated_From, x = variable))  %>%
            hc_yAxis(title = list(text = input$analysis_type_india), type = type) %>% hc_xAxis(title = list(text = "")) %>%
            hc_tooltip(valueDecimals = 0, shared = TRUE)%>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE) 
        
    })
    
    output$fcst_data_table_world <- DT::renderDataTable({
        
        x <- all[all$Country == input$c3,]
        reg_data <- x[c("Date","Confirmed(#)","Deaths(#)","Recovered(#)")]
        reg_data <- reg_data[order(reg_data$Date),]
        base_data <- data.frame(Period = c("Last 3 days","Last 1 week","Last 2 weeks","Full Time Series"))
        
        if (input$scenario2 %in% c("Very Optimistic","Extremely Severe")){
            level <- 0.90
        } else if (input$scenario2 %in% c("Mildly Optimistic","Severe")) {
            level <- 0.50
        } else {
            level <- 0.95
        }
        
        pred.type <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            if(x == "Recovered(#)"){
                if(input$scenario2 %in% c("Very Optimistic","Mildly Optimistic")) {"upr"}
                else if (input$scenario2 %in% c("Severe","Extremely Severe")) {"lwr"}
                else {"fit"}
            }
            else {
                if(input$scenario2 %in% c("Very Optimistic","Mildly Optimistic")) {"lwr"}
                else if (input$scenario2 %in% c("Severe","Extremely Severe")) {"upr"}
                else {"fit"}
            }
        }, simplify = F)
        
        final_data <- list()
        
        final_data <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            a <- do.call('rbind',sapply(c(3,7,14,nrow(reg_data)),function(l){
                d <- data.frame(y = tail(reg_data[x],n = l), x = 1:l)
                colnames(d) <- c("y","x")
                d[d$y < 1,"y"] <- 1
                reg <- lm(log(y) ~ x, data = d)
                p <- as.data.frame(predict.lm(reg,data.frame(x = c(l,l + 3, l + 7, l + 14, l + 30)), level = level, interval = "prediction"))
                p <- t(exp(p[[pred.type[[x]]]]))
                data.frame(current = last(d$y), p)
            }, simplify = F))
        }, simplify = F)
        
        b <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]]
        final_data[["Deaths(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        b <- final_data[["Confirmed(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Recovered(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        
        final_data[["Active(#)"]] <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Active(#)"]][final_data[["Active(#)"]] < 0] <- 0
        final_data[["Active Rate(%)"]] <- final_data[["Active(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Overall Death Rate(%)"]] <- final_data[["Deaths(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Death Rate(%)"]] <- final_data[["Deaths(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        final_data[["Overall Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        
        
        final_data <- sapply(names(final_data), function(x){
            a <- cbind(base_data, final_data[[x]])
            colnames(a) <- c("Growth Rate Estimated From","Current Observed","Current Predicted","Forecast 3 Days","Forecast 1 Week",
                             "Forecast 2 Weeks","Forecast 1 Month")
            a
        }, simplify = F)
        
        d <- final_data[[input$analysis_type_world]]
        if(is.null(d)) {return("Please choose other analysis type")}
        if (input$analysis_type_world %in% c("Confirmed(#)","Deaths(#)","Recovered(#)","Active(#)")){
            d <- DT::datatable(d, rownames = FALSE,
                               options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatCurrency(c(2:7),
                               currency = "", interval = 3, mark = ",", digits = 0)
        } else {
            d <- DT::datatable(d, rownames = FALSE,
                               options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatRound(c(2:7), 2)
        }
        
        return(d)
        
    })
    
    output$fcst_plot_world <- renderHighchart({
        x <- all[all$Country == input$c3,]
        reg_data <- x[c("Date","Confirmed(#)","Deaths(#)","Recovered(#)")]
        reg_data <- reg_data[order(reg_data$Date),]
        base_data <- data.frame(Period = c("Last 3 days","Last 1 week","Last 2 weeks","Full Time Series"))
        
        if (input$scenario2 %in% c("Very Optimistic","Extremely Severe")){
            level <- 0.90
        } else if (input$scenario2 %in% c("Mildly Optimistic","Severe")) {
            level <- 0.50
        } else {
            level <- 0.95
        }
        
        pred.type <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            if(x == "Recovered(#)"){
                if(input$scenario2 %in% c("Very Optimistic","Mildly Optimistic")) {"upr"}
                else if (input$scenario2 %in% c("Severe","Extremely Severe")) {"lwr"}
                else {"fit"}
            }
            else {
                if(input$scenario2 %in% c("Very Optimistic","Mildly Optimistic")) {"lwr"}
                else if (input$scenario2 %in% c("Severe","Extremely Severe")) {"upr"}
                else {"fit"}
            }
        }, simplify = F)
        
        final_data <- list()
        
        final_data <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            a <- do.call('rbind',sapply(c(3,7,14,nrow(reg_data)),function(l){
                d <- data.frame(y = tail(reg_data[x],n = l), x = 1:l)
                colnames(d) <- c("y","x")
                d[d$y < 1,"y"] <- 1
                reg <- lm(log(y) ~ x, data = d)
                p <- as.data.frame(predict.lm(reg,data.frame(x = c(l,l + 3, l + 7, l + 14, l + 30)), level = level, interval = "prediction"))
                p <- t(exp(p[[pred.type[[x]]]]))
                data.frame(current = last(d$y), p)
            }, simplify = F))
        }, simplify = F)
        
        b <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]]
        final_data[["Deaths(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        b <- final_data[["Confirmed(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Recovered(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        
        final_data[["Active(#)"]] <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Active(#)"]][final_data[["Active(#)"]] < 0] <- 0
        final_data[["Active Rate(%)"]] <- final_data[["Active(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Overall Death Rate(%)"]] <- final_data[["Deaths(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Death Rate(%)"]] <- final_data[["Deaths(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        final_data[["Overall Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        
        
        final_data <- sapply(names(final_data), function(x){
            a <- cbind(base_data, final_data[[x]])
            colnames(a) <- c("Growth Rate Estimated From","Current Observed","Current Predicted","Forecast 3 Days","Forecast 1 Week",
                             "Forecast 2 Weeks","Forecast 1 Month")
            a
        }, simplify = F)
        d <- final_data[[input$analysis_type_world]]
        if(is.null(d)) {return("Please choose other analysis type")}
        d <- reshape::melt(d,id.vars = "Growth Rate Estimated From")
        colnames(d)[1] <-"Growth_Period_Estimated_From"
        d$Growth_Period_Estimated_From <- paste("Growth -",d$Growth_Period_Estimated_From)
        type <- "linear"
        type <- ifelse(input$axis_type_fcst_world == "Linear","linear","logarithmic")
        highcharter::hchart(d, type = 'column',
                            hcaes(y = value, group = Growth_Period_Estimated_From, x = variable))  %>%
            hc_yAxis(title = list(text = input$analysis_type_world), type = type) %>% hc_xAxis(title = list(text = "")) %>%
            hc_tooltip(valueDecimals = 0, shared = TRUE)%>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE)
        
    })
    
    output$news_india <- DT::renderDataTable({
        df <- df_india$results_df
        df$ago <- sapply(df$published_at, time_diff, current_time = time())
        df$url2 <-  paste0("<a href='",df$url,"' target='_blank'>",df$title,"</a>")
        df$image <- paste0('<img src="',df$url_to_image,'" height="80"></img>')
        df$url2 <- paste(df$url2,df$description,paste0(df$author,", ",paste0("<b>",df$name), "</b>",", ", df$ago), sep = "\n")
        df$url2 <- gsub(pattern = "\n", replacement = "<br/>", x = df$url2)
        df <- df[c("image","url2")]
        
        DT::datatable(df, escape = F,rownames = F,  colnames = '', options = list(ordering=F))
        
    })
    
    output$news_world <- DT::renderDataTable({
        df_country <- get_everything(query = paste("COVID-19", input$news_country),
                                     from = Sys.Date()-1, to = Sys.Date(), sort_by = "relevancy", language = "en")
        df <- df_country$results_df
        df$ago <- sapply(df$published_at, time_diff, current_time = time())
        df$url2 <-  paste0("<a href='",df$url,"' target='_blank'>",df$title,"</a>")
        df$image <- paste0('<img src="',df$url_to_image,'" height="80"></img>')
        df$url2 <- paste(df$url2,df$description,paste0(df$author,", ",paste0("<b>",df$name), "</b>",", ", df$ago), sep = "\n")
        df$url2 <- gsub(pattern = "\n", replacement = "<br/>", x = df$url2)
        df <- df[c("image","url2")]
        
        DT::datatable(df, escape = F,rownames = F, colnames = '', options = list(ordering=F))
        
    })
    
    output$news_australia <- DT::renderDataTable({
        df <- df_australia$results_df
        df$ago <- sapply(df$published_at, time_diff, current_time = time())
        df$url2 <-  paste0("<a href='",df$url,"' target='_blank'>",df$title,"</a>")
        df$image <- paste0('<img src="',df$url_to_image,'" height="80"></img>')
        df$url2 <- paste(df$url2,df$description,paste0(df$author,", ",paste0("<b>",df$name), "</b>",", ", df$ago), sep = "\n")
        df$url2 <- gsub(pattern = "\n", replacement = "<br/>", x = df$url2)
        df <- df[c("image","url2")]
        
        DT::datatable(df, escape = F,rownames = F, colnames = '', options = list(ordering=F))
        
    })
    
    
    output$plot_modal_sate <- renderHighchart({
        c <- data_table_india()[input$data_table_india_rows_selected,1]
        x <- state_daily()[state_daily()$Country.Region %in% c ,c("Country.Region","Date",input$analysis_type_state)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        type <- "linear"
        if (input$analysis_type_state %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type5 == "Log10","logarithmic","linear")
        }
        p <- linechart(x,input$analysis_type_state, type) 
        if (input$analysis_type_state %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                             "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2)}
        p %>% hc_exporting(enabled = TRUE)
    })
    
    output$modal_axis_type_state <- renderUI({
        
        if (input$analysis_type_state %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            radioGroupButtons(
                inputId = "axis_type5",
                label = NULL,
                choices = c("Linear", "Log10"),
                selected = "Linear"
            )
        }
        else { return(NULL) }
    })
    
    
    output$modal_state <- renderUI({
        bsModal(id = "modal_compare2", title = strong("Comparison of Trends"), trigger = "compare2", size = "large",
                column(width = 12, offset = 0.5,selectInput("analysis_type_state", label = "Select", choices = names(state_daily()[3:15]))),
                column(12, offset = 0.5,uiOutput("modal_axis_type_state")),
                highchartOutput("plot_modal_sate")
        )
    })
    
    output$source_heading <- renderUI({
        
        text  <- h5("I am the developer of this dashboard. Interested people please follow/contact me at:")
        email <- h5(icon("envelope"), shiny::a("abhijatridas@gmail.com", href="mailto:abhijatridas@gmail.com", target="_blank"))
        link <- h5(icon("linkedin") ,shiny::a("https://www.linkedin.com/in/abhijatri-das-2800454b", 
                                              href="https://www.linkedin.com/in/abhijatri-das-2800454b", target="_blank"))
        github <- h5(icon("github") ,shiny::a("https://github.com/abhijatri/CoronaVirus/tree/master", 
                                              href="https://github.com/abhijatri/CoronaVirus/tree/master", target="_blank"))
        list(text,email,link,github)
    })
    
    output$source_heading2 <- renderUI({
        url1 <- shiny::a("India COVID-19 Tracker - A Crowdsourced Initiative",icon("external-link"), 
                         href="https://api.covid19india.org", target="_blank")
        url2 <- shiny::a("Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)",icon("external-link"), 
                         href="https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases", target="_blank")
        d <- h5(strong("India Live Data: "),url1)
        e <- h5( strong("World Daily Data: "),url2)
        f <- p("Novel Corona Virus (COVID-19) epidemiological data since 22 January 2020. 
               The data is compiled by the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) 
               from various sources including the World Health Organization (WHO), DXY.cn. Pneumonia. 2020, 
               BNO News, National Health Commission of the People's Republic of China (NHC), China CDC (CCDC), 
               Hong Kong Department of Health, Macau Government, Taiwan CDC, US CDC, Government of Canada, 
               Australia Government Department of Health, European Centre for Disease Prevention and Control (ECDC), 
               Ministry of Health Singapore (MOH).")
        list(d,e,f)
    })
    
    output$australia_header <- renderText({
        paste("<B>Last Updated:</B>",format(max(australia$Date, na.rm = T), format = "%d %B %Y"))
        
    })
    output$value_boxes4 <- renderUI({
        x <- australia[australia$Date == max(australia$Date, na.rm = T),]
        cnf <- comma(x[x$Country.Region == "Australia", "Confirmed(#)"], digits = 0)
        cnf_new <- comma(x[x$Country.Region == "Australia", "New Confirmed(#)"], digits = 0)
        dth <- comma(x[x$Country.Region == "Australia", "Deaths(#)"], digits = 0)
        dth_new <- comma(x[x$Country.Region == "Australia", "New Deaths(#)"], digits = 0)
        rec <- comma(x[x$Country.Region == "Australia", "Recovered(#)"], digits = 0)
        rec_new <- comma(x[x$Country.Region == "Australia", "New Recovered(#)"], digits = 0)
        act <- comma(x[x$Country.Region == "Australia", "Active(#)"], digits = 0)
        list(valueBox(cnf, icon = icon("plus-square"), subtitle = paste0("Confirmed (+",cnf_new,")"), color = "yellow", width = 3),
             valueBox(dth, icon = icon("exclamation-triangle"), subtitle = paste0("Deaths"," (+",dth_new,"; ",round(dth/cnf*100,0),"% overall; ",round(dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "red", width = 3),
             valueBox(rec, icon = icon("heartbeat"),subtitle = paste0("Recovered"," (+",rec_new,"; ",round(rec/cnf*100,0),"% overall; ",round(100 - dth/sum(rec,dth, na.rm = T)*100,0),"% of closed)"), color = "green", width  = 3),
             valueBox(act, icon = icon("diagnoses"), subtitle = paste0("Active"," (",round(act/cnf*100,0),"%)"), color = "aqua", width = 3))
    })
    
    output$analysis_type_australia_select_input <- renderUI({
        selectInput("analysis_type_australia", label = "Select", choices = names(australia)[3:15])
    })
    # 
    output$date_input_australia <- renderUI({
        min <- min(australia$Date, na.rm = T)
        max <- max(australia$Date, na.rm = T)
        sliderInput("date_input_aus","Select Date", min = min, max = max, value = max, animate = animationOptions(200), 
                    timeFormat = "%d %B %Y" )
    })
    
    output$date_world_map_view <- renderUI({
        min <- min(all$Date,na.rm = T)
        max <- max(all$Date, na.rm = T)
        sliderInput("date","Select Date", min = min, max = max, value = max, animate = F,width = '100%', 
                    timeFormat = "%d %B %Y" )
    })
    
    output$plot_australia <- renderHighchart({
        x <- australia[australia$Country.Region == "Australia",c("Country.Region","Date",input$analysis_type_australia)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        type <- "linear"
        if (input$analysis_type_australia %in% c("Confirmed(#)", "New Confirmed(#)")) {
            color <- "#f39c12"
        }else if (input$analysis_type_australia %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {
            color <- "#dd4b39"
        }else if (input$analysis_type_australia %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {
            color <- "#00a65a"
        }else { 
            color <- "#3c8dbc"
        }
        
        if (input$analysis_type_australia %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                 "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type_australia == "Log10","logarithmic","linear")
        }
        p <- linechart(x,input$analysis_type_australia, type)
        if (input$analysis_type_australia %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                                 "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2, shared = TRUE)}
        p %>% hc_colors(colors  = color) %>% hc_exporting(enabled = TRUE) %>% hc_legend(enabled = FALSE)
    })
    
    output$plot_statewise_australia <- renderHighchart({
        myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked_aus', event.point.name);}")
        x <- australia[australia$Date == input$date_input_aus & australia$Country.Region != "Australia",c("Country.Region",input$analysis_type_australia)]
        colnames(x)[2] <- "Value" 
        type <- "linear"
        
        if (input$analysis_type_australia %in% c("Confirmed(#)", "New Confirmed(#)")) {
            maxColor <- "#f39c12"
            minColor = "#efecf3"
        } else if (input$analysis_type_australia %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {
            maxColor <- "#dd4b39"
            minColor = "#efecf3"
        }else if (input$analysis_type_australia %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {
            maxColor <- "#00a65a"
            minColor = "#efecf3"
        }else if (input$analysis_type_australia %in% c("Growth Factor")) {
            maxColor <- "#dd4b39"
            minColor = "#efecf3"
        }else{
            maxColor <- "#3c8dbc"
            minColor = "#efecf3"
        }
        
        if (input$analysis_type_australia %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                 "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type_australia == "Log10","logarithmic","linear")
        }
        
        x[x$Value == 0 & !is.na(x$Value), "Value"] <- 0.01
        
        p <- hcmap(data = x ,map=map.australia,
                   joinBy = c("name","Country.Region"), value = "Value", 
                   name = input$analysis_type_australia, dataLabels = list(enabled = TRUE, format = '{point.name}'),download_map_data = F) %>%
            hc_colorAxis(type = type, minColor = minColor, maxColor = maxColor,
                         allowNegativeLog = TRUE) %>%
            hc_mapNavigation(enabled = TRUE)
        if(input$analysis_type_australia %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                                "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2)}else 
                                                { p <- p %>% hc_tooltip(valueDecimals = 0) 
                                                }
        p %>% hc_title(text = "<b>Region/Territory Split</b>", align = 'center') %>% hc_exporting(enabled = TRUE) %>%
            hc_plotOptions(series = list(stacking = FALSE, events = list(click = myClickFunc))) %>% 
            hc_subtitle(text = "Click to get the corresponding time series", align = 'center')
    })  
    
    observeEvent(input$hcClicked_aus, {
        showModal(
            modalDialog(
                renderHighchart({
                    x <- australia[australia$Country.Region == input$hcClicked_aus,c("Country.Region","Date",input$analysis_type_australia)]
                    colnames(x) <- c("Country.Region", "Date", "Value")
                    type <- "linear"
                    if (input$analysis_type_australia %in% c("Confirmed(#)", "New Confirmed(#)")) {
                        color <- "#f39c12"
                    }else if (input$analysis_type_australia %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {
                        color <- "#dd4b39"
                    }else if (input$analysis_type_australia %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {
                        color <- "#00a65a"
                    }else { 
                        color <- "#3c8dbc"
                    }
                    
                    if (input$analysis_type_australia %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
                        type <- ifelse(input$axis_type_australia == "Log10","logarithmic","linear")
                    }
                    p <- linechart(x,input$analysis_type_australia, type)
                    if (input$analysis_type_australia %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                                             "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2, shared = TRUE)}
                    p %>% hc_colors(colors  = color) %>% hc_exporting(enabled = TRUE) %>% hc_title(text = input$hcClicked_aus, align = 'left') %>% hc_legend(enabled = FALSE)
                }),
                easyClose = TRUE,
            )
        )
    })
    
    output$plot_statewise_india <- renderHighchart({
        a <- state_daily()
        a <- a[a$Date == input$date_input_ind & a$Country.Region != "India",c("Country.Region",input$analysis_type_india)]
        colnames(a)[2] <- "Value"
        a <- a[order(a$Country.Region),]
        a[which(!a$Country.Region %in% c("Delhi","Ladakh")),"Country.Region"] <- chk2
        a[a$Country.Region == "Delhi", "Country.Region"] <- "NCT of Delhi"
        a[a$Country.Region == "Ladakh", "Country.Region"] <- "Jammu and Kashmir"
        a <- a%>% group_by(Country.Region) %>% summarise(Value = sum(Value,na.rm = F))
        type <- "linear"
        
        
        if (input$analysis_type_india %in% c("Confirmed(#)", "New Confirmed(#)")) {
            maxColor <- "#f39c12"
            minColor = "#efecf3"
        } else if (input$analysis_type_india %in% c("Deaths(#)", "Overall Death Rate(%)","Finalised Death Rate(%)","New Deaths(#)")) {
            maxColor <- "#dd4b39"
            minColor = "#efecf3"
        }else if (input$analysis_type_india %in% c("Recovered(#)", "Overall Recovery Rate(%)","Finalised Recovery Rate(%)","New Recovered(#)")) {
            maxColor <- "#00a65a"
            minColor = "#efecf3"
        }else if (input$analysis_type_india %in% c("Growth Factor")) {
            maxColor <- "#dd4b39"
            minColor = "#efecf3"
        }else{
            maxColor <- "#3c8dbc"
            minColor = "#efecf3"
        }
        
        if (input$analysis_type_india %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                             "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type3 == "Log10","logarithmic","linear")
        }
        
        a[a$Value == 0 & !is.na(a$Value), "Value"] <- 0.01
        
        p <- hcmap(data = a ,map=map.india,
                   joinBy = c("name","Country.Region"), value = "Value",
                   name = input$analysis_type_india, dataLabels = list(enabled = TRUE, format = '{point.name}'),download_map_data = F) %>%
            hc_colorAxis(type = type, minColor = minColor, maxColor = maxColor,
                         allowNegativeLog = TRUE) %>%
            hc_mapNavigation(enabled = TRUE)
        if(input$analysis_type_india %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                            "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2)}else
                                            { p <- p %>% hc_tooltip(valueDecimals = 0)
                                            }
        p %>% hc_title(text = "<b>State/Union Territory Split</b>", align = 'center') %>% hc_exporting(enabled = TRUE)
        
    })
    
    output$axis_type_aus <- renderUI({
        
        if (input$analysis_type_australia %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                 "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            radioGroupButtons(
                inputId = "axis_type_australia",
                label = NULL,
                choices = c("Linear", "Log10"),
                selected = "Linear"
            )
        }
        else { return(NULL) }
    })
    
    output$fcst_data_table_australia <- DT::renderDataTable({
        
        x <- all[all$Country == "Australia",]
        reg_data <- x[c("Date","Confirmed(#)","Deaths(#)","Recovered(#)")]
        reg_data <- reg_data[order(reg_data$Date),]
        base_data <- data.frame(Period = c("Last 3 days","Last 1 week","Last 2 weeks","Full Time Series"))
        
        if (input$scenario3 %in% c("Very Optimistic","Extremely Severe")){
            level <- 0.90
        } else if (input$scenario3 %in% c("Mildly Optimistic","Severe")) {
            level <- 0.50
        } else {
            level <- 0.95
        }
        
        pred.type <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            if(x == "Recovered(#)"){
                if(input$scenario3 %in% c("Very Optimistic","Mildly Optimistic")) {"upr"}
                else if (input$scenario3 %in% c("Severe","Extremely Severe")) {"lwr"}
                else {"fit"}
            }
            else {
                if(input$scenario3 %in% c("Very Optimistic","Mildly Optimistic")) {"lwr"}
                else if (input$scenario3 %in% c("Severe","Extremely Severe")) {"upr"}
                else {"fit"}
            }
        }, simplify = F)
        
        final_data <- list()
        
        final_data <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            a <- do.call('rbind',sapply(c(3,7,14,nrow(reg_data)),function(l){
                d <- data.frame(y = tail(reg_data[x],n = l), x = 1:l)
                colnames(d) <- c("y","x")
                d[d$y < 1,"y"] <- 1
                reg <- lm(log(y) ~ x, data = d)
                p <- as.data.frame(predict.lm(reg,data.frame(x = c(l,l + 3, l + 7, l + 14, l + 30)), level = level, interval = "prediction"))
                p <- t(exp(p[[pred.type[[x]]]]))
                data.frame(current = last(d$y), p)
            }, simplify = F))
        }, simplify = F)
        
        b <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]]
        final_data[["Deaths(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        b <- final_data[["Confirmed(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Recovered(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        
        final_data[["Active(#)"]] <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Active(#)"]][final_data[["Active(#)"]] < 0] <- 0
        final_data[["Active Rate(%)"]] <- final_data[["Active(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Overall Death Rate(%)"]] <- final_data[["Deaths(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Death Rate(%)"]] <- final_data[["Deaths(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        final_data[["Overall Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        
        
        final_data <- sapply(names(final_data), function(x){
            a <- cbind(base_data, final_data[[x]])
            colnames(a) <- c("Growth Rate Estimated From","Current Observed","Current Predicted","Forecast 3 Days","Forecast 1 Week",
                             "Forecast 2 Weeks","Forecast 1 Month")
            a
        }, simplify = F)
        
        d <- final_data[[input$analysis_type_australia]]
        if(is.null(d)) {return("Please choose other analysis type")}
        if (input$analysis_type_australia %in% c("Confirmed(#)","Deaths(#)","Recovered(#)","Active(#)")){
            d <- DT::datatable(d, rownames = FALSE,
                               options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatCurrency(c(2:7),
                               currency = "", interval = 3, mark = ",", digits = 0)
        } else {
            d <- DT::datatable(d, rownames = FALSE,
                               options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                formatRound(c(2:7), 2)
        }
        
        return(d)
        
    })
    
    output$fcst_plot_australia <- renderHighchart({
        x <- all[all$Country == "Australia",]
        reg_data <- x[c("Date","Confirmed(#)","Deaths(#)","Recovered(#)")]
        reg_data <- reg_data[order(reg_data$Date),]
        base_data <- data.frame(Period = c("Last 3 days","Last 1 week","Last 2 weeks","Full Time Series"))
        
        if (input$scenario3 %in% c("Very Optimistic","Extremely Severe")){
            level <- 0.90
        } else if (input$scenario3 %in% c("Mildly Optimistic","Severe")) {
            level <- 0.50
        } else {
            level <- 0.95
        }
        
        pred.type <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            if(x == "Recovered(#)"){
                if(input$scenario3 %in% c("Very Optimistic","Mildly Optimistic")) {"upr"}
                else if (input$scenario3 %in% c("Severe","Extremely Severe")) {"lwr"}
                else {"fit"}
            }
            else {
                if(input$scenario3 %in% c("Very Optimistic","Mildly Optimistic")) {"lwr"}
                else if (input$scenario3 %in% c("Severe","Extremely Severe")) {"upr"}
                else {"fit"}
            }
        }, simplify = F)
        
        final_data <- list()
        
        final_data <- sapply(c("Confirmed(#)","Deaths(#)","Recovered(#)"), function(x){
            a <- do.call('rbind',sapply(c(3,7,14,nrow(reg_data)),function(l){
                d <- data.frame(y = tail(reg_data[x],n = l), x = 1:l)
                colnames(d) <- c("y","x")
                d[d$y < 1,"y"] <- 1
                reg <- lm(log(y) ~ x, data = d)
                p <- as.data.frame(predict.lm(reg,data.frame(x = c(l,l + 3, l + 7, l + 14, l + 30)), level = level, interval = "prediction"))
                p <- t(exp(p[[pred.type[[x]]]]))
                data.frame(current = last(d$y), p)
            }, simplify = F))
        }, simplify = F)
        
        b <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]]
        final_data[["Deaths(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        b <- final_data[["Confirmed(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Recovered(#)"]][b < 0] <- final_data[["Confirmed(#)"]][b < 0]
        
        final_data[["Active(#)"]] <- final_data[["Confirmed(#)"]] - final_data[["Deaths(#)"]] - final_data[["Recovered(#)"]]
        final_data[["Active(#)"]][final_data[["Active(#)"]] < 0] <- 0
        final_data[["Active Rate(%)"]] <- final_data[["Active(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Overall Death Rate(%)"]] <- final_data[["Deaths(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Death Rate(%)"]] <- final_data[["Deaths(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        final_data[["Overall Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/final_data[["Confirmed(#)"]]*100
        final_data[["Finalised Recovery Rate(%)"]] <- final_data[["Recovered(#)"]]/(final_data[["Deaths(#)"]] + final_data[["Recovered(#)"]])*100
        
        
        final_data <- sapply(names(final_data), function(x){
            a <- cbind(base_data, final_data[[x]])
            colnames(a) <- c("Growth Rate Estimated From","Current Observed","Current Predicted","Forecast 3 Days","Forecast 1 Week",
                             "Forecast 2 Weeks","Forecast 1 Month")
            a
        }, simplify = F)
        d <- final_data[[input$analysis_type_australia]]
        if(is.null(d)) {return("Please choose other analysis type")}
        d <- reshape::melt(d,id.vars = "Growth Rate Estimated From")
        colnames(d)[1] <-"Growth_Period_Estimated_From"
        d$Growth_Period_Estimated_From <- paste("Growth -",d$Growth_Period_Estimated_From)
        type <- "linear"
        type <- ifelse(input$axis_type_fcst_australia == "Linear","linear","logarithmic")
        highcharter::hchart(d, type = 'column',
                            hcaes(y = value, group = Growth_Period_Estimated_From, x = variable))  %>%
            hc_yAxis(title = list(text = input$analysis_type_australia), type = type) %>% hc_xAxis(title = list(text = "")) %>%
            hc_tooltip(valueDecimals = 0, shared = TRUE)%>%
            hc_chart(zoomType = 'xy') %>% hc_exporting(enabled = TRUE)
        
    })
    
    data_table_australia <- reactive({
        x <- australia[australia$Date == max(australia$Date),-2]
        x <- x[order(x["Confirmed(#)"], decreasing = T),]
        colnames(x)[1] <- "Region/Territory"
        x
    })
    
    output$data_table_australia <- DT::renderDataTable({
        DT::datatable(data_table_australia(), rownames = FALSE, filter = "top",
                      options = list(pageLength = 1000, lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
            formatStyle('Overall Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Death Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Death Rate(%)'])), '#dd4b39'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Overall Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Overall Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Finalised Recovery Rate(%)',
                        background = styleColorBar(range(na.omit(all['Finalised Recovery Rate(%)'])), '#00a65a'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Active Rate(%)',
                        background = styleColorBar(range(na.omit(all['Active Rate(%)'])), '#3c8dbc'),
                        backgroundSize = '90% 85%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center') %>%
            formatStyle('Growth Factor',
                        color = styleInterval(1,c("#00a65a","#dd4b39"))) %>%
            formatStyle(c("Region/Territory","Growth Factor"), fontWeight = "bold") %>%
            formatRound(c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                          "Finalised Recovery Rate(%)","Active Rate(%)"), 2) %>%
            formatCurrency(c("Confirmed(#)","New Confirmed(#)","Deaths(#)","New Deaths(#)",
                             "Recovered(#)","New Recovered(#)","Active(#)" ),
                           currency = "", interval = 3, mark = ",", digits = 0)
        
        
    })
    
    output$plot_modal_australia <- renderHighchart({
        c <- data_table_australia()[input$data_table_australia_rows_selected,1]
        x <- australia[australia$Country.Region %in% c ,c("Country.Region","Date",input$analysis_type_australia_modal)]
        colnames(x) <- c("Country.Region", "Date", "Value")
        type <- "linear"
        if (input$analysis_type_australia_modal %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                       "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            type <- ifelse(input$axis_type_australia_modal == "Log10","logarithmic","linear")
        }
        p <- linechart(x,input$analysis_type_australia_modal, type) 
        if (input$analysis_type_australia_modal %in% c("Growth Factor","Overall Death Rate(%)","Finalised Death Rate(%)","Overall Recovery Rate(%)",
                                                       "Finalised Recovery Rate(%)","Active Rate(%)")) {p <- p %>% hc_tooltip(valueDecimals = 2, shared = TRUE)}
        p %>% hc_exporting(enabled = TRUE)
    })
    
    output$modal_axis_type_australia <- renderUI({
        
        if (input$analysis_type_australia_modal %in% c("Confirmed(#)","New Confirmed(#)","Deaths(#)",
                                                       "New Deaths(#)","Recovered(#)","New Recovered(#)","Active(#)")) {
            radioGroupButtons(
                inputId = "axis_type_australia_modal",
                label = NULL,
                choices = c("Linear", "Log10"),
                selected = "Linear"
            )
        }
        else { return(NULL) }
    })
    
    
    output$modal_australia <- renderUI({
        bsModal(id = "modal_compare3", title = strong("Comparison of Trends"), trigger = "compare3", size = "large",
                column(width = 12, offset = 0.5,selectInput("analysis_type_australia_modal", label = "Select", choices = names(australia[3:15]))),
                column(12, offset = 0.5,uiOutput("modal_axis_type_australia")),
                highchartOutput("plot_modal_australia")
        )
    })
    
    observeEvent(input$sir_bttn, {
        SIR <- function(time, state, parameters) {
            par <- as.list(c(state, parameters))
            with(par, {
                dS <- -beta/N * I * S
                dI <- beta/N * I * S - gamma * I
                dR <- gamma * I
                list(c(dS, dI, dR))
            })
        }
        
        ## Function to solve for residual sum of squares
        ## From Learning Machines Blog: minimize the sum of the squared differences between the number of infected I at time t and the corresponding number of predicted cases by our model
        
        RSS <- function(parameters, x, initialvalues, Day) {
            names(parameters) <- c("beta", "gamma")
            parameters <- logit.inv(parameters)
            out <- ode(y = initialvalues, 
                       times = Day, 
                       func = SIR, 
                       parms = parameters)
            fit <- out[ , -1]
            sum((x - fit)^2)
        }
        country <- input$c4
        x <- all[all$Country == country & all$`Confirmed(#)` >= 1 & 
                     all$Date >= input$daterange2[1] & all$Date <= input$daterange2[2], c("Date","Confirmed(#)","Active(#)","Deaths(#)","Recovered(#)")]
        
        N <- input$pop_size
        start_date <- x$Date[1]
        end_date <- x$Date[nrow(x)]
        x <- data.frame(S = N - x['Confirmed(#)'], I = x["Active(#)"], R = x["Deaths(#)"] + x["Recovered(#)"])
        colnames(x) <- c("S","I","R")
        
        Day <- 1:(nrow(x))
        
        initialvalues <- c(S = N-x$I[1], 
                           I = x$I[1], 
                           R = x$R[1])
        
        # Finding values for Beta and gamma with smallest RSS (best fit)
        par_ini <- c(beta = input$beta, gamma = input$gamma)
        Opt <- optim(logit(par_ini), RSS, method = "BFGS", x = x, initialvalues = initialvalues, Day = Day)
        
        
        # Set the SIR Beta and Gamma parameters
        opt.par <- setNames(logit.inv(Opt$par), c("beta", "gamma"))
        
        model.overtime <- 1:(length(Day) + input$nahead) # time in days
        
        fit <- data.frame(ode(y = initialvalues, times = model.overtime, func = SIR, parms = opt.par))
        
        fit$Date <- fit$time + (start_date - 1)
        
        fit <- fit[c("Date","S","I","R")]
        colnames(fit) <- c("Date", paste("Predicted -", c("Susceptible","Infected","Removed")))
        
        fit <- reshape2::melt(fit, id.vars = "Date")
        colnames(fit) <- c("Date","Country.Region","Value")
        
        x$Date <- (1:nrow(x)) + start_date - 1
        x <- x[c("Date","S","I","R")]
        colnames(x) <- c("Date",paste("Actual -", c("Susceptible","Infected","Removed")))
        x <- reshape2::melt(x, id.vars = "Date")
        colnames(x) <- c("Date","Country.Region","Value")
        fit[fit$Value < 0,"Value"] <- 0
        
        output$plot_sir <- renderHighchart({ 
            type <- ifelse(input$axis_type_sir == "Linear", "linear","logarithmic")
            highcharter::hchart(x, type = 'line', marker = list(enabled = FALSE),
                                hcaes(y = Value, group = Country.Region, x = Date), dashStyle = "shortdash")  %>%
                hc_add_series(data = fit, type = "line", hcaes(y = Value, group = Country.Region, x = Date), marker = list(enabled = FALSE)) %>%
                hc_yAxis(title = list(text = "Cases"), type = type) %>% 
                hc_xAxis(type = "datetime") %>% 
                hc_chart(zoomType = 'xy') %>%
                hc_rangeSelector(enabled = TRUE,
                                 buttons = list(
                                     list(type = 'all', text = 'All'),
                                     list(type = 'week', count = 1, text = '1w'),
                                     list(type = 'week', count = 2, text = '2w'),
                                     list(type = 'month', count = 1, text = '1m'),
                                     list(type = 'month', count = 2, text = '2m'),
                                     list(type = 'month', count = 3, text = '3m'),
                                     list(type = 'month', count = 6, text = '6m'),
                                     list(type = 'ytd', text = 'YTD'),
                                     list(type = 'year', count = 1, text = '1y')
                                 ),
                                 verticalAlign = "top",
                                 selected = 0
                ) %>% hc_exporting(enabled = TRUE) %>% 
                hc_tooltip(valueDecimals = 0, shared = TRUE) %>%
                hc_colors(colors = list("#f39c12","#dd4b39","#00a65a"))
        })
        
        
        
        output$sir_result <- renderUI({
            beta <- h5(strong("Estimated Infection Rate (Beta) = "), round(opt.par['beta']*100,2),"%")
            gamma <- h5(strong("Estimated Recovery Rate (Gamma) = "), round(opt.par['gamma']*100,2),"%")
            list(beta,gamma)
        })
    })
    
    observe({
        SIR <- function(time, state, parameters) {
            par <- as.list(c(state, parameters))
            with(par, {
                dS <- -beta/N * I * S
                dI <- beta/N * I * S - gamma * I
                dR <- gamma * I
                list(c(dS, dI, dR))
            })
        }
        
        ## Function to solve for residual sum of squares
        ## From Learning Machines Blog: minimize the sum of the squared differences between the number of infected I at time t and the corresponding number of predicted cases by our model
        
        RSS <- function(parameters, x, initialvalues, Day) {
            names(parameters) <- c("beta", "gamma")
            parameters <- logit.inv(parameters)
            out <- ode(y = initialvalues, 
                       times = Day, 
                       func = SIR, 
                       parms = parameters)
            fit <- out[ , -1]
            sum((x - fit)^2)
        }
        
        
        N <- 1
        
        Day <- input$nahead2
        
        initialvalues <- c(S = N - 0.0001, 
                           I = 0.0001, 
                           R = 0)
        
        
        # Set the SIR Beta and Gamma parameters
        opt.par <- setNames(as.numeric(c(input$beta2,input$gamma2)), c("beta", "gamma"))
        
        fit <- data.frame(ode(y = initialvalues, times = 1:Day, func = SIR, parms = opt.par))
        
        fit <- fit[c("time","S","I","R")]
        colnames(fit) <- c("Date","Susceptible","Infected","Removed")
        
        fit <- reshape2::melt(fit, id.vars = "Date")
        colnames(fit) <- c("Date","Country.Region","Value")
        fit[fit$Value < 0,"Value"] <- 0
        fit$Value <- fit$Value*100
        
        output$plot_toy_sir <- renderHighchart({ 
            type <- "linear"
            highcharter::hchart(fit, type = 'line', marker = list(enabled = FALSE),
                                hcaes(y = Value, group = Country.Region, x = Date))  %>%
                hc_yAxis(title = list(text = "% of Susceptible"), type = type, min = 0, max = 100) %>% 
                hc_xAxis(title = list(text = "Time Units")) %>% 
                hc_chart(zoomType = 'xy') %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_tooltip(valueDecimals = 2, shared = TRUE) %>%
                hc_colors(colors = list("#f39c12","#dd4b39","#00a65a"))
        })
        
    })
    
    output$category_select <- renderUI({
        x <- essentials()
        category <- sort(unique(x$category))
        selectizeInput(
            inputId = "category",
            label = "Select Categories",
            choices = category,
            options = list(
                placeholder = 'Please select atleast one category',
                onInitialize = I('function() { this.setValue(""); }')
            ),
            multiple = T
        )
    })
    
    
    output$state_select <- renderUI({
        x <- essentials()
        y <- x[x$category %in% input$category,]
        state <- sort(unique(y$state))
        selectizeInput(
            inputId = "state5",
            label = "Select States",
            choices = state,
            options = list(
                placeholder = 'Please select atleast one state',
                onInitialize = I('function() { this.setValue(""); }')
            ),
            multiple = T
        )
    })    
    
    
    
    output$city_select <- renderUI({ 
        x <- essentials()
        y <- x[x$category %in% input$category,]
        z <- y[y$state %in% input$state5,]
        city <- sort(unique(z$city))
        selectizeInput(
            inputId = "city",
            label = "Select Cities",
            choices = city,
            options = list(
                placeholder = 'Please select atleast one city',
                onInitialize = I('function() { this.setValue(""); }')
            ),
            multiple = T
        )
    })
    
    output$essential_table <- DT::renderDataTable({
        x <- essentials()
        y <- x[x$category %in% input$category,]
        z <- y[y$state %in% input$state5,]
        z <- z[z$city %in% input$city, ]
        z <- z[c("category","state","city","contact","phonenumber","descriptionandorserviceprovided","nameoftheorganisation")]
        DT::datatable(z, rownames = FALSE)
    })
    
    output$date_world_bubble_view <- renderUI({
        min <- min(cnf_dth_pop$Date, na.rm = T)
        max <- max(cnf_dth_pop$Date, na.rm = T)
        sliderInput("date_bubble","Select Date", min = min, max = max, value = max, animate = F, 
                    timeFormat = "%d %B %Y" )
    })
    
    output$bubble_plot <- renderHighchart({
        x <- cnf_dth_pop[cnf_dth_pop$Date == input$date_bubble,]
        date <- input$date_bubble
        type <- "linear"
        highchart() %>%
            hc_add_series_scatter(x = x$cnf_pop, y = x$dth_cnf, z = x$Population, label = x$iso3, color = x$Country.Region,
                                  dataLabels = list(enabled = TRUE)) %>%
            hc_legend(enabled = FALSE) %>%
            hc_xAxis(type = type, title = list(text = "Cases per million"), crosshair = TRUE) %>% 
            hc_yAxis(type = type, title = list(text = "Deaths per 100 cases"), crosshair = TRUE) %>% 
            hc_zAxis(title = list(text = "Ratio")) %>%
            hc_colors(colors = gg_color_hue(length(unique(cnf_dth_pop$Country.Region)))) %>%
            hc_tooltip(headerFormat = '',
                       pointFormat = '<span style="font-size: 10px"><b> {point.valuecolor}</b><br>
            Cases per million: {point.x:,.0f}<br> Deaths per 100 cases: {point.y:,.0f}<br> Population: {point.z:,.0f}') %>%
            hc_chart(zoomType = 'xy') %>%
            hc_exporting(enabled = TRUE) %>%
            hc_title(text = "Cases vs Deaths", align = 'left') %>%
            hc_subtitle(text = format(date, format = "%d %B %Y"), align = 'left')
    })
}


shinyApp(ui, server)





