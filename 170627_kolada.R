library(tidyverse)
library(rkolada)

# initiate connection to kolada

db <- rkolada::rkolada()

# df of relevant KPIs w/ metadata (using rkolada)

kpis <- db$kpi()[kpi.has_ou_data == TRUE & kpi.operating_area == "Särskilt boende"]

# vector of relevant kpi ids

sabo_kpis <- kpis$kpi.id

# table of KPI values for each KPI/ou/year (not using rkolada)

years <- 2013:2017
data <- data.frame(matrix(NA,nrow = length(ous)*length(sabo_kpis)*length(years),ncol = 4)) # inititiate empty df for loop
colnames(data) <- c("ou","year","kpi","value")
rowcount = 0

for(i in seq_along(years)){
  for(j in seq_along(sabo_kpis)){
    a <- content(GET(paste0("http://api.kolada.se/v2/oudata/kpi/",sabo_kpis[j],"/year/",years[i])))
    if(a$count > 0){
      for(k in 1:a$count){
        if(!is.null(a$values[[k]]$value[[1]]$value)){
          rowcount <- rowcount + 1
          data[rowcount,] <- cbind(a$values[[k]]$ou, a$values[[k]]$period, a$values[[k]]$kpi, a$values[[k]]$value[[1]]$value)
        }
      }
    }
  }
}

data <- data[!is.na(data$ou),] # remove empty rows

#get metadata for relevant ous

ous <- unique(data$ou)
ous <- ous[!is.na(ous)]

ouinfo <- data.frame(matrix(NA,nrow = length(ous),ncol = 3)) # initiate empty df for loop
colnames(ouinfo) <- c("ou","municipality","title")

for(i in seq_along(ous)){
  b <- content(GET(paste0("http://api.kolada.se/v2/ou/",ous[i])))
  ouinfo[i,] <- cbind(b$values[[1]]$id, b$values[[1]]$municipality, b$values[[1]]$title)
}

# reshape data

datawide <- spread(data, kpi, value)
kpinames <- left_join(data.frame(kpi = colnames(datawide), stringsAsFactors = F),data.frame(kpi = kpis$kpi.id, kpi.title = kpis$kpi.title, stringsAsFactors = F), by = "kpi")
kpinames$kpi.title[1:2] <- c("ou","year")
colnames(datawide) <- kpinames$kpi.title
