
# reticulate::install_miniconda(path = "D:/Miniconda")
# reticulate::conda_install("numpy")

library(tidyverse)
library(reticulate)
library(jsonlite)
library(pbapply)
library(tigris)
library(sf)

source("datareader.R")
# get county polygons

if(!file.exists("Supp_Data/counties.gpkg")){
  counties <- tigris::counties(state=c("IL","IA","IN","MN","NE"),resolution = "20m",cb=T) |>
    select(GEOID,STATEFP,NAMELSAD)
  st_write(counties,"Supp_Data/counties.gpkg")
}


np <- import("numpy", convert = FALSE)
pred_files <- list.files("Data/corn/",pattern="_",full.names=T)
labels_file <- list.files("Data/corn/",pattern="labels.json$",full.names=T)
dates_file <- list.files("Data/",pattern="dates.csv",full.names=T)


# ancillary PAR data: solar irradiation for end of flwering season
PAR <- 
  list.files("Supp_Data/","PAR",full.names = T) |> 
  map(read_csv,show_col_types = FALSE) |> 
  bind_rows() |> 
  mutate(year=as.character(year),
         id=as.character(id))

yields <- 
  labels_file |> 
  jsonlite::read_json() |> 
  bind_rows() |>
  pivot_longer(cols=everything(),values_to="yield") |> 
  separate(col = name,into = c("id","year"),"_")

dates <- 
  dates_file|> 
  read_csv2(col_types = cols(.default = "c")) |> 
  head(-3) |> 
  pivot_longer(cols=everything(),names_to = "year",values_to = "date") |> 
  mutate(date=lubridate::ymd(date)) |> 
  group_by(year) |> 
  arrange(date) |> 
  mutate(acq_within_year=row_number()) |> 
  ungroup()


read <- 
  function(filename){
  basename <- basename(filename) |> tools::file_path_sans_ext()
  x <- np$load(filename) |> 
    py_to_r() |> 
    as.tibble() |> 
    rename(srred=V1,
           srnir=V2,
           srblue=V3,
           srgreen=V4,
           srnir2=V5,
           srswir1=V6,
           srswir2=V7,
           tmin=V8,
           tmax=V9,
           prcp=V10,
           heatcold=V11,
           drought=V12,
           NDVI=V13,
           EVI=V14,
           NDWI=V15) %>%
    #rescale
    mutate(across(starts_with("sr"), function(x){x/10000}))
  
  x$year <- str_sub(basename,1,4)
  x$id <- str_sub(basename,6,10)
  return(x)
}

data <- pred_files[] |> pblapply(read)
data <- data |> bind_rows()
data <- data |> group_by(year,id) |> mutate(acq_within_year=row_number()) |> ungroup()
data <- data |> left_join(dates,by=c("year","acq_within_year"))
data <- data |> left_join(yields,by=c("id","year"))
data <- data |> left_join(PAR,by=c("id","year"))

data |> saveRDS("Data/processed_data.rds")

