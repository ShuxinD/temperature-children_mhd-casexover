rm(list=ls()); gc()
require(data.table); setDTthreads(threads = 0)
require(fst)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
climate_dir <- "/n/dominici_nsaph_l3/Lab/exposure/raster2polygon_climate_types/climate_types_zip_zcta520.csv"

## load data ----
dt <- read_fst(file.path(wkdir,"data","data_to_analyze.fst"), as.data.table = T)
climate <- fread(climate_dir, select = 1:4)

## zipcode validation ----
official_zip <- fread(file.path(wkdir, "data", "zip_code_database.csv"))
climate[, validZIP := zip %in% official_zip[,zip]] # ZIP codes in the official database is valid
climate[, zcta:=NULL]
summary(climate[,validZIP])
summary(dt[,zip] %in% climate[,zip]) ## any ZIP codes not in climate?
## why this is happening?
## let's create a temp data

## temp merge ----
merged <- merge(dt, climate, by = "zip")
(dim(merged)[1]-dim(dt)[1])/4 ## drop # of cases
write_fst(merged, file.path(wkdir, "data", "temp", "analysis_climate.fst"))
