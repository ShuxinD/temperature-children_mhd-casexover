## fix the glitch in temperature data

rm(list=ls()); gc()
require(data.table); setDTthreads(threads = 0)
require(survival); require(fst)
require(splines); require(weathermetrics)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt <- read_fst(file.path(wkdir,"data","data_to_analyze.fst"), as.data.table = T)

### create exposure variables ----
#'  temperature Kelvin to Celcius: -273.15 
#'  heat index: `weathermetrics::heat.index`
#'  set min. humidity above 100 (slightly) to 100
summary(dt[,.(max_temp, min_humid)])
hist(dt[,max_temp])

### extreme temp ----
probZIP <- c(33070, 33036, 4631, 33033, 4652, 32625)
dt[zip %in% probZIP,.(date, zip, max_temp)][, max_temp_C:=max_temp-273.15][]
hist(dt[!(zip %in% probZIP), max_temp])
summary(dt[!(zip %in% probZIP), max_temp])
dt[!(zip %in% probZIP) & (max_temp<250),.(date, zip, max_temp)] # temperature fixed

## find how many cases were impacted
length(unique(dt[(depressionDT), bene_id])) # match data processing
length(unique(dt[(depressionDT) & !(zip %in% probZIP), bene_id]))

length(unique(dt[(anxietyDT), bene_id])) # match data processing
length(unique(dt[(anxietyDT) & !(zip %in% probZIP), bene_id]))

length(unique(dt[(emotion_disturbDT), bene_id])) # match data processing
length(unique(dt[(emotion_disturbDT) & !(zip %in% probZIP), bene_id]))

length(unique(dt[(adole_reactionDT), bene_id])) # match data processing
length(unique(dt[(adole_reactionDT) & !(zip %in% probZIP), bene_id]))

length(unique(dt[(disturb_conductDT), bene_id])) # match data processing
length(unique(dt[(disturb_conductDT) & !(zip %in% probZIP), bene_id]))

### save updated data ----
dt <- dt[!(zip %in% probZIP), ]
write_fst(dt, path = file.path(wkdir, "data", "data_to_analyze_fixedtemp.fst"))