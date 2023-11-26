#' preliminary analysis
#' 


rm(list=ls()); gc()
require(data.table); setDTthreads(threads = 0)
require(survival); require(fst)
require(splines); require(weathermetrics)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt <- read_fst(file.path(wkdir,"data","data_to_analyze.fst"), as.data.table = T)

## create exposure variables ----
#'  temperature Kelvin to Celcius: -273.15 
#'  heat index: `weathermetrics::heat.index`
#'  set min. humidity above 100 (slightly) to 100
summary(dt[,.(max_temp, min_humid)])
hist(dt[,max_temp])
dt[zip==33070,]; dt[zip==33036,]; dt[zip==4631,]; dt[zip==33033,]; dt[zip==4652, ]



hist(dt[!(zip==33070|zip==33036|zip==4631|zip==33033|zip==4652|zip==32625), max_temp])
summary(dt[!(zip==33070|zip==33036|zip==4631|zip==33033|zip==4652|zip==32625), max_temp])
dt[!(zip==33070|zip==33036|zip==4631|zip==33033|zip==4652|zip==32625)&(max_temp<250),]

summary(dt[zip==33033,max_temp])
dt[, max_temp_C:=max_temp-273.15]
dt[, min_humid_fixed := ifelse(min_humid>100, 100, min_humid)]
dt[, heat_index := heat.index(t = max_temp_C, rh = min_humid_fixed, 
                              temperature.metric = "celsius", output.metric = "celsius")]
anyNA(dt)
dt[is.na(heat_index),]

summary(dt[,.(max_temp_C, min_humid_fixed, heat_index)])

varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  cat(toupper(var),"\n")
  mod_ns <- clogit(get(var) ~ ns(max_temp, df=4) + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black")
  mod <- clogit(get(var) ~ max_temp + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
}

for (var in varList) {
  cat(toupper(var),"\n")
  mod_ns <- clogit(get(var) ~ ns(min_humid, df=4) + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black")
  mod <- clogit(get(var) ~ min_humid + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
}

for (var in varList) {
  cat(toupper(var),"\n")
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black")
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
}
