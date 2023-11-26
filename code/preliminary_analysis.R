#' preliminary analysis
#' fix temperature


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

### create new var. ----
dt[, max_temp_C:=max_temp-273.15]
dt[, min_humid_fixed := ifelse(min_humid>100, 100, min_humid)]
dt[, heat_index := heat.index(t = max_temp_C, rh = min_humid_fixed, 
                              temperature.metric = "celsius", output.metric = "celsius")]
anyNA(dt)
dt[is.na(heat_index),]

summary(dt[,.(max_temp_C, min_humid_fixed, heat_index)])

## model ----
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  cat(toupper(var),"\n")
  mod_ns <- clogit(get(var) ~ ns(max_temp, df=4) + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black")
  mod <- clogit(get(var) ~ max_temp + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp']]; se <- sqrt(vcov(mod)[1,1])
  exp(est*5); exp((est-qnorm(.975)*se)*5); exp((est+qnorm(.975)*se)*5)
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
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  exp(est*5); exp((est-qnorm(.975)*se)*5); exp((est+qnorm(.975)*se)*5)
}
