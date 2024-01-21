## EMM models

rm(list=ls())
gc()
require(data.table); require(fst)
require(survival); require(weathermetrics)
setDTthreads(threads = 0)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"

modifier <- read_fst(file.path(wkdir, "data", "modifier.fst"), as.data.table = T)
modifier[,`:=`(zip = NULL,
               year = NULL)]
dt <- read_fst(file.path(wkdir, "data", "data_to_analyze_fixedtemp.fst"), as.data.table = T)
dt <- merge(dt, modifier, by = "bene_id", all.x = T)
gc()

### create new var. ----
dt[, max_temp_C:=max_temp-273.15]
dt[, min_humid_fixed := ifelse(min_humid>100, 100, min_humid)]
dt[, heat_index := heat.index(t = max_temp_C, rh = min_humid_fixed, 
                              temperature.metric = "celsius", output.metric = "celsius")]
anyNA(dt)
dt[is.na(heat_index),]
summary(dt[,.(max_temp_C, min_humid_fixed, heat_index)])

median_poverty <- median(dt[, poverty], na.rm = T)
dt[, poverty_above_median := poverty > median_poverty]

varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
recList <- paste0("rec_", 1:9)
for (var in varList) {
  cat(toupper(var),"\n")
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  for (rec_i in recList) {
    mod <- clogit(get(var) ~ heat_index + strata(bene_id), data=dt[get(paste0(var,"DT")),][get(rec_i)])
    print(summary(mod))
    cat(rec_i," print OR for 5 unit increase in temperature and its 95CI \n")
    est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
    print(exp(est*5))
    print(exp((est-qnorm(.975)*se)*5))
    print(exp((est+qnorm(.975)*se)*5))
  }
  cat("ABOVE MEDIAN POVERTY \n")
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data=dt[get(paste0(var,"DT")),][get("poverty_above_median"),])
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  
  cat("BELOW MEDIAN POVERTY \n")
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data=dt[get(paste0(var,"DT")),][!get("poverty_above_median"),])
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
}
