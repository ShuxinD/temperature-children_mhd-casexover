#' preliminary analysis
#' fix temperature
#' "data_to_analyze_fixedtemp.fst" is the final dataset with fixed temperature

rm(list=ls()); gc()
require(data.table); setDTthreads(threads = 0)
require(survival); require(fst)
require(splines); require(weathermetrics)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt <- read_fst(file.path(wkdir,"data","data_to_analyze_fixedtemp.fst"), as.data.table = T)

### create exposure variables ----
#'  temperature Kelvin to Celcius: -273.15 
#'  heat index: `weathermetrics::heat.index`
#'  set min. humidity above 100 (slightly) to 100
dt[, max_temp_C:=max_temp-273.15]
dt[, min_humid_fixed := ifelse(min_humid>100, 100, min_humid)]
dt[, heat_index := heat.index(t = max_temp_C, rh = min_humid_fixed, 
                              temperature.metric = "celsius", output.metric = "celsius")]
anyNA(dt)
dt[is.na(heat_index),]
summary(dt[,.(max_temp_C, min_humid_fixed, heat_index)])

### merge individual info ----
info <- read_fst(file.path(wkdir, "data", "subject_individual_info.fst"))
dt <- merge(dt, info, by = "bene_id")

### model all ages all seasons ----
mainlabel <- "0-18 years old, all seasons" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 12+ years old, all seasons----
mainlabel <- "12+ years old, all seasons" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12, ] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12, ]
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12, ]
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 0-11 years old, all seasons----
mainlabel <- "0-11 years old, all seasons" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11, ] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11, ] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11, ] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model all ages, only summer----
mainlabel <- "0-18 years old, only summer" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 12+ years old, only summer----
mainlabel <- "12+ years old, only summer" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 0-11 years old, only summer----
mainlabel <- "0-11 years old, only summer" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month>=6)&(admission_month<=8),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model all ages, only winter----
mainlabel <- "0-18 years old, only winter" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month==12)&(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 12+ years old, only winter----
mainlabel <- "12+ years old, only winter" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age>=12,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

### model 0-11 years old, only winter----
mainlabel <- "0-11 years old, only winter" # change if needed
varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines
  mod_ns <- clogit(get(var) ~ ns(max_temp_C, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "max temp.", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on max. temp.
  mod <- clogit(get(var) ~ max_temp_C + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in temperature and its 95CI \n")
  est <- coef(mod)[['max_temp_C']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on min. humidity
  mod_ns <- clogit(get(var) ~ ns(min_humid_fixed, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "min humidity", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on min. humidity
  mod <- clogit(get(var) ~ min_humid_fixed + strata(bene_id), 
                data = data_analysis)
  print(summary(mod))
  rm(data_analysis); gc()
}

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),][age<=11,][(admission_month==12)|(admission_month<=2),] # change if needed
  cat(toupper(var),"\n")
  ## natural splines on heat index
  mod_ns <- clogit(get(var) ~ ns(heat_index, df=4) + strata(bene_id), 
                   data = data_analysis)
  termplot(mod_ns, xlab = "heat index", ylabs = var, 
           term = 1, se = T, col.se = "black", col.term = "black",
           main = mainlabel)
  ## linear regression on heat index
  mod <- clogit(get(var) ~ heat_index + strata(bene_id), data = data_analysis)
  print(summary(mod))
  cat("print OR for 5 unit increase in heat index and its 95CI \n")
  est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
  print(exp(est*5))
  print(exp((est-qnorm(.975)*se)*5))
  print(exp((est+qnorm(.975)*se)*5))
  rm(data_analysis); gc()
}