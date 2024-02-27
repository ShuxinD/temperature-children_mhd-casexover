rm(list=ls()); gc()
require(data.table); setDTthreads(threads = 0)
require(survival); require(fst)
require(splines); require(weathermetrics)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt <- read_fst(file.path(wkdir,"data", "temp","analysis_climate.fst"), as.data.table = T)

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

## only look at heat index
## focus on 12+
## first three health outcomes
## month: May to September
## by climate region

## adjust climate type variable and subset----
info <- read_fst(file.path(wkdir, "data", "subject_individual_info.fst"))
dt <- merge(dt, info, by = "bene_id")
dt <- dt[age>=12,][(admission_month>=5)&(admission_month<=9),]

count <- table(dt[,climate_type_short])
## drop those with climate type under 100
type_drop <- names(count[count<=100])
dt <- dt[(!climate_type_short %in% type_drop),]
dt[,climate_type_short := as.factor(climate_type_short)]
length(droplevels(dt[,climate_type_short], exclude = "ET"))

# model_list <- list() # List to store regression models
varList <- c("depression", "anxiety", "emotion_disturb")
mainlabel <- "12+, May-Sep"

for (var in varList) {
  data_analysis <- dt[get(paste0(var,"DT")),] # change if needed
  cat(toupper(var),"\n")
  for (level in levels(data_analysis[,climate_type_short])) {
    cat(level, "\n")
    subset_data <- subset(data_analysis, climate_type_short == level)
    ## natural splines on heat index
    mod_ns <- clogit(get(var) ~ ns(heat_index, df=3) + strata(bene_id), 
                     data = subset_data)
    termplot(mod_ns, xlab = "heat index", ylabs = var, 
             term = 1, se = T, col.se = "black", col.term = "black",
             main = paste(mainlabel, level))
    ## linear regression on heat index
    mod <- clogit(get(var) ~ heat_index + strata(bene_id), 
                  data = subset_data)
    print(summary(mod))
    cat("print OR for 5 unit increase in heat index and its 95CI \n")
    est <- coef(mod)[['heat_index']]; se <- sqrt(vcov(mod)[1,1])
    print(exp(est*5))
    print(exp((est-qnorm(.975)*se)*5))
    print(exp((est+qnorm(.975)*se)*5))
    cat(toupper("finish "), toupper(var), level, "\n")
  }
  rm(data_analysis); gc()
}
