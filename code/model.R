## analysis ##
rm(list=ls())
gc()
require(data.table)
setDTthreads(threads = 0)
require(survival)
require(splines)

## load
dt <- fread(file.path("data","data_to_analyze.csv"))

## model ----
### depression 
# mod <- clogit(depression ~ ns(max_temp, df=2) + strata(bene_id), data=dt[(depressionDT),])
# summary(mod) 
# termplot(mod, xlab = "max temp.", 
#          term = 1, se = T, col.se = "black", col.term = "black")
# mod <- clogit(depression ~ max_temp + strata(bene_id), data=dt[(depressionDT),])
# summary(mod) 

varList <- c("depression", "anxiety", "emotion_disturb", "adole_reaction", "disturb_conduct")
for (var in varList) {
  cat(var,"\n")
  mod_ns <- clogit(get(var) ~ ns(max_temp, df=2) + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  summary(mod_ns)
  termplot(mod_ns, xlab = "max temp.",
           term = 1, se = T, col.se = "black", col.term = "black")
  mod <- clogit(get(var) ~ max_temp + strata(bene_id), data=dt[get(paste0(var,"DT")),])
  print(summary(mod))
}
