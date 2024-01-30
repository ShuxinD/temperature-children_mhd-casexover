#' final analyzable dataset

rm(list=ls())
gc()
require(data.table); require(fst)
setDTthreads(threads = 0)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"

## generate patient ID with different diseases----
dt_original_health <- read_fst(file.path(wkdir, "data", "first_hospitalization_case.fst"), as.data.table = T)
health <- dt_original_health[,.(bene_id, admission_date, zip,
                                depression, anxiety, emotion_disturb, adole_reaction, disturb_conduct)] ## first hospitalization info with disease info
id_disease <- list(depression = health[depression==1,bene_id],
                   anxiety = health[anxiety==1,bene_id],
                   emotion_disturb = health[emotion_disturb==1,bene_id],
                   adole_reaction = health[adole_reaction==1,bene_id],
                   disturb_conduct = health[disturb_conduct==1,bene_id]) ## patient ID of first admission with such diseases

## load exposure ----
case <- read_fst(file.path(wkdir, "data","case_exposure.fst"), as.data.table = T)
control <- read_fst(file.path(wkdir, "data", "control_exposure.fst"), as.data.table = T)
colnames(case)[2] <- "date"; colnames(control)[2] <- "date"
dt <- rbind(case,control)

## merge case and control ----
dt <- merge(dt, health, by.x = c("bene_id","date","zip"), by.y = c("bene_id", "admission_date","zip"),
      all.x = T)
dt[,c(6:10)][is.na(dt[,c(6:10)]),] <- 0 # convert NA to 0 for column 6-10

## tag for analyzing ----
dt[,`:=`(depressionDT = (bene_id %in% id_disease[["depression"]]),
         anxietyDT = (bene_id %in% id_disease[["anxiety"]]),
         emotion_disturbDT = (bene_id %in% id_disease[["emotion_disturb"]]),
         adole_reactionDT = (bene_id %in% id_disease[["adole_reaction"]]),
         disturb_conductDT = (bene_id %in% id_disease[["disturb_conduct"]]))]

anyNA(dt)
anyNA(dt[,1]); anyNA(dt[,2]); anyNA(dt[,3]); anyNA(dt[,4]); anyNA(dt[,5])

# dt[is.na(max_temp),]; dt[is.na(min_humid),]

length(dt[is.na(max_temp), bene_id]); length(dt[is.na(min_humid), bene_id])
length(unique(dt[is.na(max_temp), bene_id])); length(unique(dt[is.na(min_humid), bene_id]))
invalidEXP_id <- unique(c(unique(dt[is.na(max_temp), bene_id]), unique(dt[is.na(min_humid), bene_id])))


cat("current subjects in the study \n")
length(unique(dt[(depressionDT), bene_id]))
length(unique(dt[(anxietyDT), bene_id]))
length(unique(dt[(emotion_disturbDT), bene_id]))
length(unique(dt[(adole_reactionDT), bene_id]))
length(unique(dt[(disturb_conductDT), bene_id]))

cat("after removing subjects with invalid exposure inputs \n")
length(unique(dt[(depressionDT) & (!(bene_id %in% invalidEXP_id)), bene_id]))
length(unique(dt[(anxietyDT) & (!(bene_id %in% invalidEXP_id)), bene_id]))
length(unique(dt[(emotion_disturbDT) & (!(bene_id %in% invalidEXP_id)), bene_id]))
length(unique(dt[(adole_reactionDT) & (!(bene_id %in% invalidEXP_id)), bene_id]))
length(unique(dt[(disturb_conductDT) & (!(bene_id %in% invalidEXP_id)), bene_id]))

dt_output <- dt[!(bene_id %in% invalidEXP_id), ]
anyNA(dt_output)
write_fst(dt_output, file.path(wkdir, "data","data_to_analyze.fst"))