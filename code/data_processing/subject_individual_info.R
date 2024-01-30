#' prepare an additional dataset with individual age, admission date time info
#' merge this prepared dataset with the analyzeable dataset

rm(list=ls()); gc()
require(data.table); require(fst)
setDTthreads(threads = 0)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"

### load ID from the analyzeable dataset
dt <- read_fst(file.path(wkdir, "data", "data_to_analyze_fixedtemp.fst"), as.data.table = T)
colnames(dt)
length(dt[,bene_id]); length(unique(dt[,bene_id]))
ID <- unique(dt[,bene_id])

## get individual info from Medicaid
dt_original <- read_fst(file.path(wkdir, "data", "first_hospitalization_case.fst"), as.data.table = T)

info <- dt_original[bene_id %in% ID, ][,.(bene_id, admission_date, sex, age)]
info[, admission_month := month(admission_date)]
info[, admission_date := NULL]

write_fst(info, file.path(wkdir, "data", "subject_individual_info.fst"))
