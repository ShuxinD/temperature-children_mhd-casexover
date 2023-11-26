#' prepare the dataset from Medicaid
#' only include first hospitalization

rm(list=ls())
gc()
if(!require(data.table)) install.packages("data.table")
require(data.table); require(fst)
setDTthreads(threads = 0)

## load the extracted Medicare dataset ----
dt_dir <- "/n/dominici_nsaph_l3/Lab/projects/medicaid_children_icd/data/individual_records"
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt_o <- fread(file.path(dt_dir,"disease_classification_demographics.csv"))
dim(dt_o)

## restrict the dataset to five outcome cases ----
dt <- dt_o[(depression==1)|(anxiety==1)|(emotion_disturb==1)|(adole_reaction==1)|(disturb_conduct==1),]
dim(dt)
anyDuplicated(dt) # check duplicated rows
dt <- unique(dt)
anyDuplicated(dt);anyDuplicated(dt[,c("admission_date","bene_id")])

## check all hospitalization data ----
colnames(dt)
### diag1 -- diag9, diagnosis: good

### year: the year of admission_date (admission year); admission_date: good

### bene_id: good

### n_th_admission: PROBLEMATIC, but will not affect the final dataset ----
summary(dt[, n_th_admission]) # the hospitalization admitted with outcome of interest

### depression, anxiety, emotion_disturb, adole_reaction, disturb_conduct: processed variable, decide to trust the data processing

### zip: need validation ----
official_zip <- fread(file.path(wkdir, "data", "zip_code_database.csv"))
dt[, validZIP := zip %in% official_zip[,zip]] # ZIP codes in the official database is valid
summary(dt[,validZIP])
cat("invalid ZIP proportion", 1-mean(dt[,validZIP]))
invalidZIP_id <- dt[!(validZIP), bene_id]
saveRDS(invalidZIP_id, file.path(wkdir, "data", "missing", "missing_ZIP.rds"))

### race_ethnicity_code: need processing ----
## check the zero portion
dt[, rec_0:=race_ethnicity_code %like% "0"]
cat("race/ethnicity == 0 proportion (should not be zero in coding)", sum(dt[,rec_0])/dim(dt)[1])
cat("code 0 proportion vary by year \n"); setorder(dt[,.(count_rec0=sum(rec_0),
                                                         percent_rec=sum(rec_0)/.N), by = year],year)[]

## drop the row with 0, convert code to binary variables
dt[, `:=`(rec_0 = race_ethnicity_code %like% "0",
          rec_1 = race_ethnicity_code %like% "1",
          rec_2 = race_ethnicity_code %like% "2",
          rec_3 = race_ethnicity_code %like% "3",
          rec_4 = race_ethnicity_code %like% "4",
          rec_5 = race_ethnicity_code %like% "5",
          rec_6 = race_ethnicity_code %like% "6",
          rec_7 = race_ethnicity_code %like% "7",
          rec_8 = race_ethnicity_code %like% "8",
          rec_9 = race_ethnicity_code %like% "9")]
## code: https://resdac.org/cms-data/variables/raceethnicity-msis
## need to drop rec==0 in the end
invalidREC_id <- dt[(rec_0), bene_id]
saveRDS(invalidREC_id, file.path(wkdir, "data", "missing", "missing_rec.rds"))

### sex: need processing ----
dt[,sex:=as.factor(sex)]
cat("count by levels\n"); summary(dt[,sex])
cat("prop. by levels\n"); summary(dt[,sex])/dim(dt)[1]*100
# check_sex <- dt[,.(sex_unique=uniqueN(sex)), by= bene_id]
# check_sex[sex_unique!=1,]
# > check_sex[sex_unique!=1,]
# Empty data.table (0 rows and 2 cols): bene_id,sex_unique
## no sex inconsistency
## need to drop sex input >1
invalidSEX_id <- dt[!((sex=="F")|(sex=="M")), bene_id]
saveRDS(invalidSEX_id, file.path(wkdir, "data", "missing", "missing_sex.rds"))

### age: ok ----
hist(dt[,age])
# pdf(file = file.path("results","data_processing","original_age_distribution_all_hospitalization.pdf"), paper = "USr")
# hist(dt[,age])
# dev.off()

## subset the first hospitalization ----
setorder(dt, bene_id, admission_date)
dt_1hosp <- dt[, .SD[1], by = bene_id] ## select first hospitalization
mean(dt_1hosp[, validZIP]) # proportion with valid ZIP codes
# dt_1hosp <- dt_1hosp[(validZIP),]
anyDuplicated(dt_1hosp[,c("admission_date","bene_id", "zip")])
hist(dt_1hosp[,age])
write_fst(dt_1hosp, file.path(wkdir, "data", "first_hospitalization_original.fst"))
write_fst(dt_1hosp[,.(bene_id, admission_date, zip)], file.path(wkdir, "data","first_hospitalization_only_date_zip_original.fst"))

## removing missingness ----
cat("original Mediciad cases \n")
dim(dt_1hosp[(depression),])[1]
dim(dt_1hosp[(anxiety),])[1]
dim(dt_1hosp[(emotion_disturb),])[1]
dim(dt_1hosp[(adole_reaction),])[1]
dim(dt_1hosp[(disturb_conduct),])[1]

cat("after removing subjects with invalid sex inputs \n")
dim(dt_1hosp[(depression) & (!(bene_id %in% invalidSEX_id)), ])[1]
dim(dt_1hosp[(anxiety) & (!(bene_id %in% invalidSEX_id)), ])[1]
dim(dt_1hosp[(emotion_disturb) & (!(bene_id %in% invalidSEX_id)), ])[1]
dim(dt_1hosp[(adole_reaction) & (!(bene_id %in% invalidSEX_id)), ])[1]
dim(dt_1hosp[(disturb_conduct) & (!(bene_id %in% invalidSEX_id)), ])[1]

cat("after removing subjects with invalid REC info \n")
dim(dt_1hosp[(depression) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id), ])[1]
dim(dt_1hosp[(anxiety) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id), ])[1]
dim(dt_1hosp[(emotion_disturb) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id), ])[1]
dim(dt_1hosp[(adole_reaction) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id), ])[1]
dim(dt_1hosp[(disturb_conduct) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id), ])[1]

cat("after removing subjects with invalid ZIP info \n")
dim(dt_1hosp[(depression) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id), ])[1]
dim(dt_1hosp[(anxiety) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id), ])[1]
dim(dt_1hosp[(emotion_disturb) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id), ])[1]
dim(dt_1hosp[(adole_reaction) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id), ])[1]
dim(dt_1hosp[(disturb_conduct) & !(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id), ])[1]

dt_output <- dt_1hosp[(!(bene_id %in% invalidSEX_id) & !(bene_id %in% invalidREC_id) & !(bene_id %in% invalidZIP_id)), ]

write_fst(dt_output, file.path(wkdir, "data", "first_hospitalization.fst"))
write_fst(dt_output[,.(bene_id, admission_date, zip)], file.path(wkdir, "data","first_hospitalization_only_date_zip.fst"))

# ## subject selection ----
# ## need to drop rec==0 in the end
# dt_variable <- dt[!(rec_0)]
# ## need to drop sex input >1
# dt_variable <- dt_variable[(sex=="F")|(sex=="M"),]
# # > dim(dt_variable)
# # [1] 1505822      33
# ## need to drop invalid zipcode input