## effect modifier: individual race, census poverty

rm(list=ls())
gc()
require(data.table); require(fst)
setDTthreads(threads = 0)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"

dt <- read_fst(file.path(wkdir, "data", "data_to_analyze_fixedtemp.fst"), as.data.table = T)
colnames(dt)
length(dt[,bene_id]); length(unique(dt[,bene_id]))
ID <- unique(dt[,bene_id])

dt_original <- read_fst(file.path(wkdir, "data", "first_hospitalization.fst"), as.data.table = T)
pop_info <- dt_original[bene_id %in% ID, .(bene_id, year, zip, rec_0, rec_1, rec_2, rec_3, rec_4, rec_5, rec_6, rec_7, rec_8, rec_9)]

censusdir <- "/n/dominici_nsaph_l3/projects/analytic/confounders"
f <- list.files(censusdir, pattern = "\\.csv", full.names = TRUE)
example <- fread(f[1], )
names(example)
myvars <- c("ZIP", "year", "poverty")
rm(example)
gc()

census_info <- rbindlist(lapply(f[1:13],
                               fread,
                               select = myvars))

modifier <- merge(pop_info, census_info, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = T)

write_fst(modifier, file.path(wkdir, "data", "modifier.fst"))