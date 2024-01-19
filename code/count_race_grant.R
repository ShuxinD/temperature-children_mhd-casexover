## summarize population number for grant application

rm(list=ls())
gc()
require(data.table); require(fst)
setDTthreads(threads = 0)
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/temperature-children_depression-casexover"
dt_dir <- "/n/dominici_nsaph_l3/Lab/projects/medicaid_children_icd/data/individual_records"

dt <- read_fst(file.path(wkdir, "data", "data_to_analyze_fixedtemp.fst"), as.data.table = T)
colnames(dt)
length(dt[,bene_id]); length(unique(dt[,bene_id]))
ID <- unique(dt[,bene_id])

dt_original <- read_fst(file.path(wkdir, "data", "first_hospitalization.fst"), as.data.table = T)

pop <- dt_original[bene_id %in% ID, ]
## 5: HISPANIC (CHANGED TO "HISPANIC OR LATINO - NO RACE INFORMATION AVAILABLE" BEGINNING 10/98)
## 7: HISPANIC OR LATINO AND ONE OR MORE RACES (NEW CODE BEGINNING 10/98)
dim(pop[(rec_5)|(rec_7),])[1]; dim(pop[!((rec_5)|(rec_7)),])[1]
dim(pop[(rec_5)|(rec_7),])[1] + dim(pop[!((rec_5)|(rec_7)),])[1] == dim(pop)[1]
table(pop[(rec_5)|(rec_7),][,sex])

## 3: AMERICAN INDIAN OR ALASKAN NATIVE
table(pop[(rec_5)|(rec_7),][(rec_3),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_3),][,sex]) # non-hispanic
dim(pop[(rec_3),])[1]

## 4	ASIAN OR PACIFIC ISLANDER (CHANGED TO "ASIAN" BEGINNING 10/98)
table(pop[(rec_5)|(rec_7),][(rec_4),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_4),][,sex]) # non-hispanic
dim(pop[(rec_4),])[1]

## 6	NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER (NEW CODE BEGINNING 10/98)
table(pop[(rec_5)|(rec_7),][(rec_6),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_6),][,sex]) # non-hispanic
dim(pop[(rec_6),])[1]

## 2	BLACK, NOT OF HISPANIC ORIGIN (CHANGED TO "BLACK OR AFRICAN AMERICAN" BEGINNING 10/98)
table(pop[(rec_5)|(rec_7),][(rec_2),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_2),][,sex]) # non-hispanic
dim(pop[(rec_2),])[1]

## 1	WHITE, NOT OF HISPANIC ORIGIN (CHANGED TO "WHITE" BEGINNING 10/98)
table(pop[(rec_5)|(rec_7),][(rec_1),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_1),][,sex]) # non-hispanic
dim(pop[(rec_1),])[1]

## 8	MORE THAN ONE RACE (HISPANIC OR LATINO NOT INDICATED) (NEW CODE BEGINNING 10/98)
table(pop[(rec_5)|(rec_7),][(rec_8),][,sex]) # hispanic
table(pop[!((rec_5)|(rec_7)),][(rec_8),][,sex]) # non-hispanic
dim(pop[(rec_8),])[1]

## additional check
dim(pop[!((rec_1)|(rec_2)|(rec_3)|(rec_4)|(rec_6)|(rec_8)),])[1]
