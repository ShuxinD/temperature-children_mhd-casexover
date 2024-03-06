## Temperature and major depressive disorders in children with Medicaid data

FASSE location: `/n/dominici_nsaph_l3/Lab/projects/temperature-children_mhd-casexover`

#### study population

Medicaid enrollees up to 18 years old with mental health disorders hospitalizations from 1999 to 2012.

Subgroup analysis restricts to 12-18 years old.

#### outcome

depression; anxiety; emotional disturbance; adolescence reaction; disturbance of conduct

from all available diagnositic codes

#### exposure

daily maximum temperature; daily average relative humidity (source: gridMET)

heat index calculated from above

#### study design

case-crossover with time-stratified matching strategy

#### statistical method

conditional logistic

### coding process

#### data preparation

Medicaid dataset is pre-processed as described in https://github.com/NSAPH-Data-Processing/medicaid-children-first-hospitalzation

The public database is used to check ZIP code validity: https://www.unitedstateszipcodes.org/zip-code-database/

Race/ethnicity coding: https://resdac.org/cms-data/variables/raceethnicity-msis

To create the map, we use the 2012 ZIP-code to ZCTA crosswalk file from https://udsmapper.org/zip-code-to-zcta-crosswalk/. The analysis was still conducted on ZIP-code level.

Climate types: major types


