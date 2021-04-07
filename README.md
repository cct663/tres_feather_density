# Purpose

This repository contains the complete set of code and data required to reproduce all analyses and figures included in Taff et al. *Investigating a trade-off between the quality of nest grown feathers and pace of development in an altricial bird*. This readme file briefly describes the contents of the repository.

# Raw data

The raw data folder has all of the data used in the paper saved as tab delimited text files. There are five separate data files as follows. For the data collected in this study full descriptions of each column name are given at the end of this readme file.  

- **callan_et_al_data** These data included only for a conceptual figure and come from a previously published paper for which they were publicly archived (https://doi.org/10.5061/dryad.p6m33p3). For information on these data, refer to the original Dryad data package and associated paper.

- **data_by_individual** Data organized with one row for each individual bird and various measurements in columns.  

- **data_by_nest** Data organized with one row for each individual nest.

- **data_by_observer** Data for repeatability estimates for measurements made by different observers. Not used in main analyses, but only to report inter-observer repeatability.

- **data_for_provisioning** Female provisioning data from RFID records.


# R scripts

The R scripts folder contains a single script that reproduces all of the analyses and figures that are included in the paper along with saved versions of figures (including some versions not included in the main paper). The script itself is fully annotated and organized with notes on each section. Running the script from start to finish should produce every result reported. Packages required are listed at the beginning of the script.

# Saved objects 

These are intermediate data objects saved during r processing and re-loaded in the script. They are not necessary for reproducing results because the code can produce each of these files from the raw data. They are saved only because some sections of the code take a long time to run and these intermediate objects can be loaded rather than waiting.

# Column names

Here I describe the column names for each of the raw data files included in the analysis.

## **data_by_individual**

- *band* - unique USGS band number for this individual 
- *rfid* - unique PIT tag number for this individual 
- *year* - year of sample
- *age* - age of individual (adult or nestling)
- *soc_un* - location of nest box individual was raised in (or for adults where they were breeding)
- *soc_box* - nest box number (unique within location) where individual was sampled
- *soc_ubox* - combination of location and nest box number where individual was sampled
- *soc_uby* - unique location, nest box, year combination
- *treat1* - stage one treatment (see text)
- *treat2* - stage two treatment (see text)
- *laid_un* - for nestlings, location that they were laid in (can differ from sampled box because of cross fostering)
- *laid_box* - for nestlings, box that they were laid in (can differ from sampled because of cross fostering)
- *laid_ubox* - combination of *laid_un* and *laid_ubox*
- *laid_uby* - combination of *laid_ubox* and *year*
- *lay_clutch* - clutch size of nest individual was laid in
- *gen_dad_uby* - location, box number, and year of the genetic father (if known) of this nestling
- *soc_ci* - clutch initiation day of year (first egg) for social (raised) nest
- *gen_ci* - clutch initiation day of year for genetic (laid) nest
- *hatch_doy* - hatching day of year
- *sex* - sex of individual, nestlings determined by molecular sexing protocol
- *in_xfost_exp* - was this nest part of cross fostering?
- *raised_nest* - was this individual raised in their home (laid) nest or in another (cross) nest
- *parents_knwn* - are both genetic parents known from microsatellites
- *soc_mom* - band number of female at nest where individual was raised
- *gen_mom* - band number of female identified as genetic mother by parentage analysis
- *soc_dad_raised* - band number of male at nest where individual was raised
- *soc_dad_laid* - band number of male at nest where individual was laid
- *gen_dad* - band number of male identified as genetic father by parentage analysis
- *breast_den* - measurement of breast barbs per cm used in analyses
- *back_den* - measurement of back barbs per cm used in analyses
- *br1_mm* - mm of feather measured on breast feather one
- *br1_den* - barbs per cm for breast feather one
- *br2_mm* - mm of feather measured on breast feather two
- *br2_den* - barbs per cm for breast feather two
- *back1_len* - mm of feather measured on back feather one
- *back1_den* - barbs per cm for back feather one
- *back2_len* - mm of feather measured on back feather two
- *back2_den* - barbs per cm for back feather two
- *d12_mass* - mass of nestling in grams on day 12
- *d12_wing* - wing length of nestling on day 12 in mm
- *d12_head* - length of head plus bill in mm on day 12
- *d12_bgluc* - baseline glucose on day 12
- *d12_sgluc* - stress-induced glucose on day 12
- *d12_dgluc* - post-dexamethasone glucose on day 12
- *d15_acgluc* - post-acth glucose on day 15
- *d12_lat* - latency in seconds from capture to baseline sample on day 12
- *d12_time_mins* - time (minutes after midnight) of bleeding on day 12
- *d12_base* - baseline corticosterone on day 12
- *d12_stress* - stress-induced corticosterone on day 12
- *d12_dex* - post-dexamethasone corticosterone on day 12
- *d15_acth* - post-acth corticosterone
- *fate* - fate of individual nestling
- *first_read* - time (in seconds from january 1st) of first rfid read record
- *last_read* - time (in seconds from january 1st) of last rfid read record
- *tot_read* - total number of rfid reads
- *fled_doy* - day of year of fledging if known
- *fled_age* - nestling age in days on fledging day
- *note* - any notes recorded about this indivudal
- *mass1* - for adults, mass at first capture
- *hbill1* - for adults, head plus bill length at first capture
- *wing1* - for adults, wing length in mm at first capture
- *ad_age* - for adults, age as SY = second year, ASY = after second year, AHY = after hatch year
- *bcort1* - for adults, baseline corticosterone at first capture
- *scort1* - for adults, stress-induced corticosterone at first capture
- *dcort1* - for adults, post dex cort at first capture
- *bgluc1* - for adults, baseline glucose at first capture
- *sgluc1* - for adults, stress-induced glucose at first capture
- *dgluc1* - for adults, post dex glucose at first capture

## **data_by_nest**

- *uby* - unit (location), box (unique within unit), and year of the nest
- *female* - unique band number of female associated with this nest
- *male* - unique band number of male associated with this nest
- *ci_doy* - clutch initiation day of year (first egg) for this nest
- *clutch* - clutch size of this nest
- *hatch_doy* - hatch day of year for this nest
- *max_brood* - maximum number of hatched young observed in this brood
- *d6_brood* - number of nestlings alive on day 6
- *d6_mass* - combined mass in grams of all nestlings alive on day 6
- *num_fled* - number of nestlings that fledged from this nest
- *treat_1* - treatment stage 1 (see text)
- *treat_2* - treatment stage 2 (see text)
- *full_treatment* - combination of *treat_1* and *treat_2*

## **data_by_observer**

- *feather_id* - unique code for this individual feather
- *observer* - unique observer name doing the measurement
- *density* - number of feather barbs per cm for this measurement

## **data_for_provisioning**

- *daycombo* - combiation of unit (location), nest number (unique within location), and day of year for this row
- *unitbox* - combination of unit (location) and nest number (unique within location)
- *uby* - combiation of *unitbox* and *year*
- *doy* - day of year of this observation
- *year* - year of this observation
- *offset* - days after hatching for this row; hatching = day 0
- *limit* - time limit in seconds used to determine unique provisioning trips (see Vitousek et al. 2018 PRSB)
- *f_feed* - number of unique provisioning trips by the female on this day
- *m_feed* - number of unique provisioning trips by the male on this day
- *f_reads* - number of total rfid reads of the female on this day
- *m_reads* - number of total rfid reads of the male on this day