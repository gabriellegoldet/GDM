All the R code used to create the cohort and analyse it.

1_get_first_obs_glycaemic_test_drug_hes: filters CPRD and HES data down to the first observation from the DM_code list per patid, the first glycaemic test reaching a diagnostic threshold per patid, the first relevant prescription per patid, and the first diabetes coding in HES per patid.

Dataframe then prepared to be used in 2_patient_linking.rmd in which we identify subjects with multiple patids.

3_drug_pattern.Rmd helps to identify a chronological pattern of prescription per individual (e.g. just a one off prescriptio or a long -term continuous prescription)

4_pregnancy extracts delivery dates for each idnividual

5_adjusting_dates starts to clean the data frame of spurious dates and removes data that is likely not related to chronic diabetes but could relate to gestational DM, non-diabetic hyperglycaemia or polycystic ovarian syndrome

6_final_cohort_formation collapses the subjects with multiple patids down to a single row with all the relevant info, identifies subjects to be entered in the GDM cohort and then subsequently forms the final cohort.
