SELECT 
    sp.*, 
    avt.PATIENTID,
    avt.DIAGNOSISDATEBEST, 
    avt.SITE_ICD10_O2_3CHAR, 
    avt.AGE, 
    avt.SEX, 
    avt.QUINTILE_2015,
    st_sr.MERGED_TUMOUR_ID,
    st_sr.PRIMARY_DIAGNOSIS,
    st_sr.CLINICAL_TRIAL, 
    st_sr.HEIGHT_AT_START_OF_REGIMEN, 
    st_sr.WEIGHT_AT_START_OF_REGIMEN
FROM 
    SIM_SACT_PATIENT sp
LEFT JOIN 
    SIM_AV_TUMOUR avt
    ON sp.LINK_NUMBER = avt.LINKNUMBER
LEFT JOIN 
    (SELECT 
         st.MERGED_PATIENT_ID, 
         st.MERGED_TUMOUR_ID,
         st.PRIMARY_DIAGNOSIS,
         sr.CLINICAL_TRIAL, 
         sr.HEIGHT_AT_START_OF_REGIMEN, 
         sr.WEIGHT_AT_START_OF_REGIMEN
     FROM 
         SIM_SACT_TUMOUR st
     LEFT JOIN 
         SIM_SACT_REGIMEN sr
         ON st.MERGED_PATIENT_ID = sr.MERGED_PATIENT_ID 
            AND st.MERGED_TUMOUR_ID = sr.MERGED_TUMOUR_ID) st_sr
    ON sp.MERGED_PATIENT_ID = st_sr.MERGED_PATIENT_ID;
