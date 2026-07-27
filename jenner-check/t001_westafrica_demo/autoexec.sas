/* cap input rows for the captured run */
options obs=100;

/* Bella's SUDDEN_WestAfrica_DEMO.sas imports SUDDENPilot-WestAfrica_Data.csv into
   the `sudden` libname. The real CSV is private study data and is not in the repo,
   so this autoexec stands in a small, synthetic sudden.WestAfrica table whose columns
   match exactly what the cleaning step reads (age, race, MARITAL_STATUS,
   death_veteran_2021, Gender, and the comorb_* / comorb2_* flags). Fields are
   pipe-delimited so the multi-word marital-status strings survive intact for the
   scan()-based recoding. The script's own logic below runs unchanged against it. */
libname sudden (work);

data sudden.WestAfrica;
  length age $6 race $20 Gender $6 MARITAL_STATUS $30;
  infile datalines dlm='|' dsd truncover;
  input study_id age $ race $ Gender $ MARITAL_STATUS $
        death_veteran_2021 comorb2_crd comorb_htn comorb_dyslip comorb_dm
        comorb_cad comorb2_stroke comorb2_heartfailure comorb_cmypthy
        comorb2_renal comorb_lvh;
  datalines;
1|45yr|Black|Male|Married, but separated|1|0|1|0|1|0|0|0|0|0|0
2|62yr|White|Female|Widowed|2|1|1|1|0|1|1|0|0|1|0
3|38yr|Black|Male|Never Married|3|0|0|0|0|0|0|0|0|0|0
4|71yr|Asian|Female|Married|1|0|1|2|1|2|0|1|0|0|2
5|55yr|Black|Male|Divorced|2|1|2|0|0|0|0|0|2|0|0
6|49yr|White|Male|Unknown|3|0|0|0|0|0|0|0|0|0|0
7|66yr|Black|Female|Married, but separated|1|1|1|1|1|1|1|1|1|1|1
8|33yr|White|Male|Never Married|2|0|0|0|0|0|0|0|0|0|0
9|58yr|Black|Female|Widowed|1|0|1|0|1|0|0|0|0|0|0
10|42yr|Other|Male|Married|2|0|1|1|0|1|0|0|0|0|0
;
run;
