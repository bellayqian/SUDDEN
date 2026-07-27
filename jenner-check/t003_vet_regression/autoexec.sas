/* cap input rows for the captured run */
options obs=100;

/* Bella's SUDDEN_vet_regression.sas merges Pilot_clean2 and Control_clean2 -- the
   cleaned demographic tables built by her SUDDEN_*_DEMO scripts from private study
   CSVs. Those cleaned tables are not in the repo, so this autoexec stands in two
   small synthetic tables with exactly the columns her regression consumes (vet,
   study_id, sudden, age_num, Gender, Race, marry, substance, anx_dep). Her merge,
   numVet derivation, PROC FREQ and the three PROC LOGISTIC models below run unchanged. */

data Pilot_clean2;
  infile datalines dlm='|' dsd truncover;
  length vet $30 Gender $6 Race $25 marry $30;
  input vet $ study_id sudden age_num Gender $ Race $ marry $ substance anx_dep;
  datalines;
Veteran|1000|1|36|Female|Black|Married|0|0
Veteran|1001|1|78|Female|Other|Married|1|0
Veteran|1002|1|40|Male|White|Married|0|1
Non-Veteran|1003|1|49|Male|Other|Never Married|0|0
Non-Veteran|1004|1|56|Male|White|Married|0|0
Veteran|1005|1|59|Male|Black|Never Married|0|0
Non-Veteran|1006|1|69|Male|Black|Married|0|1
Non-Veteran|1007|1|71|Male|Other|Married|0|1
Non-Veteran|1008|1|84|Male|White|Married|0|0
Non-Veteran|1009|1|64|Female|Black|Married|0|0
Non-Veteran|1010|1|77|Male|Other|Married|0|1
Non-Veteran|1011|1|45|Male|Black|Never Married|0|1
Non-Veteran|1012|1|84|Male|White|Married|0|0
Non-Veteran|1013|1|39|Male|Other|Never Married|0|1
Non-Veteran|1014|1|60|Female|Black|Married|0|0
Non-Veteran|1015|1|82|Female|Other|Never Married|1|1
Non-Veteran|1016|1|58|Male|White|Never Married|0|0
Veteran|1017|1|44|Female|White|Never Married|0|0
Non-Veteran|1018|1|73|Male|Other|Never Married|0|1
Veteran|1019|1|78|Female|Black|Never Married|0|0
Non-Veteran|1020|1|45|Male|White|Never Married|0|1
Veteran|1021|1|75|Male|Other|Married|0|0
Non-Veteran|1022|1|69|Female|White|Never Married|1|0
Veteran|1023|1|58|Male|White|Married|0|1
Veteran|1024|1|40|Female|Black|Married|0|0
Non-Veteran|1025|1|70|Male|Black|Never Married|0|1
Non-Veteran|1026|1|80|Male|Black|Never Married|1|1
Non-Veteran|1027|1|42|Male|White|Married|0|0
Non-Veteran|1028|1|72|Male|White|Married|0|0
Veteran|1029|1|37|Male|White|Married|0|1
;
run;

data Control_clean2;
  infile datalines dlm='|' dsd truncover;
  length vet $30 Gender $6 Race $25 marry $30;
  input vet $ study_id sudden age_num Gender $ Race $ marry $ substance anx_dep;
  datalines;
Non-Veteran|2000|0|48|Female|White|Never Married|0|0
Non-Veteran|2001|0|47|Male|White|Never Married|0|0
Non-Veteran|2002|0|64|Female|White|Married|0|0
Non-Veteran|2003|0|41|Male|White|Married|1|0
Non-Veteran|2004|0|46|Male|Black|Married|0|0
Veteran|2005|0|38|Female|Other|Married|0|0
Non-Veteran|2006|0|61|Male|Black|Married|1|0
Non-Veteran|2007|0|59|Male|Black|Never Married|1|0
Non-Veteran|2008|0|79|Female|Other|Never Married|0|0
Non-Veteran|2009|0|48|Male|Other|Married|0|0
Veteran|2010|0|72|Male|Other|Married|0|1
Veteran|2011|0|46|Male|Other|Married|0|0
Veteran|2012|0|71|Male|Other|Married|0|0
Non-Veteran|2013|0|51|Male|Other|Never Married|0|0
Non-Veteran|2014|0|43|Female|Other|Never Married|1|0
Veteran|2015|0|35|Male|Other|Married|0|1
Non-Veteran|2016|0|67|Male|White|Never Married|0|0
Non-Veteran|2017|0|53|Male|Black|Never Married|0|1
Non-Veteran|2018|0|77|Male|White|Never Married|0|0
Non-Veteran|2019|0|52|Male|Other|Married|0|0
Non-Veteran|2020|0|67|Male|Black|Married|0|1
Non-Veteran|2021|0|52|Male|White|Never Married|0|1
Non-Veteran|2022|0|45|Female|Black|Never Married|0|0
Veteran|2023|0|79|Male|Other|Married|0|1
Non-Veteran|2024|0|62|Male|White|Never Married|0|0
Non-Veteran|2025|0|48|Female|White|Married|0|1
Non-Veteran|2026|0|74|Female|White|Married|0|0
Non-Veteran|2027|0|36|Male|Other|Never Married|1|1
Non-Veteran|2028|0|52|Male|Other|Married|1|0
Non-Veteran|2029|0|49|Male|Black|Never Married|0|0
;
run;
