/*********************************************************************
   Adapted from: SUDDEN_WestAfrica_DEMO.sas (Yunzhe Qian / Bella)

   Description:   Demographic and Clinical Characteristics of Victims
                  of Sudden Death by US Military Veteran Status in West Africa

   This bundle keeps the data-cleaning step (substr/input/scan-based
   recoding of age, marital status, race, veteran status and comorbidity
   flags) and the frequency checks verbatim. The proc import of the private
   study CSV and the %include of the external Compare_baseline_669 macro are
   supplied by the autoexec synthetic sudden.WestAfrica table instead.
********************************************************************/

*Data Cleaning;
data work.WestAfrica_clean;
	set sudden.WestAfrica;
	length age_whole $8. age_num 8. marry $30. vet $30.;
	age_whole = substr(age,1,2);
	age_num = input(age_whole, best8.);
	drop age_whole age;

	if death_veteran_2021 = 1 then vet = "Veteran";
	if death_veteran_2021 = 2 then vet = "Non-Veteran";
	if death_veteran_2021 = 3 then vet = "Non-Veteran";

	marry = MARITAL_STATUS;
	if scan(MARITAL_STATUS,2) = "but" then marry = "Married";
	if scan(MARITAL_STATUS,1) = "Never" then marry = "Never Married";
	if scan(MARITAL_STATUS,1) = "Widowed" then marry = "Married";
	if scan(MARITAL_STATUS,1) = "Divorced" then marry = "Married";
	if MARITAL_STATUS in ("","Unknown") then marry = "" ;

	if scan(race,1) = "Black" then race = "Black";
	if race ^in ("White","Black") then race = "Other";


	if comorb_htn = 2 then comorb_htn = .;
	if comorb_dyslip = 2 then comorb_dyslip = .;
	if comorb_dm = 2 then comorb_dm = .;
	if comorb_cad = 2 then comorb_cad = .;
	if comorb_cmypthy = 2 then comorb_cmypthy = .;
	if comorb_lvh = 2 then comorb_lvh = .;

run;

proc freq data= sudden.WestAfrica;
	tables death_veteran_2021  /missing;
run;

proc freq data=work.WestAfrica_clean;
	tables  vet /missing;
run;

proc freq data=work.WestAfrica_clean;
	tables marry race /missing;
run;
