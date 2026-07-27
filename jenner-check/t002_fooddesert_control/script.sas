/*********************************************************************
   Adapted from: FoodDesert_CONTROL.sas (Yunzhe Qian / Bella)

   Description:   Sudden Death association with Food Desert in NC (controls)

   This bundle keeps the control-arm data-cleaning step (recoding the
   coded race / sex / food-desert / comorbidity fields into readable
   categories) and the frequency and contents checks verbatim. The proc
   import of the private Kothari_Control2.xlsx and the external
   %include Compare_baseline_669 macro / PROC REPORT RTF block are supplied
   by the autoexec synthetic sudden.Control_FoodDesert table instead.
********************************************************************/

data freqTest;
	set sudden.Control_FoodDesert;
	drop id_controlsd;
run;

* Frequency Test to see data tidyness;
proc freq data= freqTest noprint;
	tables _all_ / norow nocol nopercent missing;
run;

*Data Cleaning;
data work.ctrl_FoodDesert_clean1;
	set sudden.Control_FoodDesert;
	length race $25 gender $6 desert $8 CHF $8 dyslip $8
	diabetes $8 HTN $8 status $8 CAD $8;

	if race_controlsd = 1 then race = "White";
	if race_controlsd = 2 then race = "Black";
	if race_controlsd ^in (1,2) then race = "Other";

	if sex_controlsd = 0 then gender = "Female";
	if sex_controlsd = 1 then gender = "Male";

	if food_desert in (0,1) then desert = "No";
	if food_desert = 2 then desert = "Yes";
	if (food_desert = .) and (chf_controlsd = .) then delete;

	if chf_controlsd = 0 then CHF = "No";
	if chf_controlsd = 1 then CHF = "Yes";

	if dm_controlsd = 0 then diabetes = "No";
	if dm_controlsd = 1 then diabetes = "Yes";

	if dyslip_controlsd = 0 then dyslip = "No";
	if dyslip_controlsd = 1 then dyslip = "Yes";

	if htn_controlsd = 0 then HTN = "No";
	if htn_controlsd = 1 then HTN = "Yes";

	if cad_controlsd = 0 then CAD = "No";
	if cad_controlsd = 1 then CAD = "Yes";

	status = "Control";
	drop race_controlsd id_controlsd sex_controlsd address city
	zip_code state census_tract_id cad_controlsd
	dm_controlsd chf_controlsd dyslip_controlsd htn_controlsd;
run;

proc freq data= ctrl_FoodDesert_clean1;* noprint;
	tables _all_ / norow nocol nopercent missing;
run;

proc freq data= freqTest;* noprint;
	tables food_desert chf_controlsd / norow nocol nopercent missing;
run;

proc freq data= ctrl_FoodDesert_clean1;* noprint;
	tables desert CHF / norow nocol nopercent missing;
run;

proc contents data=ctrl_FoodDesert_clean1;
run;
