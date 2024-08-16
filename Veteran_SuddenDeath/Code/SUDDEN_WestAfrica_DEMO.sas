%LET job=SUDDEN_vet;
%LET outdir=/home/u49518639/SUDDEN;
proc printto log="&outdir/Logs/&job.log" new; run;

*********************************************************************
*  Assignment:    SUDDEN_WestAfrica_DEMO                                        
*                                                                    
*  Description:   Demographic and Clinical Characteristics of Victims 
*				  of Sudden Death by US Military Veteran Status in West Africa 
*
*  Name:          Yunzhe Qian (Bella)
*
*  Date:          10/05/2021                                        
*
*  Language:      SAS, VERSION 9.4  
*
*  Input:         Pilot_VeternStatus
*
*  Output:        PDF file.
*                                                                    
********************************************************************;

OPTIONS NODATE MPRINT MERGENOBY=WARN VARINITCHK=WARN NOFULLSTIMER;
ODS _ALL_ CLOSE;
FOOTNOTE "Job &job._&onyen run on &sysdate at &systime";

options orientation=landscape;
ODS RTF FILE="&outdir/Output/&job._&onyen..rtf" STYLE=JOURNAL;

ODS TRACE ON;
ODS GRAPHICS ON;

LIBNAME sudden "/home/u49518639/SUDDEN/Data";

* Import Pilot and Control veteran data set;
proc import datafile="/home/u49518639/SUDDEN/Data/SUDDENPilot-WestAfrica_Data.csv"
	out=sudden.WestAfrica dbms=csv replace;
    datarow=2;
    getnames=yes;
run;

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

%include '/home/u49518639/BIOS669/Compare_baseline_669.sas';
%Compare_baseline_669(
_DATA_IN=work.WestAfrica_clean,
_DATA_OUT=sudden.baseline_WestAfrica,
_GROUP=vet, 
_PREDICTORS= age_num Gender Race marry comorb2_crd 
			comorb_htn comorb_dyslip comorb_dm comorb_cad comorb2_stroke comorb2_heartfailure comorb_cmypthy
			comorb2_renal comorb_lvh,
_CATEGORICAL= Gender Race marry comorb2_crd
			comorb_htn comorb_dyslip comorb_dm comorb_cad comorb2_stroke comorb2_heartfailure comorb_cmypthy
			comorb2_renal comorb_lvh,
_countable= age_num,
_RQ=WestAfrica_DEMO,
_ID=study_id);

data display_westAfrica;
	set sudden.baseline_WestAfrica;
	length characteristic $200;
	if variable="RACE" and label="" then do;
		characteristic="{\i \ul Baseline Characteristics\line \line \ul0 \i0     Race, n(%)}";
		order=1; pvalue=.; end;
	if variable="RACE" and label="- White" then do;
		characteristic="{        White}";
		order=2; end;
	if variable="RACE" and label="- Black" then do;
		characteristic="{        African American}";
		order=3; end;	
	if variable="RACE" and label="- Other" then do;
		characteristic="{        Other}";
		order=4; end;
		
	if variable="AGE_NUM" and label="" then do;
		characteristic="{    Age (yr), median (25\super th}{, 75\super th}{)}";
		order=5; end;
		
	if variable="GENDER" and label="" then do;
		characteristic="{    Gender, n(%)}";
		order=6; pvalue=.; end;
	if variable="GENDER" and label="- Male" then do;
		characteristic="{        Male}";
		order=7; end;
	if variable="GENDER" and label="- Female" then do;
		characteristic="{        Female}";
		order=8; end;
	
	if variable="MARRY" and label="" then do;
		characteristic="{    Marital Status, n(%)}";
		order=9; pvalue=.; end;
	if variable="MARRY" and label="- Married" then do;
		characteristic="{        Married}";
		order=10; end;	
	if variable="MARRY" and label="- Never Married" then do;
		characteristic="{        Never Married}";
		order=11; end;
	
	if variable="COMORB2_CRD" and label="" then do;
		characteristic="{    Chronic Respiratory Disease, n(%)}";
		order=12; pvalue=.; end;
	if variable="COMORB2_CRD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=13; end;
	if variable="COMORB2_CRD" and label="- 0" then do;
		characteristic="{        No}";
		order=14; end;
		
	if variable="COMORB_HTN" and label="" then do;
		characteristic="{    Hypertension, n(%)}";
		order=54; pvalue=.; end;
	if variable="COMORB_HTN" and label="- 1" then do;
		characteristic="{        Yes}";
		order=55; end;
	if variable="COMORB_HTN" and label="- 0" then do;
		characteristic="{        No}";
		order=56; end;

	if variable="COMORB_DYSLIP" and label="" then do;
		characteristic="{    Dyslipidemia, n(%)}";
		order=57; pvalue=.; end;
	if variable="COMORB_DYSLIP" and label="- 1" then do;
		characteristic="{        Yes}";
		order=58; end;
	if variable="COMORB_DYSLIP" and label="- 0" then do;
		characteristic="{        No}";
		order=59; end;
		
	if variable="COMORB_DM" and label="" then do;
		characteristic="{    Type II Diabetes Mellitus (T2DM), n(%)}";
		order=60; pvalue=.; end;
	if variable="COMORB_DM" and label="- 1" then do;
		characteristic="{        Yes}";
		order=61; end;
	if variable="COMORB_DM" and label="- 0" then do;
		characteristic="{        No}";
		order=62; end;
		
	if variable="COMORB_CAD" and label="" then do;
		characteristic="{    Coronary Artery Disease (CAD), n(%)}";
		order=63; pvalue=.; end;
	if variable="COMORB_CAD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=64; end;
	if variable="COMORB_CAD" and label="- 0" then do;
		characteristic="{        No}";
		order=65; end;
	
	if variable="COMORB2_STROKE" and label="" then do;
		characteristic="{    Stroke, n(%)}";
		order=66; pvalue=.; end;
	if variable="COMORB2_STROKE" and label="- 1" then do;
		characteristic="{        Yes}";
		order=67; end;
	if variable="COMORB2_STROKE" and label="- 0" then do;
		characteristic="{        No}";
		order=68; end;
	
	if variable="COMORB2_HEARTFAILURE" and label="" then do;
		characteristic="{    Heart Failure, n(%)}";
		order=69; pvalue=.; end;
	if variable="COMORB2_HEARTFAILURE" and label="- 1" then do;
		characteristic="{        Yes}";
		order=70; end;
	if variable="COMORB2_HEARTFAILURE" and label="- 0" then do;
		characteristic="{        No}";
		order=71; end;
	
	if variable="COMORB_CMYPTHY" and label="" then do;
		characteristic="{    Cardiomyopathy, n(%)}";
		order=72; pvalue=.; end;
	if variable="COMORB_CMYPTHY" and label="- 1" then do;
		characteristic="{        Yes}";
		order=73; end;
	if variable="COMORB_CMYPTHY" and label="- 0" then do;
		characteristic="{        No}";
		order=74; end;
	
	if variable="COMORB2_RENAL" and label="" then do;
		characteristic="{    Chronic Kidney Disease, n(%)}";
		order=75; pvalue=.; end;
	if variable="COMORB2_RENAL" and label="- 1" then do;
		characteristic="{        Yes}";
		order=76; end;
	if variable="COMORB2_RENAL" and label="- 0" then do;
		characteristic="{        No}";
		order=77; end;
	
	if variable="COMORB_LVH" and label="" then do;
		characteristic="{    Left Ventricular Hypertension (LVH), n(%)}";
		order=78; pvalue=.; end;
	if variable="COMORB_LVH" and label="- 1" then do;
		characteristic="{        Yes}";
		order=79; end;
	if variable="COMORB_LVH" and label="- 0" then do;
		characteristic="{        No}";
		order=80; end;
		
	if missing (order) then delete;
run;

ODS RTF FILE="&outdir./Output/customised_table_westAfrica.RTF" style=journal bodytitle;
ods listing; title; footnote; ods listing close;

title1 J=center height=12pt font='ARIAL' bold ITALIC "{Table 1 Demographic and Clinical Characteristics of Victims of Sudden Death by US Military Veteran Status in West Africa}";

footnote1 J=left height=8.5pt font='ARIAL' bold
 "{*p values compare frequencies between veterans and nonveterans}" ;
 
%let st=style(column)=[just=center cellwidth=2.8 cm vjust=bottom font_size=8.5 pt]
style(header)=[just=center font_size=8.5 pt];

proc report data=display_westAfrica nowd style=[cellpadding=6 font_size=8.5 pt rules=none];
	column order characteristic(column_2 column_1 column_overall pvalue);
	define order / order noprint;
	define characteristic / display " "
	style=[just=left cellwidth=9.0 cm font_weight=bold font_size=8.5 pt];
	define column_2 / display "{Veteran \line N(%) \line n=45}" &st ;
	define column_1 / display "{Non-Veteran \line N(%) \line n=354}" &st;
	define column_overall / display "{Total \line N(%) \line n=&count_overall}" &st ;
	define pvalue / display "{P-value*}" format=pvalue5.3
	style(column)=[just=right cellwidth=2 cm vjust=bottom font_size=8.5 pt]
	style(header)=[just=right cellwidth=2 cm font_size=8.5 pt] ;
run;
ods rtf close; ods listing;


proc printto; run;
