%LET job=SUDDEN_vet;
%LET outdir=/home/u49518639/SUDDEN;
proc printto log="&outdir/Logs/&job.log" new; run;

*********************************************************************
*  Assignment:    SUDDEN_CONTROL_DEMO                                        
*                                                                    
*  Description:   Demographic and Clinical Characteristics of Victims 
*				  of Sudden Death by US Military Veteran Status in Wake
*				  County, North Carolina 
*
*  Name:          Yunzhe Qian (Bella)
*
*  Date:          08/15/2021                                        
*
*  Language:      SAS, VERSION 9.4  
*
*  Input:         Control_Vet
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
proc import datafile="&outdir/Data/SimpsonSUDDENControl-VeteranStatusProject_DATA_2021-07-22_1508 updated 02Aug21 csv.csv"
	out=sudden.Control_Vet dbms=csv replace;
	datarow=2;
    getnames=yes;
run;

*Data Cleaning;
data work.Control_clean1;
	set sudden.Control_Vet;
	length vet $30. marry $30. race $25.;
	if final_vet_sts = 1 then vet = "Veteran";
	if final_vet_sts = 2 then vet = "Non-Veteran";
	if final_vet_sts = 3 then vet = "Non-Veteran";
	
	marry = marital_sts;
	if marital_sts = 0 then marry = "Never Married";
	if marital_sts ^in (0,6) then marry = "Married";
	if marital_sts = 6 then marry = "";
	
	if race_controlsd in (3,4,5) then race = "Other";
	if race_controlsd = 1 then race = "White";
	if race_controlsd = 2 then race = "Black";
	if race_controlsd = 6 then race = "";
	drop final_vet_sts;
run;


data work.Control_clean2;
	set Control_clean1;
	length substance 8. mental 8. anx_dep 8. mental_all 8. gender $6.;
	
	if (sevmh_controlsd = 1) or (anymh_controlsd = 1) then mental = 1;
	if (sevmh_controlsd = 0) && (anymh_controlsd = 0) then mental = 0;
	
	if (anx_controlsd = 1) or (dep_controlsd = 1) then anx_dep = 1;
	if (anx_controlsd = 0) && (dep_controlsd = 0) then anx_dep = 0;
	
	if (alc_controlsd = 1) or (sub_controlsd = 1) then substance = 1;
	if (alc_controlsd = 0) && (sub_controlsd = 0) then substance = 0;
	
	if (mental = 1) or (anx_dep = 1) then mental_all = 1;
	if (mental = 0) && (anx_dep = 0) then mental_all = 0;
	
	if sex_controlsd = 1 then gender = "Male";
	if sex_controlsd = 0 then gender = "Female";
	
	sudden = 0;
	study_id = id_controlsd;
	age_num = age_controlsd;
	keep vet study_id sudden age_num gender Race marry substance anx_dep;
run;

proc contents data=Control_clean2;
run;

* Check if value are expected;
proc freq data= work.Control_clean2; *noprint;
	tables mental anx_dep substance
	/norow nocol nopercent;
run;

proc freq data= work.Control_clean1;* noprint;
	tables 	sex_controlsd*vet
	/ norow nocol nopercent;
run;
sevmh_controlsd*anymh_controlsd
		   anx_controlsd*dep_controlsd
		   alc_controlsd*sub_controlsd

proc freq data=work.Control_clean1;
	tables lvh_controlsd*vet / exact norow nocol nopercent;
run; 

* Create baseline_control data set;
%include '/home/u49518639/BIOS669/Compare_baseline_669.sas';
%Compare_baseline_669(
_DATA_IN=work.Control_clean2,
_DATA_OUT=sudden.baseline_Control,
_GROUP=vet, 
_PREDICTORS= 
	age_controlsd
	sex_controlsd
	race
	marry
	resp_controlsd
	dep_controlsd
	anx_controlsd
	anymh_controlsd
	sevmh_controlsd
	alc_controlsd
	sub_controlsd
	ptsd_controlsd
	htn_controlsd
	dyslip_controlsd
	dm_controlsd
	cad_controlsd
	stroke_controlsd
	chf_controlsd
	cm_controlsd
	ckd_controlsd
	lvh_controlsd,
_CATEGORICAL= 
	sex_controlsd
	race
	marry
	resp_controlsd
	dep_controlsd
	anx_controlsd
	anymh_controlsd
	sevmh_controlsd
	alc_controlsd
	sub_controlsd
	ptsd_controlsd
	htn_controlsd
	dyslip_controlsd
	dm_controlsd
	cad_controlsd
	stroke_controlsd
	chf_controlsd
	cm_controlsd
	ckd_controlsd
	lvh_controlsd,
_countable= age_controlsd,
_RQ=Control_DEMO,
_ID=id_controlsd);


* Decide how to display data;
data display_control;
	set sudden.baseline_control;
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
	if variable="RACE" and label="- Other Race" then do;
		characteristic="{        Other Race}";
		order=4; end;
		
	if variable="AGE_CONTROLSD" and label="" then do;
		characteristic="{    Age (yr), median (25\super th}{, 75\super th}{)}";
		order=5; end;
		
	if variable="SEX_CONTROLSD" and label="" then do;
		characteristic="{    Gender n(%)}";
		order=6; pvalue=.; end;
	if variable="SEX_CONTROLSD" and label="- 0" then do;
		characteristic="{        Female}";
		order=7; end;
	if variable="SEX_CONTROLSD" and label="- 1" then do;
		characteristic="{        Male}";
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
	
	if variable="RESP_CONTROLSD" and label="" then do;
		characteristic="{    Chronic Respiratory Disease, n(%)}";
		order=12; pvalue=.; end;
	if variable="RESP_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=13; end;
	if variable="RESP_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=14; end;
	
	if variable="DEP_CONTROLSD" and label="" then do;
		characteristic="{    Depression, n(%)}";
		order=15; pvalue=.; end;
	if variable="DEP_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=16; end;
	if variable="DEP_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=17; end;

	if variable="ANX_CONTROLSD" and label="" then do;
		characteristic="{    Anxiety, n(%)}";
		order=18; pvalue=.; end;
	if variable="ANX_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=19; end;
	if variable="ANX_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=20; end;

	if variable="ANYMH_CONTROLSD" and label="" then do;
		characteristic="{    Any mental health disorder, n(%)}";
		order=21; pvalue=.; end;
	if variable="ANYMH_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=22; end;
	if variable="ANYMH_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=23; end;
	
	if variable="SEVMH_CONTROLSD" and label="" then do;
		characteristic="{    Severe mental illness, n(%)}";
		order=24; pvalue=.; end;
	if variable="SEVMH_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=25; end;
	if variable="SEVMH_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=26; end;
			
	if variable="ALC_CONTROLSD" and label="" then do;
		characteristic="{    Alcohol Abuse, n(%)}";
		order=27; pvalue=.; end;
	if variable="ALC_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=28; end;
	if variable="ALC_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=29; end;

	if variable="SUB_CONTROLSD" and label="" then do;
		characteristic="{    Substance Abuse except alcohol, n(%)}";
		order=30; pvalue=.; end;
	if variable="SUB_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=31; end;
	if variable="SUB_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=32; end;
	
	if variable="PTSD_CONTROLSD" and label="" then do;
		characteristic="{    Post Traumatic Stress Disorder (PTSD), n(%)}";
		order=33; pvalue=.; end;
	if variable="PTSD_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=34; end;
	if variable="PTSD_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=35; end;
	
	if variable="HTN_CONTROLSD" and label="" then do;
		characteristic="{    Hypertension, n(%)}";
		order=36; pvalue=.; end;
	if variable="HTN_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=37; end;
	if variable="HTN_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=38; end;

	if variable="DYSLIP_CONTROLSD" and label="" then do;
		characteristic="{    Dyslipidemia, n(%)}";
		order=39; pvalue=.; end;
	if variable="DYSLIP_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=40; end;
	if variable="DYSLIP_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=41; end;

	if variable="DM_CONTROLSD" and label="" then do;
		characteristic="{    Type II Diabetes Mellitus (T2DM), n(%)}";
		order=42; pvalue=.; end;
	if variable="DM_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=43; end;
	if variable="DM_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=44; end;
		
	if variable="CAD_CONTROLSD" and label="" then do;
		characteristic="{    Coronary Artery Disease (CAD), n(%)}";
		order=45; pvalue=.; end;
	if variable="CAD_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=46; end;
	if variable="CAD_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=47; end;
	
	if variable="STROKE_CONTROLSD" and label="" then do;
		characteristic="{    Stroke, n(%)}";
		order=48; pvalue=.; end;
	if variable="STROKE_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=49; end;
	if variable="STROKE_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=50; end;
	
	if variable="CHF_CONTROLSD" and label="" then do;
		characteristic="{    Congestive Heart Failure, n(%)}";
		order=51; pvalue=.; end;
	if variable="CHF_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=52; end;
	if variable="CHF_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=53; end;
	
	if variable="CM_CONTROLSD" and label="" then do;
		characteristic="{    Cardiomyopathy, n(%)}";
		order=54; pvalue=.; end;
	if variable="CM_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=55; end;
	if variable="CM_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=56; end;
	
	if variable="CKD_CONTROLSD" and label="" then do;
		characteristic="{    Chronic Kidney Disease, n(%)}";
		order=57; pvalue=.; end;
	if variable="CKD_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=58; end;
	if variable="CKD_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=59; end;
	
	if variable="LVH_CONTROLSD" and label="" then do;
		characteristic="{    Left Ventricular Hypertension (LVH), n(%)}";
		order=60; pvalue=.; end;
	if variable="LVH_CONTROLSD" and label="- 1" then do;
		characteristic="{        Yes}";
		order=61; end;
	if variable="LVH_CONTROLSD" and label="- 0" then do;
		characteristic="{        No}";
		order=62; end;

	if missing (order) then delete;
run;

ODS RTF FILE="&outdir./Output/customised_table_control.RTF" style=journal bodytitle;
ods listing; title; footnote; ods listing close;

title1 J=center height=12pt font='ARIAL' bold ITALIC "{Table 2 Comparison of Clinical and Demographic Characteristics Between Sudden Death Cases and Controls in Wake County, North Carolina}";

footnote1 J=left height=8.5pt font='ARIAL' bold
 "{*p values compare frequencies between veterans and nonveterans}" ;
 
%let st=style(column)=[just=center cellwidth=2.8 cm vjust=bottom font_size=8.5 pt]
style(header)=[just=center font_size=8.5 pt];

proc report data=display_control nowd style=[cellpadding=6 font_size=8.5 pt rules=none];
	column order characteristic(column_2 column_1 column_overall pvalue);
	define order / order noprint;
	define characteristic / display " "
	style=[just=left cellwidth=9.0 cm font_weight=bold font_size=8.5 pt];
	define column_2 / display "{Veteran \line N(%) \line N=34}" &st ;
	define column_1 / display "{Non-Veteran \line N(%) \line N=1080}" &st ;
	define column_overall / display "{Total \line N(%) \line n=&count_overall}" &st ;
	define pvalue / display "{P-value*}" format=pvalue5.3
	style(column)=[just=right cellwidth=2 cm vjust=bottom font_size=8.5 pt]
	style(header)=[just=right cellwidth=2 cm font_size=8.5 pt] ;
run;
ods rtf close; ods listing;

proc printto; run;
