%LET job=SUDDEN_FoodDesert;
%LET outdir=/home/u49518639/SUDDEN;
proc printto log="&outdir/Logs/&job.log" new; run;

*********************************************************************
*  Assignment:    FoodDesert_CONTROL                                       
*                                                                    
*  Description:   Sudden Death associatino with Food Desert in NC 
*
*  Name:          Yunzhe Qian (Bella)
*
*  Date:          03/25/2022                                        
*
*  Language:      SAS, VERSION 9.4  
*
*  Input:         Control_FoodDesert
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

LIBNAME sudden "/home/u49518639/SUDDEN/Data/FoodDesert";

* Import Pilot and Control veteran data set;
proc import datafile="/home/u49518639/SUDDEN/Data/FoodDesert/Kothari_Control2.xlsx"
	out=sudden.Control_FoodDesert dbms=xlsx replace;
	datarow=2;
    getnames=yes;
run;

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

%include '/home/u49518639/BIOS669/Compare_baseline_669.sas';
%Compare_baseline_669(
_DATA_IN=work.ctrl_FoodDesert_clean1,
_DATA_OUT=sudden.baseline_ctrl_FoodDesert,
_GROUP=desert,
_PREDICTORS= gender Race CHF CAD HTN diabetes dyslip,
_CATEGORICAL= gender Race CHF CAD HTN diabetes dyslip,
_RQ=Control_FoodDesert);

data display_ctrl;
	set sudden.baseline_ctrl_FoodDesert;
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
		
	if variable="GENDER" and label="" then do;
		characteristic="{    Gender, n(%)}";
		order=5; pvalue=.; end;
	if variable="GENDER" and label="- Male" then do;
		characteristic="{        Male}";
		order=6; end;
	if variable="GENDER" and label="- Female" then do;
		characteristic="{        Female}";
		order=7; end;
	
	if variable="CHF" and label="" then do;
		characteristic="{    Congestive Heart Failure, n(%)}";
		order=8; pvalue=.; end;
	if variable="CHF" and label="- Yes" then do;
		characteristic="{        Yes}";
		order=9; end;	
	if variable="CHF" and label="- No" then do;
		characteristic="{        No}";
		order=10; end;
	
	if variable="CAD" and label="" then do;
		characteristic="{    Coronary Artery Disease, n(%)}";
		order=11; pvalue=.; end;
	if variable="CAD" and label="- Yes" then do;
		characteristic="{        Yes}";
		order=12; end;	
	if variable="CAD" and label="- No" then do;
		characteristic="{        No}";
		order=13; end;
	
	if variable="DIABETES" and label="" then do;
		characteristic="{    Diabetes, n(%)}";
		order=14; pvalue=.; end;
	if variable="DIABETES" and label="- Yes" then do;
		characteristic="{        Yes}";
		order=15; end;	
	if variable="DIABETES" and label="- No" then do;
		characteristic="{        No}";
		order=16; end;
		
	if variable="DYSLIP" and label="" then do;
		characteristic="{    Dyslipidemia, n(%)}";
		order=17; pvalue=.; end;
	if variable="DYSLIP" and label="- Yes" then do;
		characteristic="{        Yes}";
		order=18; end;	
	if variable="DYSLIP" and label="- No" then do;
		characteristic="{        No}";
		order=19; end;
		
	if variable="HTN" and label="" then do;
		characteristic="{    Hypertension, n(%)}";
		order=20; pvalue=.; end;
	if variable="HTN" and label="- Yes" then do;
		characteristic="{        Yes}";
		order=21; end;	
	if variable="HTN" and label="- No" then do;
		characteristic="{        No}";
		order=22; end;
		
	if missing (order) then delete;
run;

ODS RTF FILE="&outdir./Output/FoodDesert/customised_table_control.RTF" style=journal bodytitle;
ods listing; title; footnote; ods listing close;

title1 J=center height=12pt font='ARIAL' bold ITALIC 
"{Table 2 Demographic and Clinical Characteristics of Controls of Sudden Death by Food Desert Status in North Carolina}";

footnote1 J=left height=8.5pt font='ARIAL' bold
 "{*p values compare frequencies between food desert status}" ;
 
%let st=style(column)=[just=center cellwidth=2.8 cm vjust=bottom font_size=8.5 pt]
style(header)=[just=center font_size=8.5 pt];

proc report data=display_ctrl nowd style=[cellpadding=6 font_size=8.5 pt rules=none];
	column order characteristic(column_2 column_1 column_overall pvalue);
	define order / order noprint;
	define characteristic / display " "
	style=[just=left cellwidth=9.0 cm font_weight=bold font_size=8.5 pt];
	define column_2 / display "{With Food Desert \line N(%) \line n=173}" &st ;
	define column_1 / display "{Without Food Desert \line N(%) \line n=936}" &st ;
	define column_overall / display "{Total \line N(%) \line n=&count_overall}" &st ;
	define pvalue / display "{P-value*}" format=pvalue5.3
	style(column)=[just=right cellwidth=2 cm vjust=bottom font_size=8.5 pt]
	style(header)=[just=right cellwidth=2 cm font_size=8.5 pt] ;
run;
ods rtf close; ods listing;

proc printto; run;
*/