%LET job=SUDDEN_FoodDesert;
%LET outdir=/home/u49518639/SUDDEN;
proc printto log="&outdir/Logs/&job.log" new; run;

*********************************************************************
*  Assignment:    FoodDesert_regression                                       
*                                                                    
*  Description:   Sudden Death associatino with Food Desert in NC 
*
*  Name:          Yunzhe Qian (Bella)
*
*  Date:          03/25/2022                                         
*
*  Language:      SAS, VERSION 9.4  
*
*  Input:         Control_VeteranStatus, Pilot_VeternStatus
*
*  Output:        PDF file.
*                                                                    
********************************************************************;

OPTIONS NODATE MPRINT MERGENOBY=WARN VARINITCHK=WARN NOFULLSTIMER;
ODS _ALL_ CLOSE;
FOOTNOTE "Job &job. run on &sysdate at &systime";

options orientation=landscape;
ODS PDF FILE="&outdir/Output/FoodDesert/&job._regression_output.pdf" STYLE=JOURNAL;

ODS TRACE ON;
ODS GRAPHICS ON;

LIBNAME sudden "/home/u49518639/SUDDEN/Data/FoodDesert";

********************************************
* Merge Pilot and control;
data sudden.merge_FoodDesert_reg;
	set p_FoodDesert_clean1 ctrl_FoodDesert_clean1;
	drop urban food_desert;
run;

* Multivariable logistic regression;
data FD_reg;
	set work.merge_FoodDesert_reg;
	length sudden 8.;
	if status = "Control" then sudden = 0;
	if status = "Pilot" then sudden = 1;
run;

PROC FREQ DATA=FD_reg noprint;
	TABLES desert*(Gender Race CHF dyslip diabetes HTN CAD sudden) /NOROW NOCOL NOPERCENT;
RUN;

*Model 1 Food Desert status;
ODS SELECT ParameterEstimates OddsRatios;
title "Model 1, Food Desert Status with Sudden Death";
proc logistic data=FD_reg descending;
  class desert (ref="No") / param=ref ;
  model sudden = desert ;
run;

*Model 2 adding on age, gender, race, marital status gender, vet status;
ODS SELECT ParameterEstimates OddsRatios;
title1 "Model 2, Food Desert with Sudden Death";
title2 "Adjusted by gender and race";
proc logistic data=FD_reg descending;
  class desert (ref="No") gender (ref="Male") race (ref="White") / param=ref ;
  model sudden = desert gender race;
run;
title1; title2;

*Model 3 adding on substance/mental status;
ODS SELECT ParameterEstimates OddsRatios;
title3 "Model 3, Food Desert Status with Sudden Death";
title4 "Adjusted by gender, race, and disease level";
proc logistic data=FD_reg descending;
  class desert (ref="No") gender (ref="Male") race (ref="White")
  CHF (ref="No") CAD(ref="No") diabetes (ref="No") HTN (ref="No") 
  dyslip (ref="No") / param=ref ;
  model sudden = desert gender race CHF CAD diabetes HTN dyslip;
run;
title3; title4;

ODS PDF CLOSE;
