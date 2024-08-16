%LET job=SUDDEN_vet;
%LET outdir=/home/u49518639/SUDDEN;
proc printto log="&outdir/Logs/&job.log" new; run;

*********************************************************************
*  Assignment:    SUDDEN_PILOT_DEMO                                        
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
*  Input:         Control_VeteranStatus, Pilot_VeternStatus
*
*  Output:        PDF file.
*                                                                    
********************************************************************;

OPTIONS NODATE MPRINT MERGENOBY=WARN VARINITCHK=WARN NOFULLSTIMER;
ODS _ALL_ CLOSE;
FOOTNOTE "Job &job._&onyen run on &sysdate at &systime";

options orientation=landscape;
ODS PDF FILE="&outdir/Output/&job._regression_output.pdf" STYLE=JOURNAL;

ODS TRACE ON;
ODS GRAPHICS ON;

********************************************
Pilot Data Set;

data merge_pilot_vet;
	set Pilot_clean2 Control_clean2;
run;

* Multivariable logistic regression;
data multi_pilot;
	set work.merge_pilot_vet;
	length numVet 8.;
	if vet = "Non-Veteran" then numVet = 0;
	if vet = "Veteran" then numVet = 1;
run;


PROC FREQ DATA=multi_pilot noprint;
	TABLES numVet*(Gender Race marry substance anx_dep) /NOROW NOCOL NOPERCENT;
RUN;

*Model 1 vet status;
ODS SELECT ParameterEstimates OddsRatios;
title "Model 1, Veteran Status with Sudden Death";
proc logistic data=multi_pilot descending;
  class numVet (ref="0") / param=ref ;
  model sudden = numVet ;
run;

*Model 2 adding on age, gender, race, marital status gender, vet status;
ODS SELECT ParameterEstimates OddsRatios;
title1 "Model 2, Veteran Status with Sudden Death";
title2 "Adjusted by age, gender, race, and marital status";
proc logistic data=multi_pilot descending;
  class numVet (ref="0") Gender (ref="Male") Race (ref="White") marry (ref="Married")/ param=ref ;
  model sudden = numVet age_num Gender Race marry;
run;
title1; title2;

*Model 3 adding on substance/mental status;
ODS SELECT ParameterEstimates OddsRatios;
title3 "Model 3, Veteran Status with Sudden Death";
title4 "Adjusted by age, gender, race, and marital status, substance abuse and mental health";
proc logistic data=multi_pilot descending;
  class numVet (ref="0") Gender (ref="Male") Race (ref="White") marry (ref="Married") substance (ref="0") anx_dep (ref="0")/ param=ref ;
  model sudden = numVet age_num Gender Race marry substance anx_dep;
run;
title3; title4;

ODS PDF CLOSE;
