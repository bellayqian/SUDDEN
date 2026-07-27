/*********************************************************************
   Adapted from: SUDDEN_vet_regression.sas (Yunzhe Qian / Bella)

   Description:   Demographic and Clinical Characteristics of Victims of
                  Sudden Death by US Military Veteran Status (Wake County, NC)

   This bundle keeps the merge of the cleaned pilot and control tables,
   the numVet derivation, the PROC FREQ cross-tabs and all three of the
   PROC LOGISTIC models (unadjusted, then adjusted for age/gender/race/marital
   status, then adding substance abuse and anxiety/depression) exactly as
   written. The proc printto log redirect and the ODS PDF file targeting a
   private output folder are dropped so the listing is returned directly; the
   cleaned input tables come from the autoexec.
********************************************************************/

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
