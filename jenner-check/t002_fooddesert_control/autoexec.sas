/* cap input rows for the captured run */
options obs=100;

/* Bella's FoodDesert_CONTROL.sas imports Kothari_Control2.xlsx into the `sudden`
   libname. That workbook is private study data and is not in the repo, so this
   autoexec provides a small synthetic sudden.Control_FoodDesert table with exactly
   the columns the cleaning DATA step reads: the coded race_controlsd / sex_controlsd
   / food_desert values and the chf/dm/dyslip/htn/cad comorbidity flags (plus the id
   and the address-family columns her step drops). Her recoding logic runs unchanged. */
libname sudden (work);

data sudden.Control_FoodDesert;
  infile datalines dlm='|' dsd truncover;
  length address $20 city $20 state $2;
  input id_controlsd race_controlsd sex_controlsd food_desert
        chf_controlsd dm_controlsd dyslip_controlsd htn_controlsd cad_controlsd
        address $ city $ zip_code state $ census_tract_id;
  datalines;
1|1|1|2|1|0|1|1|0|12 Oak St|Raleigh|27601|NC|101
2|2|0|0|0|1|0|1|1|4 Elm Ave|Charlotte|28202|NC|202
3|1|1|1|0|0|0|0|0|9 Pine Rd|Greensboro|27401|NC|303
4|3|0|2|1|1|1|1|1|7 Maple Dr|Mount Airy|27030|NC|404
5|2|1|0|0|0|0|1|0|2 Birch Ln|Raleigh|27604|NC|505
6|1|0|2|1|0|0|0|0|8 Cedar Ct|Charlotte|28205|NC|606
7|4|1|1|0|1|1|1|1|3 Ash Way|Greensboro|27405|NC|707
8|2|0|2|1|0|1|0|0|5 Walnut St|Mount Airy|27030|NC|808
9|1|1|0|0|0|0|0|0|6 Spruce Ave|Raleigh|27610|NC|909
10|2|1|2|1|1|0|1|1|1 Poplar Rd|Charlotte|28210|NC|1010
;
run;
