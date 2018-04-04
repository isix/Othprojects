%macro wt_auc(data = , score = , y = , weight = );
***********************************************************;
* THE MACRO IS TO EVALUATE THE SEPARATION POWER OF A      *;
* SCORECARD WITH WEIGHTS                                  *;
* ------------------------------------------------------- *;
* PARAMETERS:                                             *;
*  DATA  : INPUT DATASET                                  *;
*  SCORE : SCORECARD VARIABLE                             *;
*  Y     : RESPONSE VARIABLE IN (0, 1)                    *;
*  WEIGHT: WEIGHT VARIABLE WITH POSITIVE VALUES           *; 
* ------------------------------------------------------- *;
* OUTPUTS:                                                *;
*  A SUMMARY TABLE WITH KS AND AUC STATISTICS             *;
* ------------------------------------------------------- *;
* CONTACT:                                                *;
*  WENSUI.LIU@53.COM                                      *;
***********************************************************;
options nocenter nonumber nodate mprint mlogic symbolgen
        orientation = landscape ls = 125 formchar = "|----|+|---+=|-/\<>*";
 
data _tmp1 (keep = &score &y &weight);
  set &data;
  where &score ~= . and &y in (0, 1) and &weight >= 0;
run;
 
*** CAPTURE THE DIRECTION OF SCORE ***;
ods listing close;
ods output spearmancorr = _cor;
proc corr data = _tmp1 spearman;
  var &y;
  with &score;
run;
ods listing;
 
data _null_;
  set _cor;
  if &y >= 0 then do;
    call symput('desc', 'descending');
  end;
  else do;
    call symput('desc', ' ');
  end;
run;
 
proc sql noprint;
create table
  _tmp2 as    
select
  &score                                         as _scr,
  sum(&y)                                        as _bcnt,
  count(*)                                       as _cnt,
  sum(case when &y = 1 then &weight else 0 end)  as _bwt,
  sum(case when &y = 0 then &weight else 0 end)  as _gwt
from
  _tmp1
group by
  &score;
 
select
  sum(_bwt) into :bsum
from
  _tmp2;
   
select
  sum(_gwt) into :gsum
from
  _tmp2;
 
select
  sum(_cnt) into :cnt
from
  _tmp2;    
quit;
%put &cnt;
 
proc sort data = _tmp2;
  by &desc _scr;
run;
 
data _tmp3;
  set _tmp2;
  by &desc _scr;
  retain _gcum _bcum _cntcum;
  _gcum + _gwt;
  _bcum + _bwt;
  _cntcum + _cnt;
  _gpct = _gcum / &gsum;
  _bpct = _bcum / &bsum;
  _ks   = abs(_gpct - _bpct) * 100;
  _rank = int(_cntcum / ceil(&cnt / 10)) + 1;
run;
 
proc sort data = _tmp3 sortsize = max;
  by _gpct _bpct;
run;
 
data _tmp4;
  set _tmp3;
  by _gpct _bpct;
  if last._gpct then do;
    _idx + 1;
    output;
  end;
run;
 
proc sql noprint;
create table
  _tmp5 as
select
  a._gcum as gcum,
  (b._gpct - a._gpct) * (b._bpct + a._bpct) / 2 as dx
from
  _tmp4 as a, _tmp4 as b
where
  a._idx + 1 = b._idx;
 
select
  sum(dx) format = 15.8 into :AUC
from
  _tmp5;
 
select
  max(_ks) format = 15.8 into :KS_STAT
from
  _tmp3;
 
select
  _scr format = 6.2 into :KS_SCORE
from
  _tmp3
where
  _ks = (select max(_ks) from _tmp3);
 
create table
  _tmp6 as
select
  _rank                       as rank,
  min(_scr)                   as min_scr,
  max(_scr)                   as max_scr,
  sum(_cnt)                   as cnt,
  sum(_gwt + _bwt)            as wt,
  sum(_gwt)                   as gwt,
  sum(_bwt)                   as bwt,
  sum(_bwt) / calculated wt   as bad_rate
from
  _tmp3
group by
  _rank;    
quit;  
 
proc report data = _last_ spacing = 1 split = "/" headline nowd;
  column("GOOD BAD SEPARATION REPORT FOR %upcase(%trim(&score)) IN DATA %upcase(%trim(&data))/
          MAXIMUM KS = %trim(&ks_stat) AT SCORE POINT %trim(&ks_score) and AUC STATISTICS = %trim(&auc)/ /"
          rank min_scr max_scr cnt wt gwt bwt bad_rate);
  define rank       / noprint order order = data;
  define min_scr    / "MIN/SCORE"             width = 10 format = 9.2        analysis min center;
  define max_scr    / "MAX/SCORE"             width = 10 format = 9.2        analysis max center;
  define cnt        / "RAW/COUNT"             width = 10 format = comma9.    analysis sum;
  define wt         / "WEIGHTED/SUM"          width = 15 format = comma14.2  analysis sum;
  define gwt        / "WEIGHTED/GOODS"        width = 15 format = comma14.2  analysis sum;
  define bwt        / "WEIGHTED/BADS"         width = 15 format = comma14.2  analysis sum;
  define bad_rate   / "BAD/RATE"              width = 10 format = percent9.2 order center;
  rbreak after / summarize dol skip;
run;
 
proc datasets library = work nolist;
  delete _tmp: / memtype = data;
run;
quit;
 
***********************************************************;
*                     END OF THE MACRO                    *;
***********************************************************;
%mend wt_auc;

*** TEST CASE OF FRACTIONAL WEIGHTS ***;
data one;
  set data.accepts;
  weight = ranuni(1);
run;
 
%wt_auc(data = one, score = bureau_score, y = bad, weight = weight);
/*
                   GOOD BAD SEPARATION REPORT FOR BUREAU_SCORE IN DATA ONE
       MAXIMUM KS = 34.89711721 AT SCORE POINT 678.00 and AUC STATISTICS = 0.73521009
 
    MIN        MAX            RAW        WEIGHTED        WEIGHTED        WEIGHTED    BAD
   SCORE      SCORE         COUNT             SUM           GOODS            BADS    RATE
 -------------------------------------------------------------------------------------------
    443.00     619.00         539          276.29          153.16          123.13   44.56%
    620.00     644.00         551          273.89          175.00           98.89   36.11%
    645.00     660.00         544          263.06          176.88           86.18   32.76%
    661.00     676.00         555          277.26          219.88           57.38   20.70%
    677.00     692.00         572          287.45          230.41           57.04   19.84%
    693.00     707.00         510          251.51          208.25           43.26   17.20%
    708.00     724.00         576          276.31          243.89           32.42   11.73%
    725.00     746.00         566          285.53          262.73           22.80    7.98%
    747.00     772.00         563          285.58          268.95           16.62    5.82%
    773.00     848.00         546          272.40          264.34            8.06    2.96%
 ========== ========== ========== =============== =============== ===============
    443.00     848.00       5,522        2,749.28        2,203.49          545.79
*/

*** TEST CASE OF INTEGER WEIGHTS ***;
data two;
  set data.accepts;
  weight = rand("poisson", 20);
run;
 
%wt_auc(data = two, score = bureau_score, y = bad, weight = weight);
/*
                   GOOD BAD SEPARATION REPORT FOR BUREAU_SCORE IN DATA TWO
       MAXIMUM KS = 35.58884479 AT SCORE POINT 679.00 and AUC STATISTICS = 0.73725030
 
    MIN        MAX            RAW        WEIGHTED        WEIGHTED        WEIGHTED    BAD
   SCORE      SCORE         COUNT             SUM           GOODS            BADS    RATE
 -------------------------------------------------------------------------------------------
    443.00     619.00         539       10,753.00        6,023.00        4,730.00   43.99%
    620.00     644.00         551       11,019.00        6,897.00        4,122.00   37.41%
    645.00     660.00         544       10,917.00        7,479.00        3,438.00   31.49%
    661.00     676.00         555       11,168.00        8,664.00        2,504.00   22.42%
    677.00     692.00         572       11,525.00        9,283.00        2,242.00   19.45%
    693.00     707.00         510       10,226.00        8,594.00        1,632.00   15.96%
    708.00     724.00         576       11,497.00       10,117.00        1,380.00   12.00%
    725.00     746.00         566       11,331.00       10,453.00          878.00    7.75%
    747.00     772.00         563       11,282.00       10,636.00          646.00    5.73%
    773.00     848.00         546       10,893.00       10,598.00          295.00    2.71%
 ========== ========== ========== =============== =============== ===============
    443.00     848.00       5,522      110,611.00       88,744.00       21,867.00
*/