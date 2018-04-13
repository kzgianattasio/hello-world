libname adams 'F:\power\HRS\ADAMS Wave A';
libname atrk 'F:\power\HRS\ADAMS CrossWave';
libname x 'F:\power\HRS\DerivedData\AD_Disparities_AlgorithmDev\Data 2018_0105'; /*derived hrs files*/
libname hrs 'F:\power\HRS\HRS data (raw)\SAS datasets'; /*raw hrs files, including Hurd probabilities*/
libname rand 'F:\power\HRS\RAND_HRS\sasdata';
libname xold 'F:\power\HRS\DerivedData\AD_Disparities_AlgorithmDev';
options fmtsearch = (rand.formats);

/*********************************************************************************************
* Created 2018.03.02
*
* 1. merge in ADAMS weights (sensitivity analysis 1)
* 2. merge in alternative dementia-outcome with 4 most prevalent types (sensitivyt analysis 4)
*
**********************************************************************************************/
*set date for output label;
%let dt=2018_0302;
*set date for last output labels;
%let pdt = 2018_0117;

/**********************************************
*											  *
*	1. Merge in weights from master file	  *
*											  *
***********************************************/
*ADAMS weights (3 versions);
data awgt; set x.master_ad_&pdt (keep = hhid pn adams_awgt /*for wave A cross-sect analysis*/
										adams_d_longwgt); /*for wave A - wave D longitudinal analysis*/
	proc sort; by hhid pn;
run;

%macro ds(s);
proc sort data=x.HRS&s._&dt;
	by hhid pn;
run;

data x.HRS&s._&dt;
	merge x.HRS&s._&dt (in = a) awgt; 
	by hhid pn ; 
	if a; 
run;
%mend;
%ds(t) %ds(v) 


/**********************************************
*											  *
*	1. Merge in alt. Dementia Dx			  *
*											  *
***********************************************/
libname adamsa 'F:\power\HRS\ADAMS Wave A';
libname adamsb 'F:\power\HRS\ADAMS Wave B';
libname adamsc 'F:\power\HRS\ADAMS Wave C';
libname adamsd 'F:\power\HRS\ADAMS Wave D';

%macro ext(w);
data wave&w; set adams&w..adams1&w.d_r (keep = hhid pn &w.DFDX1);
	if &w.DFDX1 in (1, 2, 3, 4, 13, 18, 32) then dement4 = 1; else dement4 = 0;
	ADAMSwave = "&w";
	proc sort; by hhid pn;

	label dement4 = "ADAMS classification for 4 most common dementias (AD, Vascular, FTD, Lewy)";
run;

proc freq data=wave&w; tables dement4 ADAMSwave; run;
%mend;
%ext(a) %ext(b) %ext(c) %ext(d)


data HRSt; 
	merge x.HRSt_&dt (in = a) waveA (keep = hhid pn dement4);
	by hhid pn;
	if a;

/*	proc freq; tables dement dement4;*/
run;

data HRSv; set x.HRSv_&pdt; 
	proc sort;
	by hhid pn ADAMSwave;
run;

%macro dem(w);
data HRSv; 
	merge HRSv (in = a) wave&w (keep = hhid pn dement4 ADAMSwave);
	by  hhid pn ADAMSwave;
	if a;
run;
%mend;
%dem(b) %dem(c) %dem(d)

data x.HRSt_&dt; set HRSt; run;
data x.HRSv_&dt; set HRSv; run;
