libname x 'F:\power\HRS\DerivedData\AD_Disparities_AlgorithmDev\Data 2018_0105'; /*derived hrs files*/
libname rand 'F:\power\HRS\RAND_HRS\sasdata';
options fmtsearch = (rand.formats);

/*********************************************************************************************
* Created 2018.03.02 
*
* Sensitivity analysis 2
*	- Refit regressions using standardized training data 
*		- For CRIMMINS: only use Model 3 for proxy
*		- Re-classify algorithmic dementia status
*
*   - Create second set of HRSt and HRSv datasets
**********************************************************************************************/
*set date for output label;
%let dt=2018_0302;
*set previous date output label;
%let pdt = 2018_0302;

data pred; set  x.Master_adpred_wide_&pdt; 
nowgt = 1; *set nowgt variable to run unweighted regressions;
run;

/******************************
*							  *
*	1.	WU					  *
*							  *
******************************/
%macro wu(wgt, s);
	proc surveylogistic data=pred;
		model dement_a (ref = '0') = iword_a dword_a tics13_a iqcode5_a pr_memsc5_a proxy_a hrs_age70_a male black iwordsq_a tics13sq_a dword_m_a iqcode5_m_a;
		weight &wgt;
		where commsampfinal_a = 1; *regress using common sample only;
		ods output ParameterEstimates = wu;
	run;	

	/*save regression coefficients to drive*/
	proc export data=wu
	outfile = "F:\power\HRS\Projects\Ad_Disparities_AlgorithmDev\SAS Outputs\Re-estimated algorithms_&dt..xlsx"
	dbms = xlsx;
	sheet = "wu_&wgt";
	run;

	/*create wide version for merge & predict*/
	data wu; set wu (keep = variable estimate); run;
	proc transpose data=wu out = wu; run;
	data wu; set wu (drop = _name_);
		rename col1 = intercept;
		rename col2 = c_iword;
		rename col3 = c_dword;
		rename col4 = c_tics13;
		rename col5 = c_iqcode5;
		rename col6 = c_pr_memsc5;
		rename col7 = c_proxy;
		rename col8 = c_hrs_age70;
		rename col9 = c_male;
		rename col10 = c_black;
		rename col11 = c_iwordsq;
		rename col12 = c_tics13sq;
		rename col13 = c_dword_m;
		rename col14 = c_iqcode5_m;

		nowgt = 1; /*created for merging purposes*/
	run;

	data pred; merge pred wu; by nowgt; run;

	/*compute predicted probabilities and assign dementia classification using 0.5 threshold for each wave*/ 
		%macro pred (w);
		data pred; set pred;
			wu_re_p_&wgt._&w = exp(intercept + c_iword*iword_&w + c_dword*dword_&w + c_tics13*tics13_&w + c_iqcode5*iqcode5_&w + c_pr_memsc5*pr_memsc5_&w + c_proxy*proxy_&w + c_hrs_age70*hrs_age70_&w +c_male*male + c_black*black + c_iwordsq*iwordsq_&w + c_tics13sq*tics13sq_&w + c_dword_m*dword_m_&w +c_iqcode5_m*iqcode5_m_&w)/
							   (1 + exp(intercept + c_iword*iword_&w + c_dword*dword_&w + c_tics13*tics13_&w + c_iqcode5*iqcode5_&w + c_pr_memsc5*pr_memsc5_&w + c_proxy*proxy_&w + c_hrs_age70*hrs_age70_&w +c_male*male + c_black*black + c_iwordsq*iwordsq_&w + c_tics13sq*tics13sq_&w + c_dword_m*dword_m_&w +c_iqcode5_m*iqcode5_m_&w));
			if wu_re_p_&wgt._&w > 0.5 then wu_re_dem_&wgt._&w = 1;
			else if wu_re_p_&wgt._&w NE . then wu_re_dem_&wgt._&w = 0;
		run;

		proc freq data=pred; tables wu_re_dem_&wgt._&w*dement_&w; where commsampfinal_&w = 1; run;
		%mend;
		%pred(A) %pred(B) %pred(C) %pred(D)

	/*drop coefficients*/
	data pred; set pred (drop =  intercept c_iword c_dword c_tics13 c_iqcode5 c_pr_memsc5 c_proxy c_hrs_age70 c_male c_black c_iwordsq c_tics13sq c_dword_m c_iqcode5_m); run; 
%mend;
%wu(nowgt, 1) 
/*%wu(adams_awgt) */
%wu(adams_d_longwgt)

/******************************
*							  *
*	2.	CRIMMINS			  *
*							  *
******************************/
%macro crim (wgt);
*SELF RESPONSE;
	proc surveylogistic data=pred;
		class cogn_a (ref = "1") / param = ref;
		model cogn_a = hrs_age_a female lowedu_crim midedu_crim iword_a dword_a serial7_a ticshrs_a dress_a bath_a eat_a money_a phone_a / link = glogit ;
		weight &wgt;
		ods output ParameterEstimates = crim;
		where proxy_a = 0 and commsampfinal_a = 1;
	run;	

	/*save coefficients to drive*/
	proc export data=crim
	outfile = "F:\power\HRS\Projects\Ad_Disparities_AlgorithmDev\SAS Outputs\Re-estimated algorithms_&dt..xlsx"
	dbms = xlsx;
	sheet = "crim_&wgt";
	run;

	/*create wide version for merge & predict*/
	data crim; set crim (keep = variable estimate Response);
	if Response NE 3 then delete;
	drop Response;
	run;
	proc transpose data=crim out = crim; run;

	data crim; set crim (drop = _Name_);
		rename col1 = intercept;
		rename col2 = c_hrs_age;
		rename col3 = c_female;
		rename col4 = c_lowedu_crim;
		rename col5 = c_midedu_crim;
		rename col6 = c_iword;
		rename col7 = c_dword;
		rename col8 = c_serial7;
		rename col9 = c_ticshrs;
		rename col10 = c_dress;
		rename col11 = c_bath;
		rename col12 = c_eat;
		rename col13 = c_money;
		rename col14 = c_phone;

		nowgt = 1; *for merging purposes;
	run;

	/*merge coefficients - predict probability and assign dementia classification using SIMPLE 0.5 CUTOFF RULE (disregard CIND)*/
	data pred; merge pred crim; by nowgt; run;

	%macro pred(w);
	data pred; set pred;
		crim_re_ps_&wgt._&w = exp(intercept + c_hrs_age*hrs_age_&w + c_female*female + c_lowedu_crim*lowedu_crim + c_midedu_crim*midedu_crim + c_iword*iword_&w + c_dword*dword_&w + c_serial7*serial7_&w + c_ticshrs*ticshrs_&w + c_dress*dress_&w + c_bath*bath_&w + c_eat*eat_&w + c_money*money_&w + c_phone*phone_&w)/
						 	  (1 + exp(intercept + c_hrs_age*hrs_age_&w + c_female*female + c_lowedu_crim*lowedu_crim + c_midedu_crim*midedu_crim + c_iword*iword_&w + c_dword*dword_&w + c_serial7*serial7_&w + c_ticshrs*ticshrs_&w + c_dress*dress_&w + c_bath*bath_&w + c_eat*eat_&w + c_money*money_&w + c_phone*phone_&w));
		if crim_re_ps_&wgt._&w > 0.5 then crim_re_dems_&wgt._&w = 1;
		else if crim_re_ps_&wgt._&w NE . then crim_re_dems_&wgt._&w = 0;
	run;

	proc freq data=pred; tables crim_re_dems_&wgt._&w * dement_a; where commsampfinal_&w = 1 and proxy_&w = 0; run;
	%mend;
	%pred(A) %pred(B) %pred(C) %pred(D)

	/*drop coefficients*/
	data pred; set pred (drop = intercept c_hrs_age c_female c_lowedu_crim c_midedu_crim c_iword c_dword c_serial7 c_ticshrs c_dress c_bath c_eat c_money c_phone); run;

*PROXY;
	/*model 3 - No Jorm*/
	proc surveylogistic data=pred;
		model dement_a (ref = "0") = pr_memsc1_a iadl5_a iwercog_a / link = glogit ;
		weight &wgt;
		ods output ParameterEstimates = crim;
		where proxy_a = 1 and commsampfinal_a = 1;
	run;	

	/*save coefficients to drive*/
	proc export data=crim
	outfile = "F:\power\HRS\Projects\Ad_Disparities_AlgorithmDev\SAS Outputs\Re-estimated algorithms_&dt..xlsx"
	dbms = xlsx;
	sheet = "crim_p1_&wgt";
	run;

	/*transpose to wide for merge and predict*/
	data crim; set crim (keep = variable estimate); run;
	proc transpose data=crim out = crim; run;
	data crim; set crim (drop = _Name_);
		rename col1 = intercept;
		rename col2 = c_pr_memsc1;
		rename col3 = c_iadl5;
		rename col4 = c_iwercog;

		nowgt = 1; *for merging purposes;
	run;

	data pred; merge pred crim; by nowgt; run;
	
	/*predict dementia probability and assign status using 0.5 threshold at each wave*/
		%macro pred(w);
		data pred; set pred;
			crim_re_pp_&wgt._&w = exp(intercept + c_pr_memsc1*pr_memsc1_&w + c_iadl5*iadl5_&w + c_iwercog*iwercog_&w)/
							   (1 + exp(intercept + c_pr_memsc1*pr_memsc1_&w + c_iadl5*iadl5_&w + c_iwercog*iwercog_&w));
			if crim_re_pp_&wgt._&w > 0.5 then crim_re_demp_&wgt._&w = 1;
			else if crim_re_pp_&wgt._&w NE . then crim_re_demp_&wgt._&w = 0;
		run;

		proc freq; tables  crim_re_demp_&wgt._&w*dement_&w; where commsampfinal_&w = 1 and proxy_&w = 1; run;
		%mend;
		%pred(A) %pred(B) %pred(C) %pred(D)

	/*drop coefficients*/
	data pred; set pred (drop = intercept c_pr_memsc1 c_iadl5 c_iwercog); run;

	/*create composite dementia status variables combining self-response and proxy*/
		%macro final(w);
		data pred; set pred;
			if crim_re_dems_&wgt._&w NE . then crim_re_dem_&wgt._&w = crim_re_dems_&wgt._&w;
			if crim_re_demp_&wgt._&w NE . then crim_re_dem_&wgt._&w = crim_re_demp_&wgt._&w; 

			if crim_re_ps_&wgt._&w NE . then crim_re_p_&wgt._&w = crim_re_ps_&wgt._&w;
			if crim_re_pp_&wgt._&w NE . then crim_re_p_&wgt._&w = crim_re_pp_&wgt._&w;

			proc freq; tables crim_re_dem_&wgt._&w * dement_&w; where commsampfinal_&w = 1; run;
		%mend;
		%final(A) %final(B) %final(C) %final(D)
%mend;
%crim(nowgt) 
/*%crim(adams_awgt);*/
%crim(adams_d_longwgt);



/******************************
*							  *
*		3.	HURD			  *
*							  *
******************************/
*From Table S2/S3: dementia = 1, CIND = 2, normal = 3 - First create alt. cognition variable;
data pred; set pred; 
	if cogn_a = 2 then cognr_a = 2;
	if cogn_a = 1 then cognr_a = 3;
	if cogn_a = 3 then cognr_a = 1;
run;

*convert hagecat to individual dummies;
%macro age(w);
	data pred; set pred;
		if hagecat_&w = 2 then hagecat75_&w = 1; else if hagecat_&w NE . then hagecat75_&w = 0;
		if hagecat_&w = 3 then hagecat80_&w = 1; else if hagecat_&w NE . then hagecat80_&w = 0;
		if hagecat_&w = 4 then hagecat85_&w = 1; else if hagecat_&w NE . then hagecat85_&w = 0;
		if hagecat_&w = 5 then hagecat90_&w = 1; else if hagecat_&w NE . then hagecat90_&w = 0;

/*		proc freq; tables hagecat_&w hagecat75_&w hagecat80_&w hagecat85_&w hagecat90_&w;*/
	run;
%mend;
%age(a) %age(b) %age(c) %age(d)

%macro hurd(wgt);

*SELF-RESPONSE;
	proc qlim data=pred ;
		model cognr_a = hagecat75_a hagecat80_a hagecat85_a hagecat90_a midedu_hurd highedu_hurd female adl5_a iadl5_a adl5ch_a iadl5ch_a 
						dates_a ticscount1_a serial7_a scis_a cact_a pres_a iword_a dword_a 
						datesch_a ticscount1ch_a serial7ch_a scisch_a cactch_a presch_a iwordch_a dwordch_a / discrete(d=normal) limit1=varying;
		weight &wgt;
		where proxy_a = 0 and commsampfinal_a = 1;
		ods output ParameterEstimates = hurd;
	run;

	/*save coefficients to drive*/
	proc export data=hurd
	outfile = "F:\power\HRS\Projects\Ad_Disparities_AlgorithmDev\SAS Outputs\Re-estimated algorithms_&dt..xlsx"
	dbms = xlsx;
	sheet = "hurds_&wgt";
	run;
	
	/*reshape to wide for merge and predict*/
 	data hurd; set hurd (keep = parameter estimate); run;
	proc transpose data=hurd out = hurd; run;
	data hurd; set hurd (drop = _NAME_);
		rename col1 = c_hagecat75;
		rename col2 = c_hagecat80;
		rename col3 = c_hagecat85;
		rename col4 = c_hagecat90;
		rename col5 = c_midedu_hurd;
		rename col6 = c_highedu_hurd;
		rename col7 = c_female;
		rename col8 = c_adl5;
		rename col9 = c_iadl5;
		rename col10 = c_adl5ch;
		rename col11 = c_iadl5ch;
		rename col12 = c_dates;
		rename col13 = c_ticscount1;
		rename col14 = c_serial7;
		rename col15 = c_scis;
		rename col16 = c_cact;
		rename col17 = c_pres;
		rename col18 = c_iword;
		rename col19 = c_dword;
		rename col20 = c_datesch;
		rename col21 = c_ticscount1ch;
		rename col22 = c_serial7ch;
		rename col23 = c_scisch;
		rename col24 = c_cactch;
		rename col25 = c_presch;
		rename col26 = c_iwordch;
		rename col27 = c_dwordch;
		rename col28 = cut1;
		rename col29 = cut2;

		nowgt = 1;
	run;

	data pred; merge pred hurd; by nowgt; run;

	/*calculated dementia probability and assign classification using 0.5 threshold for each wave*/
	%macro pred(w);
	data pred; set pred;
		hurd_re_ps_&wgt._&w = probnorm(cut1 - (c_hagecat75*hagecat75_&w + c_hagecat80*hagecat80_&w + c_hagecat85*hagecat85_&w + c_hagecat90*hagecat90_&w
										+ c_midedu_hurd*midedu_hurd + c_highedu_hurd*highedu_hurd + c_female*female + c_adl5*adl5_&w + c_iadl5*iadl5_&w + c_adl5ch*adl5ch_&w + c_iadl5ch*iadl5ch_&w
										+ c_dates*dates_&w + c_ticscount1*ticscount1_&w + c_serial7*serial7_&w + c_scis*scis_&w + c_cact*cact_&w + c_pres*pres_&w + c_iword*iword_&w + c_dword*dword_&w
										+ c_datesch*datesch_&w + c_ticscount1ch*ticscount1ch_&w + c_serial7ch*serial7ch_&w + c_scisch*scisch_&w + c_cactch*cactch_&w + c_presch*presch_&w + c_iwordch*iwordch_&w + c_dwordch*dwordch_&w));

		if hurd_re_ps_&wgt._&w > 0.5 then hurd_re_dems_&wgt._&w = 1;
		else if hurd_re_ps_&wgt._&w NE . then hurd_re_dems_&wgt._&w = 0;

		proc freq; tables hurd_re_dems_&wgt._&w*dement_&w; where commsampfinal_&w = 1; run;
	run;
	proc means; var hurd_re_p_&wgt._&w; run;
	%mend;
	%pred(a) %pred (b) %pred(c) %pred(d) c

	/*drop coefficients*/
	data pred; set pred; drop c_hagecat75 c_hagecat80 c_hagecat85 c_hagecat90 c_midedu_hurd c_highedu_hurd c_female c_adl5 c_iadl5 c_adl5ch c_iadl5ch
								c_dates c_ticscount1 c_serial7 c_scis c_cact c_pres c_iword c_dword c_datesch c_ticscount1ch c_serial7ch c_scisch
								c_cactch c_presch c_iwordch c_dwordch cut1 cut2; 
	run;

*PROXY;
	proc qlim data=pred ;
		model cognr_a = hagecat75_a hagecat80_a hagecat85_a hagecat90_a midedu_hurd highedu_hurd female adl5_a iadl5_a adl5ch_a iadl5ch_a 
						iqcode_a proxylag_a iqcodech_a dateslag_a serial7lag_a preslag_a iwordlag_a dwordlag_a  / discrete(d=normal) limit1=varying;
		weight &wgt;
		where proxy_a = 1 and commsampfinal_a = 1;
		ods output ParameterEstimates = hurd;
	run;

	/*save coefficients to drive*/
	proc export data=hurd
	outfile = "F:\power\HRS\Projects\Ad_Disparities_AlgorithmDev\SAS Outputs\Re-estimated algorithms_&dt..xlsx"
	dbms = xlsx;
	sheet = "hurdp_&wgt";
	run;
	
	/*reshape to wide for merge and predict*/
 	data hurd; set hurd (keep = parameter estimate); run;
	proc transpose data=hurd out = hurd; run;

	data hurd; set hurd (drop = _NAME_);
		rename col1 = c_hagecat75;
		rename col2 = c_hagecat80;
		rename col3 = c_hagecat85;
		rename col4 = c_hagecat90;
		rename col5 = c_midedu_hurd;
		rename col6 = c_highedu_hurd;
		rename col7 = c_female;
		rename col8 = c_adl5;
		rename col9 = c_iadl5;
		rename col10 = c_adl5ch;
		rename col11 = c_iadl5ch;
		rename col12 = c_iqcode;
		rename col13 = c_proxylag;
		rename col14 = c_iqcodech;
		rename col15 = c_dateslag;
		rename col16 = c_serial7lag;
		rename col17 = c_preslag;
		rename col18 = c_iwordlag;
		rename col19 = c_dwordlag;
		rename col20 = cut1; 
		rename col21 = cut2;

		nowgt = 1;
	run;

	data pred; merge pred hurd; by nowgt; run;

	/*compute dementia probability and assign classification using 0.5 threshold for each wave*/
	%macro pred(w);
	data pred; set pred;
		hurd_re_pp_&wgt._&w = probnorm(cut1 - (c_hagecat75*hagecat75_&w + c_hagecat80*hagecat80_&w + c_hagecat85*hagecat85_&w + c_hagecat90*hagecat90_&w
										+ c_midedu_hurd*midedu_hurd + c_highedu_hurd*highedu_hurd + c_female*female + c_adl5*adl5_&w + c_iadl5*iadl5_&w + c_adl5ch*adl5ch_&w + c_iadl5ch*iadl5ch_&w
										+ c_iqcode*iqcode_&w + c_proxylag*proxylag_&w + c_iqcodech*iqcodech_&w 
										+ c_dateslag*dateslag_&w + c_serial7lag*serial7lag_&w + c_preslag*preslag_&w + c_iwordlag*iwordlag_&w + c_dwordlag*dwordlag_&w));

		if hurd_re_pp_&wgt._&w > 0.5 then hurd_re_demp_&wgt._&w = 1;
		else if hurd_re_pp_&wgt._&w NE . then hurd_re_demp_&wgt._&w = 0;

		proc freq; tables hurd_re_demp_&wgt._&w*dement_&w; where commsampfinal_&w = 1; run;
	run;
	proc means; var hurd_re_pp_&wgt._&w; run;
	%mend;
	%pred(a) %pred (b) %pred(c) %pred(d)

	/*drop coefficients*/
	data pred; set pred; drop c_hagecat75 c_hagecat80 c_hagecat85 c_hagecat90 c_midedu_hurd c_highedu_hurd c_female c_adl5 c_iadl5 c_adl5ch c_iadl5ch
								c_iqcode c_proxylag c_iqcodech c_dateslag c_serial7lag c_preslag c_iwordlag c_dwordlag cut1 cut2; 
	run;

	/*create  dementia status classification variable combining self-response and proxy*/
	%macro final(w);
	data pred; set pred;
		if hurd_re_dems_&wgt._&w NE . then hurd_re_dem_&wgt._&w = hurd_re_dems_&wgt._&w;
		if hurd_re_demp_&wgt._&w NE . then hurd_re_dem_&wgt._&w = hurd_re_demp_&wgt._&w; 

		if hurd_re_ps_&wgt._&w NE . then hurd_re_p_&wgt._&w = hurd_re_ps_&wgt._&w;
		if hurd_re_pp_&wgt._&w NE . then hurd_re_p_&wgt._&w = hurd_re_pp_&wgt._&w;

	proc freq; tables hurd_re_dem_&wgt._&w * dement_&w; where commsampfinal_&w = 1; run;
	%mend;
	%final(A) %final(B) %final(C) %final(D)
%mend;
%hurd(nowgt)
/*%crim(adams_awgt);*/
%hurd(adams_d_longwgt);

%macro final(w, wgt);
proc freq data=pred; tables hurd_re_dem_&wgt._&w * dement_&w; where commsampfinal_&w = 1; run;
%mend;
%final(a, adams_d_longwgt)


/*****************************************************
*	Create HRSt and HRSv datasets
*****************************************************/

* create exclude flag;
data pred; set pred;
	if bfresult = 1 and dement_a = 1 then exclude_b = 1;
	if dfresult = 1 and dement_c = 1 then exclude_d = 1;
	exclude_a = .;
	exclude_c = .;
/*proc freq;*/
/*	tables exclude_b exclude_d; */
run;

*only keep relevant data for confusion tables;
data small; set pred;
keep 	hhid pn	
		wu_re_dem_nowgt_a  wu_re_dem_nowgt_b	wu_re_dem_nowgt_c	wu_re_dem_nowgt_d
		wu_re_p_nowgt_a  wu_re_p_nowgt_b	wu_re_p_nowgt_c	wu_re_p_nowgt_d
		wu_re_dem_adams_d_longwgt_a  wu_re_dem_adams_d_longwgt_b	wu_re_dem_adams_d_longwgt_c	wu_re_dem_adams_d_longwgt_d
		wu_re_p_adams_d_longwgt_a  wu_re_p_adams_d_longwgt_b	wu_re_p_adams_d_longwgt_c	wu_re_p_adams_d_longwgt_d

		crim_re_dem_nowgt_a  crim_re_dem_nowgt_b	crim_re_dem_nowgt_c	crim_re_dem_nowgt_d
		crim_re_p_nowgt_a  crim_re_p_nowgt_b	crim_re_p_nowgt_c	crim_re_p_nowgt_d
		crim_re_dem_adams_d_longwgt_a  crim_re_dem_adams_d_longwgt_b	crim_re_dem_adams_d_longwgt_c	crim_re_dem_adams_d_longwgt_d
		crim_re_p_adams_d_longwgt_a  crim_re_p_adams_d_longwgt_b	crim_re_p_adams_d_longwgt_c	crim_re_p_adams_d_longwgt_d

		hurd_re_dem_nowgt_a  hurd_re_dem_nowgt_b	hurd_re_dem_nowgt_c	hurd_re_dem_nowgt_d
		hurd_re_p_nowgt_a  hurd_re_p_nowgt_b	hurd_re_p_nowgt_c	hurd_re_p_nowgt_d
		hurd_re_dem_adams_d_longwgt_a  hurd_re_dem_adams_d_longwgt_b	hurd_re_dem_adams_d_longwgt_c	hurd_re_dem_adams_d_longwgt_d
		hurd_re_p_adams_d_longwgt_a  hurd_re_p_adams_d_longwgt_b	hurd_re_p_adams_d_longwgt_c	hurd_re_p_adams_d_longwgt_d

		dement_a 	dement_b 	dement_c 	dement_d 
		proxy_a 	proxy_b 	proxy_c 	proxy_d
		exclude_a	exclude_b	exclude_c	exclude_d
		commsampfinal_a  commsampfinal_b  commsampfinal_c  commsampfinal_d
		hrs_age_a	hrs_age_b	hrs_age_c	hrs_age_d
		raceeth4 	NH_white	NH_black	NH_other	hispanic
		edu_hurd	edu_crim	male		female;
run;	

*go from wide->long dataset;
%macro small(w);
*rename for stacking;
data small_&w; set small;
	rename dement_&w = dement;
	rename proxy_&w = proxy;
	rename hrs_age_&w = hrs_age;
	rename wu_re_dem_nowgt_&w = wu_re_dem_nowgt;
	rename wu_re_p_nowgt_&w = wu_re_p_nowgt;
	rename wu_re_dem_adams_d_longwgt_&w = wu_re_dem_awgt;
	rename wu_re_p_adams_d_longwgt_&w = wu_re_p_awgt;
	rename crim_re_dem_nowgt_&w = crim_re_dem_nowgt;
	rename crim_re_p_nowgt_&w = crim_re_p_nowgt;
	rename crim_re_dem_adams_d_longwgt_&w = crim_re_dem_awgt;
	rename crim_re_p_adams_d_longwgt_&w = crim_re_p_awgt;
	rename hurd_re_dem_nowgt_&w = hurd_re_dem_nowgt;
	rename hurd_re_p_nowgt_&w = hurd_re_p_nowgt;
	rename hurd_re_dem_adams_d_longwgt_&w = hurd_re_dem_awgt;
	rename hurd_re_p_adams_d_longwgt_&w = hurd_re_p_awgt;

data small_&w; set small_&w;
*exclude if ineligible;
	if commsampfinal_&w = 1;
	if exclude_&w NE 1;
*create wave indicator;
	ADAMSwave = "&w";

	keep 	hhid 	pn			ADAMSwave
		dement 		proxy 		hrs_age 	
		wu_re_dem_nowgt wu_re_p_nowgt wu_re_dem_awgt wu_re_p_awgt
		crim_re_dem_nowgt crim_re_p_nowgt crim_re_dem_awgt crim_re_p_awgt
		hurd_re_dem_nowgt hurd_re_p_nowgt hurd_re_dem_awgt hurd_re_p_awgt
		raceeth4 	NH_white	NH_black	NH_other	hispanic
		edu_hurd	edu_crim	male		female;
run;
%mend;
%small(a); %small(b); %small(c); %small(d); 


data x.HRSt_re_&dt; set small_a; 
*create edu vars for subgroups;
ltHS = .; geHS=.;
if edu_hurd = 0 then do;
	ltHS=1; geHS=0;
	end;
else if edu_hurd in (1,2) then do;
	ltHS=0; geHS=1;
	end;

*create age vars for subgroups;
lt80 = .; ge80=.;
if hrs_age<80 & hrs_age ne . then do;
	lt80=1; ge80=0;
	end;
else if hrs_age ge 80 & hrs_age ne . then do;
	lt80=0; ge80=1;
	end;
run;

data x.HRSv_re_&dt; set small_b small_c small_d; 
*create edu vars for subgroups;
ltHS = .; geHS=.;
if edu_hurd = 0 then do;
	ltHS=1; geHS=0;
	end;
else if edu_hurd in (1,2) then do;
	ltHS=0; geHS=1;
	end;

*create age vars for subgroups;
lt80 = .; ge80=.;
if hrs_age<80 & hrs_age ne . then do;
	lt80=1; ge80=0;
	end;
else if hrs_age ge 80 & hrs_age ne . then do;
	lt80=0; ge80=1;
	end;
proc freq; tables ADAMSwave; 
run;
