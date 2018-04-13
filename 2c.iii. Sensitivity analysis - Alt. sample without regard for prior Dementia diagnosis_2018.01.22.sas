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
* Sensitivity analysis 3: Construct alternative validation dataset, including those Dx'ed with dementia in the past, and known to be alive in waves B, C, D
*
* 1. Construct new 'useyear'/dementia classification accordingly
* 2. Assign Wu/Hurd/Crimmins regression variables using new useyear
*		- For Crimmins: use simple 0.5 threshold rule
* 3. Classify dementia status for all 5
* 4. Construct/Output confusion tables 
*
**********************************************************************************************/
*set date for output label;
/*%let dt=2018_0124;*/
%let dt=2018_0302;
%let pdt=2018_0117;

data master_adsens; set x.master_ad_&pdt; run; 


/************************************************************
*															*
*	1. Construct new 'useyear' and dementia classification, *
*		carrying over past dementia Dx if still alive 		*
*															*
*************************************************************/

/*E.g.
	- include those diagnosed with dementia in wave A and still alive by wave B (2002/2004)
	- since they did not receive an ADAMS assessment, we cannot determine "nearest" prior HRS wave
	- instead, use last HRS wave corresponding to ADAMS wave that they are alive 
		- e.g. for someone Dx'ed with Dementia in Wave A, use 2004 HRS wave for Wave B prediction if inw_04 = 1; otherwise use 2002 HRS wave if inw_02 = 1
*/
%macro use(w1, w2, y1, y2);
data master_adsens; set master_adsens;
	 if dement_&w1 = 1 then do;
		if inw_&y1 = 1 then do; useyear_&w2 = 20&y1; proxy_&w2 = proxy_&y1; end;
		if inw_&y2 = 1 then do; useyear_&w2 = 20&y2; proxy_&w2 = proxy_&y2; end;

		if useyear_&w2 NE . then dement_&w2 = 1;

		if &w2.fresult = 7 then do; *7=died;
			useyear_&w2 = .; 
			dement_&w2= .; 
			proxy_&w2 = .;
		end; *affects N=5 waveA->waveB, where waveB outcome listed as deceased, BUT was alive for both 00 and 02 HRS interview, currently dropped for wave B;
	end;

	proc freq; 
		tables dement_&w1 dement_&w2 useyear_&w2 proxy_&w2;
		where dement_&w1 = 1;
run;
%mend;
%use(a, b, 02, 04) %use(a, c, 04, 06) %use(a, d, 06, 08)
%use(b, c, 04, 06) %use(b, d, 06, 08)
%use(c, d, 06, 08)


/************************************************************
*															*
*	2. Assign regression variables							*
*															*
*************************************************************/

/*- Replace missing proxy cognition with prior wave score; 
/*- In Wu, if prior wave is also missing, replace with subsequent wave score -- here we do not do this
in order to make best applicable to "real world" where future data is not available*/
/*- Set up regression variables for Wu/Hurd/Crimmins*/
/*- Note: Only useyear=2002, 2004 for wave B and 2004, 2006 for wave c;

/*set up proxy cognition variables for Wu/Hurd Crimmins regressions, replacing missing variables with lag/leads*/
%macro mis(y);
data master_adsens; set master_adsens;
	if useyear_&w = 20&y and proxy_&w = 1 then do;

		if &var._&y NE . then &var._&w = &var._&y;
		else if &var._&y = . then do;
			if &var.lag_&y NE . then &var._&w = &var.lag_&y;
/*			else if &var.lag_&y = . and  &var.lead_&y NE . then &var._&w = &var.lead_&y;*/
		end;
	end;

	label  &var._&w = "&varlab: for wave &w dementia prediction"
run;
%mend;

%let w = A;
%let var = iqcode5; %let varlab = IQCODE ctrd at 5 (-4 to 1); %mis(00) %mis(02) 
%let var = iqcode5_m; %let varlab = IQCODE ctrd at 5 * male interaction; %mis(00) %mis(02) 
%let var = pr_memsc5; %let varlab = proxy memory score crd at 5 (-4 to 1); %mis(00) %mis(02) 
%let var = iqcode; %let varlab = IQCODE; %mis(00) %mis(02) 
%let var = iqcodech; %let varlab = change in IQCODE since last non-missing wave; %mis(00) %mis(02) 
%let var = jormsymp; %let varlab = Jorm symptoms out of all items possible; %mis(00) %mis(02) 
%let var = jorm5symp; %let varlab = Jorm symptoms out of 5; %mis(00) %mis(02) 
%let var = pr_memsc1; %let varlab = proxy memory score ctrd at 1 (0-4); %mis(00) %mis(02) 
%let var = iwercog; %let varlab = inteviewer assessment of cogn impairment (0-2); %mis(00) %mis(02)

%let w = B;
%let var = iqcode5; %let varlab = IQCODE ctrd at 5 (-4 to 1); %mis(02) %mis(04) 
%let var = iqcode5_m; %let varlab = IQCODE ctrd at 5 * male interaction; %mis(02) %mis(04) 
%let var = pr_memsc5; %let varlab = proxy memory score crd at 5 (-4 to 1); %mis(02) %mis(04) 
%let var = iqcode; %let varlab = IQCODE; %mis(02) %mis(04) 
%let var = iqcodech; %let varlab = change in IQCODE since last non-missing wave; %mis(02) %mis(04) 
%let var = jormsymp; %let varlab = Jorm symptoms out of all items possible; %mis(02) %mis(04) 
%let var = jorm5symp; %let varlab = Jorm symptoms out of 5; %mis(02) %mis(04) 
%let var = pr_memsc1; %let varlab = proxy memory score ctrd at 1 (0-4); %mis(02) %mis(04) 
%let var = iwercog; %let varlab = inteviewer assessment of cogn impairment (0-2); %mis(02) %mis(04)
*note:  %mis(00) not needed here;

%let w = C;
%let var = iqcode5; %let varlab = IQCODE ctrd at 5 (-4 to 1); %mis(04) %mis(06) 
%let var = iqcode5_m; %let varlab = IQCODE ctrd at 5 * male interaction; %mis(04) %mis(06) 
%let var = pr_memsc5; %let varlab = proxy memory score crd at 5 (-4 to 1); %mis(04) %mis(06) 
%let var = iqcode; %let varlab = IQCODE; %mis(04) %mis(06) 
%let var = iqcodech; %let varlab = change in IQCODE since last non-missing wave; %mis(04) %mis(06) 
%let var = jormsymp; %let varlab = Jorm symptoms out of all items possible; %mis(04) %mis(06) 
%let var = jorm5symp; %let varlab = Jorm symptoms out of 5; %mis(04) %mis(06) 
%let var = pr_memsc1; %let varlab = proxy memory score ctrd at 1 (0-4); %mis(04) %mis(06) 
%let var = iwercog; %let varlab = inteviewer assessment of cogn impairment (0-2); %mis(04) %mis(06)
*note:  %mis(08) not needed here;

%let w = D;
%let var = iqcode5; %let varlab = IQCODE ctrd at 5 (-4 to 1); %mis(06) %mis(08) 
%let var = iqcode5_m; %let varlab = IQCODE ctrd at 5 * male interaction; %mis(06) %mis(08) 
%let var = pr_memsc5; %let varlab = proxy memory score crd at 5 (-4 to 1); %mis(06) %mis(08) 
%let var = iqcode; %let varlab = IQCODE; %mis(06) %mis(08) 
%let var = iqcodech; %let varlab = change in IQCODE since last non-missing wave; %mis(06) %mis(08) 
%let var = jormsymp; %let varlab = Jorm symptoms out of all items possible; %mis(06) %mis(08) 
%let var = jorm5symp; %let varlab = Jorm symptoms out of 5; %mis(06) %mis(08) 
%let var = pr_memsc1; %let varlab = proxy memory score ctrd at 1 (0-4); %mis(06) %mis(08) 
%let var = iwercog; %let varlab = inteviewer assessment of cogn impairment (0-2); %mis(06) %mis(08)
 

/*set up other regression variables*/
%macro reg (w, y);
data master_adsens; set master_adsens ;

/*proxy*/
	if useyear_&w = 20&y and proxy_&w = 1 then do;
	/*wu - proxies done; self-response need to be set to 0*/
		hrs_age70_&w = hrs_age70_&y; label hrs_age70_&w = "age at HRS interview ctrd at 70: for wave &w dementia prediction";
		tics13_&w = 0; label tics13_&w = "TICS score out of 13 (Wu-alg): for wave &w dementia prediction";
		tics13sq_&w = 0; label tics13sq_&w = "TICS score out of 13 squared (Wu-alg): for wave &w dementia prediction";
		iword_&w = 0; label iword_&w = "immediate word recall (0-10): for wave &w dementia prediction";
		iwordsq_&w = 0; label iwordsq_&w = "immediate word recall squared: for wave &w dementia prediction";
		dword_&w = 0; label dword_&w = "delayed word recall (0-1): for wave &w dementia prediction";
		dword_m_&w = 0; label dword_m_&w = "delayed word recall * male interaction: for wave &w dementia prediction";

	/*hurd*/
		hagecat_&w = hagecat_&y; label hagecat_&w = "age at HRS interview, 1=<75, 2=75-79, 3=80-84, 4=85-89, 5=90+: for wave &w dementia prediction";
		adl5_&w = adl5_&y; label adl5_&w = "total ADL limitations out of 5: for wave &w dementia prediction";
		adl6_&w = adl6_&y; label adl6_&w = "total ADL limitations our of 6: for wave &w dementia prediction";
		iadl5_&w = iadl5_&y; label iadl5_&w = "total IADL limitations out of 5: for wave &w dementia prediction";
		adl5ch_&w = adl5ch_&y;  label adl5ch_&w = "change in ADLs (0-5) since last non-missing wave: for wave &w dementia prediction";
		adl6ch_&w = adl6ch_&y; label adl6ch_&w = "change in ADLs (0-6) since last non-missing wave: for wave &w dementia prediction";
		iadl5ch_&w = iadl5ch_&y; label iadl5ch_&w = "change in IADLs (0-5) since last non-missing wave: for wave &w dementia prediction";
		proxylag_&w = proxylag_&y; label proxylag_&w = "prior wave proxy status: for wave &w dementia prediction";
		dateslag_&w = dateslag_&y; label dateslag_&w = "prior wave dates score (0-4): for wave &w dementia prediction";
		ticscount1lag_&w = ticscount1lag_&y; label ticscount1lag_&w = "prior wave TICS backward count (1=correct attempt 1 only): for wave &w dementia prediction";
		ticscount1or2lag_&w = ticscount1or2lag_&y; label ticscount1or2lag_&w = "prior wave TICS backward count (1=correct attempt 1 or attempt 2): for wave &w dementia prediction";
		serial7lag_&w = serial7lag_&y; label serial7lag_&w = "prior wave TICS serial 7: for wave &w dementia prediction";
		preslag_&w = preslag_&y; label preslag_&w = "prior wave TICS president: for wave &w dementia prediction";
		iwordlag_&w = iwordlag_&y; label iwordlag_&w = "prior waveimmediate word recall: for wave &w dementia prediction";
		dwordlag_&w = dwordlag_&y; label dwordlag_&w = "prior wave delayed word recall: for wave &w dementia prediction";
	/*crimmins - IADLs, done*/
		hrs_age_&w = hrs_age_&y; label hrs_age_&w = "age at HRS interview: for wave &w dementia prediction";
	end;

/*self-response*/
	if useyear_&w = 20&y and proxy_&w = 0 then do;
	/*wu*/
		hrs_age70_&w = hrs_age70_&y;
		iword_&w = iword_&y;
		iwordsq_&w = iwordsq_&y;
		dword_&w = dword_&y;
		tics13_&w = tics13_&y;
		tics13sq_&w = tics13sq_&y;
		dword_m_&w = dword_m_&y;
		iqcode5_&w = 0;
		pr_memsc5_&w = 0;
		iqcode5_m_&w = 0;
	/*hurd*/
		hagecat_&w = hagecat_&y;
		adl5_&w = adl5_&y; 
		adl6_&w = adl6_&y;
		iadl5_&w = iadl5_&y;
		adl5ch_&w = adl5ch_&y; 
		adl6ch_&w = adl6ch_&y;
		iadl5ch_&w = iadl5ch_&y;
		dates_&w = dates_&y; label dates_&w = "TICS dates score (0-4): for wave &w dementia prediction";
		ticscount1_&w = ticscount1_&y; label ticscount1_&w = "TICS backward count (1=correct attempt 1 only): for wave &w dementia prediction";
		ticscount1or2_&w = ticscount1or2_&y; label ticscount1or2_&w = "TICS backward count (1=correct attempt 1 or attempt 2): for wave &w dementia prediction";
		serial7_&w = serial7_&y; label serial7_&w = "TICS serial7: for wave &w dementia prediction";
		scis_&w = scis_&y; label scis_&w = "TICS scissors: for wave &w dementia prediction";
		cact_&w = cact_&y; label cact_&w = "TICS cactus: for wave &w dementia prediction";
		pres_&w = pres_&y; label pres_&w = "TICS president: for wave &w dementia prediction";
		/*iword, dword done*/
		datesch_&w = datesch_&y; label datesch_&w = "change in TICS dates: for wave &w dementia prediction";
		ticscount1ch_&w = ticscount1ch_&y; label ticscount1ch_&w = "change in TICS backward count (1=correct attmept 1 only): for wave &w dementia prediction";
		ticscount1or2ch_&w = ticscount1or2ch_&y; label ticscount1or2ch_&w = "change in TICS backward count (1=correct attempt 1 or 2): for wave &w dementia prediction";
		serial7ch_&w = serial7ch_&y; label serial7ch_&w = "change in TICS serial 7: for wave &w dementia prediction";
		scisch_&w = scisch_&y; label scisch_&w = "change in TICS scissors: for wave &w dementia prediction";
		cactch_&w = cactch_&y; label cactch_&w = "change in TICS cactus: for wave &w dementia prediction";
		presch_&w = presch_&y; label presch_&w = "change in TICS president: for wave &w dementia prediction";
		iwordch_&w = iwordch_&y; label iwordch_&w = "change in immediate word recall: for wave &w dementia prediction";
		dwordch_&w = dwordch_&y; label dwordch_&w = "chage in delayed word recall: for wave &w dementia prediction";
	/*crimmins*/
		/*iword, dword, serial7s done*/
		hrs_age_&w = hrs_age_&y;
		ticshrs_&w = ticshrs_&y; label ticshrs_&w = "TICS score from HRS (0-10): for wave &w dementia prediction";
		cogtot_&w = cogtot_&y; label cogtot_&w = "RAND cognition total, Potentially CRIMMINS TICS (0-35): for wave &w dementia prediction";
		dress_&w = dress_&y; label dress_&w = "ADL getting dressed 0=No Difficulty, 1=Difficulty,: for wave &w dementia prediction";
		bath_&w = bath_&y; label bath_&w = "ADL bathing 0=No Difficulty, 1=Difficulty,: for wave &w dementia prediction";
		eat_&w = eat_&y; label eat_&w = "ADL eating 0=No Difficulty, 1=Difficulty,: for wave &w dementia prediction";
		money_&w = money_&y; label money_&w = "IADL managing finances 0=No Difficulty, 1=Difficulty,: for wave &w dementia prediction";
		phone_&w = phone_&y; label phone_&w = "IADL using phone 0=No Difficulty, 1=Difficulty,: for wave &w dementia prediction";
	end;
run;
%mend;

%reg(A, 00) %reg(A, 02) 
%reg(B, 02) %reg(B, 04)
%reg(C, 04) %reg(C, 06)
%reg(D, 06) %reg(D, 08)


/************************************************************
*
*	3. Determine dementia Dx for Hurd, Wu, Crimmins
*
*************************************************************/

/*Hurd - used transposed probabilities (only avaailable up to 2006)
* use threshold of 0.5 */

/* Merge in Hurd probabilities first*/

data master_adsens; 
	merge master_adsens (in = a) xold.hurdprobabilities_wide;
	by hhid pn;
	if a;
run;


/*1998: no proxy for LKW  only (due to missing interviewer assessment variable*/

%macro dem(py, y);
data master_adsens; set master_adsens;	
/*Hurd*/
* use threshold of 0.5;
	if hurd_prob_&py > 0.5 then hurd_dem_&y = 1;
	else if hurd_prob_&py NE . then hurd_dem_&y = 0;
	hurd_p_&y = hurd_prob_&py;
	label hurd_p_&y = "Hurd dementia probability provided by authors y&y";
	label hurd_dem_&y = "Hurd dementia Dx using 0.5 threshold from probabilities provided by authors y&y";

	if proxy_&y = 0 then do;
	/*Herzog-Wallace*/
		if 0 le cogtot_&y le 8 then hw_dem_&y = 1;
		else if 9 le cogtot_&y le 35 then hw_dem_&y = 0;
		label hw_dem_&y = "Herzog-Wallace dementia : self-resp use <9 on 0-35 cognition; proxy use 2+ jorm symptoms, y&y";
	/*Langa-Kabeto-Weir*/
		if 0 le cogsc27_&y le 6 then do; lkw_dem_&y = 1; lkw_cogn_&y = 3; end;
		else if 7 le cogsc27_&y le 11 then do; lkw_dem_&y = 0; lkw_cogn_&y = 2; end;
		else if cogsc27_&y NE . then do; lkw_dem_&y = 0; lkw_cogn_&y = 1; end;
		label lkw_dem_&y = "Langa-Kabeto-Weir dementia Dx: self-resp use <7 on 0-27 cognition; proxy use 6+ on 11-pt scale (prmem, iwercog, iadl), y&y";
		label lkw_cogn_&y = "Langa-Kabeto-Weir cognition Dx: 1=normal(12-27  self, 0-2 proxy), 2=CIND(7-11 self, 3-5 proxy), 3=dementia(0-6 self, 6-11 proxy), y&y";
	end;

	if proxy_&y = 1 then do;
	/*Herzog-Wallace*/
		hw_dem_&y = jormsymp2_&y;
	end;
run;
%mend;
%dem(1999, 98)

%macro dem(py, y);
data master_adsens; set master_adsens;	
/*Hurd*/
************for now use threshold of 0.5;
	if hurd_prob_&py > 0.5 then hurd_dem_&y = 1;
	else if hurd_prob_&py NE . then hurd_dem_&y = 0;
	hurd_p_&y = hurd_prob_&py;
	label hurd_p_&y = "Hurd dementia probability provided by authors y&y";
	label hurd_dem_&y = "Hurd dementia Dx using 0.5 threshold from probabilities provided by authors y&y";

	if proxy_&y = 0 then do;
	/*Herzog-Wallace*/
		if 0 le cogtot_&y le 8 then hw_dem_&y = 1;
		else if 9 le cogtot_&y le 35 then hw_dem_&y = 0;
		label hw_dem_&y = "Herzog-Wallace dementia : self-resp use <9 on 0-35 cognition; proxy use 2+ jorm symptoms, y&y";
	/*Langa-Kabeto-Weir*/
		if 0 le cogsc27_&y le 6 then do; lkw_dem_&y = 1; lkw_cogn_&y = 3; end;
		else if 7 le cogsc27_&y le 11 then do; lkw_dem_&y = 0; lkw_cogn_&y = 2; end;
		else if cogsc27_&y NE . then do; lkw_dem_&y = 0; lkw_cogn_&y = 1; end;
		label lkw_dem_&y = "Langa-Kabeto-Weir dementia Dx: self-resp use <7 on 0-27 cognition; proxy use 6+ on 11-pt scale (prmem, iwercog, iadl), y&y";
		label lkw_cogn_&y = "Langa-Kabeto-Weir cognition Dx: 1=normal(12-27  self, 0-2 proxy), 2=CIND(7-11 self, 3-5 proxy), 3=dementia(0-6 self, 6-11 proxy), y&y";
	end;

	if proxy_&y = 1 then do;
	/*Herzog-Wallace*/
		hw_dem_&y = jormsymp2_&y;
	/*Langa-Kabeto-Weir*/
		if 6 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 11 then do; lkw_dem_&y = 1; lkw_cogn_&y = 3; end;
		else if 3 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 5 then do; lkw_dem_&y = 0; lkw_cogn_&y = 2; end;
		else if 0 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 2 then do; lkw_dem_&y = 0; lkw_cogn_&y = 1; end;
	end;
run;
%mend;
%dem(2001, 00) %dem(2003, 02) %dem(2005, 04) %dem(2007, 06)


%macro dem(y);
data master_adsens; set master_adsens;	
	if proxy_&y = 0 then do;
	/*Herzog-Wallace*/
		if 0 le cogtot_&y le 8 then hw_dem_&y = 1;
		else if 9 le cogtot_&y le 35 then hw_dem_&y = 0;
		label hw_dem_&y = "Herzog-Wallace dementia : self-resp use <9 on 0-35 cognition; proxy use 2+ jorm symptoms, y&y";
	/*Langa-Kabeto-Weir*/
		if 0 le cogsc27_&y le 6 then do; lkw_dem_&y = 1; lkw_cogn_&y = 3; end;
		else if 7 le cogsc27_&y le 11 then do; lkw_dem_&y = 0; lkw_cogn_&y = 2; end;
		else if cogsc27_&y NE . then do; lkw_dem_&y = 0; lkw_cogn_&y = 1; end;
		label lkw_dem_&y = "Langa-Kabeto-Weir dementia Dx: self-resp use <7 on 0-27 cognition; proxy use 6+ on 11-pt scale (prmem, iwercog, iadl), y&y";
		label lkw_cogn_&y = "Langa-Kabeto-Weir cognition Dx: 1=normal(12-27  self, 0-2 proxy), 2=CIND(7-11 self, 3-5 proxy), 3=dementia(0-6 self, 6-11 proxy), y&y";
	end;

	if proxy_&y = 1 then do;
	/*Herzog-Wallace*/
		hw_dem_&y = jormsymp2_&y;
	/*Langa-Kabeto-Weir*/
		if 6 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 11 then do; lkw_dem_&y = 1; lkw_cogn_&y = 3; end;
		else if 3 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 5 then do; lkw_dem_&y = 0; lkw_cogn_&y = 2; end;
		else if 0 le (pr_memsc1_&y + iadl5_&y + iwercog_&y) le 2 then do; lkw_dem_&y = 0; lkw_cogn_&y = 1; end;
	end;
%mend;
%dem(08) 

/**/
/*proc means data=pred;*/
/*	var hurd_prob_2007; where hurd_dem_06 = 0;*/
/*run;*/
/*proc means data=pred;*/
/*	var hurd_prob_2007; where hurd_dem_06 = 1;*/
/*run;*/
/*proc freq;*/
/*	tables cogtot_98 hw_dem_98 cogsc27_98 lkw_dem_98; where proxy_98 = 0;*/
/*run;*/
/*proc freq;*/
/*	tables jormsymp2_08 hw_dem_08  lkw_dem_08; where proxy_08 = 1;*/
/*run;*/

/*Assign dementia status to each wave for Hurd/HW/LKW*/
%macro dem(w, y);
data master_adsens; set master_adsens;
if useyear_&w = 20&y then do;
	hurd_dem_&w = hurd_dem_&y; label hurd_dem_&w = "Hurd dementia classification, wave &w";
	hurd_p_&w = hurd_p_&y; label hurd_p_&w = "Hurd dementia probability provided by authors, wave &w";
	hw_dem_&w = hw_dem_&y; label hw_dem_&w = "Herzog-Wallace dementia classification, wave &w";
	lkw_dem_&w = lkw_dem_&y; label lkw_dem_&w = "Langa-Kabeto-Weir dementia classification, wave &w";
	lkw_cogn_&w = lkw_cogn_&y; label lkw_cogn_&w = "Langa-Kabeto-Weir cognition classification (1=normal, 2=CIND, 3=dementia), wave &w";
end;
run;
proc freq; tables dement_&w*(hurd_dem_&w hw_dem_&w lkw_dem_&w);
/*run;*/
%mend;
%dem(A, 00) %dem(A, 02);
%dem(B, 02) %dem(B, 04);
%dem(C, 04) %dem(C, 06);
%dem(D, 06) %dem(D, 08); 
data master_adsens; set master_adsens (drop = hurd_dem_08 hurd_p_08); run; /*Hurd probability not provided for 2008*/

/****WU****/
%macro wu (w);
data master_adsens; set master_adsens;
	wu_or_&w = exp(4.608 + 1.889*proxy_&w + 0.933*iword_&w - 0.266*iwordsq_&w - 0.797*dword_&w - 1.075*tics13_&w + 0.043*tics13sq_&w + 2.220*iqcode5_&w + 1.096*pr_memsc5_&w 
					 - 0.854*male + 0.095*hrs_age70_&w - 0.695*black + 0.543*dword_m_&w + +1.551*iqcode5_m_&w);
	wu_p_&w = wu_or_&w/(1+wu_or_&w);

	if wu_p_&w > 0.5 then wu_dem_&w = 1;
	else if wu_p_&w NE . then wu_dem_&w = 0;

	label wu_or_&w = "Wu Dementia odds ratio, wave &w";
	label wu_p_&w = "Wu Dementia probability, wave &w";
	label wu_dem_&w = "Wu Dementia classification, wave &w";
proc freq;
	tables dement_&w*wu_dem_&w;
run;
%mend;
%wu(A) %wu(B)%wu(C)%wu(D)

/***CRIMMINS***/
/*Fit Crimmins algorithm and determine dementia status:
	- 2 versions for self-response
		- 1 using HRSTICS (out of 10)
		- 2 using COGTOT (out of 35)
	- 2 versions for proxy
		- 1 using model 3 
		- 2 using model 4
*/

%macro crim(w);
data master_adsens; set master_adsens;
*SELF RESPONSE;
if proxy_&w = 0 then do;
	crim_ors_&w = exp(-8.3247 + log(1.2)*hrs_age_&w + log(1.02)*female + log(0.36)*lowedu_crim + log(0.45)*midedu_crim 
						 + log(0.73)*iword_&w + log(0.65)*dword_&w + log(0.68)*serial7_&w + log(0.6)*ticshrs_&w
						 + log(0.33)*dress_&w + log(1.30)*bath_&w + log(4.34)*eat_&w + log(9.72)*money_&w + log(2.38)*phone_&w);

	crim_ps_&w = crim_ors_&w/(1 + crim_ors_&w);
	if crim_ps_&w > 0.5 then crim_dems_&w = 1;
	else if crim_ps_&w NE . then crim_dems_&w = 0;

	label crim_ors_&w = "Crimmins Dementia odds ratio for self-respondent using HRS TICS (0-10), wave &w";
	label crim_ps_&w = "Crimmins Dementia probability for self-respondent using HRS TICS (0-10), wave &w";
	label crim_dems_&w = "Crimmins Dementia classification for self-respondent using HRS TICS (0-10), wave &w";
end;
*PROXY (use model 3 no Jorm);
if proxy_&w = 1 then do;
	crim_orp_&w = exp(-2.3448 + log(2.39)*pr_memsc1_&w + log(1.45)*iadl5_&w + log(1.37)*iwercog_&w);
	crim_pp_&w = crim_orp_&w/(1 + crim_orp_&w);
	if crim_pp_&w > 0.5 then crim_demp_&w = 1;
	else if crim_pp_&w NE . then crim_demp_&w = 0;

	label crim_orp_&w = "Crimmins Dementia odds ratio for proxies using model 3 (no Jorm), wave &w";
	label crim_pp_&w = "Crimmins Dementia probability for proxies using model 3 (no Jorm), wave &w";
	label crim_demp_&w = "Crimmins Dementia classification for proxies using model 3 (no Jorm), wave &w";
end;
	*consolidate self-response and proxy into single variable;
	if crim_dems_&w NE . then crim_dem_&w = crim_dems_&w;
	if crim_demp_&w NE . then crim_dem_&w = crim_demp_&w;

	if crim_ps_&w NE . then crim_p_&w = crim_ps_&w;
	if crim_pp_&w NE . then crim_p_&w = crim_pp_&w;

	proc freq; tables crim_dem_&w*dement_&w;
run;
%mend;
%crim(A) %crim(B) %crim(C) %crim(D) 

/*create common sample flag with all non-missing classifications*/
%macro com(w);
data master_adsens; set master_adsens;
	if hw_dem_&w NE . and lkw_dem_&w NE . and crim_dem_&w NE .  and hurd_dem_&w NE . and wu_dem_&w NE . then commsamp_&w = 1; 
	proc freq; tables commsamp_&w;
run;
%mend;
%com(A) %com(B) %com(C)%com(D)

/*proc freq data=x.Master_adpred_wide_&pdt; tables commsamp1_a commsamp2_a commsamp1_b commsamp2_b commsamp1_c commsamp2_c commsamp1_d commsamp2_d; run;*/
*same N's for wave A, higher N's for waves B, C, D, as expected;

/*save to drive*/
data x.master_adpredalt_wide_&dt; set master_adsens; run;

/************************************************************
*
*	4. Create datasets for accuracy score outputs & output
*			- no need to recreate wave A (training) - no changes
*
*************************************************************/

/***create datasets****/
data small; set master_adsens;
keep 	hhid pn	
		hw_dem_a	hw_dem_b	hw_dem_c	hw_dem_d 
		lkw_dem_a	lkw_dem_b	lkw_dem_c	lkw_dem_d
		lkw_cogn_a 	lkw_cogn_b	lkw_cogn_c	lkw_cogn_d	
		crim_dem_a  crim_dem_b	crim_dem_c	crim_dem_d
		crim_p_a 	crim_p_b	crim_p_c	crim_p_d
		hurd_dem_a 	hurd_dem_b	hurd_dem_c	hurd_dem_d
		hurd_p_a 	hurd_p_b	hurd_p_c	hurd_p_d
		wu_dem_a	wu_dem_b	wu_dem_c	wu_dem_d
		wu_p_a		wu_p_b		wu_p_c		wu_p_d
		dement_a 	dement_b 	dement_c 	dement_d 
		cogn_a		cogn_b		cogn_c		cogn_d
		proxy_a 	proxy_b 	proxy_c 	proxy_d
		commsamp_a  commsamp_b  commsamp_c  commsamp_d
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
	rename hw_dem_&w = hw_dem;
	rename lkw_dem_&w = lkw_dem;
	rename crim_dem_&w = crim_dem;
	rename crim_p_&w = crim_p;
	rename hurd_dem_&w = hurd_dem;
	rename hurd_p_&w = hurd_p;
	rename wu_dem_&w = wu_dem;
	rename wu_p_&w = wu_p;
run;

data small_&w; set small_&w;
*exclude if ineligible;
	if commsamp_&w = 1;
*create wave indicator;
	ADAMSwave = "&w";

	keep 	hhid 	pn			ADAMSwave
		dement 		proxy 		hrs_age 	
		hw_dem		lkw_dem		hurd_dem	wu_dem
		crim_dem 	hurd_p		wu_p		crim_p
		raceeth4 	NH_white	NH_black	NH_other	hispanic
		edu_hurd	edu_crim	male		female;
run;
%mend;
%small(b); %small(c); %small(d); 

*set up SES categories data;
data x.HRSv_alt_&dt; set small_b small_c small_d; 
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

*get Ns for proxy, ADAMS dementia, and overall;
proc freq data=x.HRSv_alt_&dt;
tables proxy dement;
run;



