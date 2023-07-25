*Loading data from server location;
%macro LoadFileProcHttp(FileName=/*Name of the file*/);
*[1] Specify the location to store data in Viya;
filename Data "&folder./&FileName."; 

*Importing data from URL and output to specified Viya location;
proc http 
	method="get"
	url="&url./&FileName."
	out=Data; 
run;
%mend LoadFileProcHttp;

*Loading individual data file;
%let folder=/home/5dsasdemo02/oo124072/data;
%let url=https://web.sgh.waw.pl/~akorczy/files/logreg/data;

*Importing CSV file;
%LoadFileProcHttp(FileName=ESS10.csv);


*Reading in data;
libname ess "&folder.";

FILENAME REFFILE DISK "&folder./ESS10.csv";

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=ess.ess10;
	GETNAMES=YES;
RUN;

proc format;
 value gndr
 1='Male'
 2='Female'
;

data e02;
    set ess.ess10;
    where cntry='CH';
run;

%macro dist001(Var=/*Variable*/);
 title "&Var.";
	proc freq data=e02;
	 tables &Var. / out=f02;
	run;
	proc sgplot data=e02;
	 vbar &Var.;
	run;
	title;
%mend dist001;
%dist001(Var=stflife);
%dist001(Var=stfdem);*d;
%dist001(Var=polintr);
%dist001(Var=hinctnta);*d;
%dist001(Var=SCLACT);
%dist001(Var=trstplt);
%dist001(Var=GNDR);


data e03;
set e02;
*Response: LQ - life satisfaction;
if ^missing(stflife) and stflife<11 then do;
	if 0<=stflife<=6 then lq=1;
	else if 7<=stflife<=8 then lq=2;
	else if 9<=stflife<=10 then lq=3;
end;
else lq=.;
*Response: stfdem - How satisfied with the way democracy works in the country;
if ^missing(stfdem) and stfdem<11 then do;
	if 0<=stfdem<=5 then democracy=1;
	else if 6<=stfdem<=7 then democracy=2;
	else if 8<=stfdem<=10 then democracy=3;
end;
else democracy=.;
*Age;
if ^missing(AGEA) and AGEA<101 then do;
	if AGEA<=100 then age=AGEA;
end;
else age=.;
*Years of education;
if ^missing(eduyrs) and eduyrs<77 then edu=eduyrs;
else if missing(eduyrs) or eduyrs>=77 then edu=.;
*Response: polintr - How interested in politics;
if ^missing(polintr) and polintr<5 then do;
	if 0<=polintr<=1 then interest=1;
	else if 2<=polintr<=3 then interest=2;
	else if polintr=4 then interest=3;
end;
else interest=.;
*Response: trstplt - trust in politicians;
if ^missing(trstplt) and trstplt<11 then do;
	if 0<=trstplt<=4 then trust=1;
	else if 5<=trstplt<=8 then trust=2;
	else if 7<=trstplt<=10  then trust=3;
end;
else trust=.;
*Social activities;
if ^missing(SCLACT) then do;
 if SCLACT in (1 2) then _SCLACT=1;
 else if SCLACT=3 then _SCLACT=2;
 else if SCLACT in (4 5) then _SCLACT=3;
end;
else _SCLACT=.;
*Explanatory;
*Income decile;
if ^missing(hinctnta) and hinctnta<11 then do;
	if 0<=hinctnta<=4 then income=1;
	else if 5<=hinctnta<=8 then income=2;
	else if 9<=hinctnta<=10  then income=3;
end;
else income=.;
if ^missing(gndr) and gndr<9 then _gndr=putn(gndr,'sfmt');
else _gndr='';
run;

proc sgplot data=e03;
	histogram age;
run;

proc sgplot data=e03;
	histogram edu;
run;

data e04;
	set e03;
	where cntry='CH';
run;

proc mi data=e04 nimpute=0;
	var lq democracy age interest trust _SCLACT gndr edu income;
run;

*Ex.4 Discrimatory performance;
%macro FreqInY001(Var);
	proc freq data=e04;
		tables &Var.*lq;
		ods output CrossTabFreqs=pct01;
	run;
	proc sgplot data=pct01(where=(^missing(RowPercent)));
		vbar &Var. / group=lq groupdisplay=cluster response=RowPercent datalabel datalabelfitpolicy=none;
	run;
%mend FreqInY001;

%FreqInY001(democracy);
%FreqInY001(income);
%FreqInY001(_gndr);
%FreqInY001(trust);
%FreqInY001(interest);
%FreqInY001(_SCLACT);
*Continuous predictors;
proc means data=e03;
	var age edu;
	class lq;
run;
proc sgpanel data=e04;
	panelby lq / columns=1;
	histogram age;
run;
proc sgpanel data=e04;
	panelby lq / columns=1;
	histogram edu;
run;

*Ex.5 Univariate ordinal models;
%macro UniModel001(Var=/*Variable*/,Ref=/*Refernce category*/);
proc logistic data=e03;
 %if %length(&Ref.)>0 %then %do;
	 class &Var. (param=ref ref="&ref.");
	%end;
	model lq = &var.;
	weight anweight;
run;
%Mend UniModel001;
%UniModel001(Var=democracy, Ref=3);
%UniModel001(Var=income, Ref=3);
%UniModel001(Var=gndr, Ref=2);
%UniModel001(Var=trust, Ref=3);
%UniModel001(Var=interest, Ref=3);
%UniModel001(Var=_SCLACT, Ref=3);

%UniModel001(Var=age);
%UniModel001(Var=edu);

*Ex. 6 Exploratory analysis;
proc logistic data=e04;
	class democracy (param=ref ref='3')  income (param=ref ref='3') _SCLACT (param=ref ref='3') trust (param=ref ref='3');;
	model lq =  democracy income age _SCLACT trust /*edu*/ / expb  
	selection=Stepwise sle=0.2 /*Forward | Backward | Stepwise*/;
run;

*Ex. 7 Final model of the life satisfaction;
ods graphics on;
proc logistic data=e04 plots(only)=(oddsratio calibration);
	class democracy (param=ref ref='3')  income (param=ref ref='3') 
	_sclact (param=ref ref='3') trust (param=ref ref='3')
	;
	model lq /*(ref='1')*/=  democracy income _sclact trust /*edu*/ / expb /*link=glogit*/  lackfit rsq;
	output out=out p=pred difdev=difdev difchisq=difchisq h=leverage c=c;
	
	*Ex. 8 Probabilities democracy=1 and income=1 and trust=0 and _sclact=1;
	estimate Intercept 1		trust 1 0	democracy 1 0    income 1 0  _sclact 1 0  /*edu 1*/ / e cl category='1';
										
	*Ex. 9 Contrast for difference in requested levels;
	estimate '1 vs 2 democracy scale' democracy 1 0 / e cl;
run;
ods graphics off;


