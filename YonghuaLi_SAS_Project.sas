*******************************************************
SAS Project

bank_customer_data analysis

*******************************************************


*assign sas library;

LIBNAME AKM "F:\Metro\SAS Project\Data";

*1.DATA PROFILING;

PROC IMPORT
DATAFILE="F:\Metro\SAS Project\Data\bank_customer_data.csv" 
OUT=AKM.Bankdata
replace;
run;

PROC CONTENTS DATA=AKM.Bankdata;RUN;

PROC CONTENTS DATA=AKM.Bankdata VARNUM SHORT;
RUN;

PROC PRINT DATA = AKM.Bankdata (OBS=20);RUN;

*******************************************************
Business questions 
1.What factors have relationship with customer status?

2.Are there any associations among different kinds of products? 

3.Can we predict the customer behaviours based on dataset?


*******************************************************
;
PROC CONTENTS DATA = AKM.Bankdata OUT= AKM.Bankdata_VARS;RUN;

*SPLIT PROJECT DATA INTO NUMERIC AND CHARACTER VARABLES;
*for numeric varibles;
%MACRO DP_NUMERIC (SAS_LIB_PATH =  ,DSN = ,DATA_FILE_PATH = );
PROC IMPORT OUT= AKM.&DSN. 
            DATAFILE= &DATA_FILE_PATH.
            DBMS=CSV REPLACE;
   GETNAMES=YES;
   DATAROW=2; 
   GUESSINGROWS=100; RUN;

PROC CONTENTS DATA = AKM.&DSN. OUT= AKM.&DSN._PROJECT_VARS;RUN;
PROC SQL;
SELECT NAME INTO : NUM_ONLY SEPARATED BY " "
FROM AKM.&DSN._PROJECT_VARS
WHERE TYPE EQ 1
;QUIT;
%LET N = %SYSFUNC(COUNTW(&NUM_ONLY));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&NUM_ONLY,&I);
ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\DATA_PROFILING_&X._&SYSDATE9..PDF";
TITLE "DISTRIBUTION OF NUMERIC VARIABLE : &X.";
PROC MEANS DATA = AKM.&DSN N NMISS MIN MEDIAN MEAN MAX STD MAXDEC=2;
 VAR &X.;
RUN;
TITLE "GRAPHIC DISTRIBUTION OF NUMERIC VARAIBLE : &X.";
PROC SGPLOT DATA = AKM.&DSN.;
 HISTOGRAM &X.;
 DENSITY &X./TYPE = KERNEL;
KEYLEGEND / LOCATION=INSIDE POSITION=TOPRIGHT ACROSS=1 NOBORDER;
RUN;QUIT;

TITLE "GRAPHIC DISTRIBUTION (VERTICAL BAR) OF NUMERIC VARAIBLE : &X.";
PROC SGPLOT DATA = AKM.&DSN.;
 VBOX &X.;
 yaxis grid;
 xaxis display=(nolabel);
RUN;QUIT;
ODS PDF CLOSE;
%END;
%MEND;

%dp_numeric 
(SAS_LIB_PATH = "F:\Metro\SAS Project\Data" ,
DSN =Bankdata ,
DATA_FILE_PATH = "F:\Metro\SAS Project\Data\bank_customer_data.csv");


*for Category variables;
%MACRO DP_Category (SAS_LIB_PATH =  ,DSN = );
PROC SQL;
SELECT NAME INTO : CHAR_ONLY SEPARATED BY " "
FROM AKM.&DSN._PROJECT_VARS
WHERE TYPE EQ 2
;QUIT;
%LET N = %SYSFUNC(COUNTW(&CHAR_ONLY));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&CHAR_ONLY,&I);
ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\DATA_PROFILING_&X._&SYSDATE9..PDF";
TITLE "DISTRIBUTION OF CATEGORY VARIABLE : &X.";
PROC FREQ DATA = AKM.&DSN ORDER=FREQ;
 TABLE &X./MISSING;
RUN;

TITLE "GRAPHIC DISTRIBUTION (VERTICAL BAR) OF CATEGORY VARAIBLE : &X.";
PROC SGPLOT DATA = AKM.&DSN.;
 VBAR &X.;
 STYLEATTRS
 BACKCOLOR=LIGHTBLUE
 WALLCOLOR=LIGHTYELLOW;
RUN;QUIT;
ODS PDF CLOSE;
%END;
%MEND;

%dp_category 
(SAS_LIB_PATH = "F:\Metro\SAS Project\Data" ,
DSN =Bankdata );



*missing values for Numeric;
%MACRO DP_Numeric (SAS_LIB_PATH =, DSN = );
PROC SQL;
SELECT NAME INTO : NUM_ONLY SEPARATED BY " "
FROM AKM.&DSN._PROJECT_VARS
WHERE TYPE EQ 1
;
QUIT;

%LET N = %SYSFUNC(COUNTW(&NUM_ONLY));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&NUM_ONLY,&I);

ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\MISSING_Numeric_&SYSDATE9..PDF";
PROC MEANS DATA =AKM.&DSN. N NMISS MIN MEDIAN MAX;
 VAR &X.;
RUN;

%END;
%MEND;

%DP_Numeric 
(SAS_LIB_PATH = "F:\Metro\SAS Project\Data",DSN =Bankdata );


*missing values for Category;
%MACRO DP_Category (SAS_LIB_PATH =,DSN = );

PROC SQL;
SELECT NAME INTO : CHAR_ONLY SEPARATED BY " "
FROM AKM.&DSN._PROJECT_VARS
WHERE TYPE EQ 2
;
QUIT;
%LET N = %SYSFUNC(COUNTW(&CHAR_ONLY));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&CHAR_ONLY,&I);

ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\MISSING_Category_&SYSDATE9..PDF";
PROC FREQ DATA = AKM.&DSN.;
 TABLE &X. /MISSING;
RUN;

%END;
%MEND;
%DP_Category 
(SAS_LIB_PATH ="F:\Metro\SAS Project\Data",DSN =Bankdata );

*Replace missing values;

*Age is normal distributed;
PROC STDIZE DATA = AKM.Bankdata OUT= AKM.Bankdata METHOD = MEAN REPONLY;
 VAR Age;RUN;

*Income is not normal distributed;
PROC STDIZE DATA =  AKM.Bankdata OUT=  AKM.Bankdata METHOD = MEDIAN REPONLY;
VAR Income;RUN;

*According bank business, most of missing value should be 0;
proc stdize DATA =  AKM.Bankdata OUT=  AKM.Bankdata reponly missing=0;run;


*OUTLIERS DETECTION;
%MACRO Outliers_Numeric (SAS_LIB_PATH =  ,DSN = );

PROC SQL;
SELECT NAME INTO : NUM_ONLY SEPARATED BY " "
FROM AKM.&DSN._PROJECT_VARS
WHERE TYPE EQ 1
;
QUIT;
%LET N = %SYSFUNC(COUNTW(&NUM_ONLY));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&NUM_ONLY,&I);

ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\Outliers_&SYSDATE9..PDF";
PROC MEANS DATA = AKM.&DSN.  N Q1 Q3 QRANGE;
 VAR &X.;
 OUTPUT  OUT= TEMP_&DSN. Q1 = Q1 Q3=Q3 QRANGE =IQR;
RUN;
PROC PRINT DATA = TEMP_&DSN.;RUN;

DATA TEMP1_&DSN.;
SET TEMP_&DSN.;
LOWER_LIMIT =  Q1 - (3*IQR);
UPPER_LIMIT = Q3 +(3*IQR);
RUN;

*CARTESIAN JOINS;
PROC SQL;
CREATE TABLE TEMP2_&DSN. AS
SELECT A.*,B.LOWER_LIMIT,B.UPPER_LIMIT
FROM AKM.&DSN. AS A , TEMP1_&DSN. AS B
;
QUIT;

DATA TEMP3_&DSN.;
 SET TEMP2_&DSN.;
 IF &X. LE LOWER_LIMIT THEN &X._RAGNE = "BELOW LOWER LIMIT";
 ELSE  IF &X. GE UPPER_LIMIT THEN &X._RAGNE = "ABOVE LOWER LIMIT";
 ELSE &X._RAGNE = "WITHIN RANGE";
RUN;

PROC SQL;
 CREATE TABLE AKM.Outliers AS
 SELECT *
 FROM TEMP3_&DSN
 WHERE &X._RAGNE EQ "BELOW LOWER LIMIT" and "ABOVE LOWER LIMIT";
 QUIT;

%END;
%MEND;
%Outliers_Numeric
(SAS_LIB_PATH = "F:\Metro\SAS Project\Data" ,DSN =Bankdata );

*no outliers in dataset;

*Create new category columns;
DATA AKM.Bankdata_DUMMIES;
 SET AKM.Bankdata;
 if Age lt 25 then Age_Group = '1:< 25 ';
 else if Age le 40 then Age_Group = '2:25-40 ';
 else if Age le 65 then Age_Group = '3:40-65 ';
 else Age_Group = '4:66+ ';
 
 if AcctAge lt 1 then AcctAge_Group = '1:< 1 ';
 else if AcctAge le 2 then AcctAge_Group = '2:1-2 ';
 else if AcctAge le 4 then AcctAge_Group = '3:2-4 ';
 else if AcctAge le 10 then AcctAge_Group = '4:4-10 ';
 else AcctAge_Group = '5:10+ ';

 if Income lt 10 then Income_Group = '1:< 10k ';
 else if Income le 30 then Income_Group = '2:10-30 ';
 else if Income le 60 then Income_Group = '3:30-60 ';
 else Income_Group = '4:60+ ';

 if CRScore lt 600 then CRScore_Group = '1:Very Poor ';
 else if CRScore le 650 then CRScore_Group = '2:Poor ';
 else if CRScore le 700 then CRScore_Group = '3:Average ';
 else if CRScore le 750 then CRScore_Group = '4:Good ';
 else if CRScore le 800 then CRScore_Group = '5:Very Good ';
 else CRScore_Group = '6:Ecxellent ';
RUN;


*Create table with index;
proc sql;
create table AKM.products as 
   select DDABal, SavBal, CDBal, IRABal, LOCBal, ILSBal,
          MMBal, LORes,MTGBal, CCBal, HMVal, InvBal
   from AKM.Bankdata;
run;

proc corr data=AKM.products plots=matrix(histogram);run;

proc template;
	define statgraph corrHeatmap;
   dynamic _Title;
		begingraph;
         entrytitle _Title;
			rangeattrmap name='map';
			/* select a series of colors that represent a "diverging"  */
			/* range of values: stronger on the ends, weaker in middle */
			/* Get ideas from http://colorbrewer.org                   */
			range -1 - 1 / rangecolormodel=(cxD8B365 cxF5F5F5 cx5AB4AC);
			endrangeattrmap;
			rangeattrvar var=r attrvar=r attrmap='map';
			layout overlay / 
				xaxisopts=(display=(line ticks tickvalues)) 
				yaxisopts=(display=(line ticks tickvalues));
				heatmapparm x = x y = y colorresponse = r / xbinaxis=false ybinaxis=false
					colormodel=THREECOLORRAMP name = "heatmap" display=all;
				continuouslegend "heatmap" / 
					orient = vertical location = outside title="Pearson Correlation";
			endlayout;
		endgraph;
	end;
run;

/* Prepare the correlations coeff matrix: Pearson's r method */
%macro prepCorrData(in=,out=);
	/* Run corr matrix for input data, all numeric vars */
	proc corr data=&in. noprint
		pearson
		outp=work._tmpCorr
		vardef=df
	;
	run;

	/* prep data for heatmap */
data &out.;
	keep x y r;
	set work._tmpCorr(where=(_TYPE_="CORR"));
	array v{*} _numeric_;
	x = _NAME_;
	do i = dim(v) to 1 by -1;
		y = vname(v(i));
		r = v(i);
		/* creates a diagonally sparse matrix */
		if (i<_n_) then
			r=.;
		output;
	end;
run;

proc datasets lib=work nolist nowarn;
	delete _tmpcorr;
quit;
%mend;

/* Build the graphs */
ods graphics /height=600 width=800 imagemap;
ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\products_&SYSDATE9..PDF";
%prepCorrData(in=AKM.products,out=product_r);
proc sgrender data=product_r template=corrHeatmap;
   dynamic _title="Corr matrix for products";
run;


*Create sequence;
proc sql;
create table Bank as
select *,monotonic() as seq
from AKM.Bankdata_DUMMIES;
run;

*Create table which products not active;
 PROC SQL;
  CREATE TABLE AKM.Inactive AS
  SELECT *
 FROM Bank
 WHERE DDA EQ 0 and Sav EQ 0 and CC EQ 0 and DirDep EQ 0 and
       NSF EQ 0 and ATM EQ 0 and CD EQ 0 and CC EQ 0 and IRA EQ 0 and
       LOC EQ 0 and ILS EQ 0 and MM EQ 0 and MTG EQ 0 and SDB EQ 0 and
       Inv EQ 0 ;
 run;

  *Create column show the account status;
DATA STATUS1;
SET AKM.Inactive;
Status='0';RUN;

*Merge new table with original index table;
DATA STATUS2;
    merge Bank
        STATUS1;
    by seq;
run;

*Fill the miss account status in original dataset; 
data AKM.STATUS;
	set STATUS2;
	Status = coalesce(Status,1);
run;

PROC CONTENTS DATA=AKM.STATUS;RUN;



*UNIVARIATE ANALYSIS;
ods graphics;
%MACRO BIVARIATE_CHISQ (DSN= ,Y=  );
PROC CONTENTS DATA = AKM.STATUS OUT= AKM.STUDY_VAR;
RUN;

PROC SQL;
SELECT NAME INTO : STUDY_CHAR_VAR SEPARATED BY " "
FROM AKM.STUDY_VAR
WHERE TYPE EQ 2
 AND NAME NOT IN ("&Y.");
QUIT;
%LET N = %SYSFUNC(COUNTW(&STUDY_CHAR_VAR));
%DO I = 1 %TO &N;	
%LET CHAR = %SCAN(&STUDY_CHAR_VAR,&I);
ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\RELATIONSHIP BET &CHAR. AND &Y..&SYSDATE9..PDF"; 
TITLE "RELATIONSHIP BETWEEN &CHAR. AND &Y. ";
PROC FREQ DATA = &DSN.; 
TABLE &CHAR. * &Y./CHISQ NOROW NOCOL PLOTS=FREQPLOT(groupby=row twoway=cluster) ;
RUN;
ODS PDF CLOSE;
%END;
%MEND;
%BIVARIATE_CHISQ (DSN=AKM.STATUS ,Y= Status)

*;

ods graphics;
ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\RELATIONSHIP BET InArea AND Status.&SYSDATE9..PDF"; 
TITLE "RELATIONSHIP BETWEEN InArea AND Status ";
PROC FREQ DATA = AKM.STATUS;  
TABLE InArea * Status/CHISQ NOROW NOCOL PLOTS=FREQPLOT(TWOWAY=STACKED SCALE=GROUPPCT) ;
RUN;

ODS PDF FILE = "F:\Metro\SAS Project\Data\Project PDF\RELATIONSHIP BET Moved AND Status.&SYSDATE9..PDF"; 
TITLE "RELATIONSHIP BETWEEN Moved AND Status ";
PROC FREQ DATA = AKM.STATUS;  
TABLE Moved * Status/CHISQ NOROW NOCOL PLOTS=FREQPLOT(groupby=row twoway=cluster) ;
RUN;



*Modeling
*SPLIT DATA INTO 70%(TRAINING) AND 30% TESTING;
PROC SURVEYSELECT DATA =AKM.STATUS  OUT=AKM.STATUS_SAMP  RATE = .7 OUTALL;
RUN;

PROC FREQ DATA = AKM.STATUS_SAMP;
 TABLE SELECTED;
RUN;

DATA TRAINING TESTING;
 SET AKM.STATUS_SAMP;
 IF SELECTED EQ 1 THEN OUTPUT TRAINING;
 ELSE IF SELECTED EQ 0 THEN OUTPUT TESTING;
RUN;


TITLE "LOGISTIC REGRESSION WITH ONE CATEGORICAL PREDICTOR VARIABLE";
PROC LOGISTIC DATA = TRAINING;
 CLASS Res (PARAM=REF REF ="R");*to create a dummy/design variable;
 MODEL Status (EVENT="0") = Res /CLODDS =PL;
RUN;

TITLE "USING A COMBINATION OF CONTINUOUS AND CATEGORICAL VARIABLES";
ODS PDF FILE = "F:\Metro\SAS Project\Data\Model.&SYSDATE9..PDF"; 
PROC LOGISTIC DATA = TRAINING plots(only) =(roc oddsratio);
	CLASS Branch (PARAM =REF REF= 'B1' )
		  AcctAge_Group(PARAM =REF REF = '1:< 1 ')
          Moved (PARAM =REF REF= '0 ' )
          InArea (PARAM =REF REF= '0 ' );
	MODEL Status (EVENT="0") = AcctAge_Group Branch Moved InArea/CLODDS =PL;                              
 RUN;
 QUIT;
*Model Performance;
*Model Score;

TITLE "MODEL SCORING: SCORING OPTION";
PROC LOGISTIC DATA = TRAINING ;
	CLASS Branch (PARAM =REF REF= 'B1' )
		  AcctAge_Group(PARAM =REF REF = '1:< 1 ')
          Moved (PARAM =REF REF= '0 ' )
          InArea (PARAM =REF REF= '0 ' );
	MODEL Status (EVENT="0") = AcctAge_Group Branch Moved InArea/CLODDS =PL;     
	Output out= test p=ppred;
	Score data=TESTING out = AKM.TESTING_SCORE;
 RUN;
 QUIT;