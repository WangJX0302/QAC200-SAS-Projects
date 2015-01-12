/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 3:27:05 PM
PROJECT: WangJ_SAS_Project_20150112
PROJECT PATH: P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp
---------------------------------------- */

/* Library assignment for Local.WJX */
Libname WJX BASE 'P:\QAC\qac200\students\jwang03' ;
/* Library assignment for Local.WJX */
Libname WJX BASE 'P:\QAC\qac200\students\jwang03' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (WJX)   */
%LET _CLIENTTASKLABEL='Assign Project Library (WJX)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
LIBNAME WJX BASE "P:\QAC\qac200\students\jwang03" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code SF12 variables   */
LIBNAME EC100007 "C:\Users\jwang03\Desktop\Assignment 5 20150112";


%LET _CLIENTTASKLABEL='Reverse Code SF12 variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_FOR_MEPS_FULLYR_2012__0006);

PROC SQL;
   CREATE TABLE WJX.QUERY_FOR_MEPS_FULLYR_2012__0006(label="QUERY_FOR_MEPS_FULLYR_2012__0006") AS 
   SELECT t1.DUPERSID, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS, 
          t1.ACCP_LESS_BC_PHY_PRBS, 
          t1.WORK_LIMT_BC_PJY_PROBS, 
          t1.ACCP_LESS_BC_MNT_PRBS, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.HAD_A_LOT_OF_ENERGY, 
          t1.FELT_DOWN, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          /* REVERSE_ADGENH42 */
            (6 - t1.HEALTH_IN_GENERAL) LABEL="Reverse Coded ADGENH42" AS REVERSE_ADGENH42, 
          /* REVERSE_ADPAIN42 */
            (6 - t1.PAIN_LIMITS_NORMAL_WORK) LABEL="Reverse coding ADPAIN42" AS REVERSE_ADPAIN42, 
          /* REVERSE_ADCAPE42 */
            (6 - t1.FELT_CALM_PEACEFUL) LABEL="Reverse coding ADCAPE42" AS REVERSE_ADCAPE42, 
          /* REVERSE_ADNRGY42 */
            (6 - t1.HAD_A_LOT_OF_ENERGY) LABEL="Reverse coding ADNRGY42" AS REVERSE_ADNRGY42
      FROM EC100007.query_for_meps_fullyr_2012__0004 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:34 PM
   By task: Table Analysis

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.REVERSE_ADGENH42, T.HEALTH_IN_GENERAL
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__0006 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES HEALTH_IN_GENERAL * REVERSE_ADGENH42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:34 PM
   By task: Table Analysis1

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAIN_LIMITS_NORMAL_WORK, T.REVERSE_ADPAIN42
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__0006 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PAIN_LIMITS_NORMAL_WORK * REVERSE_ADPAIN42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:34 PM
   By task: Table Analysis2

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.FELT_CALM_PEACEFUL, T.REVERSE_ADCAPE42
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__0006 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES FELT_CALM_PEACEFUL * REVERSE_ADCAPE42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:34 PM
   By task: Table Analysis3

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HAD_A_LOT_OF_ENERGY, T.REVERSE_ADNRGY42
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__0006 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES HAD_A_LOT_OF_ENERGY * REVERSE_ADNRGY42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Sum SF12 Variables   */
%LET _CLIENTTASKLABEL='Sum SF12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE);

PROC SQL;
   CREATE TABLE WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE(label="QUERY_FOR_MEPS_FULLYR_AGGREGATE") AS 
   SELECT t1.DUPERSID, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS, 
          t1.ACCP_LESS_BC_PHY_PRBS, 
          t1.WORK_LIMT_BC_PJY_PROBS, 
          t1.ACCP_LESS_BC_MNT_PRBS, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.HAD_A_LOT_OF_ENERGY, 
          t1.FELT_DOWN, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.REVERSE_ADGENH42, 
          t1.REVERSE_ADPAIN42, 
          t1.REVERSE_ADCAPE42, 
          t1.REVERSE_ADNRGY42, 
          /* SUM_SF12 */
            
            (SUM(t1.HLTH_LIMITS_MOD_ACTIVITIES,t1.HLTH_LIMITS_CLIMBING_STAIRS,t1.ACCP_LESS_BC_PHY_PRBS,t1.WORK_LIMT_BC_PJY_PROBS,t1.ACCP_LESS_BC_MNT_PRBS,t1.WORK_LIMT_BC_MNT_PROBS,t1.FELT_DOWN,t1.HLTH_STOPPED_SOC_ACTIV,t1.REVERSE_ADGENH42,t1.REVERSE_ADPAIN42,t1.REVERSE_ADCAPE42,t1.REVERSE_ADNRGY42)) 
            LABEL="Sum of 12SF" AS SUM_SF12
      FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__0006 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:34 PM
   By task: List Data

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HLTH_LIMITS_MOD_ACTIVITIES, T.HLTH_LIMITS_CLIMBING_STAIRS, T.ACCP_LESS_BC_PHY_PRBS, T.WORK_LIMT_BC_PJY_PROBS, T.ACCP_LESS_BC_MNT_PRBS, T.WORK_LIMT_BC_MNT_PROBS, T.FELT_DOWN, T.HLTH_STOPPED_SOC_ACTIV, T.REVERSE_ADGENH42
		     , T.REVERSE_ADPAIN42, T.REVERSE_ADCAPE42, T.REVERSE_ADNRGY42, T.SUM_SF12
	FROM WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE as T
;
QUIT;
TITLE;
TITLE1 "Check SF-12 Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR HLTH_LIMITS_MOD_ACTIVITIES HLTH_LIMITS_CLIMBING_STAIRS ACCP_LESS_BC_PHY_PRBS WORK_LIMT_BC_PJY_PROBS ACCP_LESS_BC_MNT_PRBS WORK_LIMT_BC_MNT_PROBS FELT_DOWN HLTH_STOPPED_SOC_ACTIV REVERSE_ADGENH42 REVERSE_ADPAIN42 REVERSE_ADCAPE42 REVERSE_ADNRGY42
	  SUM_SF12;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for SF12 Aggregate Variables   */
%LET _CLIENTTASKLABEL='Summary Statistics for SF12 Aggregate Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: Summary Statistics for SF12 Aggregate Variables

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12
	FROM WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 SF12 Aggregate Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_SF12;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for SF12 Aggregate Variables   */
%LET _CLIENTTASKLABEL='Distribution Analysis for SF12 Aggregate Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: Distribution Analysis for SF12 Aggregate Variables

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12
	FROM WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_SF12 Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_SF12;
	HISTOGRAM   SUM_SF12 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Sum SF12 Categorical   */
%LET _CLIENTTASKLABEL='Sum SF12 Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_AGGRE_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_AGGRE_0001 AS 
   SELECT t1.SUM_SF12, 
          /* SUM_SF12_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_SF12 >=2 and t1.SUM_SF12 <=32
               THEN 1
               WHEN t1.SUM_SF12 >32 and t1.SUM_SF12 <=41
               THEN 2
               WHEN t1.SUM_SF12 >41 and t1.SUM_SF12 <=48
               THEN 3
               WHEN t1.SUM_SF12 >48 and t1.SUM_SF12 <=52
               THEN 4
               WHEN t1.SUM_SF12 >52 
               THEN 5
            END) LABEL="Sum SF12 Categorical" AS SUM_SF12_CATEGORICAL
      FROM WJX.QUERY_FOR_MEPS_FULLYR_AGGREGATE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_AGGRE_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_AGGRE_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_SF12, T.SUM_SF12_CATEGORICAL
	FROM WORK.QUERY_FOR_MEPS_FULLYR_AGGRE_0001 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_SF12 /  SCORES=TABLE;
	TABLES SUM_SF12_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Sum Doctor Quality   */
LIBNAME EC100008 "C:\Users\jwang03\Desktop\Assignment 5 20150112";


%LET _CLIENTTASKLABEL='Sum Doctor Quality';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW);

PROC SQL;
   CREATE TABLE WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW(label="QUERY_FOR_MEPS_FULLYR_2012__NEW") AS 
   SELECT t1.DUPERSID, 
          t1.REGION12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUCYR, 
          t1.EMPST31, 
          t1.MARTIAL_STATUS, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS, 
          t1.ACCP_LESS_BC_PHY_PRBS, 
          t1.WORK_LIMT_BC_PJY_PROBS, 
          t1.ACCP_LESS_BC_MNT_PRBS, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.HAD_A_LOT_OF_ENERGY, 
          t1.FELT_DOWN, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.YEARS_EDUC, 
          t1.EMPLOYMENT_STATUS_53, 
          t1.EMPLOYMENT_STATS_31, 
          t1.'EMPLOYMENT_STATUS _42'n, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADPRX42, 
          t1.ADULT_PROXY, 
          t1.ILL_NEED_CARE, 
          t1.GOT_CARE_WHEN_NEEDED, 
          t1.MADE_APPT_ROUTINE, 
          t1.GOT_MED_APPT_WHEN_NEEDED, 
          t1.NEED_ANY_CARE, 
          t1.EXPLAIN_UNDERSTOOD, 
          t1.SHOW_RESPECT, 
          t1.HAD_TO_FILL_FORM, 
          t1.OFFERED_HELP_FILL_FORM, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.FAMINC12, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.TOTMCR12, 
          t1.MDUNAB42, 
          t1.STRKDX, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.OHRTDX, 
          t1.ERDEXP12, 
          t1.IPTEXP12, 
          t1.EASY_GET_MED_CARE, 
          t1.DOC_LISTEN_TO, 
          t1.DOC_SPENT_ENUF_TIME, 
          t1.RATING_HEALTH_CARE, 
          t1.DONT_NEED_HLTH_INS, 
          t1.HLTH_INS_WORTH, 
          t1.OVERALL_RATING_OF_FEELINGS, 
          t1.OVERCOME_WITHOUT_INSURC, 
          t1.CANCER, 
          t1.FELT_HOPELESS, 
          /* Sum_doctor_quality */
            (SUM(t1.EXPLAIN_UNDERSTOOD,t1.SHOW_RESPECT,t1.EASY_GET_MED_CARE,t1.DOC_LISTEN_TO,t1.DOC_SPENT_ENUF_TIME)) 
            LABEL="Sum of doctor quality" AS Sum_doctor_quality
      FROM EC100008.query_for_meps_fullyr_2012__0004 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: List Data1

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EXPLAIN_UNDERSTOOD, T.DOC_LISTEN_TO, T.SHOW_RESPECT, T.DOC_SPENT_ENUF_TIME, T.EASY_GET_MED_CARE, T.Sum_doctor_quality
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Dr. Quality Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR EXPLAIN_UNDERSTOOD DOC_LISTEN_TO SHOW_RESPECT DOC_SPENT_ENUF_TIME EASY_GET_MED_CARE Sum_doctor_quality;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Doctor Quality   */
%LET _CLIENTTASKLABEL='Summary Statistics for Doctor Quality';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: Summary Statistics for Doctor Quality

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_doctor_quality
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for Doctor Quality";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_doctor_quality;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Doctor Quality   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Doctor Quality';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:35 PM
   By task: Distribution Analysis for Doctor Quality

   Input Data: Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_doctor_quality
	FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Supervision of Doctor Quality";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_doctor_quality;
	HISTOGRAM   Sum_doctor_quality / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__NEW);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__NEW AS 
   SELECT /* DQ_Categorical */
            (CASE  
               WHEN t1.Sum_doctor_quality >=1 and t1.Sum_doctor_quality <=12
               THEN 1
               WHEN t1.Sum_doctor_quality >12 and t1.Sum_doctor_quality <=15
               THEN 2
               WHEN t1.Sum_doctor_quality >15 and t1.Sum_doctor_quality <=16
               THEN 3
               WHEN t1.Sum_doctor_quality >16 and t1.Sum_doctor_quality <=19
               THEN 4
               WHEN t1.Sum_doctor_quality >19
               THEN 5
            END) LABEL="DQ Categories" AS DQ_Categorical, 
          t1.Sum_doctor_quality
      FROM WJX.QUERY_FOR_MEPS_FULLYR_2012__NEW t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:36 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__NEW
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__NEW
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DQ_Categorical, T.Sum_doctor_quality
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__NEW as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DQ_Categorical /  SCORES=TABLE;
	TABLES Sum_doctor_quality /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter and Sort   */
%LET _CLIENTTASKLABEL='Filter and Sort';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.FILTER_FOR_MEPS_FULLYR_2012_0112);

PROC SQL;
   CREATE TABLE WJX.FILTER_FOR_MEPS_FULLYR_2012_0112(label="FILTER_FOR_MEPS_FULLYR_2012_0112") AS 
   SELECT t1.DUPERSID, 
          t1.MARRY12X, 
          t1.EDRECODE
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for Maritual Status and Education Level   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Maritual Status and Education Level';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:36 PM
   By task: One-Way Frequencies for Maritual Status and Education Level

   Input Data: Local:WJX.FILTER_FOR_MEPS_FULLYR_2012_0112
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.FILTER_FOR_MEPS_FULLYR_2012_0112
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRY12X, T.EDRECODE
	FROM WJX.FILTER_FOR_MEPS_FULLYR_2012_0112(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for Marital Status and Education Level";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES EDRECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recoded Missing Variables   */
%LET _CLIENTTASKLABEL='Recoded Missing Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_REV2);

PROC SQL;
   CREATE TABLE WJX.QUERY_REV2(label="QUERY_REV2") AS 
   SELECT t1.DUPERSID, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          /* MARITUAL_STATUS */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Maritual Status (Recoded missing)" AS MARITUAL_STATUS, 
          /* EDUCATION_RECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education Level (recoded missing)" AS EDUCATION_RECODE
      FROM WJX.FILTER_FOR_MEPS_FULLYR_2012_0112 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Maritual Status   */
%LET _CLIENTTASKLABEL='Maritual Status';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:36 PM
   By task: Maritual Status

   Input Data: Local:WJX.QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARITUAL_STATUS, T.MARRY12X
	FROM WJX.QUERY_REV2 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARITUAL_STATUS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Education Level   */
%LET _CLIENTTASKLABEL='Education Level';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:37 PM
   By task: Education Level

   Input Data: Local:WJX.QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCATION_RECODE, T.EDRECODE
	FROM WJX.QUERY_REV2 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE * EDUCATION_RECODE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Marital Status   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Marital Status';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:37 PM
   By task: Distribution Analysis for Marital Status

   Input Data: Local:WJX.QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARITUAL_STATUS
	FROM WJX.QUERY_REV2(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Marital Status";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR MARITUAL_STATUS;
	HISTOGRAM   MARITUAL_STATUS / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Education Level   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Education Level';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:37 PM
   By task: Distribution Analysis for Education Level

   Input Data: Local:WJX.QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCATION_RECODE
	FROM WJX.QUERY_REV2(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Education Level";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR EDUCATION_RECODE;
	HISTOGRAM   EDUCATION_RECODE / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_REV2);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_REV2 AS 
   SELECT /* Marital_Status_Cate */
            (CASE  
               WHEN t1.MARITUAL_STATUS >=1 and t1.MARITUAL_STATUS <=4
               THEN 1
               WHEN t1.MARITUAL_STATUS =5
               THEN 2
            END) LABEL="Marital_Status_Cate" AS Marital_Status_Cate, 
          t1.DUPERSID, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.MARITUAL_STATUS, 
          t1.EDUCATION_RECODE, 
          /* EDRECODE_Categories */
            (CASE  
               WHEN t1.EDUCATION_RECODE = 0
               THEN 1
              WHEN t1.EDUCATION_RECODE >=1 and t1.EDUCATION_RECODE <= 6
               THEN 2
              WHEN t1.EDUCATION_RECODE >=7 and t1.EDUCATION_RECODE <=12
               THEN 3
              WHEN t1.EDUCATION_RECODE >=13 and t1.EDUCATION_RECODE <=14
               THEN 4
              WHEN t1.EDUCATION_RECODE >14
               THEN 5
            END) LABEL="Education Level Categories" AS EDRECODE_Categories
      FROM WJX.QUERY_REV2 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:37 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Marital_Status_Cate, T.MARITUAL_STATUS
	FROM WORK.QUERY_FOR_QUERY_REV2 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Marital_Status_Cate /  SCORES=TABLE;
	TABLES MARITUAL_STATUS /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwang03\Assignments\Assignment 5 20150112\WangJ_SAS_Project_20150112.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150112.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:26:38 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_QUERY_REV2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_REV2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.EDRECODE, T.EDRECODE_Categories
	FROM WORK.QUERY_FOR_QUERY_REV2 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES EDRECODE /  SCORES=TABLE;
	TABLES EDRECODE_Categories /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
