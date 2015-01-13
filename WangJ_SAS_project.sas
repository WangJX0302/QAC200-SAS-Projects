/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 11:37:43 AM
PROJECT: WangJ_SAS_Project_20150113
PROJECT PATH: C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp
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
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
LIBNAME WJX BASE "P:\QAC\qac200\students\jwang03" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
LIBNAME EC100068 "C:\Users\jwang03\Desktop\Assignment 6 20150113\MEPS";


%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000 AS 
   SELECT /* INER */
            (1) LABEL="INER" AS INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU
      FROM EC100068.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
LIBNAME EC100070 "C:\Users\jwang03\Desktop\Assignment 6 20150113";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_2012_FUL_M__0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_2012_FUL_M__0000 AS 
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
          t1.DUPERSID1, 
          t1.Marital_Status_Cate, 
          t1.DUPERSID2, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.MARITUAL_STATUS, 
          t1.EDUCATION_RECODE, 
          t1.EDRECODE_Categories, 
          t1.HEALTH_IN_GENERAL1, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES1, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS1, 
          t1.ACCP_LESS_BC_PHY_PRBS1, 
          t1.WORK_LIMT_BC_PJY_PROBS1, 
          t1.ACCP_LESS_BC_MNT_PRBS1, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.PAIN_LIMITS_NORMAL_WORK1, 
          t1.FELT_CALM_PEACEFUL1, 
          t1.HAD_A_LOT_OF_ENERGY1, 
          t1.FELT_DOWN1, 
          t1.HLTH_STOPPED_SOC_ACTIV1, 
          t1.REVERSE_ADGENH421, 
          t1.REVERSE_ADPAIN421, 
          t1.REVERSE_ADCAPE421, 
          t1.REVERSE_ADNRGY421, 
          t1.SUM_SF12, 
          t1.DUPERSID3, 
          t1.REGION12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X1, 
          t1.EDUCYR, 
          t1.EMPST31, 
          t1.MARTIAL_STATUS, 
          t1.HEALTH_IN_GENERAL2, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES2, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS2, 
          t1.ACCP_LESS_BC_PHY_PRBS2, 
          t1.WORK_LIMT_BC_PJY_PROBS2, 
          t1.ACCP_LESS_BC_MNT_PRBS2, 
          t1.WORK_LIMT_BC_MNT_PROBS2, 
          t1.PAIN_LIMITS_NORMAL_WORK2, 
          t1.FELT_CALM_PEACEFUL2, 
          t1.HAD_A_LOT_OF_ENERGY2, 
          t1.FELT_DOWN2, 
          t1.HLTH_STOPPED_SOC_ACTIV2, 
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
          /* INFULLYR */
            (1) LABEL="INFULLYR" AS INFULLYR
      FROM EC100070.query_2012_ful_m t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_ER_2012_M);

PROC SQL;
   CREATE TABLE WJX.QUERY_ER_2012_M(label="QUERY_ER_2012_M") AS 
   SELECT t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t2.DUPERSID AS DUPERSID1, 
          t2.HEALTH_IN_GENERAL, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS, 
          t2.ACCP_LESS_BC_PHY_PRBS, 
          t2.WORK_LIMT_BC_PJY_PROBS, 
          t2.ACCP_LESS_BC_MNT_PRBS, 
          t2.WORK_LIMT_BC_MNT_PROBS, 
          t2.PAIN_LIMITS_NORMAL_WORK, 
          t2.FELT_CALM_PEACEFUL, 
          t2.HAD_A_LOT_OF_ENERGY, 
          t2.FELT_DOWN, 
          t2.HLTH_STOPPED_SOC_ACTIV, 
          t2.REVERSE_ADGENH42, 
          t2.REVERSE_ADPAIN42, 
          t2.REVERSE_ADCAPE42, 
          t2.REVERSE_ADNRGY42, 
          t2.DUPERSID1 AS DUPERSID11, 
          t2.Marital_Status_Cate, 
          t2.DUPERSID2, 
          t2.MARRY12X, 
          t2.EDRECODE, 
          t2.MARITUAL_STATUS, 
          t2.EDUCATION_RECODE, 
          t2.EDRECODE_Categories, 
          t2.HEALTH_IN_GENERAL1, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES1, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS1, 
          t2.ACCP_LESS_BC_PHY_PRBS1, 
          t2.WORK_LIMT_BC_PJY_PROBS1, 
          t2.ACCP_LESS_BC_MNT_PRBS1, 
          t2.WORK_LIMT_BC_MNT_PROBS1, 
          t2.PAIN_LIMITS_NORMAL_WORK1, 
          t2.FELT_CALM_PEACEFUL1, 
          t2.HAD_A_LOT_OF_ENERGY1, 
          t2.FELT_DOWN1, 
          t2.HLTH_STOPPED_SOC_ACTIV1, 
          t2.REVERSE_ADGENH421, 
          t2.REVERSE_ADPAIN421, 
          t2.REVERSE_ADCAPE421, 
          t2.REVERSE_ADNRGY421, 
          t2.SUM_SF12, 
          t2.DUPERSID3, 
          t2.REGION12, 
          t2.AGE12X, 
          t2.SEX, 
          t2.RACETHX, 
          t2.MARRY12X1, 
          t2.EDUCYR, 
          t2.EMPST31, 
          t2.MARTIAL_STATUS, 
          t2.HEALTH_IN_GENERAL2, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES2, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS2, 
          t2.ACCP_LESS_BC_PHY_PRBS2, 
          t2.WORK_LIMT_BC_PJY_PROBS2, 
          t2.ACCP_LESS_BC_MNT_PRBS2, 
          t2.WORK_LIMT_BC_MNT_PROBS2, 
          t2.PAIN_LIMITS_NORMAL_WORK2, 
          t2.FELT_CALM_PEACEFUL2, 
          t2.HAD_A_LOT_OF_ENERGY2, 
          t2.FELT_DOWN2, 
          t2.HLTH_STOPPED_SOC_ACTIV2, 
          t2.YEARS_EDUC, 
          t2.EMPLOYMENT_STATUS_53, 
          t2.EMPLOYMENT_STATS_31, 
          t2.'EMPLOYMENT_STATUS _42'n, 
          t2.EMPST42, 
          t2.EMPST53, 
          t2.ADPRX42, 
          t2.ADULT_PROXY, 
          t2.ILL_NEED_CARE, 
          t2.GOT_CARE_WHEN_NEEDED, 
          t2.MADE_APPT_ROUTINE, 
          t2.GOT_MED_APPT_WHEN_NEEDED, 
          t2.NEED_ANY_CARE, 
          t2.EXPLAIN_UNDERSTOOD, 
          t2.SHOW_RESPECT, 
          t2.HAD_TO_FILL_FORM, 
          t2.OFFERED_HELP_FILL_FORM, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADAPPT42, 
          t2.ADNDCR42, 
          t2.ADEGMC42, 
          t2.ADLIST42, 
          t2.ADEXPL42, 
          t2.ADRESP42, 
          t2.ADPRTM42, 
          t2.ADINST42, 
          t2.ADEZUN42, 
          t2.ADTLHW42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADHECR42, 
          t2.ADSMOK42, 
          t2.ADNSMK42, 
          t2.ADDRBP42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADGENH42, 
          t2.ADDAYA42, 
          t2.ADCLIM42, 
          t2.ADPALS42, 
          t2.ADPWLM42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADPAIN42, 
          t2.ADCAPE42, 
          t2.ADNRGY42, 
          t2.ADDOWN42, 
          t2.ADSOCA42, 
          t2.PCS42, 
          t2.MCS42, 
          t2.SFFLAG42, 
          t2.ADNERV42, 
          t2.ADHOPE42, 
          t2.ADREST42, 
          t2.ADSAD42, 
          t2.ADEFRT42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.ADINTR42, 
          t2.ADDPRS42, 
          t2.PHQ242, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADRISK42, 
          t2.ADOVER42, 
          t2.ADCMPM42, 
          t2.ADCMPD42, 
          t2.ADCMPY42, 
          t2.ADLANG42, 
          t2.FAMINC12, 
          t2.INSCOV12, 
          t2.INSURC12, 
          t2.TOTMCR12, 
          t2.MDUNAB42, 
          t2.STRKDX, 
          t2.ANGIDX, 
          t2.ARTHDX, 
          t2.ASTHDX, 
          t2.CANCERDX, 
          t2.OHRTDX, 
          t2.ERDEXP12, 
          t2.IPTEXP12, 
          t2.EASY_GET_MED_CARE, 
          t2.DOC_LISTEN_TO, 
          t2.DOC_SPENT_ENUF_TIME, 
          t2.RATING_HEALTH_CARE, 
          t2.DONT_NEED_HLTH_INS, 
          t2.HLTH_INS_WORTH, 
          t2.OVERALL_RATING_OF_FEELINGS, 
          t2.OVERCOME_WITHOUT_INSURC, 
          t2.CANCER, 
          t2.FELT_HOPELESS, 
          t2.INFULLYR, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU
      FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000 t1
           FULL JOIN WORK.QUERY_FOR_QUERY_2012_FUL_M__0000 t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t1.INER = 1 AND t2.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data for Merged Dataset   */
%LET _CLIENTTASKLABEL='List Data for Merged Dataset';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:06 AM
   By task: List Data for Merged Dataset

   Input Data: Local:WJX.QUERY_ER_2012_M
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_ER_2012_M
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.INER, T.DUPERSID1, T.DUPERSID
	FROM WJX.QUERY_ER_2012_M(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing for Merged Dataset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR INER DUPERSID1 DUPERSID;
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:07 AM
   By task: Data Set Attributes

   Input Data: Local:WJX.QUERY_ER_2012_M
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WJX.CONTCONTENTSFORQUERY_ER_2012_M);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WJX.QUERY_ER_2012_M OUT=WORK.SUCOUT1;

RUN;

DATA WJX.CONTCONTENTSFORQUERY_ER_2012_M(LABEL="Contents Details for QUERY_ER_2012_M");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WJX.CONTCONTENTSFORQUERY_ER_2012_M
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_ER_2012_M';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WJX.CONTCONTENTSFORQUERY_ER_2012_M OUT=WJX.CONTCONTENTSFORQUERY_ER_2012_M;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WJX.CONTCONTENTSFORQUERY_ER_2012_M
		WHERE memname='QUERY_ER_2012_M';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recoded Missing for MRI & Xrays   */
%LET _CLIENTTASKLABEL='Recoded Missing for MRI & Xrays';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_ER_2012_M_Recoded);

PROC SQL;
   CREATE TABLE WJX.QUERY_ER_2012_M_Recoded(label="QUERY_ER_2012_M_Recoded") AS 
   SELECT t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.DUPERSID1, 
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
          t1.DUPERSID11, 
          t1.Marital_Status_Cate, 
          t1.DUPERSID2, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.MARITUAL_STATUS, 
          t1.EDUCATION_RECODE, 
          t1.EDRECODE_Categories, 
          t1.HEALTH_IN_GENERAL1, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES1, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS1, 
          t1.ACCP_LESS_BC_PHY_PRBS1, 
          t1.WORK_LIMT_BC_PJY_PROBS1, 
          t1.ACCP_LESS_BC_MNT_PRBS1, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.PAIN_LIMITS_NORMAL_WORK1, 
          t1.FELT_CALM_PEACEFUL1, 
          t1.HAD_A_LOT_OF_ENERGY1, 
          t1.FELT_DOWN1, 
          t1.HLTH_STOPPED_SOC_ACTIV1, 
          t1.REVERSE_ADGENH421, 
          t1.REVERSE_ADPAIN421, 
          t1.REVERSE_ADCAPE421, 
          t1.REVERSE_ADNRGY421, 
          t1.SUM_SF12, 
          t1.DUPERSID3, 
          t1.REGION12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X1, 
          t1.EDUCYR, 
          t1.EMPST31, 
          t1.MARTIAL_STATUS, 
          t1.HEALTH_IN_GENERAL2, 
          t1.HLTH_LIMITS_MOD_ACTIVITIES2, 
          t1.HLTH_LIMITS_CLIMBING_STAIRS2, 
          t1.ACCP_LESS_BC_PHY_PRBS2, 
          t1.WORK_LIMT_BC_PJY_PROBS2, 
          t1.ACCP_LESS_BC_MNT_PRBS2, 
          t1.WORK_LIMT_BC_MNT_PROBS2, 
          t1.PAIN_LIMITS_NORMAL_WORK2, 
          t1.FELT_CALM_PEACEFUL2, 
          t1.HAD_A_LOT_OF_ENERGY2, 
          t1.FELT_DOWN2, 
          t1.HLTH_STOPPED_SOC_ACTIV2, 
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
          t1.INFULLYR, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* MRI_ */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="MRI recoded missing" AS MRI_, 
          /* XRAYS_ */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="Xrays recoded missing" AS XRAYS_
      FROM WJX.QUERY_ER_2012_M t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for MRI   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for MRI';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:08 AM
   By task: One-Way Frequencies for MRI

   Input Data: Local:WJX.QUERY_ER_2012_M_RECODED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_ER_2012_M_RECODED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MRI_
	FROM WJX.QUERY_ER_2012_M_RECODED(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MRI";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_ / NOCUM  SCORES=TABLE;
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


/*   START OF NODE: One-Way Frequencies for Xrays   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Xrays';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:08 AM
   By task: One-Way Frequencies for Xrays

   Input Data: Local:WJX.QUERY_ER_2012_M_RECODED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WJX.QUERY_ER_2012_M_RECODED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_
	FROM WJX.QUERY_ER_2012_M_RECODED(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for Xrays";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_ / NOCUM  SCORES=TABLE;
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


/*   START OF NODE: Count variable   */
%LET _CLIENTTASKLABEL='Count variable';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID1 */
            (COUNT(t1.DUPERSID1)) AS COUNT_of_DUPERSID1
      FROM WJX.QUERY_ER_2012_M_RECODED t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for Count Variables   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Count Variables';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:08 AM
   By task: One-Way Frequencies for Count Variables

   Input Data: Local:WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID1
	FROM WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for Count Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 / NOCUM  SCORES=TABLE;
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


/*   START OF NODE: Distribution Analysis for number of ER Visits   */
%LET _CLIENTTASKLABEL='Distribution Analysis for number of ER Visits';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:09 AM
   By task: Distribution Analysis for number of ER Visits

   Input Data: Local:WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1
	FROM WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Counts of Visits for ER";
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
	FREQ
;
	VAR COUNT_of_DUPERSID1;
	HISTOGRAM   COUNT_of_DUPERSID1 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: Count and Original Merged   */
%LET _CLIENTTASKLABEL='Count and Original Merged';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WJX.QUERY_ER_2012_M_R_1);

PROC SQL;
   CREATE TABLE WJX.QUERY_ER_2012_M_R_1(label="QUERY_ER_2012_M_R_1") AS 
   SELECT t1.DUPERSID, 
          t1.COUNT_of_DUPERSID1, 
          t2.INER, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.DUPERSID1 AS DUPERSID11, 
          t2.HEALTH_IN_GENERAL, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS, 
          t2.ACCP_LESS_BC_PHY_PRBS, 
          t2.WORK_LIMT_BC_PJY_PROBS, 
          t2.ACCP_LESS_BC_MNT_PRBS, 
          t2.WORK_LIMT_BC_MNT_PROBS, 
          t2.PAIN_LIMITS_NORMAL_WORK, 
          t2.FELT_CALM_PEACEFUL, 
          t2.HAD_A_LOT_OF_ENERGY, 
          t2.FELT_DOWN, 
          t2.HLTH_STOPPED_SOC_ACTIV, 
          t2.REVERSE_ADGENH42, 
          t2.REVERSE_ADPAIN42, 
          t2.REVERSE_ADCAPE42, 
          t2.REVERSE_ADNRGY42, 
          t2.DUPERSID11 AS DUPERSID111, 
          t2.Marital_Status_Cate, 
          t2.DUPERSID2, 
          t2.MARRY12X, 
          t2.EDRECODE, 
          t2.MARITUAL_STATUS, 
          t2.EDUCATION_RECODE, 
          t2.EDRECODE_Categories, 
          t2.HEALTH_IN_GENERAL1, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES1, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS1, 
          t2.ACCP_LESS_BC_PHY_PRBS1, 
          t2.WORK_LIMT_BC_PJY_PROBS1, 
          t2.ACCP_LESS_BC_MNT_PRBS1, 
          t2.WORK_LIMT_BC_MNT_PROBS1, 
          t2.PAIN_LIMITS_NORMAL_WORK1, 
          t2.FELT_CALM_PEACEFUL1, 
          t2.HAD_A_LOT_OF_ENERGY1, 
          t2.FELT_DOWN1, 
          t2.HLTH_STOPPED_SOC_ACTIV1, 
          t2.REVERSE_ADGENH421, 
          t2.REVERSE_ADPAIN421, 
          t2.REVERSE_ADCAPE421, 
          t2.REVERSE_ADNRGY421, 
          t2.SUM_SF12, 
          t2.DUPERSID3, 
          t2.REGION12, 
          t2.AGE12X, 
          t2.SEX, 
          t2.RACETHX, 
          t2.MARRY12X1, 
          t2.EDUCYR, 
          t2.EMPST31, 
          t2.MARTIAL_STATUS, 
          t2.HEALTH_IN_GENERAL2, 
          t2.HLTH_LIMITS_MOD_ACTIVITIES2, 
          t2.HLTH_LIMITS_CLIMBING_STAIRS2, 
          t2.ACCP_LESS_BC_PHY_PRBS2, 
          t2.WORK_LIMT_BC_PJY_PROBS2, 
          t2.ACCP_LESS_BC_MNT_PRBS2, 
          t2.WORK_LIMT_BC_MNT_PROBS2, 
          t2.PAIN_LIMITS_NORMAL_WORK2, 
          t2.FELT_CALM_PEACEFUL2, 
          t2.HAD_A_LOT_OF_ENERGY2, 
          t2.FELT_DOWN2, 
          t2.HLTH_STOPPED_SOC_ACTIV2, 
          t2.YEARS_EDUC, 
          t2.EMPLOYMENT_STATUS_53, 
          t2.EMPLOYMENT_STATS_31, 
          t2.'EMPLOYMENT_STATUS _42'n, 
          t2.EMPST42, 
          t2.EMPST53, 
          t2.ADPRX42, 
          t2.ADULT_PROXY, 
          t2.ILL_NEED_CARE, 
          t2.GOT_CARE_WHEN_NEEDED, 
          t2.MADE_APPT_ROUTINE, 
          t2.GOT_MED_APPT_WHEN_NEEDED, 
          t2.NEED_ANY_CARE, 
          t2.EXPLAIN_UNDERSTOOD, 
          t2.SHOW_RESPECT, 
          t2.HAD_TO_FILL_FORM, 
          t2.OFFERED_HELP_FILL_FORM, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADAPPT42, 
          t2.ADNDCR42, 
          t2.ADEGMC42, 
          t2.ADLIST42, 
          t2.ADEXPL42, 
          t2.ADRESP42, 
          t2.ADPRTM42, 
          t2.ADINST42, 
          t2.ADEZUN42, 
          t2.ADTLHW42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADHECR42, 
          t2.ADSMOK42, 
          t2.ADNSMK42, 
          t2.ADDRBP42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADGENH42, 
          t2.ADDAYA42, 
          t2.ADCLIM42, 
          t2.ADPALS42, 
          t2.ADPWLM42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADPAIN42, 
          t2.ADCAPE42, 
          t2.ADNRGY42, 
          t2.ADDOWN42, 
          t2.ADSOCA42, 
          t2.PCS42, 
          t2.MCS42, 
          t2.SFFLAG42, 
          t2.ADNERV42, 
          t2.ADHOPE42, 
          t2.ADREST42, 
          t2.ADSAD42, 
          t2.ADEFRT42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.ADINTR42, 
          t2.ADDPRS42, 
          t2.PHQ242, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADRISK42, 
          t2.ADOVER42, 
          t2.ADCMPM42, 
          t2.ADCMPD42, 
          t2.ADCMPY42, 
          t2.ADLANG42, 
          t2.FAMINC12, 
          t2.INSCOV12, 
          t2.INSURC12, 
          t2.TOTMCR12, 
          t2.MDUNAB42, 
          t2.STRKDX, 
          t2.ANGIDX, 
          t2.ARTHDX, 
          t2.ASTHDX, 
          t2.CANCERDX, 
          t2.OHRTDX, 
          t2.ERDEXP12, 
          t2.IPTEXP12, 
          t2.EASY_GET_MED_CARE, 
          t2.DOC_LISTEN_TO, 
          t2.DOC_SPENT_ENUF_TIME, 
          t2.RATING_HEALTH_CARE, 
          t2.DONT_NEED_HLTH_INS, 
          t2.HLTH_INS_WORTH, 
          t2.OVERALL_RATING_OF_FEELINGS, 
          t2.OVERCOME_WITHOUT_INSURC, 
          t2.CANCER, 
          t2.FELT_HOPELESS, 
          t2.INFULLYR, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.MRI_, 
          t2.XRAYS_
      FROM WORK.QUERY_FOR_QUERY_ER_2012_M_RECODE t1
           INNER JOIN WJX.QUERY_ER_2012_M_RECODED t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ER Visits Categorical   */
%LET _CLIENTTASKLABEL='ER Visits Categorical';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_ER_2012_M_R_1);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_ER_2012_M_R_1 AS 
   SELECT /* Count_ER_Visits */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID1 =1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID1 =2
               THEN 2
               WHEN t1.COUNT_of_DUPERSID1 >2
               THEN 3
            END) LABEL="Count of ER visits" AS Count_ER_Visits, 
          t1.COUNT_of_DUPERSID1
      FROM WJX.QUERY_ER_2012_M_R_1 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ER Visits for ER Visits   */
%LET _CLIENTTASKLABEL='ER Visits for ER Visits';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:10 AM
   By task: ER Visits for ER Visits

   Input Data: Local:WORK.QUERY_FOR_QUERY_ER_2012_M_R_1
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_ER_2012_M_R_1
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1, T.Count_ER_Visits
	FROM WORK.QUERY_FOR_QUERY_ER_2012_M_R_1(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ER Visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 * Count_ER_Visits /
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


/*   START OF NODE: One-Way Frequencies for ER Visits Categories   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for ER Visits Categories';
%LET _CLIENTPROJECTPATH='C:\Users\jwang03\Desktop\Assignment 6 20150113\WangJ_SAS_Project_20150113.egp';
%LET _CLIENTPROJECTNAME='WangJ_SAS_Project_20150113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:37:10 AM
   By task: One-Way Frequencies for ER Visits Categories

   Input Data: Local:WORK.QUERY_FOR_QUERY_ER_2012_M_R_1
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_ER_2012_M_R_1
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Count_ER_Visits, T.COUNT_of_DUPERSID1
	FROM WORK.QUERY_FOR_QUERY_ER_2012_M_R_1(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for ER Visits Categories";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Jingxuan Wang";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Count_ER_Visits / NOCUM  SCORES=TABLE;
	TABLES COUNT_of_DUPERSID1 / NOCUM  SCORES=TABLE;
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
