/* MATT'S FIRST ATTEMPT AT GETTING PROC TRAJ OUTPUT FROM MEDICARE DATA */
LIBNAME MATT "S:\USER\MMASTERS\Medicare\Corinne-Frailty\Data" ;

PROC IMPORT DATAFILE="S:\USER\MMASTERS\Medicare\Corinne-Frailty\Data\TRAJTest.csv" OUT=TRAJDATA DBMS=CSV REPLACE; RUN;

PROC UNIVARIATE DATA=TRAJDATA; VAR VAL0; HISTOGRAM; RUN;

DATA TRAJDATA2; SET TRAJDATA; IF INCCANV = "" THEN HASCA=0; ELSE HASCA=1; RUN;

PROC TRAJ DATA=TRAJDATA2 OUTPLOT=OP OUTSTAT=OS OUT=OF OUTEST=OE ITDETAIL;
ID BENE_ID; VAR VAL0-VAL15; INDEP TIME0-TIME15;
MODEL CNORM; MAX 15; NGROUPS 8; ORDER 2 2 2 2 2 2 2 2;
RUN;

%TRAJPLOT(OP,OS,'Comorbidities Over Time','Cnorm Model','NCI Rolling Score','Year Num')

DATA TRAJNOCA; SET TRAJDATA2; WHERE HASCA=0; RUN;
DATA TRAJHASCA; SET TRAJDATA2; WHERE HASCA=1; RUN;

PROC TRAJ DATA=TRAJHASCA OUTPLOT=OP OUTSTAT=OS OUT=OF OUTEST=OE ITDETAIL;
ID BENE_ID; VAR VAL0-VAL15; INDEP TIME0-TIME15;
MODEL CNORM; MAX 15; NGROUPS 5; ORDER 2 2 2 2 2;
RUN;

%TRAJPLOT(OP,OS,'Comorbidities Over Time','Cnorm Model','NCI Rolling Score','Year Num')
