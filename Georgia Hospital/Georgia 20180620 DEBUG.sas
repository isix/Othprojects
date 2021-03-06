LIBNAME GEORGIA V9 '/home/isaias.prestes/Data' ;

******************
* Exploring data *
******************;

%MACRO DEBUGSELECTION(FLAG, STRRGX);
	DATA DBG ;
		SET TAB_Merge ;
		FLAG_SELECT = 0;
		IF PRXMATCH(&STRRGX., STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_SELECT = 1 ;
		IF FLAG_SELECT = 0 THEN DELETE ;
	RUN;
	
	PROC SQL ;
		CREATE TABLE TMP AS
			SELECT DISTINCT EXAME_DIAGNOSTICO FROM DBG ;
	QUIT;
	
	title "&FLAG. - &STRRGX.";
	PROC PRINT DATA=TMP ;
	title;
	
	PROC SQL ;
		DROP TABLE TMP, DBG ;
	QUIT;
%MEND ;

%DEBUGSELECTION(FLAG_DIABETES, '/DIABETES MELLITUS/');

%DEBUGSELECTION(CARDIOVACULAR, '/ATEROSCLERO/');

%DEBUGSELECTION(FLAG_HIPERTENSAO, '/HIPERTENSAO ESSENCIAL/');


