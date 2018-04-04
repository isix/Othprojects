LIBNAME EXM V9 "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia" ;

PROC IMPORT OUT= EXM.EXAMES_2014 
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\dados_2014_NUTRICAO.xlsx" 
            DBMS=EXCEL REPLACE;
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT= EXM.EXAMES_2015
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\dados_2015_NUTRICAO.xlsx" 
            DBMS=EXCEL REPLACE;
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT= EXM.EXAMES_2016 
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\dados_2016_NUTRICAO.xlsx" 
            DBMS=EXCEL REPLACE;
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

LIBNAME AGS V9 "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia" ;

PROC IMPORT OUT= AGS.AGS_2014 
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\Source\AGS_2014_LIMPO_UNIAO_ANO_27.09.017.sav" 
            DBMS=SAV REPLACE;
			fmtlib = WORK.FORMATS;
RUN;

PROC IMPORT OUT= AGS.AGS_2015
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\Source\AGS_2015_LIMPO_UNIAO_ANO_27.09.2017.sav" 
            DBMS=SAV REPLACE;
			fmtlib = WORK.FORMATS;
RUN;

PROC IMPORT OUT= AGS.AGS_2016 
            DATAFILE= "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\Source\AGS_2016.sav" 
            DBMS=SAV REPLACE;
			fmtlib = WORK.FORMATS;
RUN;

PROC CONTENTS data=AGS.ags_2014 ; RUN;
PROC CONTENTS data=AGS.ags_2015 ; RUN;
PROC CONTENTS data=AGS.ags_2016 ; RUN;

PROC CONTENTS data=EXM.EXAMES_2014 ; RUN;
PROC CONTENTS data=EXM.EXAMES_2015 ; RUN;
PROC CONTENTS data=EXM.EXAMES_2016 ; RUN;

DATA AGS.Ags_2016 ;
	SET AGS.Ags_2016 ;
	IF MISSING(NOME) THEN DELETE ;
	IF MISSING(PRT) THEN DELETE ;
RUN;
DATA AGS.Ags_2015 ;
	SET AGS.Ags_2015 ;
	IF MISSING(NOME) THEN DELETE ;
	IF MISSING(PRT) THEN DELETE ;
RUN;
DATA AGS.Ags_2014 ;
	SET AGS.Ags_2014 ;
	IF MISSING(NOME) THEN DELETE ;
	IF MISSING(PRT) THEN DELETE ;
RUN;

DATA EXM.EXAMES_2014 ;
	SET EXM.EXAMES_2014 ;
	PRT = PACIENTEID * 1 ;
	EXAME_NOME = NOME ;
	EXAME_DIAGNOSTICO = DIAGNOSTICO ;
RUN;
DATA EXM.EXAMES_2015 ;
	SET EXM.EXAMES_2015 ;
	PRT = PACIENTEID * 1 ;
	EXAME_NOME = NOME ;
	EXAME_DIAGNOSTICO = DIAGNOSTICO ;
RUN;
DATA EXM.EXAMES_2016 ;
	SET EXM.EXAMES_2016 ;
	PRT = PACIENTEID * 1 ;
	EXAME_NOME = NOME ;
	EXAME_DIAGNOSTICO = DIAGNOSTICO ;
RUN;

PROC SQL ;
	CREATE TABLE TAB_MERGE_2014 AS
		SELECT A.*, B.* FROM AGS.ags_2014  AS A
		LEFT JOIN EXM.EXAMES_2014 AS B
		ON A.PRT = B.PRT ;
	CREATE TABLE TAB_MERGE_2015 AS
		SELECT A.*, B.* FROM AGS.ags_2015  AS A
		LEFT JOIN EXM.EXAMES_2015 AS B
		ON A.PRT = B.PRT ;
	CREATE TABLE TAB_MERGE_2016 AS
		SELECT A.*, B.* FROM AGS.ags_2016  AS A
		LEFT JOIN EXM.EXAMES_2016 AS B
		ON A.PRT = B.PRT ;
QUIT;

LIBNAME OUT V9 "C:\Users\Isaias Prestes\Documents\Statistical_Analysis\Georgia\Data" ; RUN;

* FLAGS PARA OS MERGE ;
DATA TAB_MERGE_2014 ;
	SET TAB_MERGE_2014 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

DATA TAB_MERGE_2015 ;
	SET TAB_MERGE_2015 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

DATA TAB_MERGE_2016 ;
	SET TAB_MERGE_2016 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(EXAME_DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

PROC FREQ data=TAB_MERGE_2014 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

PROC FREQ data=TAB_MERGE_2015 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

PROC FREQ data=TAB_MERGE_2016 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

* APENAS EXAMES COM FLAGS ;

DATA EXM.EXAMES_2014 ;
	SET EXM.EXAMES_2014 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

DATA EXM.EXAMES_2015 ;
	SET EXM.EXAMES_2015 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

DATA EXM.EXAMES_2016 ;
	SET EXM.EXAMES_2016 ;
	FLAG_DESNUTRICAO = 0 ;
	IF PRXMATCH('/DESNUTRICAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_DESNUTRICAO = 1 ;
	LABEL FLAG_DESNUTRICAO="Desnutrio exame binria" ;
	FLAG_CANCER = 0 ;
	IF PRXMATCH('/NEOPLASIA/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/NEOPLASIAS/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMOR/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMEFACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CANCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/CNCER/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORES/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMORACAO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	IF PRXMATCH('/TUMO/', STRIP(UPCASE(DIAGNOSTICO)))>0 THEN FLAG_CANCER = 1 ;
	LABEL FLAG_CANCER="Cncer binria" ;
	DIF_INTER_EXAME = DATDIF(DATAINTERNACAO, DATAPEDIDO, 'ACT/ACT') ;
	LABEL DIF_INTER_EXAME="Dias entre Pedido e Internao" ;
RUN;

PROC FREQ data=EXM.EXAMES_2014 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

PROC FREQ data=EXM.EXAMES_2015 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

PROC FREQ data=EXM.EXAMES_2016 ORDER=FREQ ;
	TABLE FLAG_DESNUTRICAO FLAG_CANCER ;
RUN;

* FLAG DE MENOR DISTANCIA ENTRE PEDIDO E INTERNACAO ;
PROC SORT data=TAB_MERGE_2014 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA TAB_MERGE_2014 ;
	SET TAB_MERGE_2014 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

PROC SORT data=TAB_MERGE_2015 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA TAB_MERGE_2015 ;
	SET TAB_MERGE_2015 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

PROC SORT data=TAB_MERGE_2016 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA TAB_MERGE_2016 ;
	SET TAB_MERGE_2016 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

PROC SORT data=EXM.EXAMES_2014 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2014 ;
	SET EXM.EXAMES_2014 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

PROC SORT data=EXM.EXAMES_2015 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2015 ;
	SET EXM.EXAMES_2015 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

PROC SORT data=EXM.EXAMES_2016 ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2016 ;
	SET EXM.EXAMES_2016 ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
RUN;

* SALVA EM DISCO ;
DATA OUT.EXAMES_2014 ;
	SET EXM.EXAMES_2014 ;
RUN;

DATA OUT.EXAMES_2015 ;
	SET EXM.EXAMES_2015 ;
RUN;
DATA OUT.EXAMES_2016 ;
	SET EXM.EXAMES_2016 ;
RUN;


* SALVA EM DISCO ;
DATA OUT.TAB_MERGE_2014 ;
	SET TAB_MERGE_2014 ;
RUN;

DATA OUT.TAB_MERGE_2015 ;
	SET TAB_MERGE_2015 ;
RUN;
DATA OUT.TAB_MERGE_2016 ;
	SET TAB_MERGE_2016 ;
RUN;

* ===================================================================================
 Second trial - filtering before database merge
* ===================================================================================
;

DATA EXM.EXAMES_2014_FLG ;
	SET EXM.EXAMES_2014 ;
RUN;
* FLAG DE MENOR DISTANCIA ENTRE PEDIDO E INTERNACAO ;
PROC SORT data=EXM.EXAMES_2014_FLG ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2014_FLG ;
	SET EXM.EXAMES_2014_FLG ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
	IF FLAG_PRIMEIROEX = 1 ;
RUN;

DATA EXM.EXAMES_2015_FLG ;
	SET EXM.EXAMES_2015 ;
RUN;
* FLAG DE MENOR DISTANCIA ENTRE PEDIDO E INTERNACAO ;
PROC SORT data=EXM.EXAMES_2015_FLG ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2015_FLG ;
	SET EXM.EXAMES_2015_FLG ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
	IF FLAG_PRIMEIROEX = 1 ;
RUN;

DATA EXM.EXAMES_2016_FLG ;
	SET EXM.EXAMES_2016 ;
RUN;
* FLAG DE MENOR DISTANCIA ENTRE PEDIDO E INTERNACAO ;
PROC SORT data=EXM.EXAMES_2016_FLG ; BY PRT DATAINTERNACAO DATAPEDIDO ; RUN;
DATA EXM.EXAMES_2016_FLG ;
	SET EXM.EXAMES_2016_FLG ;
	BY PRT ;
	FLAG_PRIMEIROEX = first.PRT ;
	IF FLAG_PRIMEIROEX = 1 ;
RUN;

PROC SQL ;
	CREATE TABLE TAB_MERGE_2014 AS
		SELECT A.*, B.* FROM AGS.ags_2014  AS A
		LEFT JOIN EXM.EXAMES_2014_FLG AS B
		ON A.PRT = B.PRT ;
	CREATE TABLE TAB_MERGE_2015 AS
		SELECT A.*, B.* FROM AGS.ags_2015  AS A
		LEFT JOIN EXM.EXAMES_2015_FLG AS B
		ON A.PRT = B.PRT ;
	CREATE TABLE TAB_MERGE_2016 AS
		SELECT A.*, B.* FROM AGS.ags_2016  AS A
		LEFT JOIN EXM.EXAMES_2016_FLG AS B
		ON A.PRT = B.PRT ;
QUIT;

* SALVA EM DISCO ;
DATA OUT.TAB_MERGE_2014 ;
	SET TAB_MERGE_2014 ;
RUN;

DATA OUT.TAB_MERGE_2015 ;
	SET TAB_MERGE_2015 ;
RUN;
DATA OUT.TAB_MERGE_2016 ;
	SET TAB_MERGE_2016 ;
RUN;
