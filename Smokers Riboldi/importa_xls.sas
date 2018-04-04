PROC IMPORT OUT= WORK.Dados 
            DATAFILE= "F:\Riboldi_Trabalho\Dados_Trabalho_Analise.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="Plan1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC CONTENTS data=WORK.Dados  ; RUN;

LIBNAME L0 V9 "F:\Riboldi_Trabalho\SAS" ; 

DATA L0.Dados_Africa ;
	SET WORK.Dados  ;
	WHERE Regi_o = "AFR" ;

   LABEL  	Country="Países"
			PRICE_USD_PPP="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, ajustado para paridade de poder de compra 2006"
			OExtRate="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, segundo taxa de câmbio de 2006"
			Tax_Pre_SE="Taxa de imposto como percentual do preço - exercício específico"
			Tax_Pre_AdVal="Taxa de imposto como percentual do preço - imposto sobre valor agragado"
			Tax_Pre_Iduties="Taxa de imposto como percentual do preço - para importação"
			Tax_Total="Taxa de imposto como percentual do preço - Total"
			Esc_Ban_Adv="Escore OMS para Aplicação geral de proibição de publicidade directa"
			Esc_Ban_Promo="Escore OMS para Aplicação geral da proibição de promoção"
			Esc_Ban_Reg_Smk_Free="Escore OMS para aplicação geral da regulamentação sobre combate ao fumo"
			TCA_USD="Orçamento nacional global para as atividades de controle do tabaco - Em dólares, em paridade de poder de compra de 2006"
			TCA_USD_ER="Orçamento nacional global para as atividades de controle do tabaco - em dólares, segundo taxas de câmbio oficial de 2006"
			LITERACY="Taxa de Alfabetização"
			CLASS_HDI="Classification HDI"
			HDIEst="Estimativas para o IDH para 2010"
			HDIChange="Variação do IDH comparado com 2009"

			PREV_Curr_Male="Prevalência de fumantes homens ajustada para os países membros da OMS"
			PREV_Day_Male="Prevalência de fumantes homens diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Male="Prevalência de fumantes homens comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Male="Prevalência de fumantes homens diários comparável ajustada para os países membros da OMS"

			PREV_Curr_Female="Prevalência de fumantes mulheres ajustada para os países membros da OMS"
			PREV_Day_Female="Prevalência de fumantes mulheres diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Female="Prevalência de fumantes mulheres comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Female="Prevalência de fumantes mulheres diários comparável ajustada para os países membros da OMS"

			GDP_PPP_IMF="PIB ajustado para paridade de poder de compra (FMI)"
			GDP_PPP_Capta="PIB ajustado para paridade de poder de compra per capta";
RUN;

PROC CONTENTS data=L0.Dados_Africa  ; RUN;


DATA L0.Dados_Oriente_Medio ;
	SET WORK.Dados  ;
	WHERE Regi_o = "OM" ;
   LABEL  	Country="Países"
			PRICE_USD_PPP="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, ajustado para paridade de poder de compra 2006"
			OExtRate="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, segundo taxa de câmbio de 2006"
			Tax_Pre_SE="Taxa de imposto como percentual do preço - exercício específico"
			Tax_Pre_AdVal="Taxa de imposto como percentual do preço - imposto sobre valor agragado"
			Tax_Pre_Iduties="Taxa de imposto como percentual do preço - para importação"
			Tax_Total="Taxa de imposto como percentual do preço - Total"
			Esc_Ban_Adv="Escore OMS para Aplicação geral de proibição de publicidade directa"
			Esc_Ban_Promo="Escore OMS para Aplicação geral da proibição de promoção"
			Esc_Ban_Reg_Smk_Free="Escore OMS para aplicação geral da regulamentação sobre combate ao fumo"
			TCA_USD="Orçamento nacional global para as atividades de controle do tabaco - Em dólares, em paridade de poder de compra de 2006"
			TCA_USD_ER="Orçamento nacional global para as atividades de controle do tabaco - em dólares, segundo taxas de câmbio oficial de 2006"
			LITERACY="Taxa de Alfabetização"
			CLASS_HDI="Classification HDI"
			HDIEst="Estimativas para o IDH para 2010"
			HDIChange="Variação do IDH comparado com 2009"

			PREV_Curr_Male="Prevalência de fumantes homens ajustada para os países membros da OMS"
			PREV_Day_Male="Prevalência de fumantes homens diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Male="Prevalência de fumantes homens comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Male="Prevalência de fumantes homens diários comparável ajustada para os países membros da OMS"

			PREV_Curr_Female="Prevalência de fumantes mulheres ajustada para os países membros da OMS"
			PREV_Day_Female="Prevalência de fumantes mulheres diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Female="Prevalência de fumantes mulheres comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Female="Prevalência de fumantes mulheres diários comparável ajustada para os países membros da OMS"

			GDP_PPP_IMF="PIB ajustado para paridade de poder de compra (FMI)"
			GDP_PPP_Capta="PIB ajustado para paridade de poder de compra per capta";
RUN;

PROC CONTENTS data=L0.Dados_Oriente_Medio  ; RUN;


DATA L0.Dados_America ;
	SET WORK.Dados  ;
	WHERE Regi_o = "AM" ;
   LABEL  	Country="Países"
			PRICE_USD_PPP="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, ajustado para paridade de poder de compra 2006"
			OExtRate="Preço do maço de 20 cigarros do tipo mais vendido, em dólares, segundo taxa de câmbio de 2006"
			Tax_Pre_SE="Taxa de imposto como percentual do preço - exercício específico"
			Tax_Pre_AdVal="Taxa de imposto como percentual do preço - imposto sobre valor agragado"
			Tax_Pre_Iduties="Taxa de imposto como percentual do preço - para importação"
			Tax_Total="Taxa de imposto como percentual do preço - Total"
			Esc_Ban_Adv="Escore OMS para Aplicação geral de proibição de publicidade directa"
			Esc_Ban_Promo="Escore OMS para Aplicação geral da proibição de promoção"
			Esc_Ban_Reg_Smk_Free="Escore OMS para aplicação geral da regulamentação sobre combate ao fumo"
			TCA_USD="Orçamento nacional global para as atividades de controle do tabaco - Em dólares, em paridade de poder de compra de 2006"
			TCA_USD_ER="Orçamento nacional global para as atividades de controle do tabaco - em dólares, segundo taxas de câmbio oficial de 2006"
			LITERACY="Taxa de Alfabetização"
			CLASS_HDI="Classification HDI"
			HDIEst="Estimativas para o IDH para 2010"
			HDIChange="Variação do IDH comparado com 2009"

			PREV_Curr_Male="Prevalência de fumantes homens ajustada para os países membros da OMS"
			PREV_Day_Male="Prevalência de fumantes homens diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Male="Prevalência de fumantes homens comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Male="Prevalência de fumantes homens diários comparável ajustada para os países membros da OMS"

			PREV_Curr_Female="Prevalência de fumantes mulheres ajustada para os países membros da OMS"
			PREV_Day_Female="Prevalência de fumantes mulheres diários ajustada para os países membros da OMS"
			PREV_Comp_Curr_Female="Prevalência de fumantes mulheres comparável ajustada para os países membros da OMS"
			PREV_Comp_Day_Female="Prevalência de fumantes mulheres diários comparável ajustada para os países membros da OMS"

			GDP_PPP_IMF="PIB ajustado para paridade de poder de compra (FMI)"
			GDP_PPP_Capta="PIB ajustado para paridade de poder de compra per capta";
RUN;

PROC CONTENTS data=L0.Dados_America  ; RUN;


/*############ ANALISE ################## */

/*### CORRELACOES ### */
      title;                                                                                                                            
      footnote;                                                                                                                         
*** Correlations ***;                                                                                                                   
proc corr data=L0.dados_america pearson;                                                                                               
   var GDP_PPP_CAPTA GDP_PPP_IMF PRICE_USD_PPP OEXTRATE TAX_PRE_SE                                                                      
      TAX_PRE_ADVAL TAX_PRE_IDUTIES TAX_TOTAL ESC_BAN_ADV ESC_BAN_PROMO                                                                 
      ESC_BAN_REG_SMK_FREE TCA_USD TCA_USD_ER HDIEST PREV_CURR_MALE                                                                     
      PREV_CURR_FEMALE LITERACY;                                                                                                        
run;                                                                                                                                    
                                                                      
      title;                                                                                                                            
      footnote;                                                                                                                         
*** Correlations ***;                                                                                                                   
proc corr data=L0.dados_america RANK pearson;                                                                                          
   var GDP_PPP_CAPTA GDP_PPP_IMF PRICE_USD_PPP OEXTRATE TAX_PRE_SE                                                                      
      TAX_PRE_ADVAL TAX_PRE_IDUTIES TAX_TOTAL ESC_BAN_ADV ESC_BAN_PROMO                                                                 
      ESC_BAN_REG_SMK_FREE TCA_USD TCA_USD_ER HDIEST PREV_CURR_MALE                                                                     
      PREV_CURR_FEMALE LITERACY;                                                                                                        
run;                                                                                                                                    
                                                                                                                                        
/*### SELECAO DO MODELO ### */

/* Scatterplot */
proc insight data=L0.dados_america; 
  scatter TAX_TOTAL LITERACY PRICE_USD_PPP *
          TAX_TOTAL LITERACY PRICE_USD_PPP ;
run;
quit;

      title;                                                                                                                            
      footnote;   
 
*** Linear Regression ***;                                                                                                              
options pageno=1;                                                                                                                       
proc reg data=L0.dados_america;                                                                                                        
   model PREV_CURR_MALE = GDP_PPP_CAPTA GDP_PPP_IMF PRICE_USD_PPP OEXTRATE                                                              
   TAX_TOTAL HDIEST LITERACY / selection=stepwise sle=0.1 sls=0.1;                                                                      
run;                                                                                                                                    
quit;                                                                                                                                   

/* ### COMPUTANDO MODELO FINAL PARA HOMENS ### */

options pageno=1;    
ods html; 
ods graphics on;   

      title;                                                                                                                            
      footnote;                                                                                                                         
*** Linear Regression ***;                                                                                                              
options pageno=1;                                                                                                                       
goptions reset=all device=WIN;                                                                                                          
      title;                                                                                                                            
      footnote;                                                                                                                         
goptions ftext=SWISS ctext=BLACK htext=1 cells                                                                                          
         gunit=pct htitle=6;                                                                                                            
symbol1 c=BLUE v=SQUARE h=1 cells;                                                                                                      
proc reg data=L0.dados_america;                                                                                                        
   model PREV_CURR_MALE = PRICE_USD_PPP TAX_TOTAL LITERACY / stb clb corrb                                                              
      collin tol vif spec dw;                                                                                                           
** Create output data set for saving data **;                                                                                           
   output OUT=WORK.SCORE PREDICTED=_PRED RESIDUAL=_RESID RSTUDENT=_RSTUDENT                                                             
      DFFITS=_DFFITS COOKD=_COOKD H=_H PRESS=_PRESS;                                                                                    
** Create output data set for predictions **;                                                                                           
   output out=WORK.PRED p=_pred r=_resid;                                                                                               
* Plot residuals by regressors ;                                                                                                        
   plot (RSTUDENT.) * PRICE_USD_PPP / nostat name='RESID'                                                                               
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
   plot (RSTUDENT.) * TAX_TOTAL / nostat name='RESID'                                                                                   
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
   plot (RSTUDENT.) * LITERACY / nostat name='RESID'                                                                                    
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
* Plot Normal probability plot of residuals ;                                                                                           
   plot npp. * r. / nostat name='RESID'                                                                                                 
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
* Plot Normal quantile plot of residuals ;                                                                                              
   plot r. * nqq. / nostat noline name='RESID'                                                                                          
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
run;                                                                                                                                    
quit;                                                                                                                                   
goptions ftext= ctext= htext= reset=symbol;                                                                                             
goptions reset=all device=WIN;                                                                                                          
proc datasets library=WORK nolist;                                                                                                      
   modify PRED;                                                                                                                         
   label _pred = 'Predicted PREV_Curr_Male';                                                                                            
   label _resid = 'Residual of PREV_Curr_Male';                                                                                         
run; quit;                                                                                                                              
** Predictions **;                                                                                                                      
/*      title;                                                                                                                            */
/*      footnote;                                                                                                                         */
/*options pageno=1;                                                                                                                       */
/*proc print data=WORK.PRED label;                                                                                                        */
/*title1 'Predictions';                                                                                                                   */
/*run;                                                                                                                                    */
/*title1;         
*/
                  
DATA america2 ;
	SET L0.dados_america ;
	IF _N_ = 02 then delete ; 
	IF _N_ = 18 then delete ; 
	IF _N_ = 33 then delete ; 
RUN;

proc reg data=america2;                                                                                                        
   model PREV_CURR_MALE = PRICE_USD_PPP TAX_TOTAL LITERACY / stb clb corrb influence                                                           
      collin tol vif spec dw;                                                                                                           
** Create output data set for saving data **;                                                                                           
   output OUT=WORK.SCORE PREDICTED=_PRED RESIDUAL=_RESID RSTUDENT=_RSTUDENT                                                             
      DFFITS=_DFFITS COOKD=_COOKD H=_H PRESS=_PRESS;                                                                                    
** Create output data set for predictions **;                                                                                           
   output out=WORK.PRED p=_pred r=_resid;                                                                                               
* Plot residuals by regressors ;                                                                                                        
   plot (RSTUDENT.) * PRICE_USD_PPP / nostat name='RESID'                                                                               
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
   plot (RSTUDENT.) * TAX_TOTAL / nostat name='RESID'                                                                                   
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
   plot (RSTUDENT.) * LITERACY / nostat name='RESID'                                                                                    
                        cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                             
* Plot Normal probability plot of residuals ;                                                                                           
   plot npp. * r. / nostat name='RESID'                                                                                                 
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
* Plot Normal quantile plot of residuals ;                                                                                              
   plot r. * nqq. / nostat noline name='RESID'                                                                                          
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
run;                                                                                                                                    
quit;                                                                                                                                   
goptions ftext= ctext= htext= reset=symbol;                                                                                             
goptions reset=all device=WIN;                                                                                                          
proc datasets library=WORK nolist;                                                                                                      
   modify PRED;                                                                                                                         
   label _pred = 'Predicted PREV_Curr_Male';                                                                                            
   label _resid = 'Residual of PREV_Curr_Male';                                                                                         
run; quit;                                              

/*### ANALISANDO A INFLUENCIA ###*/

proc reg data=L0.dados_america;                                                                                                        
   model PREV_CURR_MALE = PRICE_USD_PPP TAX_TOTAL LITERACY / stb clb corrb influence;     
   output out=meumodelo(keep=PREV_CURR_MALE PRICE_USD_PPP TAX_TOTAL LITERACY
                       r rs lev cd dffit)
                       rstudent=rs residual=r predicted=fv
						h=lev cookd=cd dffits=dffit;
run;                                                                                                                                    
quit;

/*### NORMALIDADE DOS RESIDUOS ###*/
proc kde data=WORK.SCORE out=den;
  var _RESID;
run;

proc sort data=den;
  by _RESID;
run;

goptions reset=all;
symbol1 c=blue i=join v=none height=1;
title "Gráfico de densidade - Normalidade dos Resíduos";

proc gplot data=den;
  plot density*_RESID=1;
run;
quit;

title;

proc univariate data=den normal;
 var _RESID;
 qqplot _RESID / normal(mu=est sigma=est);
run;

/*####################*/
/*### MULHERES #######*/
/*####################*/

      title;                                                                                                                            
      footnote;                                                                                                                         
*** Linear Regression ***;                                                                                                              
options pageno=1;                                                                                                                       
goptions reset=all device=WIN;                                                                                                          
      title;                                                                                                                            
      footnote;                                                                                                                         
goptions ftext=SWISS ctext=BLACK htext=1 cells                                                                                          
         gunit=pct htitle=6;                                                                                                            
symbol1 c=BLUE v=SQUARE h=1 cells;                                                                                                      
proc reg data=L0.dados_america;                                                                                                        
   model PREV_CURR_FEMALE = LITERACY TAX_PRE_ADVAL HDIEST TAX_TOTAL                                                                     
   GDP_PPP_CAPTA GDP_PPP_IMF / stb clb corrb collin tol vif spec dw influence                                                                    
      selection=stepwise sle=0.1 sls=0.1;                                                                                               
* Plot Normal probability plot of residuals ;                                                                                           
   plot npp. * r. / nostat name='RESID'                                                                                                 
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
* Plot Normal quantile plot of residuals ;                                                                                              
   plot r. * nqq. / nostat noline name='RESID'                                                                                          
                    cframe=CXF7E1C2 caxis=BLACK cline=BLUE ctext=BLACK;                                                                 
run;                                                                                                                                    
quit;                                                                                                                                   
goptions ftext= ctext= htext= reset=symbol;                                                                                             
goptions reset=all device=WIN;                                                                                                          
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                                                                                        
                                                                   



ods graphics off; 
ods html close;
