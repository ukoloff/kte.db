* main program
PUBLIC direction, roughness, cur_kte, cur_chist, d_max, H1, cur_id_mat, kod_instr, name_instr, obozn_instr, P_tc, M_tc, F_mx, F_mz, V, F, Ar, X_max, X_min, hardness
PUBLIC  H_kan, B_kan, L_obr, d_nach, D_rezb, P_pezb, ugol_rezb, KOD_REZB 

SET PROCEDURE TO instr_proc
SET TALK OFF
SET SAFETY off


m.X_max=0
m.X_min=0 
m.d_max=0
m.H_kan=0
m.B_kan=0
m.L_obr=0
m.d_nach=0
m.D_rezb=0.0
m.P_pezb=0.0
m.ugol_rezb=0 
m.KOD_REZB=1

m.H1=0
m.cur_id_mat=0
m.kod_instr=0
m.name_instr=""
m.obozn_instr=""
m.hardness=0
m.direction="R"
m.roughness=6.3

 m.V=0
 m.F=0.0
 m.Ar=0.0

*Параметры станка
 m.P_tc = 3.7
 m.M_tc = 30.0
 m.F_mx = 4000
 m.F_mz = 6000
 
 
 SET STEP ON 

if !used ("PRIORITET")
   use  PRIORITET IN 0 exclu
endif

if !used ("METAL")
   use  METAL IN 0 exclu
ENDIF


if !used ("Cutters")
   use  Cutters IN 0 exclu
ENDIF


if !used ("TURN_1")
   use  TURN_1 IN 0 exclu
ENDIF

if !used ("K_mpa")
   use  K_mpa IN 0 exclu
ENDIF


if !used ("K_hrc")
   use  K_hrc IN 0 exclu
ENDIF


if !used ("R_shift")
   use  R_shift IN 0 exclu
ENDIF

if !used ("GROOVE")
   use  GROOVE IN 0 exclu
ENDIF

if !used ("Boring_1")
   use  Boring_1 IN 0 exclu
ENDIF

if !used ("DRILLING")
   use  DRILLING IN 0 exclu
ENDIF

if !used ("SPLAV")
   use  SPLAV IN 0 exclu
ENDIF

if !used ("INTGROOVE")
   use  INTGROOVE IN 0 exclu
ENDIF

if !used ("V_rezb")
   use  V_rezb IN 0 exclu
ENDIF

if !used ("N_metr")
   use  N_metr IN 0 exclu
ENDIF

if !used ("N_TPI")
   use  N_TPI IN 0 exclu
ENDIF

* Чтение входного файла	

IF FILE('instr_input.txt')  && Does file exist? 
   gnErrFile = FOPEN('instr_input.txt',12)     && If so, open read-write
ENDIF
IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть входной файл' WINDOW NOWAIT
ELSE  &&  
   m.input_inst_info= Fread(gnErrFile , 100)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.input_inst_info,0,"Запись из входного файла  instr_input.txt ")	
	
FOR ii=1 TO 15 STEP 1
   DO case
       CASE  ii=1
         npred=0
         n= AT(",",m.input_inst_info,1) 
         L=n-npred-1       
         n1=1
         m.cur_kte=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 	             
       CASE  ii=2
         n= AT(",",m.input_inst_info,2)  
         L=n-npred-1      
         n1=n-L
         m.cur_chist=VAL(SUBSTR(m.input_inst_info,n1,L))
         npred=n   
       CASE  ii=3
         n= AT(",",m.input_inst_info,3)  
         L=n-npred-1      
         n1=n-L
         m.cur_id_mat=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n   
       CASE  ii=4
         n= AT(",",m.input_inst_info,4)  
         L=n-npred-1      
         n1=n-L
        m.hardness=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n   
       CASE  ii=5
         n= AT(",",m.input_inst_info,5)  
         L=n-npred-1      
         n1=n-L
        m.X_max=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n   
       CASE  ii=6
         n= AT(",",m.input_inst_info,6)  
         L=n-npred-1      
         n1=n-L
        m.X_min=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n   
       CASE  ii=7
         n= AT(",",m.input_inst_info,7)  
         L=n-npred-1      
         n1=n-L
         m.H_kan=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n    		                		                     		             		      		    
       CASE  ii=8
         n= AT(",",m.input_inst_info,8)  
         L=n-npred-1      
         n1=n-L
         m.B_kan=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n    	
       CASE  ii=9
         n= AT(",",m.input_inst_info,9)  
         L=n-npred-1      
         n1=n-L
         m.d_nach=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=10
         n= AT(",",m.input_inst_info,10)  
         L=n-npred-1      
         n1=n-L
         m.L_obr=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=11
         n= AT(",",m.input_inst_info,11)  
         L=n-npred-1      
         n1=n-L
         m.KOD_REZB=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=12
         n= AT(",",m.input_inst_info,12)  
         L=n-npred-1      
         n1=n-L
         m.P_pezb=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=13
         n= AT(",",m.input_inst_info,13)  
         L=n-npred-1      
         n1=n-L
         m.ugol_rezb=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=14
         n= AT(",",m.input_inst_info,14)  
         L=n-npred-1      
         n1=n-L
         m.roughness=VAL(SUBSTR(m.input_inst_info,n1,L)) 
         npred=n 
       CASE  ii=15
         L=1      
         n1=npred+1
         m.direction=SUBSTR(m.input_inst_info,n1,L)           
     endcase  

ENDFOR 

* проверка

IF m.cur_kte=0
   MESSAGEBOX("Не указан КТЭ",0," ")
  return  
endif


IF m.cur_id_mat=0
   MESSAGEBOX("Не указан материал",0," ")
  return  
endif

IF m.cur_chist=1 AND  EMPTY(m.roughness)
   MESSAGEBOX("Не указана шероховатость",0," ")
  return  
endif


IF m.cur_chist=0 AND  m.X_max=0
   MESSAGEBOX("Не указан X_max обработки",0," ")
  return  
endif


SET STEP ON 


DO CASE
   CASE m.cur_kte=1 AND m.cur_chist=0
         DO kte_var1
   CASE m.cur_kte=1 AND m.cur_chist=1 
        DO kte_var1_1
   CASE m.cur_kte=2 AND m.cur_chist=0 
        DO kte_var2
   CASE m.cur_kte=2 AND m.cur_chist=1 
        DO kte_var2_1  
   CASE m.cur_kte=3 AND m.cur_chist=0 
        DO kte_var3 
   CASE m.cur_kte=3 AND m.cur_chist=1 
       DO kte_var3_1   
   CASE m.cur_kte=4 AND m.cur_chist=0 
       DO kte_var4
   CASE m.cur_kte=5 AND m.cur_chist=0 
       DO kte_var5
   CASE m.cur_kte=5 AND m.cur_chist=1 
       DO kte_var5_1 
   CASE m.cur_kte=6 AND m.cur_chist=0 
       DO kte_var6 
   CASE m.cur_kte=6 AND m.cur_chist=1 
       DO kte_var6_1
   CASE m.cur_kte=7 AND m.cur_chist=0 
        DO kte_var7 
   CASE m.cur_kte=8 AND m.cur_chist=0 
        DO kte_var8 
   CASE m.cur_kte=8 AND m.cur_chist=1 
        DO kte_var8_1 
   CASE m.cur_kte=9 AND m.cur_chist=0 
        DO kte_var9 
   CASE m.cur_kte=9 AND m.cur_chist=1 
       DO kte_var9_1 
   CASE m.cur_kte=10 AND m.cur_chist=0 
       DO kte_var10 
   CASE m.cur_kte=10 AND m.cur_chist=1 
       DO kte_var10_1 
   CASE m.cur_kte=11 AND m.cur_chist=0 
       DO kte_var11 
   CASE m.cur_kte=12 AND m.cur_chist=0 
       DO kte_var12 
   CASE m.cur_kte=12 AND m.cur_chist=1 
       DO kte_var12_1
   CASE m.cur_kte=13 AND m.cur_chist=0 
       DO kte_var13 
   CASE m.cur_kte=13 AND m.cur_chist=1 
        DO kte_var13_1
   CASE m.cur_kte=14 AND m.cur_chist=0 
       DO kte_var14 
   CASE m.cur_kte=15 AND m.cur_chist=0 
       DO kte_var15
   CASE m.cur_kte=16 AND m.cur_chist=0 
       DO kte_var16 


   OTHERWISE 
       MESSAGEBOX("Вариант для расчета не определен",0," ")
ENDCASE


QUIT

*****************************************************