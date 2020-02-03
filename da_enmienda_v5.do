*********************************************************************
* Project: 	El Niño cohort study - ammendment						*
* Authors: 	Carlos Culquichicon, David Astudillo, Roberto Niño	    *
*			Raisa Martinez, Nicole Merino, Karen Levy, and 			*
*			Andres G. Lescano										*
* Data analysists: 	Carlos Culquichicon, Roberto Niño				*
*					and Andres G. Lescano							*
* Last update:		06/21/19										*
*********************************************************************

version 15.1
set more off
local dir `c(pwd)'
cd "`c(pwd)'"
capture log close

						************************
						* Socioeconomic status *
						************************
use "ms_reclutamiento madre 2016-2018_clear.dta",clear

gen 	hospital=""
replace hospital= substr(codigo,1,4)

gen 	n_participante = ""
replace n_participante = substr(codigo,-4,4)
destring n_participante, gen(n_participante_dec)


#delimit;
global pcl_ch = "
13 14 15 62 65 118 119 120 121 123 125 145 147 148 149 162 169 184 186 188
213 215 233
";
#delimit cr;

egen match_pcl_ch = anyvalue(n_participante_dec) if hospital=="PICH", value($pcl_ch)


#delimit;
global pcl_sr="
17	28	35	53	78	81	82	84	107	109	111	119	126	128	129	136	171	194	210	212	
226	229	230	238	240	287	502	503	505	507	509	511	514	515	518	519	521	523	524	525	
528	534	535	536	537	539	540	541	542	544	545	546	548	549	550	552	554	555	556	557	
558	562	563	564	566	568	569	574	575	576	578	579	581	582	583	584	585	586	587	614	
615	616	619	621	622	623	626	627	628	629	631	635	636	637	638	639	640	641	643	646	
648	650	651	653	654	655	656	657	658	659	660	661	662	663	667	671	674	675	677	678	
680	681	682	683	684	685	686	687	688	690	691	692	693	694	694	696	696	698	698	727	
727
";
#delimit cr;

egen match_pcl_sr = anyvalue(n_participante_dec) if hospital=="PISR", value($pcl_sr)


#delimit;
global hfias_ch="
13	14	62	65	118	119	120	121	123	125	145	148	149	162	184	186	188	213	215	223	
233	693
";
#delimit cr;

egen match_hfias_ch = anyvalue(n_participante_dec) if hospital=="PISR", value($hfias_ch)

#delimit;
global hfias_sr="
15	17	28	35	53	78	81	82	84	107	109	111	119	126	128	129	136	147	159	171	
194	210	212	226	229	230	238	240	287	502	503	505	507	509	511	514	515	518	519	521	
523	524	525	528	534	535	536	537	539	540	541	542	543	544	545	546	548	549	550	552	
554	555	556	557	558	562	563	564	566	568	569	574	575	576	578	579	581	582	583	584	
585	586	587	614	615	616	619	621	622	623	626	627	628	629	631	635	636	637	638	639	
640	641	646	648	650	651	653	654	655	656	657	658	659	660	661	662	663	667	671	674	
675	677	678	680	681	682	683	684	685	686	687	688	690	691	692	698	693	727	694	694	
696	696	698	727
";
#delimit cr;

egen match_hfias_sr = anyvalue(n_participante_dec) if hospital=="PISR", value($hfias_sr)


#delimit;
global sascat_ch="
13	14	62	65	118	119	120	121	123	125	147	148	149	162	169	184	186	188	213	215	
233	534	683
";
#delimit cr;

egen match_sascat_ch = anyvalue(n_participante_dec) if hospital=="PISR", value($sascat_ch)

#delimit;
global sascat_sr="
15	17	28	35	52	53	78	81	82	84	107	109	111	119	126	128	136	166	171	194	
210	212	226	229	230	238	240	287	502	503	505	507	509	511	514	515	518	519	521	523	
524	525	528	535	536	537	539	540	541	542	544	545	546	548	549	550	552	554	555	556	
557	558	562	563	564	566	568	569	574	575	576	578	579	581	582	583	584	585	586	587	
614	615	616	619	621	622	623	626	627	628	629	631	635	636	637	638	639	640	641	643	
646	648	650	651	653	654	655	656	657	658	659	660	661	662	663	667	671	674	675	677	
678	680	681	682	683	684	685	686	687	688	690	691	692	693	727	694	696	696	698	698	
727
";
#delimit cr;

egen match_sascat_sr = anyvalue(n_participante_dec) if hospital=="PISR", value($sascat_sr)


keep if match_pcl_ch!=. | match_pcl_ch!=. | match_hfias_ch!=. | /// 
match_hfias_sr!=. | match_sascat_ch !=. | match_sascat_sr!=.



recode distri_reco (4 6 9=2) (5 12=3) (8 10 11 13=1)
lab def distri_recodef 1"Piura" 2"Castilla" 3"Catacaos y Narihuala"
lab val distri_reco distri_recodef 

replace distri_reco=1 if comun=="A.A.H.H Aledaños Curver"
replace distri_reco=1 if comun=="A.A.H.H Ignacio Merino"
replace distri_reco=1 if comun=="A.A.H.H Los Tallanes"
replace distri_reco=1 if comun=="A.A.H.H Las Peñitas"
replace distri_reco=1 if comun=="A.A.H.H Alan Perú"
replace distri_reco=1 if comun=="A.A.H.H 18 de Mayo"
replace distri_reco=1 if comun=="A.A.H.H Nvo Horizonte"
replace distri_reco=1 if comun=="Enace"
replace distri_reco=1 if comun=="Expolvorines"
replace distri_reco=1 if comun=="Nueva Esperanza"
replace distri_reco=1 if comun=="Titanes I etapa"
replace distri_reco=1 if comun=="Urb. hermanos Carcamo"
replace distri_reco=1 if otro=="Paita Alta"
replace distri_reco=2 if comun=="A.A.H.H Tacalá"
replace distri_reco=2 if comun=="Ciudad del Niño Tacalá"
replace distri_reco=2 if comun=="A.A.H.H San Pedro"
replace distri_reco=2 if comun=="Tacalá"
replace distri_reco=2 if comun=="Yapatera"
replace distri_reco=2 if codigo=="PICH0111"
replace distri_reco=3 if comun=="Monte Sullon"


#delimit;
global comun_complete="
PISR0017 PISR0035 PISR0626 PISR0627	PISR0628 PISR0629 PISR0631 PISR0636 PISR0638 
PISR0640 PISR0641 PISR0646 PISR0651 PISR0653 PISR0654 PISR0655 PISR0656 PISR0658 
PISR0659 PISR0660 PISR0662 PISR0663 PISR0667 PISR0671 PISR0674 PISR0675 PISR0677 
PISR0678 PISR0680 PISR0682 PISR0683 PISR0684 PISR0685 PISR0686 PISR0688 PISR0690 
PISR0691 PISR0692 PISR0693 PISR0694 PISR0696
";
#delimit cr;


foreach i in $comun_complete {
	lis codigo dist_ent loc_ent distri comun tipovia mz lte nro otro observ_s3_1 if codigo=="`i'"
}



replace distri_reco=1 if codigo=="PISR0017"
replace distri_reco=3 if codigo=="PISR0035"
replace distri_reco=3 if codigo=="PISR0626"
replace distri_reco=1 if codigo=="PISR0627"
replace distri_reco=1 if codigo=="PISR0628"
replace distri_reco=2 if codigo=="PISR0629"
replace distri_reco=1 if codigo=="PISR0631"
replace distri_reco=1 if codigo=="PISR0636"
replace distri_reco=3 if codigo=="PISR0638"
replace distri_reco=2 if codigo=="PISR0640"
replace distri_reco=1 if codigo=="PISR0641"
replace distri_reco=1 if codigo=="PISR0646"
replace distri_reco=1 if codigo=="PISR0651"
replace distri_reco=3 if codigo=="PISR0653"
replace distri_reco=1 if codigo=="PISR0654"
replace distri_reco=1 if codigo=="PISR0655"
replace distri_reco=2 if codigo=="PISR0656"
replace distri_reco=1 if codigo=="PISR0658"
replace distri_reco=1 if codigo=="PISR0659"
replace distri_reco=2 if codigo=="PISR0660"
replace distri_reco=2 if codigo=="PISR0662"
replace distri_reco=3 if codigo=="PISR0663"
replace distri_reco=2 if codigo=="PISR0667"
replace distri_reco=1 if codigo=="PISR0671"
replace distri_reco=1 if codigo=="PISR0674"
replace distri_reco=1 if codigo=="PISR0675"
replace distri_reco=3 if codigo=="PISR0677"
replace distri_reco=3 if codigo=="PISR0678"
replace distri_reco=2 if codigo=="PISR0680"
replace distri_reco=2 if codigo=="PISR0682"
replace distri_reco=1 if codigo=="PISR0683"
replace distri_reco=1 if codigo=="PISR0684"
replace distri_reco=1 if codigo=="PISR0685"
replace distri_reco=1 if codigo=="PISR0686"
replace distri_reco=2 if codigo=="PISR0688"
replace distri_reco=1 if codigo=="PISR0690"
replace distri_reco=2 if codigo=="PISR0691"
replace distri_reco=1 if codigo=="PISR0692"
replace distri_reco=2 if codigo=="PISR0693"
replace distri_reco=2 if codigo=="PISR0694"
replace distri_reco=2 if codigo=="PISR0696"
recode ginst_mad (1=2)
recode distri_reco (7=2)
recode trab_mad (1=12)
recode nrecipalm (1=.)

recode mpared (7=8)
lab def mpareddef2 1"Tapial, piedra con barro u otro" 2"Estera, quincha, (caña con barro)" ///
3"Adobe" 4"Cartón, maderba, Panel, Nordex, Triplay" 8"Ladrillo, cemento"
lab val mpared mpareddef2

recode mtecho (1 7=3)
recode fteagua(6=5)
recode trab_mad (7=12)(3 4 9=2)(15=2)(11=10)(-1=.)
recode fteagua (1=3) (11=4)
recode elimexcre (3=.)(7 =6)(9=8)
recode elimbasur (3 4 7=1)

gen hijostotal=hnonincasa+1
gen hijostotal_dico=.
replace hijostotal_dico=1 if hijostotal==1
replace hijostotal_dico=2 if hijostotal==2
replace hijostotal_dico=3 if hijostotal==3
replace hijostotal_dico=4 if hijostotal==4
lab def hijostotal_dicodef 1"Uno" 2"Dos" 3"Tres" 4"Cuatro o mas"
lab val hijostotal_dico hijostotal_dicodef


#delimit;
global ssses="
distri_reco trab_mad  ginst_mad refriger tvcolor mpared mtecho combcocin fteagua            
seguragua nrecipalm tpenvase elimexcre cinstagua nvivcompag ainstagua cinstdesag 
nvivcompdg ainstdesag elimbasur criaanim
";
#delimit cr;



foreach i in $ssses {
	tab1 `i'
}

sum hijostotal edad_mad


foreach i in $ssses {
	tab `i' cohorte,col
}
bys cohorte: sum hijostotal edad_mad



						
								**********
								*Nino-PCL*
								**********
use "clear_Nino-PCL.dta", clear
/*
decode CODIGO, gen(ID)
gen 	ID2 =	ID
replace ID2 = 	substr(ID,1,4)
replace ID2 = 	"PISR" if ID2=="PIRS"
replace ID2 = 	"PISR" if ID2=="pisr"
replace ID	=	ID2

decode CODIGO, gen(ID3)
gen 	ID4 = 	ID3
replace ID4	=	substr(ID3,-4,4)
egen codigo2=	concat (ID2 ID4),d
drop codigo ID*
rename codigo2 codigo_real
order codigo,first

save "clear_Nino-PCL.dta", replace
*/

gen n_visit=.
replace n_visit=1 if tiempo==1 | tiempo==2 | tiempo==3
replace n_visit=2 if (tiempo==1 & tiempo==2)|(tiempo==2 & tiempo==3)|(tiempo==1 & tiempo==3)
replace n_visit=3 if tiempo==1 & tiempo==2 & tiempo==3


global B_PCL  p_intrusiv suenos_rec flashbacks react_emo reac_fisic
global C_PCL  eva_pens eva_exp evocacion_exp perd_interes desapego restricc_afecto futuro_corto
global D_PCL  sueno irritabilidad concentracion hipervigilancia asombro

*>Table 1
tab PCL_cat tiempo, col chi2
*reliability / internal consistency (Cronbach's Alpha)
alpha $B_PCL $C_PCL $D_PCL, detail std item label

foreach i in B_PCL  C_PCL  D_PCL {
	alpha $`i', detail std item label
}

** Sub-group analysis
foreach i in PCL_cat DSM_IV {
	tab `i' tiempo, col exp chi2 exact
}

tabstat PCL_sum, by(tiempo) stat(n mean sd median min max skew kurt cv)
swilk PCL_sum
histogram PCL_sum, by(tiempo) normal 


kdensity PCL_sum if tiempo==1, nodraw
	graph save kd1, replace
kdensity PCL_sum if tiempo==2, nodraw
	graph save kd2, replace
kdensity PCL_sum if tiempo==3, nodraw
	graph save kd3, replace
graph combine "kd1.gph" "kd2.gph" "kd3.gph", col(3) xsize(10) saving(scalebcd)

kwallis PCL_sum, by(tiempo)
dunntest PCL_sum, by(tiempo) ma(bonferroni)
bootstrap r(chi2_adj), reps(10000) seed(666) bca nodots: kwallis PCL_sum, by(tiempo) 
estat bootstrap, all


foreach i in scaleb scalec scaled {
	dunntest `i',by(tiempo) ma(bonferroni)
}


*>Table 2
**Cambio según dimensiones
foreach i in B C D {
egen `i'_sum= rowtotal ($`i'_PCL)
}


foreach i in B_sum C_sum D_sum{
	di "-----------------------------------------------------------------------"
	di "`i'"
	swilk `i'
	tabstat `i', by(tiempo) stat(n mean sd median min max skew kurt cv)
	
	kdensity `i' if tiempo==1, nodraw
		graph save `i'1, replace
	kdensity `i' if tiempo==2, nodraw
		graph save `i'2, replace
	kdensity `i' if tiempo==3, nodraw
		graph save `i'3, replace
	graph combine "`i'1.gph" "`i'2.gph" "`i'3.gph", col(3) xsize(10) saving(`i'_full)
}

foreach i in B_sum C_sum D_sum{
	graph use `i'_full
}


foreach i in B_sum C_sum D_sum {
	di "-----------------------------------------------------------------------"
	di "`i'"
	
	kwallis `i', by(tiempo)
	dunntest `i', by(tiempo) ma(bonferroni)
	bootstrap r(chi2_adj), reps(10000) seed(666) bca nodots: kwallis `i', by(tiempo)
	estat bootstrap, all
}



								************
								*Nino-HFIAS*
								************
use "clear_Nino-HFIAS.dta", clear


/*
decode CODIGO, gen(ID)
gen 	ID2 =	ID
replace ID2 = 	substr(ID,1,4)
replace ID2 = 	"PISR" if ID2=="PIRS"
replace ID2 = 	"PISR" if ID2=="pisr"
replace ID	=	ID2

decode CODIGO, gen(ID3)
gen 	ID4 = 	ID3
replace ID4	=	substr(ID3,-4,4)
egen codigo2=	concat (ID2 ID4),d
drop ID*
rename codigo2 codigo_real
order codigo,first

save "clear_Nino-HFIAS.dta", replace
*/

#delimit;
global ssitem="
j1_alimentos_sufi_clas j2_alimentos_pref_clas j3_alimentos_limit_clas 
j4_alimentos_indeseados_clas j6_alimentos_comer_menos_clas 
j7_alimentos_ninguno_clas j8_alimentos_noche_clas j9_dia_sin_comer_clas 
";
#delimit;

gen hfias_sum= j1_alimentos_sufi_clas + j2_alimentos_pref_clas + ///
            j3_alimentos_limit_clas + j4_alimentos_indeseados_clas + ///
			j6_alimentos_comer_menos_clas + j7_alimentos_ninguno_clas + ///
			j8_alimentos_noche_clas + j9_dia_sin_comer_clas
			
lab def etiqueta 0 "seguridad alimentaria" 1 "inseguridad alimentaria"

foreach i in $ssitem {
recode `i' (2=1) (3=1)
lab val `i' etiqueta
}

			
*>Table 1 - Food insecurity
tab hfias tiempo, col chi2
tabstat hfias_sum, by(tiempo) stat(n mean sd median min max skew kurt cv)
swilk hfias_sum
histogram hfias_sum, by(tiempo) normal

kdensity hfias_sum if tiempo==1, nodraw
	graph save hd1, replace
kdensity hfias_sum if tiempo==2, nodraw
	graph save hd2, replace
kdensity hfias_sum if tiempo==3, nodraw
	graph save hd3, replace
graph combine "hd1.gph" "hd2.gph" "hd3.gph", col(3) xsize(10)

kwallis hfias_sum, by(tiempo)
dunntest hfias_sum, by(tiempo) ma(bonferroni)
bootstrap r(chi2_adj), reps(10000) seed(666) bca nodots: kwallis hfias_sum, by(tiempo) 
estat bootstrap, all


*> Table 2 - Items
tabstat $ssitem, by(tiempo) stat(n mean sd median min max skew kurt cv)

foreach i in $ssitem {
	kwallis `i',by(tiempo)
}

foreach i in $ssitem {
	dunntest `i',by(tiempo) ma(bonferroni)
}

foreach i in $ssitem{
	di "-----------------------------------------------------------------------"
	di "`i'"
	swilk `i'
	tabstat `i', by(tiempo) stat(n mean sd median min max skew kurt cv)
	
	kdensity `i' if tiempo==1, nodraw
		graph save `i'1, replace
	kdensity `i' if tiempo==2, nodraw
		graph save `i'2, replace
	kdensity `i' if tiempo==3, nodraw
		graph save `i'3, replace
	graph combine "`i'1.gph" "`i'2.gph" "`i'3.gph", col(3) xsize(10) saving(`i'_full)
}

foreach i in $ssitem {
	graph use "`i'_full.gph"
}


foreach i in $ssitem {
	di "-----------------------------------------------------------------------"
	di "`i'"
	
	kwallis `i', by(tiempo)
	dunntest `i', by(tiempo) ma(bonferroni)
	bootstrap r(chi2_adj), reps(10000) seed(666) bca nodots: kwallis `i', by(tiempo)
	estat bootstrap, all
}



						    	**************
								*Nino-capital*
								**************
use "clear_Nino-capital.dta", clear


/*
decode CODIGO, gen(ID)
gen 	ID2 =	ID
replace ID2 = 	substr(ID,1,4)
replace ID2 = 	"PISR" if ID2=="PIRS"
replace ID2 = 	"PISR" if ID2=="pisr"
replace ID2 = 	"PISR" if ID2=="RL"
replace ID	=	ID2

decode CODIGO, gen(ID3)
gen 	ID4 = 	ID3
replace ID4	=	substr(ID3,-4,4)
replace ID4="0166" if CODIGO==166
egen codigo2=	concat (ID2 ID4),d
drop ID*
rename codigo2 codigo_real
order codigo,first

save "clear_Nino-capital.dta", replace
*/

*Domain: Group membership
gen miembro_de_grupo_n = q0007_0001_0001 + q0007_0002_0001 + q0007_0003_0001 + q0007_0004_0001 + ///
q0007_0005_0001 + q0007_0006_0001 + q0007_0007_0001 + q0007_0008_0001
lab var miembro_de_grupo "Membresía grupo"

gen miembro_de_grupo=.
replace miembro_de_grupo=9 if q0007_0001_0001==0 | q0007_0002_0001==0 | ///
q0007_0003_0001==0 | q0007_0004_0001==0 | q0007_0005_0001==0 | q0007_0006_0001==0 | ///
q0007_0007_0001==0 | q0007_0008_0001==0 
replace miembro_de_grupo=1 if q0007_0001_0001==1
replace miembro_de_grupo=2 if q0007_0002_0001==1
replace miembro_de_grupo=3 if q0007_0003_0001==1
replace miembro_de_grupo=4 if q0007_0004_0001==1
replace miembro_de_grupo=5 if q0007_0005_0001==1
replace miembro_de_grupo=6 if q0007_0006_0001==1
replace miembro_de_grupo=7 if q0007_0007_0001==1
replace miembro_de_grupo=8 if q0007_0008_0001==1
lab def miembro_de_grupodef 1"Organizacion de su comunidad" 2"Grupos que reciben ayuda alimentaria" ///
3"Grupo politico" 4"Grupo religioso" 5"Grupo deportivo" 6"Comites de salud, agua, luz" ///
7"Comites de colegio" 8"Comites de vigilancia" 9 "No fui miembro de grupo"
lab val miembro_de_grupo miembro_de_grupodef 


gen apoyo_emo_n = q0007_0001_0002 + q0007_0002_0002 + q0007_0003_0002 + q0007_0004_0002 + ///
q0007_0005_0002 + q0007_0006_0002 + q0007_0007_0002 + q0007_0008_0002
lab var apoyo_emo "Apoyo Emocional"

gen apoyo_emo=.
replace apoyo_emo=9 if q0007_0001_0002==0 | q0007_0002_0002==0 | ///
q0007_0003_0002==0 | q0007_0004_0002==0 | q0007_0005_0002==0 | q0007_0006_0002==0 | ///
q0007_0007_0002==0 | q0007_0008_0002==0 
replace apoyo_emo=1 if q0007_0001_0002==1
replace apoyo_emo=2 if q0007_0002_0002==1
replace apoyo_emo=3 if q0007_0003_0002==1
replace apoyo_emo=4 if q0007_0004_0002==1
replace apoyo_emo=5 if q0007_0005_0002==1
replace apoyo_emo=6 if q0007_0006_0002==1
replace apoyo_emo=7 if q0007_0007_0002==1
replace apoyo_emo=8 if q0007_0008_0002==1
lab def apoyo_emodef 1"Organizacion de su comunidad" 2"Grupos que reciben ayuda alimentaria" ///
3"Grupo politico" 4"Grupo religioso" 5"Grupo deportivo" 6"Comites de salud, agua, luz" ///
7"Comites de colegio" 8"Comites de vigilancia" 9 "No apoyo emocional"
lab val apoyo_emo apoyo_emodef 


gen apoyo_mat_n = q0007_0001_0003 + q0007_0002_0003 + q0007_0003_0003 + q0007_0004_0003 + ///
q0007_0005_0003 + q0007_0006_0003 + q0007_0007_0003 + q0007_0008_0003
lab var apoyo_mat "Apoyo Material"

gen apoyo_mat=.
replace apoyo_mat=9 if q0007_0001_0003==0 | q0007_0002_0003==0 | ///
q0007_0003_0003==0 | q0007_0004_0003==0 | q0007_0005_0003==0 | q0007_0006_0003==0 | ///
q0007_0007_0003==0 | q0007_0008_0003==0 
replace apoyo_mat=1 if q0007_0001_0003==1
replace apoyo_mat=2 if q0007_0002_0003==1
replace apoyo_mat=3 if q0007_0003_0003==1
replace apoyo_mat=4 if q0007_0004_0003==1
replace apoyo_mat=5 if q0007_0005_0003==1
replace apoyo_mat=6 if q0007_0006_0003==1
replace apoyo_mat=7 if q0007_0007_0003==1
replace apoyo_mat=8 if q0007_0008_0003==1
lab def apoyo_matdef 1"Organizacion de su comunidad" 2"Grupos que reciben ayuda alimentaria" ///
3"Grupo politico" 4"Grupo religioso" 5"Grupo deportivo" 6"Comites de salud, agua, luz" ///
7"Comites de colegio" 8"Comites de vigilancia" 9 "No apoyo material"
lab val apoyo_mat apoyo_matdef 



gen apoyo_aprend_n = q0007_0001_0004 + q0007_0002_0004 + q0007_0003_0004 + q0007_0004_0004 + ///
q0007_0005_0004 + q0007_0006_0004 + q0007_0007_0004 + q0007_0008_0004
lab var apoyo_aprend "Apoyo Aprendizaje"

gen apoyo_aprend=.
replace apoyo_aprend=9 if q0007_0001_0004==0 | q0007_0002_0004==0 | ///
q0007_0003_0004==0 | q0007_0004_0004==0 | q0007_0005_0004==0 | q0007_0006_0004==0 | ///
q0007_0007_0004==0 | q0007_0008_0004==0 
replace apoyo_aprend=1 if q0007_0001_0004==1
replace apoyo_aprend=2 if q0007_0002_0004==1
replace apoyo_aprend=3 if q0007_0003_0004==1
replace apoyo_aprend=4 if q0007_0004_0004==1
replace apoyo_aprend=5 if q0007_0005_0004==1
replace apoyo_aprend=6 if q0007_0006_0004==1
replace apoyo_aprend=7 if q0007_0007_0004==1
replace apoyo_aprend=8 if q0007_0008_0004==1
lab def apoyo_aprenddef 1"Organizacion de su comunidad" 2"Grupos que reciben ayuda alimentaria" ///
3"Grupo politico" 4"Grupo religioso" 5"Grupo deportivo" 6"Comites de salud, agua, luz" ///
7"Comites de colegio" 8"Comites de vigilancia" 9 "No apoyo aprendizaje"
lab val apoyo_aprend apoyo_aprenddef 




global cat grupo_apoyo_emo grupo_apoyo_mat grupo_apoyo_aprend
lab def etiqueta 0 "sin apoyo" 1 "con apoyo"

foreach i in $cat {
	recode `i' (2=1) (3=1) (4=1) (5=1)
	lab val `i' etiqueta
}


recode q0008_000?_0001 (2=0)
gen apoyo_de_individuo = q0008_0001_0001 + q0008_0002_0001 + q0008_0003_0001 + q0008_0004_0001 + ///
q0008_0005_0001 + q0008_0006_0001 + q0008_0007_0001 + q0008_0008_0001 + q0008_0009_0001
lab var apoyo_de_individuo "Apoyo de individuos"


*>Table 1 - Social capital domains
foreach i in miembro_de_grupo apoyo_emo apoyo_mat apoyo_aprend apoyo_de_individuo {
	tabstat `i', by(tiempo) stat(n mean sd median min max skew kurt cv)
}

global k grupo_miemb grupo_apoyo apoyo_indiv act_comunidad capsoc_cog
foreach i in $k {
	tab `i' tiempo, col chi2
}

foreach i in $cat {
	tab `i'
}

	
*>Table 2 -
tab1 miembro_de_grupo apoyo_emo apoyo_mat apoyo_aprend

foreach i in miembro_de_grupo apoyo_emo apoyo_mat apoyo_aprend {
	*tab1 `i' if `i'!=9 & tiempo==1
	tab1 `i' if tiempo==1
	*tab `i' tiempo,col
	*tab `i' tiempo if `i'!=9
}

tab1 q0007_000*_0001
tab1 q0007_000*_0002
tab1 q0007_000*_0003
tab1 q0007_000*_0004

*>Table 3 - 
#delimit;
global aifull ="
q0008_0001_0001 q0008_0002_0001 q0008_0003_0001 q0008_0004_0001 q0008_0005_0001
q0008_0006_0001 q0008_0007_0001 q0008_0008_0001 q0008_0009_0001
";
#delimit cr;

foreach i in $aifull{
	tab q0008_0001_0001 tiempo, row chi2
}
