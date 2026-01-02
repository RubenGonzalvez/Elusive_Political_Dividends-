*==============================================================================*
* Title: Results with a monthly database and quarter regression results 
* Author: Rubén Gonzálvez 
* Data: 25/11/2025
*==============================================================================*

*------------------------------------------------------------------------------*
**# Directories and preferences
*------------------------------------------------------------------------------*


if ("`c(username)'" == "ruben") {
	global Folder = "C:/Users/ruben/Dropbox/Ideas/Dar dinero y voto. Anuncios"
	global Results = "C:/Users/ruben/Dropbox/Ideas/Dar dinero y voto. Anuncios/Resultados"
	global GraphPath ="C:/Users/ruben/Dropbox/Ideas/Dar dinero y voto. Anuncios/Resultados/Graphs"

}

	capture mkdir "$Folder/Resultados/Graphs/Jubilacion/Montly"
	capture mkdir "$Folder/Resultados"
	capture mkdir "$Results/Graphs"
	capture mkdir "$Results/Graphs/Jubilacion"
	
	set scheme Modern 

*------------------------------------------------------------------------------*
**# Clean Data
*------------------------------------------------------------------------------*
	
	use "$Folder/Final-Data/Data_EPSA_Montly.dta", clear
	
	* Generate a monthly Stata date
	gen month = ym(YEAR, MES)
	format month %tm
	* Drop if missing date: 
	drop if month==.
	drop if YEAR>2024
	
	* Generate quarter data
	gen qtr = qofd(dofm(month))
	format qtr %tq
	
	* Correct variables and labels
	replace Voto=5 if Voto==.
	label define partidos 1 "PP" 2 "PSOE" 5 "RESTO" 101 "No Sabe Alternativo" 77 "Nulo" 96 "Blanco" 97 "NO_Votara" 98 "No_Sabe" 99 "N_C"
	label values Voto partidos
	notes _dta: No sabe alternativo porque siempre suele preguntar no sabe como indeciso. ///
	En este caso pregunta no lo tiene decidido aún y no sabe como 2 posibles respeustas distintas.
	order estu DIA MES YEAR month DIA MES YEAR PROV CCAA Voto EDAD SITLAB
	
	* Unemployment: 
	preserve
	import excel "$Folder/Input-Data/Datos_Adicionales/Datos_Macro/65334.xlsx", sheet("PARO") firstrow clear
	reshape long region, i(Mes) j(id_region)
	sort id_region Mes
	rename region Paro
	gen year = real(substr(Mes, 1, 4))
	gen quarter = real(substr(Mes, -1, 1))
	gen month_num = (quarter - 1) * 3 + 1
	gen month = ym(year, month_num)
	format month %tm
	rename Mes MES
	keep if id_region>0
	* Redefine region as in CIS: 
	gen CCAA=1 if id_region==1
	replace CCAA=2 if id_region==2
	replace CCAA=3 if id_region==3
	replace CCAA=4 if id_region==4
	replace CCAA=5 if id_region==5
	replace CCAA=6 if id_region==6
	replace CCAA=7 if id_region==7
	replace CCAA=8 if id_region==8
	replace CCAA=9 if id_region==9
	replace CCAA=10 if id_region==10
	replace CCAA=11 if id_region==11
	replace CCAA=12 if id_region==12
	replace CCAA=13 if id_region==13
	replace CCAA=14 if id_region==14
	replace CCAA=15 if id_region==15
	replace CCAA=16 if id_region==16
	replace CCAA=17 if id_region==17
	replace CCAA=18 if id_region==18
	replace CCAA=19 if id_region==19
	keep CCAA month Paro
	tempfile tmp 
	save `tmp', replace 
	restore
	
	merge m:1 CCAA month using `tmp'
	drop if _merge==2
	drop _merge
	
	* CPI: 
	preserve
	import excel "$Folder/Input-Data/Datos_Adicionales/Datos_Macro/50913.xlsx", sheet("Datos") firstrow clear
	reshape long IPC Inflacion, i(Mes) j(id_region)
	sort id_region Mes
	gen year = real(substr(Mes, 1, 4))
	gen month = real(substr(Mes, strpos(Mes, "M") + 1, .))
	gen fecha_mensual = ym(year, month)
	format fecha_mensual %tm
	drop Mes 
	gen qtr = qofd(dofm(fecha_mensual))
	format qtr %tq
	drop if id_region==0
	gen CCAA=1 if id_region==1
	replace CCAA=2 if id_region==2
	replace CCAA=3 if id_region==3
	replace CCAA=4 if id_region==4
	replace CCAA=5 if id_region==5
	replace CCAA=6 if id_region==6
	replace CCAA=7 if id_region==7
	replace CCAA=8 if id_region==8
	replace CCAA=9 if id_region==9
	replace CCAA=10 if id_region==10
	replace CCAA=11 if id_region==11
	replace CCAA=12 if id_region==12
	replace CCAA=13 if id_region==13
	replace CCAA=14 if id_region==14
	replace CCAA=15 if id_region==15
	replace CCAA=16 if id_region==16
	replace CCAA=17 if id_region==17
	replace CCAA=18 if id_region==18
	replace CCAA=19 if id_region==19
	collapse (mean) IPC Inflacion, by(CCAA fecha_mensual)
	keep CCAA fecha_mensual IPC Inflacion
	rename fecha_mensual month
	tempfile tmp 
	save `tmp', replace 
	restore 
	
	merge m:1 CCAA month using `tmp'
	drop if _merge==2
	drop _merge
	
	save "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", replace

*------------------------------------------------------------------------------*
**# DESCRIPTIVES: 
*------------------------------------------------------------------------------*

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0 
	replace PP=1 if Voto==1 
	gen OTRO=0 
	replace OTRO=1 if Voto==5
	
	** Vote Intention
	
	preserve
	collapse (mean) PSOE PP OTRO, by(month)
	
	tsset month
	
	twoway (line PSOE month, lcolor(cranberry ) lpattern(dash)) ///
		(line PP month, lcolor(navy)) ///
		(line OTRO month, lcolor(green) lpattern(dash_dot)), ///
		ylabel(0(0.1)0.5, nogrid) ///
		xlabel(, nogrid) ///
		xtitle("") ///
		ytitle("Vote Share") ///
		legend(label(1 "PSOE") label(2 "PP") label(3 "Other"))
		
	restore

*------------------------------------------------------------------------------*
**# Repeated Cross Section
*------------------------------------------------------------------------------*

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	
	gen participation=0
	replace participation=1 if Voto!=77 & Voto!=97 & Voto!=98
	drop if CCAA==18 | CCAA==19 | CCAA==.
	gen empleo=0
	replace empleo=1 if SITLAB==1
	
	** Definitions and aggregation
	
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0
	replace PP=1 if Voto==1
	
	** Generate Incumbent party
	
	gen Incumbent=0
	replace Incumbent=1 if Voto==1 & qtr<177 // PP before second quarter 2004 
	replace Incumbent=1 if Voto==2 & qtr>=178 & qtr<=208 // PSOE after    until 
	replace Incumbent=1 if Voto==1 & qtr>208 & qtr<233 // PP after     until
	replace Incumbent=1 if Voto==2 & qtr>=233 // PSOE onwards
	gen PSOE_gobern=0 if qtr<177
	replace PSOE_gobern=1 if qtr>=178 & qtr<=208
	replace  PSOE_gobern=0 if qtr>=209 & qtr<233
	replace  PSOE_gobern=1 if qtr>=233
	
	** Generate Principal Opposition party
	
	gen Incumbente_PSOE=0
	replace Incumbente_PSOE= 1 if PSOE_gobern==1
	
	gen Incumbente_PP=0
	replace Incumbente_PP= 1 if PSOE_gobern==0
	
	** Generate Rest
	
	gen Resto=0
	replace Resto=1 if Voto==5 
	
	** Pensionist is SITLAB 2 and 3
	
	gen post=YEAR>=2023
	gen treated=1 if SITLAB==2 | SITLAB==3
	replace treated=0 if treated==.
	keep if treated==1 | (EDAD>=50 & EDAD<=60)
	
	keep if YEAR>=2021 & YEAR<=2024
	
	reghdfe PSOE post c.treated#c.post, vce(robust)
	
	destring PESO, replace
	
	replace PSOE=PSOE*100
	
	preserve 
	keep if qtr>=247 & qtr<=256
	reghdfe PSOE ib251.qtr ib251.qtr##c.treated [aweight=PESO], vce(robust)
	
	local labels
	forvalues i = 1/10 {
		local reltime = `i' - 6
		local labels `labels' `i' "`reltime'"
	}
	
	coefplot, vertical keep(*.qtr#*.treated) ///
		yline(0, lpattern(solid)) ciopts(recast(rcap) color(black)) level(95) ///
		xlabel(`labels', angle(horizontal)) ///
		msymbol(o) mcolor(black) ///
		ytitle("Treatment Effect. PSOE Voting Intention (p.p.)") ///
		xtitle("Relative time to treatment change (quarters)") ///
		baselevels xline(5.35, lpattern(dash) lcolor(black)) ///
		yscale(range(-15(5)15)) ylabel(-15(5)15, nogrid) ///
		addplot(line @b @at if @at<=7, lc(black) lpattern(solid) ///
		|| line @b @at if @at>6, lc(black) lpattern(solid)) ///
		graphregion(color(white)) omitted
		
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/TWFE.pdf", as(pdf) name("Graph") replace
	restore

*------------------------------------------------------------------------------*
**# Raw Means
*------------------------------------------------------------------------------* 

	preserve
	reg PSOE i.PROV Inflacion EDAD i.DIA#i.MES [aweight=PESO]
	predict PSOE_resid, resid
	collapse (mean) PSOE [aw=PESO], by(qtr treated)
	
	twoway ///
	(connected PSOE qtr if treated == 1, ///
		lcolor(ltblue) lwidth(medthick) lpattern(solid) ///
		mcolor(black) mfcolor(ltblue) msymbol(square) msize(medium)) ///
	(connected PSOE qtr if treated == 0, ///
		lcolor(pink) lwidth(medthick) lpattern(dot) ///
		mcolor(black) mfcolor(pink) msymbol(triangle) msize(medium)), ///
		xline(251.5, lpattern(dash) lcolor(gray)) ///
	xlabel(, nogrid) ylabel(20(5)35, nogrid) ///
	legend(label(1 "Treated") label(2 "Control")) ///
	ytitle("PSOE Voting Intention (p.p.)") xtitle("Quarter") ///
	title("") ///
	graphregion(color(white))  
	
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/TWFE_Raw_Means.pdf", as(pdf) name("Graph") replace
	
	restore

*------------------------------------------------------------------------------*
**# Synthetic DiD: (Pseudopanel)
*------------------------------------------------------------------------------*

	gen tmp=1
	collapse (mean) PSOE YEAR (sum) tmp [aw=PESO], by(CCAA treated qtr)
	gen year_int = round(YEAR)
	replace YEAR=year_int
	gen post=YEAR>=2023
	egen id=group(CCAA treated)
	xtset id qtr
	gen treated_post=treated*post
	
	* Run the event study
	preserve
	set seed 1234
	*keep if qtr>=247 & qtr<=255
	
	sdid  PSOE id qtr treated_post, vce(bootstrap) graph g2_opt(ylab(-0(10)40, nogrid) ///
		ytitle("PSEO Intention to Vote") scheme(Modern) xlab(,nogrid)) 
		
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/Synthetic_Control_Graph.pdf", as(pdf) replace
		
	restore
		
	set seed 1234
	sdid_event PSOE id qtr treated_post, placebo(all) 
	mat res = e(H)[2..17,1..5]
	svmat res
	gen id2 = _n - 1 if !missing(res1)
	replace id2 = 9 - _n if _n > 9 & !missing(res1)
	sort id2
	
	keep if id2>=-4 & id2<=4
	
	* Plot event-study chart
	
	twoway ///
	(rcap res3 res4 id2, lc(black)) ///         // error bars
	(line res1 id2, lc(black)) ///              // connected line
	(scatter res1 id2, mc(black) msymbol(o)), /// points
	legend(off) ///
	title() ///
	ytitle(Treatment Effect. PSOE Voting Intention (p.p.)) ///
	xtitle(Relative time to treatment change (quarters)) ///
	yline(0, lc(black) lp(solid)) ///
	xline(-0.5, lc(black) lp(dash)) ///
	xlabel(-4(1)4, nogrid) ylabel(-15(5)15,nogrid) 
	
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/Synthetic_DiD.pdf", as(pdf) name("Graph") replace

*----------------------------------
**# Time shifted DiD):
*----------------------------------

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	
	gen participation=0
	replace participation=1 if Voto!=77 & Voto!=97 & Voto!=98
	drop if CCAA==18 | CCAA==19 | CCAA==.
	gen empleo=0
	replace empleo=1 if SITLAB==1
	
	** Definitions and aggregation
	
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0
	replace PP=1 if Voto==1
	** Generate Incumbent: 
	gen Incumbent=0
	replace Incumbent=1 if Voto==1 & qtr<177 // PP before second quarter 2004 
	replace Incumbent=1 if Voto==2 & qtr>=178 & qtr<=208 // PSOE after    until 
	replace Incumbent=1 if Voto==1 & qtr>208 & qtr<233 // PP after     until
	replace Incumbent=1 if Voto==2 & qtr>=233 // PSOE onwards
	gen PSOE_gobern=0 if qtr<177
	replace PSOE_gobern=1 if qtr>=178 & qtr<=208
	replace  PSOE_gobern=0 if qtr>=209 & qtr<233
	replace  PSOE_gobern=1 if qtr>=233
	
	** Generate Principal Oposition: 
	
	gen Incumbente_PSOE=0
	replace Incumbente_PSOE= 1 if PSOE_gobern==1
	
	gen Incumbente_PP=0
	replace Incumbente_PP= 1 if PSOE_gobern==0
	
	** Generate Resto: 
	gen Resto=0
	replace Resto=1 if Voto==5 
	
	** Pensionista is SITLAB 2 and 3
	keep if SITLAB==2 | SITLAB==3
	keep if YEAR>=2021 & YEAR<=2023
	
	preserve 
	gen treated=1 if YEAR>=2022
	keep if treated==1
	drop if YEAR<2022
	tempfile tmp 
	save `tmp'
	restore
	
	preserve 
	gen control=1 if YEAR>=2021
	keep if control==1
	drop if YEAR<2021
	drop if YEAR>=2023
	tempfile tmp2 
	save `tmp2'
	restore

	use `tmp', clear
	append using `tmp2'
	
	gen time=qtr -243 if control==1
	replace time= qtr-247 if treated==1
	
	destring PESO, replace
	replace treated=0 if control==1
	
	gen post=time>=5
	
	replace PSOE=PSOE*100
	
	reghdfe PSOE post c.treated#c.post i.PROV [aweight=PESO], vce(robust)
	
	reghdfe PSOE ib4.time ib4.time##i.treated i.PROV  [aweight=PESO], vce(robust)
	
	local labels
	forvalues i = 1/8 {
		local reltime = `i' - 5
		local labels `labels' `i' "`reltime'"
	}
	
	coefplot, vertical keep(*.time#*.treated) ///
		yline(0, lpattern(solid) lcolor(black)) ///
		ciopts(recast(rcap) color(black)) level(95) ///
		msymbol(o) mcolor(black) ///
		xlabel(`labels', angle(horizontal)) ///
		ytitle("Treatment Effect. PSOE Voting Intention (p.p.)") ///
		xtitle("Relative time to treatment change (quarters)") ///
		baselevels xline(4.5, lpattern(dash) lcolor(black)) ///
		yscale(range(-15(5)15)) ylabel(-15(5)15, nogrid) ///a
		addplot(line @b @at if @at<=7, lc(black) lpattern(solid) ///
		|| line @b @at if @at>6, lc(black) lpattern(solid)) ///
		graphregion(color(white))
		
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/Time_Shifted_DiD.pdf", as(pdf) name("Graph") replace
		
	** Raw Means: 
	preserve
	reg PSOE i.PROV Inflacion EDAD i.DIA#i.MES [aweight=PESO]
	predict PSOE_resid, resid
	
	collapse (mean) PSOE [aw=PESO], by(time treated)
	
	twoway ///
	(connected PSOE time if treated == 1, ///
		lcolor(ltblue) lwidth(medthick) lpattern(solid) ///
		mcolor(black) mfcolor(ltblue) msymbol(square) msize(medium)) ///
	(connected PSOE time if treated == 0, ///
		lcolor(pink) lwidth(medthick) lpattern(dot) ///
		mcolor(black) mfcolor(pink) msymbol(triangle) msize(medium)), ///
	xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "0" 6 "1" 7 "2" 8 "4", nogrid) ///
	ylabel(20(5)35, nogrid) ///
	xline(4.5, lpattern(dash) lcolor(gray)) ///
	legend(label(1 "Treated") label(2 "Control")) ///
	ytitle("PSOE Voting Intention (p.p.)") xtitle("Quarter") ///
	title("") ///
	graphregion(color(white))
	
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/Raw_means_Time_shifted.pdf", as(pdf) name("Graph") replace
	
	restore

*-----------------------------------------------------------
**# Impact of the announcement (2021): 
*-----------------------------------------------------------

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	
	gen participation=0
	replace participation=1 if Voto!=77 & Voto!=97 & Voto!=98
	drop if CCAA==18 | CCAA==19 | CCAA==.
	gen empleo=0
	replace empleo=1 if SITLAB==1
	
	** Definitions and aggregation
	
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0
	replace PP=1 if Voto==1
	** Generate Incumbent: 
	gen Incumbent=0
	replace Incumbent=1 if Voto==1 & qtr<177 // PP before second quarter 2004 
	replace Incumbent=1 if Voto==2 & qtr>=178 & qtr<=208 // PSOE after    until 
	replace Incumbent=1 if Voto==1 & qtr>208 & qtr<233 // PP after     until
	replace Incumbent=1 if Voto==2 & qtr>=233 // PSOE onwards
	gen PSOE_gobern=0 if qtr<177
	replace PSOE_gobern=1 if qtr>=178 & qtr<=208
	replace  PSOE_gobern=0 if qtr>=209 & qtr<233
	replace  PSOE_gobern=1 if qtr>=233
	
	** Generate Principal Oposition: 
	
	gen Incumbente_PSOE=0
	replace Incumbente_PSOE= 1 if PSOE_gobern==1
	
	gen Incumbente_PP=0
	replace Incumbente_PP= 1 if PSOE_gobern==0

	** Generate Resto: 
	gen Resto=0
	replace Resto=1 if Voto==5 
	
	** Pensionist is SITLAB 2 and 3
	
	gen post=YEAR>=2023
	gen treated=1 if SITLAB==2 | SITLAB==3
	replace treated=0 if treated==.
	keep if treated==1 | (EDAD>=50 & EDAD<=60)
	
	keep if YEAR>=2020 & YEAR<=2024
	
	reghdfe PSOE post c.treated#c.post, vce(robust)
	
	destring PESO, replace
	
	replace PSOE=PSOE*100
	
	preserve 
	
	drop if qtr==240 | qtr==241 | qtr==242
	drop if qtr>252
	*drop if qtr<247
	
	reghdfe PSOE ib247.qtr ib247.qtr##c.treated  [aweight=PESO], vce(robust)
	
	local labels
	forvalues i = 1/10 {
		local reltime = `i' - 6
		local labels `labels' `i' "`reltime'"
	}
	
	coefplot, vertical keep(*.qtr#*.treated) ///
		yline(0, lpattern(solid)) ciopts(recast(rcap) color(black)) level(95) ///
		xlabel(`labels', angle(horizontal)) ///
		msymbol(o) mcolor(black) ///
		ytitle("Treatment Effect. PSOE Voting Intention (p.p.)") ///
		xtitle("Relative time to treatment change (quarters)") ///
		baselevels xline(5.35, lpattern(dash) lcolor(black)) ///
		yscale(range(-15(5)15)) ylabel(-15(5)15, nogrid) ///
		addplot(line @b @at if @at<=7, lc(black) lpattern(solid) ///
		|| line @b @at if @at>6, lc(black) lpattern(solid)) ///
		graphregion(color(white)) omitted
		
	graph export "$Folder/Resultados/Graphs/Jubilacion/Montly/TWFE_Announcement.pdf", as(pdf) name("Graph") replace
	
	restore

*-----------------------------------------------------------
**# Main Table 
*-----------------------------------------------------------

	{
**# Repeated Cross Section:
	}

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	
	gen participation=0
	replace participation=1 if Voto!=77 & Voto!=97 & Voto!=98
	drop if CCAA==18 | CCAA==19 | CCAA==.
	gen empleo=0
	replace empleo=1 if SITLAB==1
	
	** Definitions and aggregation
	
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0
	replace PP=1 if Voto==1
	
	** Generate Incumbent: 
	
	gen Incumbent=0
	replace Incumbent=1 if Voto==1 & qtr<177 // PP before second quarter 2004 
	replace Incumbent=1 if Voto==2 & qtr>=178 & qtr<=208 // PSOE after    until 
	replace Incumbent=1 if Voto==1 & qtr>208 & qtr<233 // PP after     until
	replace Incumbent=1 if Voto==2 & qtr>=233 // PSOE onwards
	gen PSOE_gobern=0 if qtr<177
	replace PSOE_gobern=1 if qtr>=178 & qtr<=208
	replace  PSOE_gobern=0 if qtr>=209 & qtr<233
	replace  PSOE_gobern=1 if qtr>=233

	** Generate Principal Opposition

	gen Incumbente_PSOE=0
	replace Incumbente_PSOE= 1 if PSOE_gobern==1
	
	gen Incumbente_PP=0
	replace Incumbente_PP= 1 if PSOE_gobern==0
	
	** Generate Rest: 
	
	gen Resto=0
	replace Resto=1 if Voto==5 
	
	** Pensionist is SITLAB 2 and 3
	
	gen post=YEAR>=2023
	gen treated=1 if SITLAB==2 | SITLAB==3
	replace treated=0 if treated==.
	keep if treated==1 | (EDAD>=50 & EDAD<=60)
	
	keep if YEAR>=2021 & YEAR<=2024
	
	reghdfe PSOE post c.treated#c.post, vce(robust)
	
	destring PESO, replace
	
	replace PSOE=PSOE*100
	
	preserve 
	
	keep if qtr>=247 & qtr<=256
	
	reghdfe PSOE ib251.qtr ib251.qtr##c.treated  [aweight=PESO], vce(robust)
	
	sum PSOE if treated==0 & post==0 [aweight=PESO] // 20.483
	
	
	reghdfe PSOE i.treated##c.post [aweight=PESO], vce(robust) 
	eststo m1
	esttab m1, se keep(1.treated#c.post)
	
	restore

	{
**# Synthetic DiD: (Pseudopanel)
	}

	** Make a pseudo_panel by province and weight by time

	gen tmp=1
	collapse (mean) PSOE YEAR (sum) tmp [aw=PESO], by(CCAA treated qtr)
	gen year_int = round(YEAR)
	replace YEAR=year_int
	gen post=YEAR>=2023
	egen id=group(CCAA treated)
	xtset id qtr
	gen treated_post=treated*post
	
	* Run the event study
	preserve
	
	keep if qtr>=247 & qtr<=255
	
	sum PSOE if treated==0 & post==0  // 20.581	
	
	sdid  PSOE id qtr treated_post, vce(bootstrap) graph g2_opt(ylab(-0(10)40, nogrid) ///
		ytitle("Women in Parliament") scheme(Modern) xlab(,nogrid)) 
		
	eststo m2	
	esttab m1 m2, se keep(1.treated#c.post treated_post)
		
	restore

	{
**# Time Shifted DiD
	}

	use "$Folder/Final-Data/Data_EPSA_Final_Montly.dta", clear  
	
	gen participation=0
	replace participation=1 if Voto!=77 & Voto!=97 & Voto!=98
	drop if CCAA==18 | CCAA==19 | CCAA==.
	gen empleo=0
	replace empleo=1 if SITLAB==1
	
	** Definiciones y agregación
	
	gen PSOE=0 
	replace PSOE=1 if Voto==2
	gen PP=0
	replace PP=1 if Voto==1
	
	** Generate Incumbent: 
	gen Incumbent=0
	replace Incumbent=1 if Voto==1 & qtr<177 // PP before second quarter 2004 
	replace Incumbent=1 if Voto==2 & qtr>=178 & qtr<=208 // PSOE after    until 
	replace Incumbent=1 if Voto==1 & qtr>208 & qtr<233 // PP after     until
	replace Incumbent=1 if Voto==2 & qtr>=233 // PSOE onwards
	gen PSOE_gobern=0 if qtr<177
	replace PSOE_gobern=1 if qtr>=178 & qtr<=208
	replace  PSOE_gobern=0 if qtr>=209 & qtr<233
	replace  PSOE_gobern=1 if qtr>=233
	
	** Generate Principal Oposition party 

	gen Incumbente_PSOE=0
	replace Incumbente_PSOE= 1 if PSOE_gobern==1
	
	gen Incumbente_PP=0
	replace Incumbente_PP= 1 if PSOE_gobern==0
	
	** Generate Rest: 
	gen Resto=0
	replace Resto=1 if Voto==5 
	
	** Pensionista is SITLAB 2 and 3
	keep if SITLAB==2 | SITLAB==3
	keep if YEAR>=2021 & YEAR<=2023
	
	preserve 
	gen treated=1 if YEAR>=2022
	keep if treated==1
	drop if YEAR<2022
	tempfile tmp 
	save `tmp'
	restore
	
	preserve 
	gen control=1 if YEAR>=2021
	keep if control==1
	drop if YEAR<2021
	drop if YEAR>=2023
	tempfile tmp2 
	save `tmp2'
	restore
	
	use `tmp', clear
	append using `tmp2'
	
	gen time=qtr -243 if control==1
	replace time= qtr-247 if treated==1
	
	destring PESO, replace
	replace treated=0 if control==1
	
	gen post=time>=5
	
	replace PSOE=PSOE*100
	
	reghdfe PSOE post i.treated##c.post i.PROV [aweight=PESO], vce(robust)
	eststo m3
	
	esttab m1 m2 m3, se keep(1.treated#c.post treated_post)
	
	sum PSOE if treated==0 & post==0 [aweight=PESO] // 26.596
	
	capture mkdir "$Folder/Resultados/Tables"

	esttab m1 m2 m3 using "$Folder/Resultados/Tables/results.csv", se replace ///
		keep(1.treated#c.post treated_post) gaps compress par
