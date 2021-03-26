use "D:\Dropbox\Latin America Vaccine Hesitancy\Vaccine-Hesitancy\02. Cleaned data\vaccine_wide.dta", clear
drop if speeder==1



*** Bounding tests for Supllementary Table 11

eststo clear
quietly foreach y of varlist hesitancy_post_rec hesitancy_dummy_post quickly_post_1_text_reversed encourage2 {
	eststo, title("`y'"): leebounds `y' any_info if group!=. & std_months_pre!=. [weight=IPW_any], cieffect
	local L: di %6.3f `e(cilower)'
	local U: di %6.3f `e(ciupper)'
	estadd local CI = "[`L',`U']"
	estadd scalar trimming = `e(trim)'
	sum `y' [weight=IPW_any] if e(sample)
	estadd scalar control_mean = `r(mean)'
	estadd scalar control_sd = `r(sd)'
}
estout, style(tex) stats(CI control_mean control_sd Nsel trimming, fmt(%9.0fc 2 2 %9.0fc 3) labels("Treatment effect 95\% confidence interval" ///
	"\\ Control outcome mean" "Control outcome std. dev." "Number of selected observations" "Share of control observations trimmed")) drop(upper lower)


	
*** Bounding tests for Supllementary Table 12
cap tab motivation_treatment_enc, g(motiv)

* Panel A
eststo clear
quietly foreach y of varlist hesitancy_post_rec hesitancy_dummy_post quickly_post_1_text_reversed encourage2 {
	eststo, title("`y'"): leebounds `y' motiv2 if motivation_treatment_enc==0 | motivation_treatment_enc==1, cieffect
	local L: di %6.3f `e(cilower)'
	local U: di %6.3f `e(ciupper)'
	estadd local CI = "[`L',`U']"
	estadd scalar trimming = `e(trim)'
	sum `y' if e(sample)
	estadd scalar control_mean = `r(mean)'
	estadd scalar control_sd = `r(sd)'
}
estout, style(tex) stats(CI control_mean control_sd Nsel trimming, fmt(%9.0fc 2 2 %9.0fc 3) labels("Treatment effect 95\% confidence interval" ///
	"\\ Control outcome mean" "Control outcome std. dev." "Number of selected observations" "Share of control observations trimmed")) drop(upper lower)

* Panel B
eststo clear
quietly foreach y of varlist hesitancy_post_rec hesitancy_dummy_post quickly_post_1_text_reversed encourage2 {
	eststo, title("`y'"): leebounds `y' motiv3 if motivation_treatment_enc==0 | motivation_treatment_enc==2, cieffect
	local L: di %6.3f `e(cilower)'
	local U: di %6.3f `e(ciupper)'
	estadd local CI = "[`L',`U']"
	estadd scalar trimming = `e(trim)'
	sum `y' if e(sample)
	estadd scalar control_mean = `r(mean)'
	estadd scalar control_sd = `r(sd)'
}
estout, style(tex) stats(CI control_mean control_sd Nsel trimming, fmt(%9.0fc 2 2 %9.0fc 3) labels("Treatment effect 95\% confidence interval" ///
	"\\ Control outcome mean" "Control outcome std. dev." "Number of selected observations" "Share of control observations trimmed")) drop(upper lower)

* Panel C
eststo clear
quietly foreach y of varlist hesitancy_post_rec hesitancy_dummy_post quickly_post_1_text_reversed encourage2 {
	eststo, title("`y'"): leebounds `y' motiv4 if motivation_treatment_enc==0 | motivation_treatment_enc==3, cieffect
	local L: di %6.3f `e(cilower)'
	local U: di %6.3f `e(ciupper)'
	estadd local CI = "[`L',`U']"
	estadd scalar trimming = `e(trim)'
	sum `y' if e(sample)
	estadd scalar control_mean = `r(mean)'
	estadd scalar control_sd = `r(sd)'
}
estout, style(tex) stats(CI control_mean control_sd Nsel trimming, fmt(%9.0fc 2 2 %9.0fc 3) labels("Treatment effect 95\% confidence interval" ///
	"\\ Control outcome mean" "Control outcome std. dev." "Number of selected observations" "Share of control observations trimmed")) drop(upper lower)

	
