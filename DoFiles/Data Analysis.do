*Do File

//Paper: Poisoned Generosity - Negative Effects of Economic Aid on African Institutions 
//Author: Juan Sebastian Salazar Garcia
//Institution: University Carlos III of Madrid
//Last Update: 17/06/2023

*Step 1 - Import Ad-Hoc Dataset
	**Set working space
cd "C:\Users\SEBASTIAN\Documents\MA Social Science\Research Design & TFM\TFM\Data"
	**Import Dataset
use "African Economic and Political Dataset", replace
*Step 2 - Set scheme for Visuals 
set scheme tufte
*Step 3 - Set XT scheme
xtset ccode year
*Step 4 - Dataset analysis

	**Descriptive tables
		**Socioeconomic measure
table CountryName, c(mean gdp_pc mean ictd_taxexsc mean expenses mean aid_percentage_expenses mean vdem_libdem)
		**Region
table CountryName,c(m northafrica m centralafrica m eastafrica m southernafrica m westafrica)
		**Institutions
table CountryName,c(m bmr_dem m prp_prp m cbi_cbiw m lld_capacity m vdem_corr)

	**Net official development assistance (ODA) received
hexplot aid_percentage_expenses CountryName year
hexplot aid_percentage_gni CountryName year
		***Mean Net ODA received
twoway (line mean_aid year if  ccode==12) (lfit mean_aid year if ccode==12), yti("ODA Disbursements (USD Millions)")

	**Hanson & Sigman's State Capacity Index
		***Overlapping lines
xtline lld_capacity, legend(off) overlay
		***Mean Hanson & Sigman's State Capacity Index
twoway (line mean_lld_capacity year if  ccode==12) (lfit mean_lld_capacity year if ccode==12), yti("Mean Hanson & Sigman's State Capacity Index")
		***Correlation Analysis with Aid
			****Aid Disbursements
graph twoway (lfit ln_lld_capacity  ln_aid) (scatter ln_lld_capacity  ln_aid)
pwcorr ln_lld_capacity ln_aid
			****Aid as share of Public Expenditure
graph twoway (lfit lld_capacity  ln_aid_percentage_expenses) (scatter lld_capacity  ln_aid_percentage_expenses)
pwcorr lld_capacity ln_aid_percentage_expenses
			****Aid as share of GNI
graph twoway (lfit ln_lld_capacity  ln_aid_percentage_gni) (scatter ln_lld_capacity  ln_aid_percentage_gni)
pwcorr ln_lld_capacity ln_aid_percentage_gni

	**Corruption Indexes (vdem_corr vdem_execorr vdem_jucorrdc vdem_pubcorr)
		***Correlation Analysis with Aid
		
		//N Corruption Index - Aid Disbursements
		
			****Politcal Corrution - Aid Disbursements
graph twoway (lfit ln_vdem_corr  ln_aid) (scatter ln_vdem_corr  ln_aid)
pwcorr ln_vdem_corr ln_aid
			****Executive Corrution - Aid Disbursements
graph twoway (lfit vdem_execorr  ln_aid) (scatter vdem_execorr  ln_aid)
pwcorr vdem_execorr ln_aid
			****Judicial Corrution - Aid Disbursements
graph twoway (lfit vdem_jucorrdc  ln_aid) (scatter vdem_jucorrdc  ln_aid)
pwcorr vdem_jucorrdc ln_aid
			****Public Corrution - Aid Disbursements
graph twoway (lfit vdem_pubcorr  ln_aid) (scatter vdem_pubcorr  ln_aid)
pwcorr vdem_pubcorr ln_aid

		//N Corruption Index - Aid as share of Public Expenditure
		
			****Politcal Corrution - Aid as share of Public Expenditure
graph twoway (lfit ln_vdem_corr  ln_aid_percentage_expenses) (scatter ln_vdem_corr  ln_aid_percentage_expenses)
pwcorr ln_vdem_corr ln_aid_percentage_expenses
			****Executive Corrution - Aid as share of Public Expenditure
graph twoway (lfit vdem_execorr  ln_aid_percentage_expenses) (scatter vdem_execorr  ln_aid_percentage_expenses)
pwcorr vdem_execorr ln_aid_percentage_expenses
			****Judicial Corrution - Aid as share of Public Expenditure
graph twoway (lfit vdem_jucorrdc  ln_aid_percentage_expenses) (scatter vdem_jucorrdc  ln_aid_percentage_expenses)
pwcorr vdem_jucorrdc ln_aid_percentage_expenses
			****Public Corrution - Aid as share of Public Expenditure
graph twoway (lfit vdem_pubcorr  ln_aid_percentage_expenses) (scatter vdem_pubcorr  ln_aid_percentage_expenses)
pwcorr vdem_pubcorr ln_aid_percentage_expenses

		//N Corruption Index - Aid as share of GNI
		
			****Politcal Corrution - Aid as share of GNI
graph twoway (lfit ln_vdem_corr  ln_aid_percentage_gni) (scatter ln_vdem_corr  ln_aid_percentage_gni)
pwcorr ln_vdem_corr ln_aid_percentage_gni
			****Executive Corrution - Aid as share of GNI
graph twoway (lfit vdem_execorr  ln_aid_percentage_gni) (scatter vdem_execorr  ln_aid_percentage_gni)
pwcorr vdem_execorr ln_aid_percentage_gni
			****Judicial Corrution - Aid as share of GNI
graph twoway (lfit vdem_jucorrdc  ln_aid_percentage_gni) (scatter vdem_jucorrdc  ln_aid_percentage_gni)
pwcorr vdem_jucorrdc ln_aid_percentage_gni
			****Public Corrution - Aid as share of GNI
graph twoway (lfit vdem_pubcorr  ln_aid_percentage_gni) (scatter vdem_pubcorr  ln_aid_percentage_gni)
pwcorr vdem_pubcorr ln_aid_percentage_gni

*Step 5 - Regression models

	** Independent var: Hanson & Sigman's State Capacity Index

		***End. Dependent var: Tax Revenue
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxexsc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, replace tex
		***End. Dependent var: Tax on Payroll
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxpaywf = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid
		***End. Dependent var: Tax on Int. trade
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxtrade = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid
		***End. Dependent var: Tax on Corporate
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxcorp = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid
		***End. Dependent var: Tax on Goods and Services
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxgs = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid
		***End. Dependent var: Tax on Income and Capital Gains
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxinc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid
		***End. Dependent var: Tax on Property
ivregress gmm lld_capacity vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp (ictd_taxprop = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_state_capacity_and_aid_exp, append tex
estat overid

			****Robustness Check (OLS Fixed Effects)
			
xtreg lld_capacity ictd_taxexsc aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp centralafrica eastafrica southernafrica westafrica
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxpaywf aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxtrade aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxcorp aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxgs aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxinc aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxprop aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

xtreg lld_capacity ictd_taxpaywf ictd_taxtrade ictd_taxcorp ictd_taxgs ictd_taxinc ictd_taxprop ictd_taxgs ictd_taxinc  aid_percentage_expenses vdem_corr wdi_popurb gdp_pc wr_regtype wdi_lifexp
outreg2 using GMM_state_capacity_and_aid_exp, append tex

	** Independent var: Political Corruption Index

		***End. Dependent var: Tax Revenue

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxexsc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, replace tex
estat overid

		***End. Dependent var: Tax on Payroll

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxpaywf = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Int. trade

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxtrade = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Corporate

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxcorp = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Goods and Services

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxgs = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Income and Capital Gains

ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxinc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Property
		
ivregress gmm vdem_corr gdp_pc wdi_gini wr_regtype fh_feb biu_relleg (ictd_taxprop = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_corruption_and_aid_exp, append tex
estat overid


	** Independent var: Inflation Rate

		***End. Dependent var: Tax Revenue

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxexsc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, replace tex
estat overid

		***End. Dependent var: Tax on Payroll

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxpaywf = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Int. trade

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxtrade = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Corporate

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxcorp = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Goods and Services

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxgs = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Income and Capital Gains

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxinc = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid

		***End. Dependent var: Tax on Property

ivregress gmm wdi_inflation expenses cbi_cbiw wdi_gdpgr fi_sog_pd (ictd_taxprop = aid_crsc aid_crsio aid_percentage_expenses gdp_pc wdi_trade wdi_export gdp_pc), first small
outreg2 using GMM_inflation_and_aid_exp, append tex
estat overid


