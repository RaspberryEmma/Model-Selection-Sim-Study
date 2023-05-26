# ****************************************
# Variable Selection Simulation Study
# 
# Run just the results phase of the simulation from command line
#   1. Generate all relevant plots
# 	2. Generate data, run variable selection methods on data-sets, fit models, measure bias and record
#   3. Interpret bias results wrt. data generation assumed DAG
# 
# Emma Tarmey
#
# Started:          10/04/2023
# Most Recent Edit: 26/05/2023
# ****************************************


clear
echo " *** Starting ***"


# Interpret results (columns sums?)
echo ""
echo ""
echo " *** Interpreting Results ***"
Rscript interpret_results.R


echo ""
echo " *** Complete ***"



