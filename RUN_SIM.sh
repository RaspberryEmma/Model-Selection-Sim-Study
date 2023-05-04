# ****************************************
# Variable Selection Simulation Study
# 
# Run Entire Simulation from Command Line
#   1. Generate all relevant plots
# 	2. Generate data, run variable selection methods on data-sets, fit models, measure bias and record
#   3. Interpret bias results wrt. data generation assumed DAG
# 
# Emma Tarmey
#
# Started:          10/04/2023
# Most Recent Edit: 04/05/2023
# ****************************************


clear
echo " *** ✨ Starting ✨ ***"


# Generate all relevant plots
echo ""
echo ""
echo " *** Generating Plots ***"
Rscript generate_data.R


# Run variable selection on data-sets
echo ""
echo ""
echo " *** Performing Simulation ***"
Rscript simulation.R


# Interpret results (columns sums?)
echo ""
echo ""
echo " *** Interpreting Results ***"
Rscript interpret_results.R


echo ""
echo " *** ✨ Complete ✨ ***"




