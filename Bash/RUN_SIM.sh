# ****************************************
# Variable Selection Simulation Study
# 
# Run Entire Simulation from Command Line
#   1. Generate all relevant plots
#   2. Generate data, run variable selection methods on data-sets, fit models, measure bias and record
#   3. Interpret bias results wrt. data generation assumed DAG
# 
# Emma Tarmey
#
# Started:          10/04/2023
# Most Recent Edit: 03/10/2023
# ****************************************


echo " *** Starting ***"


# Generate all relevant plots
echo ""
echo ""
echo " *** Generating Plots ***"
Rscript "../R/generate_data_plots.R"


# Run variable selection on data-sets
echo ""
echo ""
echo " *** Performing Simulation ***"
Rscript "../R/simulation.R"


# Interpret coefficient bias and coefficient value results
echo ""
echo ""
echo " *** Interpreting Results ***"
Rscript "../R/interpret_bias_results.R"
Rscript "../R/interpret_coef_results.R"


echo ""
echo " *** Complete ***"




