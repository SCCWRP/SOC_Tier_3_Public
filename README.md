# SOC_Tier_3_Public

# SOC_Tier_3_New

#  Code index

1 - 00_Format_chub_data_and_model.R
2 - 01_Chub_model.R
3 - 02_Willow_model.R
4 - 03_Combine_suitability.R
5 - 04_Chub_flow_targets.R
6 - 05_overall_suitability.R
7 - 06_Suitability_Maps.R

# 00_Format_chub_data_and_model.R

formatting and cleaning of Wulff et al data to build models

produces models and figures

# Depth

00_chub_Adult_depth_Prob_curve.png 
chub_depth_mod.RData 

#  Velocity

00_chub_Adult_velocity_Prob_curve.png
00_chub_adult_velocity_prob_curve_data.csv

# 01_Chub_model.R

calculates the percentage of time that Q is within the flow ranges define by the models and Taniguchi-Quan et al xxxx

DFs for each node, scenario and hydraulic saved separately in "output_data/Chub"

# 02_Willow_model.R

calculates the percentage of time that Q is within the flow ranges define by Willow_Flow_Ranges_Ruleset.csv and Taniguchi-Quan et al xxxx

Current conditions and water conservation included. Produces two files:

02_Willow_Current.csv
02_Willow_WaterCons.csv

# 03_Combine_suitability.R

combines all chub and willow models and calculates suitability based on percentage of time Q is within limits for each scenario

dfs produced:

suitability per year

03_Willow_time_stats_combined_current_water_conservation.csv
03_Chub_time_stats_combined_water_conservation.csv

overall suitability

03_chub_suitability_combined_years_all_probs_current.csv
03_willow_suitability_combined_years_all_probs_current.csv

# 04_Chub_flow_targets.R

extracts flow targets for chub for each node based on Q limits and calculation from model run. filters to only main channel slice

04_chub_limits_calcs_best_slice.csv
04_chub_flow_targets_best_slices_only.csv ## flow targets for each node

# 05_overall_suitability.R

defines overall class for each Node, species and scenario:

05_chubs_suit_class_wide_updated_current_50_prob.csv # based on medium probability
05_willow_suit_class_wide_updated_current_watercons.csv

#  06_Suitability_Maps.R

Creates suitability maps for each species

