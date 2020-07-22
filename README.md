# netball_prediction
Running a deep learning model to predict netball matches

This model assumes you have already gathered the super netball or ANZ premiership data using SupernetballR https://github.com/SteveLane/superNetballR and saved the matches as JSON files in a folder.

The more data you have to train your model the better it will be so consider adding in ANZ Premiership, Super Netball and even ANZ Premiership.

The scripts are designed to be run in sequence after you've installed the required packages and added the upcoming fixture to the the fixture.csv file:

## First 
run glicko.R to get team ratings.

## Second 
run the netball_match_function.R to make organising the data easier.

## Third 
run match_stats.R to merge ratings with match stats, here you can modify the input variables as well. It is important that in the fixture.csv file the result is 999 or blank and match numbers follow the previous match in the match_data dataframe.

## Fourth 
run match_predictions.R to train a keras model and inspect the results.

## Fifth 
run Store_predictions.R to show your tips and keep track of them as they occur through the season.
