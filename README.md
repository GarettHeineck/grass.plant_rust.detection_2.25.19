# grass.plant_rust.detection_2.25.19

## Project information
### 1. This project demonstrates how to use R to quantify crown rust on spaced plants.  
   1. Three example perennial ryegrass plants were imaged in the field under ambient light conditions.
   1. These are saved as JPGs in the 'original images' folder.
### 2. A metadata sheet is included in the 'results' folder.  
   1. The data sheet contains important information to validate the computer output.
   1. Variables were collected manually on crown rust severity taken on the Cobb scale.
### 3. Several training data csv files are included in the 'training data' folder.  
   1. Collectively, these mixes (csv files) make up the training palette for this project.
   1. The training palette is used to fit random forest models.
   1. The training palette can be expanded or shrunk by adding or subtracting mixes.
   1. New mixes can be made easily in ImageJ (SEE MODULE ON MAKING TRAINING DATA).
   1. Mixes can be subtracted by removing the files after downloading or using R.


## Setup
1. Start by downloading the ZIP file with all documents.
1. Save these documents in a folder called 'plant.demo_2.25.19'
1. NOTE: if you do not use this file name it will take a little more work to run the R script.
1. Open the R file in R studio.
1. Go through each required package and download or update packages if needed.
1. Update R if you have not done so recently.
1. In the R file go to line 32 and change the object file path to wherever your 'plant.demo_2.25.19' folder is located.
1. The script should run and conduct the following tasks:
   1. Create new folders within 'plant.demo_2.25.19'.
   1. Build two random forest models to detect foreground and crown rust.
   1. Operate image analysis using the models and EBImage functions.
   1. Output and save a summary data file.
   1. Conduct a few basic visualizations in ggplot.
