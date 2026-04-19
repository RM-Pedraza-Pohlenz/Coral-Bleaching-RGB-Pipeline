from optparse import OptionParser
import os
import pandas as pd
import warnings
warnings.filterwarnings("ignore")


#Parse the options
usage = "USAGE: python color_clustering_and_distance.py --f folder_name \n"
parser = OptionParser(usage=usage)

#options
# parser.add_option("--initial",help="folder name", dest="i")
parser.add_option("--f",help="folder name", dest="fi")


(options, args) = parser.parse_args()
header = f"Working on {options.fi}"
header = str.encode(header)
if options.fi :
    print (header)
else:
    print ("not suffient argumets")
    print (usage)
    quit()


from manipulation_library_dev import get_image,get_colors,get_color_distance

folder_image_path = "/".join(os.getcwd().split("/")[:-1])
print ("Working directory:{}".format(folder_image_path))

target_folder = os.path.join(folder_image_path,options.fi)

## check the folder exist 
if os.path.isdir(target_folder):
    print ("this is your target folder absolute path:{}".format(target_folder))
else:
    print ( "Can not find :{}".format(target_folder))
    print ("Is this the correct path ?" )
    quit()

## read the content of the folder 
images_list = [os.path.join(target_folder,x) for x in os.listdir(target_folder) if x.endswith(".jpg") or x.endswith(".JPG")]


if len(images_list) != 2 : 
    print ("Check the folder,  there is not just 2 images in it")

## get the colors
list_of_dataframes =[]
for image in images_list:
    im = get_image(image)
    # df_temp = get_colors(im, 2, True) ## 2 because 1 is black in the segmentation
    df_temp = get_colors(im, 1, False) ## 1 because we filtered black from the image
    name = image.split("/")[-1]
    df_temp["Image_name"] = name
    # add a columns that will be boolean if the name contains the keyword coral
    df_temp["is_coral"] = df_temp["Image_name"].str.contains("coral", case=False)
    list_of_dataframes.append(df_temp)

all_colors = pd.concat (list_of_dataframes) 
name = target_folder+'/all_colors.csv'
all_colors.to_csv(path_or_buf=name)
# print (all_colors)
## calculate the distance between the colors : 
# ligth_colors_rgb = all_colors[all_colors["is_dark"] == False ]["rgb_colors"]
# ligth_colors_hex = all_colors[all_colors["is_dark"] == False ]["hex_colors"]
blank_colors_rgb = all_colors[all_colors["is_coral"] == False ]["rgb_colors"]
coral_colors_rgb = all_colors[all_colors["is_coral"] == True ]["rgb_colors"]


print (blank_colors_rgb.iloc[0])
print (coral_colors_rgb.iloc[0])
name = target_folder+'/distance_colors.txt'
# d_1 = get_color_distance(ligth_colors_rgb.iloc[0], ligth_colors_rgb.iloc[1])
d_1 = get_color_distance(blank_colors_rgb.iloc[0], coral_colors_rgb.iloc[0])

output = open(name,"w")
# output.write(f"colors used RGB:{ligth_colors_rgb.iloc[0]},{ligth_colors_rgb.iloc[1]}\n")
output.write(f"colors used RGB:{blank_colors_rgb.iloc[0]},{coral_colors_rgb.iloc[0]}\n")
output.write(f"distance:{d_1}\n")

