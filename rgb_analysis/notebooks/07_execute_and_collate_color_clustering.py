import os
from optparse import OptionParser
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

#Parse the options
usage = "USAGE: python execute_and_collate_color_clustering.py --f folder_name --o output_name\n"
parser = OptionParser(usage=usage)

#options
# parser.add_option("--initial",help="folder name", dest="i")
parser.add_option("--f",help="folder name", dest="f")
parser.add_option("--o",help="output name", dest="o")

(options, args) = parser.parse_args()
header = f"Working on {options.f}"
header = str.encode(header)
if options.f and options.o :
    print (header)
else:
    print ("not suffient argumets")
    print (usage)
    quit()


def read_the_distance_file(subfolder):
    for line in open(f"{subfolder}/distance_colors.txt"):
        if line.startswith("distance"):
            return float(line.split(":")[-1])
        
def read_the_distance_file_2(subfolder):
    for line in open(f"{subfolder}/distance_colors.txt"):
        if line.startswith("colors"):
            return line.split(":")[-1].split(",")[0],  line.split(":")[-1].split(",")[1].strip()
            # final_file_list.append((tag_name,float(line.split(":")[-1])))

folder_image_path = "/".join(os.getcwd().split("/")[:-1])
print ("Root directory:{}".format(folder_image_path))
absolute_path_to_folder = os.path.join(folder_image_path,f"{options.f}")
print ("Working directory:{}".format(absolute_path_to_folder))
subfolders = [os.path.join(absolute_path_to_folder,x) for x in os.listdir(absolute_path_to_folder) ]

for subfolder in subfolders:
    if os.path.isdir(subfolder):
        # print (subfolder)
        os.system(f"python color_clustering_and_distance.py --f {subfolder}")

final_file_list =[]
for subfolder in subfolders:
    if os.path.isdir(subfolder):
        if os.path.isfile(f"{subfolder}/distance_colors.txt"):
            tag_name = subfolder.split("/")[-1]
            distance = read_the_distance_file(subfolder)
            rgb1 , rgb2 = read_the_distance_file_2(subfolder)
            final_file_list.append((tag_name,distance, rgb1 , rgb2))
        else:
            print (f"No file found in {subfolder}/distance_colors.txt")

    else :
        print (f"No folder {subfolder} found")

        


df = pd.DataFrame(final_file_list)
df.columns = ["Tag","Distance","RGB_1","RGB_2"]
print (f"The results are:{folder_image_path}/output/")
df.to_csv(path_or_buf=f"{folder_image_path}/output/{options.o}.csv",index=False)
