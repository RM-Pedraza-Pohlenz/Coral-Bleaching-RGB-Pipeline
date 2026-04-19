import sys , os

# from glob import glob

import pandas as pd

import shutil

 

def copy_files(path_to_files):

    # files = glob(f'{path_to_files}/*.jpg')

    # check if the file exists

    if os.path.exists(f'{path_to_files}/order.csv') == False:

        print(f'The file {path_to_files}/order.csv  does not exist')

        exit()

        return

    else:

        df_temp = pd.read_csv(f'{path_to_files}/order.csv', header=None)

        df_temp.columns = ['old_name', 'new_name']

        for index, row in df_temp.iterrows():

            try :

                if os.path.exists(f'{path_to_files}/{row["old_name"]}.jpg') == False:

                    print(f'The file {path_to_files}/{row["old_name"]}.jpg does not exist')

                    continue

                # print (f'{path_to_files}/{row["old_name"]}.jpg', f'{path_to_files}/{row["old_name"]}.change.jpg')

                # print (f'{path_to_files}/{row["old_name"]}.change.jpg', f'{path_to_files}/{row["new_name"]}')

                # create a copy of the file with a new name

                else :

                    shutil.copy(f'{path_to_files}/{row["old_name"]}.jpg', f'{path_to_files}/{row["old_name"]}.change.jpg')

                    os.rename(f'{path_to_files}/{row["old_name"]}.change.jpg', f'{path_to_files}/{row["new_name"]}.jpg')

            except Exception as e:

                print(f'Error: {e}')

                continue

 

path = sys.argv[1]

copy_files(path)