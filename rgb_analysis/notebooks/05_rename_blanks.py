import os
import pandas as pd

# Path to the parent directory containing the folders
parent_dir = "/home/rmijailpp/CoralSCOP_copy/data/05_rename_blanks"

# List all directories in the parent directory
directories = [d for d in os.listdir(parent_dir) if os.path.isdir(os.path.join(parent_dir, d))]

# Process each directory
for folder in directories:
    folder_path = os.path.join(parent_dir, folder)
    order_csv_path = os.path.join(folder_path, "order.csv")
    
    # Check if order.csv exists in the folder
    if not os.path.exists(order_csv_path):
        print(f"order.csv not found in {folder}")
        continue
    
    # Read the order.csv file without headers
    order_data = pd.read_csv(order_csv_path, header=None)
    
    # Assume the first column is the old name index and the second column is the new name
    for index, row in order_data.iterrows():
        old_name_jpg = os.path.join(folder_path, f"{folder}_{row[0]}.jpg")  # Lowercase .jpg
        old_name_JPG = os.path.join(folder_path, f"{folder}_{row[0]}.JPG")  # Uppercase .JPG
        new_temp_name_jpg = os.path.join(folder_path, f"{row[0]}.jpg")      # Temporary name lowercase
        new_temp_name_JPG = os.path.join(folder_path, f"{row[0]}.JPG")      # Temporary name uppercase
        final_name_jpg = os.path.join(folder_path, f"{row[1]}.jpg")         # Final name lowercase
        final_name_JPG = os.path.join(folder_path, f"{row[1]}.JPG")         # Final name uppercase
        
        # Rename for .JPG files
        if os.path.exists(old_name_JPG):
            os.rename(old_name_JPG, new_temp_name_JPG)
        
        if os.path.exists(new_temp_name_JPG):
            os.rename(new_temp_name_JPG, final_name_JPG)
            print(f"Renamed {new_temp_name_JPG} to {final_name_JPG}")
        
        # Rename for .jpg files
        if os.path.exists(old_name_jpg):
            os.rename(old_name_jpg, new_temp_name_jpg)
        
        if os.path.exists(new_temp_name_jpg):
            os.rename(new_temp_name_jpg, final_name_jpg)
            print(f"Renamed {new_temp_name_jpg} to {final_name_jpg}")
        
        # Log if no matching files were found
        if not os.path.exists(new_temp_name_jpg) and not os.path.exists(new_temp_name_JPG):
            print(f"File {row[0]} not found during renaming process.")

print("Renaming completed.")

