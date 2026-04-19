import os
import shutil

# Path to the main directory containing the Exp# folders
main_dir = "/home/rmijailpp/CoralSCOP_copy/data/06_combine"

# Iterate over all directories in the main directory
for exp_folder in os.listdir(main_dir):
    exp_folder_path = os.path.join(main_dir, exp_folder)
    
    # Check if it is a directory
    if not os.path.isdir(exp_folder_path):
        continue

    # Define paths for corals, blanks, and the new combine folder
    corals_dir = os.path.join(exp_folder_path, "corals")
    blanks_dir = os.path.join(exp_folder_path, "blanks")
    combine_dir = os.path.join(exp_folder_path, "combine")

    # Ensure corals and blanks folders exist
    if not os.path.exists(corals_dir) or not os.path.exists(blanks_dir):
        print(f"Missing 'corals' or 'blanks' folder in {exp_folder}")
        continue

    # Create the combine folder
    os.makedirs(combine_dir, exist_ok=True)

    # Get the list of files in corals and blanks directories
    coral_files = {os.path.splitext(f)[0]: f for f in os.listdir(corals_dir) if f.lower().endswith(('.jpg', '.jpeg', '.png'))}
    blank_files = {os.path.splitext(f)[0]: f for f in os.listdir(blanks_dir) if f.lower().endswith(('.jpg', '.jpeg', '.png'))}

    # Process files present in both folders
    for common_file in coral_files.keys() & blank_files.keys():
        # Create a subfolder in the combine folder named after the common file
        subfolder_path = os.path.join(combine_dir, common_file)
        os.makedirs(subfolder_path, exist_ok=True)
        
        # Copy the corresponding coral and blank images into the subfolder
        coral_image_path = os.path.join(corals_dir, coral_files[common_file])
        blank_image_path = os.path.join(blanks_dir, blank_files[common_file])
        
        shutil.copy(coral_image_path, os.path.join(subfolder_path, f"coral_{coral_files[common_file]}"))
        shutil.copy(blank_image_path, os.path.join(subfolder_path, f"blank_{blank_files[common_file]}"))
        
        print(f"Created folder {subfolder_path} and copied images.")

print("Processing completed.")
