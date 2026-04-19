import os
import shutil

# Paths
source_dir = "/home/rmijailpp/CoralSCOP_copy/data/04_blank_folders"
destination_dir = "/home/rmijailpp/CoralSCOP_copy/data/05_rename_blanks"

# Ensure the destination directory exists
os.makedirs(destination_dir, exist_ok=True)

# Debugging: Print the source directory contents
print(f"Looking for images in: {source_dir}")
print(f"Contents of source directory: {os.listdir(source_dir)}")

# Loop through each image in the source directory
for image_file in os.listdir(source_dir):
    # Normalize extension handling to be case-insensitive
    if image_file.lower().endswith(('.png', '.jpg', '.jpeg', '.tif', '.bmp', '.gif', '.JPG')):  
        print(f"Processing image: {image_file}")
        
        # Create a folder for each image
        folder_name = os.path.splitext(image_file)[0].replace(" ", "_")  # Replace spaces with underscores
        folder_path = os.path.join(destination_dir, folder_name)
        os.makedirs(folder_path, exist_ok=True)
        print(f"Created folder: {folder_path}")
        
        # Copy the image 25 times into the folder
        for i in range(1, 26):
            new_file_name = f"{folder_name}_{i}{os.path.splitext(image_file)[1]}"
            new_file_path = os.path.join(folder_path, new_file_name)
            shutil.copy(os.path.join(source_dir, image_file), new_file_path)
            print(f"Copied to: {new_file_path}")

print("Folders created and images copied successfully.")
