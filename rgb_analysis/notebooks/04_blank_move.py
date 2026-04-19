import os
import shutil

# Path to the parent directory containing both types of folders
parent_dir = "/home/rmijailpp/CoralSCOP_copy/data/05_rename_blanks"

# List all directories in the parent directory
directories = [d for d in os.listdir(parent_dir) if os.path.isdir(os.path.join(parent_dir, d))]

# Separate original and copy directories
original_dirs = {d.replace("_-_Copy", ""): d for d in directories if "_-_Copy" not in d}
copy_dirs = {d: os.path.join(parent_dir, d) for d in directories if "_-_Copy" in d}

# Iterate through original directories and copy the order.csv to corresponding copy directories
for base_name, orig_dir in original_dirs.items():
    copy_dir_name = f"{base_name}_-_Copy"
    if copy_dir_name in copy_dirs:
        # Path to the order.csv in the original directory
        order_csv_path = os.path.join(parent_dir, orig_dir, "order.csv")
        # Path to the target directory for order.csv
        target_csv_path = os.path.join(copy_dirs[copy_dir_name], "order.csv")

        if os.path.exists(order_csv_path):
            shutil.copy(order_csv_path, target_csv_path)
            print(f"Copied {order_csv_path} to {target_csv_path}")
        else:
            print(f"order.csv not found in {orig_dir}")
    else:
        print(f"No matching copy directory for {orig_dir}")

print("order.csv copying completed.")
