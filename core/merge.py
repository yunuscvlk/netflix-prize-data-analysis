import os
import fnmatch
import pandas as pd

def merge(dist_path, prefix, extension):
    csv_list = []

    file_name = f"{prefix}_merged"
    file_csv = f"{dist_path}/{file_name}.csv"

    for f in fnmatch.filter(os.listdir(dist_path), f"{prefix}{extension}"):
        print(f"Merge => Target file: {f}")

        csv = pd.read_csv(f"{dist_path}/{f}")
        csv_list.append(csv)

        merged_csv = pd.concat(csv_list, ignore_index=True)
        merged_csv.to_csv(file_csv, index=False)

    print(f"Merge => Successfully merged and saved as `{file_csv}`.")
