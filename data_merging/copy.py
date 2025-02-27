import os
import fnmatch
import shutil

def copy(source_path, dist_path, extension):
    for f in fnmatch.filter(os.listdir(source_path), extension):
        print(f"Copy => Target file: {f}")

        shutil.copyfile(f"{source_path}/{f}", f"{dist_path}/{f}")
        
        print(f"Copy => `{f}` copied to directory `{dist_path}`.")