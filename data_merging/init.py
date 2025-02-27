import os

def init(source_path, dist_path):
    if not os.path.exists(source_path):
        os.makedirs(source_path)
        print(f"Initialize => `{source_path}` folder created.")

    if not os.path.exists(dist_path):
        os.makedirs(dist_path)
        print(f"Initialize => `{dist_path}` folder created.")