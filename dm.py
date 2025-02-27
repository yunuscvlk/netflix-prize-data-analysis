from data_merging.init import *
from data_merging.extract import *
from data_merging.convert import *
from data_merging.copy import *
from data_merging.merge import *

def main():
    source_path = "./datasets"
    dist_path = "./dist"
    data_name = "archive.zip"

    init(source_path, dist_path)

    if not os.path.exists(f"{source_path}/{data_name}"):
        print(f"Main => {data_name} file is required.")
        exit()

    extract(source_path, data_name)
    convert(source_path, dist_path, "*.txt")
    copy(source_path, dist_path, "*.csv")
    merge(dist_path, "combined_data", "*.csv")

if __name__ == "__main__":
    main()

