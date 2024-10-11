from core.init import *
from core.extract import *
from core.convert import *
from core.copy import *
from core.merge import *

def main():
    source_path = "./datasets1"
    dist_path = "./dist1"
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

