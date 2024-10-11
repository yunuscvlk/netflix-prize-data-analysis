import os
import fnmatch

def convert(source_path, dist_path, extension):
    for f in fnmatch.filter(os.listdir(source_path), extension):
        print(f"Convert => Target file: `{f}`")

        datas = []

        last_id = -1
        last_data = []

        with open(f"{source_path}/{f}", "r") as file:
            for l in file:
                line = l.strip()
                seperator = line.find(":")

                if seperator >= 0:
                    last_id = int(line[:seperator])
                else:
                    last_data = line.split(",")

                if not last_id == -1:
                    datas.append(last_id)
                else: 
                    datas.append(last_data)

                last_id = -1
                last_data = []

        file_name = f.split(".")[0]
        file_csv = f"{dist_path}/{file_name}.csv"

        if os.path.exists(file_csv):
            os.remove(file_csv)

        print(f"Convert => Preparing `{file_csv}` file...")

        with open(file_csv, "w") as file:
            if file_csv.find("qualifying") > 0:
                file.write("MovieID,CustomerID,Date\n")
                file.flush()
            elif file_csv.find("probe") > 0:
                file.write("MovieID,CustomerID\n")
                file.flush()
            elif file_csv.find("combined") > 0:
                file.write("MovieID,CustomerID,Rate,Date\n")
                file.flush()
            else:
                print(f"Convert => Undefined file name format: `{f}`")
                exit()

            for d in datas:
                if isinstance(d, int):
                    last_id = d
                else:
                    last_data = d

                if last_id != -1 and len(last_data) > 0:
                    current_data_str = str()

                    for ld in last_data:
                        current_data_str += ld + ","

                    current_data_str = str(last_id) +  "," + current_data_str[:-1] + "\n"

                    file.write(current_data_str)
                    file.flush()

        print(f"Convert => `{file_csv}` file created.")