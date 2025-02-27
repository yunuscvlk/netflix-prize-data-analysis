import os
import zipfile

def extract(source_path, file_name):
    full_source_path = os.path.abspath(source_path)

    os.chmod(full_source_path, 0o777)

    with zipfile.ZipFile(f"{full_source_path}/{file_name}", 'r') as zip_ref:
        zip_ref.extractall(full_source_path)
        
    print(f"Extract => All files in `{file_name}` were extracted to `{source_path}`.")