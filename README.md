# Netflix Prize Data Analysis

## Content

- [Introduction](#introduction)
- [Repository Structure](#repository-structure)
- [Requirements](#requirements)
- [Installation & Setup](#installation--setup)
- [Usage](#usage)

---

## Introduction

This project comprises comprehensive analytical studies conducted using the Netflix Prize dataset obtained from Kaggle. The project employs both Python and R languages to perform data processing, modeling, and analysis tasks. The objective is to develop applications in this field by experimenting with recommendation systems and other analytical methods on large datasets.

---

## Repository Structure

```bash
.
├── data          # Contains processed datasets used in the models.
├── data_merging  # Hosts Python modules that merge and prepare data.
├── models        # Directory for different model examples (Python and R).
├── src           # Folder containing files where the main model is implemented.
├── tests         # Directory for test and exploratory scripts.
├── datasets      # Stores raw data files.
└── dist          # Contains prepared data files for analysis.
```

---

## Requirements

- Python 3.8
- Conda (for environment management)
- Jupyter Notebook (for Python-based analysis)
- R (version 4.0 or newer)
- RStudio (for R-based analysis)

---

## Installation & Setup

### 1. Clone the Repository

- Run the following commands in your terminal:

  ```bash
  git clone https://github.com/yunuscvlk/netflix-prize-data-analysis.git
  cd netflix-prize-data-analysis
  ```

### 2. Prepare the Dataset

- Create a `datasets/` directory and set permissions:

  ```bash
  mkdir datasets
  sudo chown $USER:$USER datasets # Adjust ownership (if needed)
  chmod 755 datasets
  ```

- Download the [Netflix Prize Dataset](https://www.kaggle.com/datasets/netflix-inc/netflix-prize-data) as `archive.zip` and move it to the `datasets/` folder.

### 3. Set Up Python Environment

- Create a Conda environment and install dependencies:

  ```bash
  conda create -n netflix-prize-data python=3.8
  conda activate netflix-prize-data
  pip install -r requirements.txt
  pip install ipykernel
  python -m ipykernel install --user --name netflix-prize-data --display-name "Python (netflix-prize-data)"
  ```

---

## Usage

### 1. Preprocess Data

- Run the preprocessing script to generate analysis-ready data:

  ```bash
  python dm.py # Ensure the Conda environment is active
  ```

  This generates processed files in the `dist/` directory.

### 2. Exploratory Data Analysis (EDA)

  - Open `tests/eda.R` in **RStudio** and execute it to perform EDA.

### 3. Run Models

#### Python Models (Jupyter Notebooks)

- Launch Jupyter Notebook:
  ```bash
  jupyter notebook
  ```

- Open notebooks in `models/`:
  - ``neucf.ipynb:`` Neural Collaborative Filtering
  - ``neumf.ipynb:`` Neural Matrix Factorization

- Select the `Python (netflix-prize-data)` kernel.

#### R Models (RStudio)

- Open `models/collaborative_filtering.R` in **RStudio** and run the script.

### 4. Execute Main Model

- Open `src/collaborative_filtering.R` in **RStudio** and run the script.
