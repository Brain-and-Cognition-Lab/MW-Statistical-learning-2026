### Overview
This repository contains analysis scripts for the two experiments described in our manuscript *"Mind wandering impedes statistical learning during serial but not parallel visual search"*. 
Experimental tasks were implemented in **Python**, and data analyses were conducted in **R**.

* **Experiment code:** Python 3.8
* **Analysis code:** R 4.4.2

The dataset for this project is available on **OSF** at: [https://osf.io/awsyd/overview](https://osf.io/awsyd/overview)

* **Experiment 1:** `/Exp1` folder
* **Experiment 2:** `/Exp2` folder

> **Note:** Download the datasets and place the relevant `Dataset.csv` file in the working directory before running the scripts.

---

### Analysis scripts
* `Analysis_Script_Exp1.R` → **Experiment 1**
* `Analysis_Script_Exp2.R` → **Experiment 2**

Each script:
1. Performs **data cleaning** and preprocessing.
2. Computes **learning indices** (trial-level and block-level).
3. Runs **mixed-effects models** (linear and nonlinear).
4. Generates **figures and summary tables**.

---

### How to run
* Open the **R script** corresponding to the experiment.
* Ensure the working directory contains the appropriate `Dataset.csv`.
* Install required **R packages** (refer to `library()` calls at the top of each script).
* Run the script **sequentially**.

Outputs (*figures and tables*) will be saved automatically in the `/Graphs` directory.


