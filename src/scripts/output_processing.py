'''
Script Name: Hazard Model 2 Output Processing
Author: Nicholas Hollingshead, Cornell University
Description: Converts output files from the Hazard Model 2
             to file formats appropriate for the CWD Data Warehouse.
Inputs: 
  OutputHazards.csv
Outputs: 
  output.json
  attachments.json
  info.html
  execution_log.log 

Date Created: 2024-08-19
Date Modified: 2024-08-20
Version: 1.0
'''

from pathlib import Path
import json
import csv
import os
import sys
import logging

data_path = Path('/data')
model_metadata_log_file = data_path / "attachments" / "info.html"
attachments_json_path = data_path / "attachments.json"
logging_path = data_path / "attachments" / "execution_log.log"
model_output_path = data_path / "attachments" / "OutputHazards.csv"
weights_csv_path = data_path / "Weights.csv"
data_totals_csv_path = data_path / "DataTotals.csv"
adjusted_distance_csv_path = data_path / "AdjustedDistance.csv"

###################
# Functions

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
    """
    Wraps text in an HTML element and appends it to the user-facing log.

    Args:
        line (str): Content to log.
        html_element (str): HTML tag name (e.g., "p", "h4").
        filename (Path): Destination file path.
    """
    with open(filename, 'a') as f:
        f.write(f"<{html_element}>{line}</{html_element}>" + '\n') 

def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2) 

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise

def safely_convert_float(val):
    """Attempts to convert a value to float; returns None if it fails."""
    try:
        return float(val)
    except (ValueError, TypeError):
        # Handles empty strings, 'N/A', or unexpected None types gracefully
        return None
      
################
# LOGGING CONFIG

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'a', # a is append, w is overwrite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

###############################################################################
# CONVERT MODEL OUTPUTS
###############################################################################

logging.info("Starting model output post-processing...")
model_log_html("Model Exports", "h3")

# Verify that the model output file exists before proceeding
if not model_output_path.exists():
    logging.error(f"Hazard model output file not found: {model_output_path}")
    model_log_html("Hazard model output was not found. Post-processing aborted.", "p")
    sys.exit(1)

try:
    model_output_dict_list = []
    with open(model_output_path, 'r') as f:
        csv_rdr = csv.DictReader(f)
        model_output_dict_list = list(csv_rdr)

    if not model_output_dict_list:
        logging.warning("Hazard model output file is empty.")
        model_log_html("Model output contains no results to export.", "p")
        sys.exit(0)

    # Identify inactive risk factors from Weights.csv to filter output fields
    inactive_factors = []
    if weights_csv_path.exists():
        with open(weights_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                if row.get('UserSelection') == '0':
                    inactive_factors.append(row.get('RiskFactor'))

    # Map inactive factors to column names (elasticity 'e_' and weight elasticity 'ew_')
    cols_to_exclude = []
    for factor in inactive_factors:
        cols_to_exclude.extend([f"e_{factor}", f"ew_{factor}"])

    # Set excluded columns to None in the data records
    for row in model_output_dict_list:
        for col in cols_to_exclude:
            if col in row:
                row[col] = None

    # -------------------------------------------------------------------------
    # Load input parameter files for joining to output records
    # -------------------------------------------------------------------------

    # Load DataTotals.csv (risk factor input values per subadmin area)
    data_totals_by_id = {}
    if data_totals_csv_path.exists():
        with open(data_totals_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                sa_id = row.get('SubAdminID')
                if sa_id:
                    # Exclude SubAdminID and FullName — already present in output
                    data_totals_by_id[sa_id] = {
                        k: v for k, v in row.items()
                        if k not in ('SubAdminID', 'FullName')
                    }
        logging.info(f"Loaded DataTotals for {len(data_totals_by_id)} subadmin areas.")
    else:
        logging.warning("DataTotals.csv not found. Input risk factor values will not be included in output.")

    # Load AdjustedDistance.csv (proximity to nearest CWD-positive area)
    adjusted_distance_by_id = {}
    if adjusted_distance_csv_path.exists():
        with open(adjusted_distance_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                sa_id = row.get('SubAdminID')
                if sa_id:
                    adjusted_distance_by_id[sa_id] = row.get('AdjustedDistance')
        logging.info(f"Loaded AdjustedDistance for {len(adjusted_distance_by_id)} subadmin areas.")
    else:
        logging.warning("AdjustedDistance.csv not found. Proximity values will not be included in output.")

    # -------------------------------------------------------------------------
    # Build enriched records with inputs first, then model outputs
    # Column order: SubAdminID, FullName, AdjustedDistance, risk factor inputs,
    #               then all model output columns (hazard scores, elasticities)
    # -------------------------------------------------------------------------
    enriched_output_list = []
    # Determine output-only keys (excluding the metadata keys present in all rows)
    metadata_keys = ['SubAdminID', 'FullName']
    output_only_keys = [k for k in model_output_dict_list[0].keys() if k not in metadata_keys]

    for row in model_output_dict_list:
        sa_id = row.get('SubAdminID')
        enriched_row = {}
        # 1. Metadata
        enriched_row['SubAdminID'] = row.get('SubAdminID')
        enriched_row['FullName'] = row.get('FullName')
        # 2. Proximity input
        enriched_row['AdjustedDistance'] = adjusted_distance_by_id.get(sa_id)
        # 3. Risk factor inputs from DataTotals
        if sa_id in data_totals_by_id:
            enriched_row.update(data_totals_by_id[sa_id])
        # 4. Model output columns
        for k in output_only_keys:
            enriched_row[k] = row.get(k)
        enriched_output_list.append(enriched_row)

    # -------------------------------------------------------------------------
    # Identify numeric keys and perform type conversion / rounding
    # -------------------------------------------------------------------------
    protected_keys = {'FullName', 'SubAdminID'}
    float_keys = [k for k in enriched_output_list[0].keys() if k not in protected_keys]

    for each_row in enriched_output_list:
        for each_key in float_keys:
            val = safely_convert_float(each_row[each_key])
            each_row[each_key] = round(val, 4) if val is not None else None

    # -------------------------------------------------------------------------
    # Write outputs
    # -------------------------------------------------------------------------

    # Write to output.json for platform integration
    model_output_json_path = data_path / "attachments" / "output.json"
    with open(model_output_json_path, 'w', newline='') as f:
        json.dump(enriched_output_list, f, indent=3)
    attachment = {"filename": "output.json", "content_type": "application/json", "role": "primary"}
    add_item_to_json_file_list(attachments_json_path, attachment)

    # Write to OutputHazards.csv
    output_csv_path = data_path / "attachments" / "OutputHazards.csv"
    with open(output_csv_path, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=enriched_output_list[0].keys(), restval='NA')
        writer.writeheader()
        writer.writerows(enriched_output_list)
    attachment = {"filename": "OutputHazards.csv", "content_type": "text/csv", "role": "downloadable"}
    add_item_to_json_file_list(attachments_json_path, attachment)

    logging.info("Post-processing complete. output.json and OutputHazards.csv created.")
    model_log_html("Model exports successfully created.", "p")

except Exception as e:
    logging.exception("An error occurred during output processing.")
    model_log_html("An internal error occurred during file export.", "p")
    sys.exit(1)
