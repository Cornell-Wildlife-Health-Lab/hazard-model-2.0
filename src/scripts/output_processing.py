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
parameters_file = data_path / 'params.json'
model_metadata_log_file = data_path / "attachments" / "info.html"
attachments_json_path = data_path / "attachments.json"
logging_path = data_path / "attachments" / "execution_log.log"
model_output_path = data_path / "attachments" / "OutputHazards.csv"
weights_csv_path = data_path / "Weights.csv"
data_totals_csv_path = data_path / "DataTotals.csv"
effective_distance_csv_path = data_path / "EffectiveDistance.csv"
cwd_status_csv_path = data_path / "CWDStatus.csv"
all_subadmins_file_path = data_path / "sub_administrative_area.ndJson"
subadmin_areas_csv_path = data_path / "SubadminAreas.csv"
demographic_weight_file_path = data_path / "demographic_weight.ndJson"

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

# Load params for surveillance_point_total and other settings
try:
    with open(parameters_file, 'r') as f:
        params = json.load(f)
except Exception as e:
    logging.error(f"Failed to load params.json: {e}")
    params = {}

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
                    # Exclude SubAdminID and FullName - already present in output
                    data_totals_by_id[sa_id] = {
                        k: v for k, v in row.items()
                        if k not in ('SubAdminID', 'FullName')
                    }
        logging.info(f"Loaded DataTotals for {len(data_totals_by_id)} subadmin areas.")
    else:
        logging.warning("DataTotals.csv not found. Input risk factor values will not be included in output.")

    # Load EffectiveDistance.csv (proximity to nearest CWD-positive area)
    effective_distance_by_id = {}
    if effective_distance_csv_path.exists():
        with open(effective_distance_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                sa_id = row.get('SubAdminID')
                if sa_id:
                    effective_distance_by_id[sa_id] = row.get('EffectiveDistance')
        logging.info(f"Loaded EffectiveDistance for {len(effective_distance_by_id)} subadmin areas.")
    else:
        logging.warning("EffectiveDistance.csv not found. Proximity values will not be included in output.")

    # -------------------------------------------------------------------------
    # Build enriched records with inputs first, then model outputs
    # Column order: SubAdminID, FullName, EffectiveDistance, risk factor inputs,
    #               then all model output columns (hazard scores, elasticities)
    # -------------------------------------------------------------------------
    enriched_output_list = []
    # Determine output-only keys (excluding the metadata keys present in all rows)
    metadata_keys = ['SubAdminID', 'FullName']
    output_only_keys = [k for k in model_output_dict_list[0].keys() if k not in metadata_keys]

    for row in model_output_dict_list:
        sa_id = row.get('SubAdminID')
        enriched_row = {}
        enriched_row['SubAdminID'] = row.get('SubAdminID')
        enriched_row['FullName'] = row.get('FullName')
        enriched_row['EffectiveDistance'] = effective_distance_by_id.get(sa_id)
        if sa_id in data_totals_by_id:
            enriched_row.update(data_totals_by_id[sa_id])
        for k in output_only_keys:
            enriched_row[k] = row.get(k)
        enriched_output_list.append(enriched_row)

    # -------------------------------------------------------------------------
    # Expand to all subadmin areas
    # sub_administrative_area.ndJson is the authoritative base.Data is 
    # joined in order:
    #   1. SubAdminID, FullName (from geographic file)
    #   2. CWDStatus, prevalence fields (from CWDStatus.csv)
    #   3. EffectiveDistance, risk factor inputs, model outputs (selected areas
    #      only)
    # -------------------------------------------------------------------------

    # Load full geographic list as base
    all_subadmin_areas = []
    with open(all_subadmins_file_path, 'r', encoding='utf-8') as f:
        for line in f:
            if line.strip():
                all_subadmin_areas.append(json.loads(line))
    logging.info(f"Loaded {len(all_subadmin_areas)} total subadmin areas for full output.")

    # Load CWDStatus.csv into a lookup by SubAdminID
    cwd_status_by_id = {}
    if cwd_status_csv_path.exists():
        with open(cwd_status_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                sa_id = row.get('SubAdminID')
                if sa_id:
                    cwd_status_by_id[sa_id] = row
        logging.info(f"Loaded CWDStatus for {len(cwd_status_by_id)} subadmin areas.")
    else:
        logging.warning("CWDStatus.csv not found. CWD status and prevalence will not be included in output.")

    # Build a lookup of enriched model results by SubAdminID
    enriched_by_id = {row['SubAdminID']: row for row in enriched_output_list}

    # Determine the model output field names (excluding metadata)
    output_field_names = [k for k in enriched_output_list[0].keys() if k not in ('SubAdminID', 'FullName')]

    # CWD status field names (excluding SubAdminID and FullName - sourced from base)
    cwd_field_names = ['CWDStatus', 'SurveillanceCategory']

    full_output_list = []
    for area in all_subadmin_areas:
        sa_id = area['_id']
        row = {}
        row['SubAdminID'] = sa_id
        row['FullName'] = area.get('full_name')
        cwd_row = cwd_status_by_id.get(sa_id, {})
        for field in cwd_field_names:
            val = cwd_row.get(field)
            row[field] = val if val != '' else None
        if sa_id in enriched_by_id:
            for field in output_field_names:
                row[field] = enriched_by_id[sa_id].get(field)
        else:
            for field in output_field_names:
                row[field] = None
            # Areas with CWD detected have an effective distance of zero -
            # they are co-located with a positive area by definition.
            if row.get('CWDStatus') == 'Detected':
                row['EffectiveDistance'] = 0
        full_output_list.append(row)

    logging.info(f"Full output contains {len(full_output_list)} subadmin areas ({len(enriched_by_id)} with model results).")

    # -------------------------------------------------------------------------
    # Demographic weight and surveillance point allocation
    # Loads demographic_weight.ndJson and computes a total population weight
    # per subadmin area by:
    #   - total population: sum values directly across records
    #   - deer density: multiply by subadmin area (kmÂ²) then sum
    #   - total harvest: sum values directly across records
    # Null values are treated as zero.
    # Surveillance points are then allocated proportionally to TotalHazard
    # weighted by the demographic weight, scaled to surveillance_point_total.
    # -------------------------------------------------------------------------

    # Load subadmin areas (kmÂ²) for density â†’ population conversion
    subadmin_area_km2 = {}
    if subadmin_areas_csv_path.exists():
        with open(subadmin_areas_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                sa_id = row.get('SubAdminID')
                if sa_id:
                    subadmin_area_km2[sa_id] = float(row.get('AreaKm2', 0) or 0)
        logging.info(f"Loaded area data for {len(subadmin_area_km2)} subadmin areas.")
    else:
        logging.warning("SubadminAreas.csv not found. Density conversion will not be possible.")

    # Load and aggregate demographic weights
    demographic_weight_by_id = {}  # {subadmin_id: total_population_weight}
    if demographic_weight_file_path.exists():
        metric = None
        with open(demographic_weight_file_path, 'r', encoding='utf-8') as f:
            for line in f:
                if not line.strip():
                    continue
                record = json.loads(line)
                metric = record.get('metric')  # same across all records
                for sa_id, value in record.get('data', {}).items():
                    value = float(value) if value is not None else 0.0
                    if metric == 'deer density':
                        area_km2 = subadmin_area_km2.get(sa_id, 0)
                        value = value * area_km2
                    # total population and total harvest: use directly
                    demographic_weight_by_id[sa_id] = demographic_weight_by_id.get(sa_id, 0.0) + value
        logging.info(f"Demographic weights aggregated for {len(demographic_weight_by_id)} subadmin areas (metric: {metric}).")
        model_log_html(f"Demographic weights loaded (metric: {metric}).", "p")
    else:
        logging.info("demographic_weight.ndJson not found. Falling back to TotalHazard-only allocation for surveillance points.")
        model_log_html("demographic_weight.ndJson not found. Allocating surveillance points using TotalHazard only.", "p")

    # Add demographic weight to each row
    for row in full_output_list:
        sa_id = row.get('SubAdminID')
        row['DemographicWeight'] = demographic_weight_by_id.get(sa_id)

    # -------------------------------------------------------------------------
    # Surveillance point allocation
    # Implements the constrained iterative demography-weighting algorithm
    # from DemographyWeighting.R.
    #
    # Algorithm:
    #   1. Standardize TotalHazard and DemographicWeight (each sums to 1).
    #   2. Integrate: units with zero DemographicWeight score 0 (no deer ->
    #      hazard is irrelevant); others get hazard_std and demography_std.
    #   3. Standardize the integrated vector.
    #   4. Cap the budget at the jurisdiction-wide maximum (sum demog * 1.5).
    #   5. Initial allocation: floor(budget * integrated_std) per unit.
    #   6. Iteratively cap over-capacity units and redistribute the surplus
    #      to remaining units proportional to their TotalHazard.
    #
    # Fallbacks:
    #   - No surveillance_point_total  -> all areas receive None.
    #   - No demographic weights       -> simple hazard-proportional allocation
    #                                     with no capacity cap.
    # -------------------------------------------------------------------------
    surveillance_point_total = params.get('surveillance_planning', {}).get('surveillance_point_total')

    if not surveillance_point_total:
        for row in full_output_list:
            row['totalQuota'] = None
        logging.info("No surveillance_point_total provided. Surveillance Points (totalQuota) set to 0 for all areas.")

    elif not demographic_weight_by_id:
        # Fallback: no demographic weights -- allocate by TotalHazard only.
        hazard_sum = sum(
            float(row.get('TotalHazard') or 0)
            for row in full_output_list
            if row.get('TotalHazard') is not None
        )
        for row in full_output_list:
            hazard = row.get('TotalHazard')
            if hazard is not None and hazard_sum > 0:
                row['totalQuota'] = int(float(hazard) / hazard_sum * surveillance_point_total)
            else:
                row['totalQuota'] = 0
        logging.info(f"Surveillance points allocated using TotalHazard only (no demographic weights; total: {surveillance_point_total}).")

    else:
        # Full constrained demography-weighted allocation.

        # Eligible rows: must be in the 'early detection' 
        # SurveillanceCategory and have both TotalHazard and DemographicWeight.
        # Areas outside early detection receive zero surveillance points.
        eligible = [
            row for row in full_output_list
            if row.get('SurveillanceCategory') == 'early detection'
            and row.get('TotalHazard') is not None
            and row.get('DemographicWeight') is not None
        ]
        logging.info(
            f"{len(eligible)} subadmin areas eligible for surveillance point allocation "
            f"(SurveillanceCategory == 'Early' with TotalHazard and DemographicWeight)."
        )

        sa_ids  = [row['SubAdminID']               for row in eligible]
        hazards = [float(row['TotalHazard'])        for row in eligible]
        demog   = [float(row['DemographicWeight'])  for row in eligible]
        n       = len(sa_ids)

        hazard_total = sum(hazards)
        demog_total  = sum(demog)

        if hazard_total == 0 or demog_total == 0:
            for row in full_output_list:
                row['totalQuota'] = 0
            logging.warning(
                "Surveillance point allocation skipped: sum of TotalHazard or "
                "DemographicWeight is zero across all eligible areas."
            )
        else:
            # Step 1 -- Standardize
            hazard_std = [h / hazard_total for h in hazards]
            demog_std  = [d / demog_total  for d in demog]

            # Step 2 -- Integrate (units with zero demography are excluded)
            integrated = [
                (hazard_std[i] + demog_std[i]) if demog_std[i] > 0 else 0.0
                for i in range(n)
            ]

            # Step 3 -- Standardize integrated vector
            integrated_total = sum(integrated)
            if integrated_total == 0:
                for row in full_output_list:
                    row['totalQuota'] = 0
                logging.warning("Surveillance point allocation skipped: integrated vector sums to zero.")
            else:
                integrated_std = [v / integrated_total for v in integrated]

                # Per-unit capacity ceiling: each demographic unit yields ~1.5 points
                max_possible = [d * 1.5 for d in demog]

                # Step 4 -- Cap the budget at the jurisdiction-wide maximum
                max_jurisdiction_wide = sum(max_possible)
                point_budget = min(float(surveillance_point_total), max_jurisdiction_wide)

                # Step 5 -- Initial allocation
                ideal = [int(point_budget * s) for s in integrated_std]

                # Step 6 -- Iterative cap-and-redistribute
                # Each pass checks whether any unit exceeds its ceiling.
                # Surplus is redistributed to units with remaining capacity,
                # weighted by hazard. Stops when no unit is over-capacity.
                for _pass in range(n):
                    points_checked = [0.0] * n
                    difference     = [0.0] * n

                    for i in range(n):
                        if max_possible[i] <= ideal[i]:
                            points_checked[i] = max_possible[i]
                            difference[i]     = ideal[i] - max_possible[i]
                        else:
                            points_checked[i] = float(ideal[i])

                    surplus = sum(difference)
                    if surplus <= 0:
                        # No unit is over capacity -- allocation is already feasible
                        # Leave ideal unchanged, matching R's behavior of skipping
                        # the reallocation block entirely when sum(Difference) == 0.
                        break

                    # Redistribute to units that still have capacity.
                    wiggle_room = [max(0.0, max_possible[i] - points_checked[i]) for i in range(n)]
                    hazard_redist = [
                        hazard_std[i] if wiggle_room[i] > 0 else 0.0
                        for i in range(n)
                    ]
                    hazard_redist_total = sum(hazard_redist)

                    if hazard_redist_total == 0:
                        ideal = [int(p) for p in points_checked]
                        break

                    hazard_redist_std = [h / hazard_redist_total for h in hazard_redist]
                    reallocation = [int(surplus * s) for s in hazard_redist_std]
                    ideal = [int(points_checked[i]) + reallocation[i] for i in range(n)]

                points_by_id = dict(zip(sa_ids, ideal))
                for row in full_output_list:
                    row['totalQuota'] = points_by_id.get(row.get('SubAdminID'), 0)

                logging.info(
                    f"Surveillance points allocated using constrained demography-weighted algorithm "
                    f"(requested: {surveillance_point_total}, "
                    f"jurisdiction max: {round(max_jurisdiction_wide)}, "
                    f"allocated: {sum(ideal)})."
                )
    # -------------------------------------------------------------------------
    # Identify numeric keys and perform type conversion / rounding
    # -------------------------------------------------------------------------
    protected_keys = {'FullName', 'SubAdminID', 'CWDStatus', 'SurveillanceCategory'}
    float_keys = [k for k in full_output_list[0].keys() if k not in protected_keys]

    for each_row in full_output_list:
        for each_key in float_keys:
            val = safely_convert_float(each_row[each_key])
            each_row[each_key] = round(val, 4) if val is not None else None

    # Round totalQuota to nearest integer for final output
    for row in full_output_list:
        if row.get('totalQuota') is not None:
            row['totalQuota'] = round(row['totalQuota'])

    # -------------------------------------------------------------------------
    # Write outputs
    # -------------------------------------------------------------------------

    # Write to output.json for platform integration
    model_output_json_path = data_path / "attachments" / "output.json"
    with open(model_output_json_path, 'w', newline='') as f:
        json.dump(full_output_list, f, indent=3)
    attachment = {"filename": "output.json", "content_type": "application/json", "role": "primary"}
    add_item_to_json_file_list(attachments_json_path, attachment)

    # Write to OutputHazards.csv
    output_csv_path = data_path / "attachments" / "OutputHazards.csv"
    with open(output_csv_path, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=full_output_list[0].keys(), restval='NA')
        writer.writeheader()
        writer.writerows(full_output_list)
    attachment = {"filename": "OutputHazards.csv", "content_type": "text/csv", "role": "downloadable"}
    add_item_to_json_file_list(attachments_json_path, attachment)

    logging.info("Post-processing complete. output.json and OutputHazards.csv created.")
    model_log_html("Model exports successfully created.", "p")

except Exception as e:
    logging.exception("An error occurred during output processing.")
    model_log_html("An internal error occurred during file export.", "p")
    sys.exit(1)
