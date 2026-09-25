#!/bin/bash

echo "Starting Model Execution Sequence..."

# Step 1: Input Processing
echo "--- Step 1: Running Input Processing ---"
python3 "/app/scripts/input_processing.py"
status=$?
echo "Input Processing finished with exit status: $status"

if [ $status -ne 0 ]; then
    echo "Critical failure in Step 1. Halting."
    exit $status
fi

# Step 2: Hazard Model
echo "--- Step 2: Running Hazard Model (R) ---"
Rscript "/app/scripts/HazardModel2.R"
status=$?
echo "Hazard Model finished with exit status: $status"

if [ $status -ne 0 ]; then
    echo "Critical failure in Step 2. Halting."
    exit $status
fi

# Step 3: Output Processing
echo "--- Step 3: Running Output Processing ---"
python3 "/app/scripts/output_processing.py"
status=$?
echo "Output Processing finished with exit status: $status"

exit $status
