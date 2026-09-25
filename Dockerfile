FROM rocker/geospatial:4.6.0

# 1. Install ONLY the Python package managers & virtual env tool
RUN apt-get update && apt-get install -y \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

# 2. Set up the Python Virtual Environment sandbox
ENV VIRTUAL_ENV=/opt/venv
RUN python3 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

# 3. Upgrade pip inside the sandbox
RUN pip install --no-cache-dir --upgrade pip

# 4. Setup your application folders
RUN mkdir /app
COPY src /app
WORKDIR /app

# 5. Install python packages 
# (Remember to loosen the version constraints to ~= in requirements.txt if needed!)
RUN pip install --no-cache-dir -r /app/software/requirements.txt

# 6. Ensure the entrypoint script is executable
RUN chmod +x /app/scripts/run_scripts.sh

CMD ["./scripts/run_scripts.sh"]