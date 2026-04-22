from __future__ import annotations
import json, subprocess
from pathlib import Path
from flask import Flask, jsonify, send_file
from flask_cors import CORS

BASE_DIR    = Path(__file__).parent.resolve()
R_SCRIPT    = BASE_DIR / "analysis.R"
RESULT_JSON = BASE_DIR / "outputs" / "results.json"

app = Flask(__name__)
CORS(app)

@app.route("/")
def index():
    return send_file(str(BASE_DIR / "index.html"))

@app.route("/api/run")
def api_run():
    (BASE_DIR / "outputs").mkdir(exist_ok=True)
    try:
        proc = subprocess.run(
            ["Rscript", "--vanilla", str(R_SCRIPT)],
            capture_output=True, text=True, timeout=600, cwd=str(BASE_DIR)
        )
    except FileNotFoundError:
        return jsonify({"status":"error","error":"Rscript not found. Install R and add it to PATH."}), 500
    if proc.returncode != 0:
        return jsonify({"status":"error","error":proc.stderr[-2000:]}), 500
    try:
        return jsonify({"status":"ok","data": json.loads(RESULT_JSON.read_text())})
    except Exception as e:
        return jsonify({"status":"error","error":str(e)}), 500

@app.route("/api/data")
def api_data():
    if RESULT_JSON.exists():
        return jsonify({"status":"ok","data": json.loads(RESULT_JSON.read_text())})
    return api_run()

@app.route("/api/status")
def api_status():
    r_ok = subprocess.run(["Rscript","--version"], capture_output=True).returncode == 0
    return jsonify({"r_available": r_ok, "cache_ready": RESULT_JSON.exists(), "base_dir": str(BASE_DIR)})

if __name__ == "__main__":
    print(f"\n  Serving from: {BASE_DIR}")
    print(f"  index.html exists: {(BASE_DIR / 'index.html').exists()}")
    print(f"  analysis.R exists: {R_SCRIPT.exists()}")
    print("\n  Open → http://localhost:5000\n")
    app.run(debug=True, port=5000)