#!/usr/bin/env python3
"""
Real Web Dashboard for WolfCog
Removes all mock features and implements actual system monitoring
"""

import os
import sys
import time
import json
import threading
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional

from flask import Flask, render_template, jsonify, request
from flask_socketio import SocketIO, emit

try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False


class RealWolfCogDashboard:
    """Real web dashboard with actual system monitoring"""
    
    def __init__(self, port=8080):
        self.app = Flask(__name__)
        self.app.config['SECRET_KEY'] = 'wolfcog-real-dashboard'
        self.socketio = SocketIO(self.app, cors_allowed_origins="*")
        
        self.port = port
        self.running = False
        
        # Real system state (no mock features)
        self.system_state = {
            "timestamp": 0,
            "performance": {
                "cpu_usage": 0.0,
                "memory_usage": 0.0,
                "disk_usage": 0.0,
                "task_throughput": 0.0
            },
            "components": {},
            "atomspace": {
                "size": 0,
                "available": False
            },
            "symbolic_spaces": {
                "u": {"files": 0, "size": 0},
                "e": {"files": 0, "size": 0},
                "s": {"files": 0, "size": 0}
            },
            "task_processing": {
                "pending": 0,
                "processed": 0,
                "errors": 0
            }
        }
        
        # Real metrics history
        self.metrics_history = {
            "timestamps": [],
            "cpu_usage": [],
            "memory_usage": [],
            "task_throughput": []
        }
        
        self.collection_interval = 5
        self._setup_routes()
        self._setup_websocket_handlers()
        
    def _setup_routes(self):
        """Setup Flask routes for real dashboard"""
        
        @self.app.route('/')
        def dashboard():
            """Main dashboard page"""
            return self._generate_dashboard_html()
            
        @self.app.route('/api/system/status')
        def system_status():
            """Get real system status"""
            return jsonify(self.system_state)
            
        @self.app.route('/api/metrics/history')
        def metrics_history():
            """Get real metrics history"""
            return jsonify(self.metrics_history)
            
        @self.app.route('/api/tasks/submit', methods=['POST'])
        def submit_task():
            """Submit a real task"""
            try:
                task_data = request.json
                task_id = self._submit_real_task(task_data)
                return jsonify({"status": "success", "task_id": task_id})
            except Exception as e:
                return jsonify({"status": "error", "message": str(e)}), 500
    
    def _setup_websocket_handlers(self):
        """Setup WebSocket handlers"""
        
        @self.socketio.on('connect')
        def handle_connect():
            """Handle client connection"""
            print("📱 Real dashboard client connected")
            emit('system_state', self.system_state)
            
        @self.socketio.on('disconnect')
        def handle_disconnect():
            """Handle client disconnection"""
            print("📱 Real dashboard client disconnected")
            
        @self.socketio.on('request_update')
        def handle_update_request():
            """Handle real-time update request"""
            emit('system_state', self.system_state)
            emit('metrics_update', self.metrics_history)
    
    def start(self):
        """Start the real dashboard"""
        print("🚀 Starting Real WolfCog Dashboard...")
        print("🎯 Focus: Actual system monitoring, no mock features")
        print("❌ Removed: Amazing metrics, transcendence, emergence detection")
        
        self.running = True
        
        # Start real data collection
        collector_thread = threading.Thread(target=self._data_collector)
        collector_thread.daemon = True
        collector_thread.start()
        
        # Start Flask app
        print(f"✅ Real dashboard running on http://localhost:{self.port}")
        self.socketio.run(self.app, host='0.0.0.0', port=self.port, debug=False)
    
    def _data_collector(self):
        """Collect real system data"""
        while self.running:
            try:
                # Collect real performance metrics
                self._collect_real_metrics()
                
                # Collect WolfCog specific data
                self._collect_wolfcog_data()
                
                # Emit updates to clients
                self.socketio.emit('system_state', self.system_state)
                self.socketio.emit('metrics_update', self.metrics_history)
                
                time.sleep(self.collection_interval)
                
            except Exception as e:
                print(f"❌ Data collection error: {e}")
                time.sleep(self.collection_interval)
    
    def _collect_real_metrics(self):
        """Collect real system performance metrics"""
        current_time = datetime.now()
        
        if PSUTIL_AVAILABLE:
            # Real CPU and memory usage
            cpu_percent = psutil.cpu_percent(interval=1)
            memory = psutil.virtual_memory()
            disk = psutil.disk_usage('/')
            
            self.system_state["performance"].update({
                "cpu_usage": cpu_percent,
                "memory_usage": memory.percent,
                "disk_usage": (disk.used / disk.total) * 100
            })
            
            # Update history
            self.metrics_history["timestamps"].append(current_time.strftime("%H:%M:%S"))
            self.metrics_history["cpu_usage"].append(cpu_percent)
            self.metrics_history["memory_usage"].append(memory.percent)
            
        else:
            # Basic fallback metrics
            self.system_state["performance"].update({
                "cpu_usage": 0.0,
                "memory_usage": 0.0,
                "disk_usage": 0.0
            })
        
        # Maintain history size
        max_points = 60  # Last 5 minutes at 5-second intervals
        for key in self.metrics_history:
            if len(self.metrics_history[key]) > max_points:
                self.metrics_history[key] = self.metrics_history[key][-max_points:]
        
        self.system_state["timestamp"] = time.time()
    
    def _collect_wolfcog_data(self):
        """Collect WolfCog specific data"""
        # Check task processing
        task_dir = Path("/tmp/ecron_tasks")
        if task_dir.exists():
            pending = len(list(task_dir.glob("*.json")))
            processed = len(list(task_dir.glob("*.processed")))
            
            errors_dir = Path("/tmp/ecron_errors")
            errors = len(list(errors_dir.glob("*.json"))) if errors_dir.exists() else 0
            
            self.system_state["task_processing"].update({
                "pending": pending,
                "processed": processed,
                "errors": errors
            })
        
        # Check symbolic spaces
        for space in ['u', 'e', 's']:
            space_path = Path(f"spaces/{space}")
            if space_path.exists():
                files = list(space_path.glob("*"))
                total_size = sum(f.stat().st_size for f in files if f.is_file())
                
                self.system_state["symbolic_spaces"][space] = {
                    "files": len(files),
                    "size": total_size
                }
        
        # Check component health
        if PSUTIL_AVAILABLE:
            wolfcog_processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
                try:
                    cmdline = ' '.join(proc.info['cmdline']) if proc.info['cmdline'] else ''
                    if any(keyword in cmdline.lower() for keyword in ['wolfcog', 'agent', 'daemon']):
                        wolfcog_processes.append({
                            "pid": proc.info['pid'],
                            "name": proc.info['name'],
                            "cmdline": cmdline
                        })
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            
            self.system_state["components"] = {
                "total_processes": len(wolfcog_processes),
                "processes": wolfcog_processes
            }
    
    def _submit_real_task(self, task_data: Dict) -> str:
        """Submit a real task to the system"""
        task_id = f"task_{int(time.time())}"
        
        # Validate task data
        if not task_data.get('action') or not task_data.get('expression'):
            raise ValueError("Task must have 'action' and 'expression' fields")
        
        # Create task file
        task_dir = Path("/tmp/ecron_tasks")
        task_dir.mkdir(exist_ok=True)
        
        task_file_data = {
            "id": task_id,
            "action": task_data['action'],
            "expression": task_data['expression'],
            "submitted_at": datetime.now().isoformat(),
            "submitted_via": "real_dashboard"
        }
        
        task_file = task_dir / f"{task_id}.json"
        with open(task_file, 'w') as f:
            json.dump(task_file_data, f, indent=2)
        
        return task_id
    
    def _generate_dashboard_html(self) -> str:
        """Generate real dashboard HTML without mock features"""
        return '''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>WolfCog Real Dashboard</title>
    <script src="https://cdn.socket.io/4.0.0/socket.io.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 20px;
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            min-height: 100vh;
        }
        
        .dashboard-header {
            text-align: center;
            margin-bottom: 30px;
            padding: 20px;
            background: rgba(255, 255, 255, 0.1);
            border-radius: 15px;
            backdrop-filter: blur(10px);
        }
        
        .dashboard-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        
        .card {
            background: rgba(255, 255, 255, 0.15);
            border-radius: 15px;
            padding: 20px;
            backdrop-filter: blur(10px);
            border: 1px solid rgba(255, 255, 255, 0.2);
        }
        
        .card h3 {
            margin-top: 0;
            color: #fff;
            border-bottom: 2px solid rgba(255, 255, 255, 0.3);
            padding-bottom: 10px;
        }
        
        .metric {
            display: flex;
            justify-content: space-between;
            padding: 8px 0;
            border-bottom: 1px solid rgba(255, 255, 255, 0.1);
        }
        
        .metric:last-child {
            border-bottom: none;
        }
        
        .metric-label {
            font-weight: 500;
        }
        
        .metric-value {
            font-weight: bold;
            color: #00ff88;
        }
        
        .chart-container {
            position: relative;
            height: 300px;
            margin-top: 20px;
        }
        
        .connection-status {
            position: fixed;
            top: 20px;
            right: 20px;
            padding: 10px 15px;
            border-radius: 25px;
            background: rgba(0, 0, 0, 0.7);
            font-size: 14px;
        }
        
        .status-indicator {
            display: inline-block;
            width: 12px;
            height: 12px;
            border-radius: 50%;
            margin-right: 8px;
        }
        
        .status-healthy { background-color: #00ff88; }
        .status-warning { background-color: #ffaa00; }
        .status-error { background-color: #ff4444; }
        
        .task-form {
            margin-top: 20px;
            padding: 15px;
            background: rgba(0, 0, 0, 0.3);
            border-radius: 10px;
        }
        
        .task-form input, .task-form select, .task-form textarea {
            width: 100%;
            padding: 8px;
            margin: 5px 0;
            border: none;
            border-radius: 5px;
            background: rgba(255, 255, 255, 0.9);
            color: #333;
        }
        
        .btn {
            padding: 10px 20px;
            border: none;
            border-radius: 8px;
            background: rgba(0, 255, 136, 0.8);
            color: white;
            cursor: pointer;
            font-size: 14px;
            transition: all 0.3s;
        }
        
        .btn:hover {
            background: rgba(0, 255, 136, 1);
            transform: translateY(-2px);
        }
    </style>
</head>
<body>
    <div class="connection-status" id="connectionStatus">
        <span class="status-indicator status-healthy"></span>
        Connected
    </div>
    
    <div class="dashboard-header">
        <h1>🔧 WolfCog Real System Dashboard</h1>
        <p>Actual system monitoring and control - No mock features</p>
        <div id="systemOverview">
            <span id="systemStatus">🔄 Loading...</span>
        </div>
    </div>
    
    <div class="dashboard-grid">
        <!-- Real System Metrics -->
        <div class="card">
            <h3>📊 System Performance</h3>
            <div id="systemMetrics">
                <div class="metric">
                    <span class="metric-label">CPU Usage</span>
                    <span class="metric-value" id="cpuUsage">--%</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Memory Usage</span>
                    <span class="metric-value" id="memoryUsage">--%</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Disk Usage</span>
                    <span class="metric-value" id="diskUsage">--%</span>
                </div>
            </div>
            <div class="chart-container">
                <canvas id="performanceChart"></canvas>
            </div>
        </div>
        
        <!-- Task Processing -->
        <div class="card">
            <h3>⚙️  Task Processing</h3>
            <div id="taskMetrics">
                <div class="metric">
                    <span class="metric-label">Pending Tasks</span>
                    <span class="metric-value" id="pendingTasks">0</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Processed Tasks</span>
                    <span class="metric-value" id="processedTasks">0</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Error Count</span>
                    <span class="metric-value" id="errorCount">0</span>
                </div>
            </div>
            <div class="task-form">
                <h4>Submit New Task</h4>
                <select id="taskAction">
                    <option value="evaluate">Evaluate</option>
                    <option value="analyze">Analyze</option>
                    <option value="store">Store</option>
                    <option value="query">Query</option>
                </select>
                <textarea id="taskExpression" placeholder="Enter symbolic expression..." rows="3"></textarea>
                <button class="btn" onclick="submitTask()">Submit Task</button>
            </div>
        </div>
        
        <!-- Symbolic Spaces -->
        <div class="card">
            <h3>🌌 Symbolic Spaces</h3>
            <div id="spacesMetrics">
                <div class="metric">
                    <span class="metric-label">U-Space Files</span>
                    <span class="metric-value" id="uSpaceFiles">0</span>
                </div>
                <div class="metric">
                    <span class="metric-label">E-Space Files</span>
                    <span class="metric-value" id="eSpaceFiles">0</span>
                </div>
                <div class="metric">
                    <span class="metric-label">S-Space Files</span>
                    <span class="metric-value" id="sSpaceFiles">0</span>
                </div>
            </div>
        </div>
        
        <!-- AtomSpace Status -->
        <div class="card">
            <h3>🧠 AtomSpace Status</h3>
            <div id="atomspaceMetrics">
                <div class="metric">
                    <span class="metric-label">Available</span>
                    <span class="metric-value" id="atomspaceAvailable">Checking...</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Size (atoms)</span>
                    <span class="metric-value" id="atomspaceSize">0</span>
                </div>
            </div>
        </div>
        
        <!-- Component Status -->
        <div class="card">
            <h3>🔧 Components</h3>
            <div id="componentsMetrics">
                <div class="metric">
                    <span class="metric-label">WolfCog Processes</span>
                    <span class="metric-value" id="totalProcesses">0</span>
                </div>
            </div>
            <div id="processList">
                <!-- Process list will be populated here -->
            </div>
        </div>
    </div>

    <script>
        // WebSocket connection
        const socket = io();
        
        // Charts
        let performanceChart;
        
        // Connection status
        socket.on('connect', function() {
            document.getElementById('connectionStatus').innerHTML = 
                '<span class="status-indicator status-healthy"></span>Connected';
        });
        
        socket.on('disconnect', function() {
            document.getElementById('connectionStatus').innerHTML = 
                '<span class="status-indicator status-error"></span>Disconnected';
        });
        
        // Real system state updates
        socket.on('system_state', function(data) {
            updateSystemMetrics(data);
            updateTaskMetrics(data);
            updateSpacesMetrics(data);
            updateAtomspaceMetrics(data);
            updateComponentsMetrics(data);
        });
        
        // Real metrics history updates
        socket.on('metrics_update', function(data) {
            updatePerformanceChart(data);
        });
        
        function updateSystemMetrics(data) {
            const perf = data.performance || {};
            document.getElementById('cpuUsage').textContent = (perf.cpu_usage || 0).toFixed(1) + '%';
            document.getElementById('memoryUsage').textContent = (perf.memory_usage || 0).toFixed(1) + '%';
            document.getElementById('diskUsage').textContent = (perf.disk_usage || 0).toFixed(1) + '%';
        }
        
        function updateTaskMetrics(data) {
            const tasks = data.task_processing || {};
            document.getElementById('pendingTasks').textContent = tasks.pending || 0;
            document.getElementById('processedTasks').textContent = tasks.processed || 0;
            document.getElementById('errorCount').textContent = tasks.errors || 0;
        }
        
        function updateSpacesMetrics(data) {
            const spaces = data.symbolic_spaces || {};
            document.getElementById('uSpaceFiles').textContent = (spaces.u && spaces.u.files) || 0;
            document.getElementById('eSpaceFiles').textContent = (spaces.e && spaces.e.files) || 0;
            document.getElementById('sSpaceFiles').textContent = (spaces.s && spaces.s.files) || 0;
        }
        
        function updateAtomspaceMetrics(data) {
            const atomspace = data.atomspace || {};
            document.getElementById('atomspaceAvailable').textContent = atomspace.available ? 'Yes' : 'No';
            document.getElementById('atomspaceSize').textContent = atomspace.size || 0;
        }
        
        function updateComponentsMetrics(data) {
            const components = data.components || {};
            document.getElementById('totalProcesses').textContent = components.total_processes || 0;
            
            // Update process list
            const processList = document.getElementById('processList');
            processList.innerHTML = '';
            
            if (components.processes) {
                components.processes.forEach(proc => {
                    const procDiv = document.createElement('div');
                    procDiv.className = 'metric';
                    procDiv.innerHTML = `
                        <span class="metric-label">PID ${proc.pid}: ${proc.name}</span>
                        <span class="metric-value">Running</span>
                    `;
                    processList.appendChild(procDiv);
                });
            }
        }
        
        function updatePerformanceChart(data) {
            if (performanceChart) {
                performanceChart.data.labels = data.timestamps || [];
                performanceChart.data.datasets[0].data = data.cpu_usage || [];
                performanceChart.data.datasets[1].data = data.memory_usage || [];
                performanceChart.update('none');
            }
        }
        
        function submitTask() {
            const action = document.getElementById('taskAction').value;
            const expression = document.getElementById('taskExpression').value.trim();
            
            if (!expression) {
                alert('Please enter a symbolic expression');
                return;
            }
            
            fetch('/api/tasks/submit', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    action: action,
                    expression: expression
                })
            })
            .then(response => response.json())
            .then(data => {
                if (data.status === 'success') {
                    alert(`Task submitted successfully: ${data.task_id}`);
                    document.getElementById('taskExpression').value = '';
                } else {
                    alert(`Error: ${data.message}`);
                }
            })
            .catch(error => {
                alert(`Network error: ${error}`);
            });
        }
        
        // Initialize charts
        document.addEventListener('DOMContentLoaded', function() {
            // Performance Chart
            const performanceCtx = document.getElementById('performanceChart').getContext('2d');
            performanceChart = new Chart(performanceCtx, {
                type: 'line',
                data: {
                    labels: [],
                    datasets: [
                        {
                            label: 'CPU Usage %',
                            data: [],
                            borderColor: '#ff6b6b',
                            backgroundColor: 'rgba(255, 107, 107, 0.1)',
                            tension: 0.4
                        },
                        {
                            label: 'Memory Usage %',
                            data: [],
                            borderColor: '#4ecdc4',
                            backgroundColor: 'rgba(78, 205, 196, 0.1)',
                            tension: 0.4
                        }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            labels: { color: 'white' }
                        }
                    },
                    scales: {
                        x: {
                            ticks: { color: 'white' },
                            grid: { color: 'rgba(255, 255, 255, 0.1)' }
                        },
                        y: {
                            ticks: { color: 'white' },
                            grid: { color: 'rgba(255, 255, 255, 0.1)' }
                        }
                    }
                }
            });
            
            // Request initial data
            socket.emit('request_update');
            
            // Set up periodic updates
            setInterval(() => {
                socket.emit('request_update');
            }, 5000);
        });
    </script>
</body>
</html>'''
    
    def stop(self):
        """Stop the dashboard"""
        print("🛑 Stopping Real WolfCog Dashboard...")
        self.running = False


def main():
    """Main function"""
    print("📊 WolfCog Real Web Dashboard")
    print("🎯 Focus: Actual system monitoring and control")
    print("❌ Removed: Mock features, amazing metrics, transcendence indicators")
    print()
    
    dashboard = RealWolfCogDashboard()
    
    try:
        dashboard.start()
    except KeyboardInterrupt:
        dashboard.stop()
    except Exception as e:
        print(f"❌ Dashboard error: {e}")
        dashboard.stop()


if __name__ == "__main__":
    main()
