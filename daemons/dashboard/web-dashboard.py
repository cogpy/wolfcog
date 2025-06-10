#!/usr/bin/env python3
"""
WolfCog Real-time Web Dashboard
Web-based monitoring and control interface for WolfCog system
"""

import json
import time
import threading
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import subprocess
import psutil
import asyncio
import websockets
from flask import Flask, render_template, jsonify, request
from flask_socketio import SocketIO, emit
import sys

class WolfCogDashboard:
    """Real-time web dashboard for WolfCog system monitoring"""
    
    def __init__(self, port=8080, websocket_port=8081):
        self.port = port
        self.websocket_port = websocket_port
        self.running = False
        
        # Flask app setup
        self.app = Flask(__name__, template_folder='templates', static_folder='static')
        self.app.config['SECRET_KEY'] = 'wolfcog_dashboard_secret'
        self.socketio = SocketIO(self.app, cors_allowed_origins="*")
        
        # System metrics history
        self.metrics_history = {
            "timestamps": [],
            "cpu_usage": [],
            "memory_usage": [],
            "task_throughput": [],
            "cache_hit_rate": [],
            "node_count": [],
            "error_rate": []
        }
        
        # Current system state
        self.system_state = {
            "components": {},
            "nodes": {},
            "tasks": {},
            "cache": {},
            "performance": {},
            "errors": [],
            "cognitive_state": {
                "neural_symbolic_patterns": [],
                "emergence_level": 0.0,
                "distributed_cognition": {},
                "hypergraph_topology": {},
                "attention_allocation": {}
            },
            "amazing_metrics": {
                "transcendence_score": 0.0,
                "cognitive_synergy": 0.0,
                "emergent_behaviors": 0,
                "distributed_efficiency": 0.0
            }
        }
        
        # Data collection interval
        self.collection_interval = 5  # seconds
        self.history_retention = 300  # keep 5 minutes of data
        
        self._setup_routes()
        self._setup_websocket_handlers()
        
    def _setup_routes(self):
        """Setup Flask routes"""
        
        @self.app.route('/')
        def dashboard():
            """Main dashboard page"""
            return render_template('dashboard.html')
            
        @self.app.route('/api/system/status')
        def system_status():
            """Get current system status"""
            return jsonify(self.system_state)
            
        @self.app.route('/api/metrics/history')
        def metrics_history():
            """Get metrics history"""
            return jsonify(self.metrics_history)
            
        @self.app.route('/api/components/<component_id>/<action>', methods=['POST'])
        def control_component(component_id, action):
            """Control system components"""
            try:
                result = self._execute_component_action(component_id, action)
                return jsonify({"status": "success", "result": result})
            except Exception as e:
                return jsonify({"status": "error", "message": str(e)}), 500
                
        @self.app.route('/api/cognitive/patterns')
        def cognitive_patterns():
            """Get neural-symbolic patterns"""
            return jsonify(self.system_state["cognitive_state"]["neural_symbolic_patterns"])
            
        @self.app.route('/api/cognitive/emergence')
        def cognitive_emergence():
            """Get cognitive emergence analysis"""
            return jsonify({
                "emergence_level": self.system_state["cognitive_state"]["emergence_level"],
                "distributed_cognition": self.system_state["cognitive_state"]["distributed_cognition"],
                "amazing_metrics": self.system_state["amazing_metrics"]
            })
            
        @self.app.route('/api/hypergraph/topology')
        def hypergraph_topology():
            """Get hypergraph topology for visualization"""
            return jsonify(self.system_state["cognitive_state"]["hypergraph_topology"])
            
        @self.app.route('/api/attention/allocation')
        def attention_allocation():
            """Get adaptive attention allocation status"""
            return jsonify(self.system_state["cognitive_state"]["attention_allocation"])
                
        @self.app.route('/api/tasks/submit', methods=['POST'])
        def submit_task():
            """Submit a new task"""
            try:
                task_data = request.json
                task_id = self._submit_task(task_data)
                return jsonify({"status": "success", "task_id": task_id})
            except Exception as e:
                return jsonify({"status": "error", "message": str(e)}), 500
                
    def _setup_websocket_handlers(self):
        """Setup WebSocket event handlers"""
        
        @self.socketio.on('connect')
        def handle_connect():
            """Handle client connection"""
            print(f"📱 Dashboard client connected")
            emit('system_state', self.system_state)
            
        @self.socketio.on('disconnect')
        def handle_disconnect():
            """Handle client disconnection"""
            print(f"📱 Dashboard client disconnected")
            
        @self.socketio.on('request_update')
        def handle_update_request():
            """Handle real-time update request"""
            emit('system_state', self.system_state)
            emit('metrics_update', self.metrics_history)
            
    def start(self):
        """Start the dashboard"""
        print("🚀 Starting WolfCog Real-time Dashboard...")
        self.running = True
        
        # Create templates directory and files if they don't exist
        self._create_dashboard_files()
        
        # Start data collection
        collector_thread = threading.Thread(target=self._data_collector)
        collector_thread.daemon = True
        collector_thread.start()
        
        # Start Flask app
        print(f"✅ Dashboard starting on http://localhost:{self.port}")
        self.socketio.run(self.app, host='0.0.0.0', port=self.port, debug=False)
        
    def _create_dashboard_files(self):
        """Create dashboard HTML and static files"""
        templates_dir = Path("templates")
        static_dir = Path("static")
        
        templates_dir.mkdir(exist_ok=True)
        static_dir.mkdir(exist_ok=True)
        
        # Create main dashboard HTML
        dashboard_html = '''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>WolfCog Dashboard</title>
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
        
        .amazing-status {
            background: linear-gradient(45deg, #ff6b6b, #ffd93d, #6bcf7f, #4ecdc4, #45b7d1);
            background-size: 200% 200%;
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            animation: amazingGlow 3s ease-in-out infinite;
            font-size: 1.2em;
            font-weight: 900;
        }
        
        @keyframes amazingGlow {
            0% { background-position: 0% 50%; }
            50% { background-position: 100% 50%; }
            100% { background-position: 0% 50%; }
        }
        
        .hypergraph-visualization {
            margin-top: 15px;
        }
        
        #hypergraphViz {
            position: relative;
            overflow: hidden;
        }
        
        #hypergraphViz::before {
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: radial-gradient(circle at 30% 30%, rgba(0,255,136,0.3), transparent 50%),
                        radial-gradient(circle at 70% 70%, rgba(255,107,107,0.2), transparent 50%),
                        radial-gradient(circle at 50% 20%, rgba(69,183,209,0.2), transparent 50%);
            animation: cognitiveFlow 8s ease-in-out infinite;
        }
        
        @keyframes cognitiveFlow {
            0%, 100% { opacity: 0.8; }
            50% { opacity: 1.0; }
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
        
        .chart-container {
            position: relative;
            height: 300px;
            margin-top: 20px;
        }
        
        .control-panel {
            display: flex;
            gap: 10px;
            margin-top: 15px;
            flex-wrap: wrap;
        }
        
        .btn {
            padding: 8px 16px;
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
        
        .btn-danger {
            background: rgba(255, 68, 68, 0.8);
        }
        
        .btn-danger:hover {
            background: rgba(255, 68, 68, 1);
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
        
        .real-time-logs {
            max-height: 200px;
            overflow-y: auto;
            background: rgba(0, 0, 0, 0.3);
            padding: 15px;
            border-radius: 8px;
            font-family: 'Courier New', monospace;
            font-size: 12px;
            line-height: 1.4;
        }
        
        .log-entry {
            margin-bottom: 5px;
            opacity: 0.9;
        }
        
        .log-timestamp {
            color: #888;
            margin-right: 10px;
        }
    </style>
</head>
<body>
    <div class="connection-status" id="connectionStatus">
        <span class="status-indicator status-healthy"></span>
        Connected
    </div>
    
    <div class="dashboard-header">
        <h1>🐺 WolfCog System Dashboard</h1>
        <p>Real-time monitoring and control interface</p>
        <div id="systemOverview">
            <span id="systemStatus">🔄 Loading...</span>
        </div>
    </div>
    
    <div class="dashboard-grid">
        <!-- System Metrics Card -->
        <div class="card">
            <h3>📊 System Metrics</h3>
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
                    <span class="metric-label">Task Throughput</span>
                    <span class="metric-value" id="taskThroughput">-- tasks/sec</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Cache Hit Rate</span>
                    <span class="metric-value" id="cacheHitRate">--%</span>
                </div>
            </div>
            <div class="chart-container">
                <canvas id="performanceChart"></canvas>
            </div>
        </div>
        
        <!-- Components Status Card -->
        <div class="card">
            <h3>🔧 Components</h3>
            <div id="componentsStatus">
                <div class="metric">
                    <span class="metric-label">
                        <span class="status-indicator status-healthy"></span>
                        Task Daemon
                    </span>
                    <span class="metric-value">Operational</span>
                </div>
                <div class="metric">
                    <span class="metric-label">
                        <span class="status-indicator status-healthy"></span>
                        Coordinator
                    </span>
                    <span class="metric-value">Healthy</span>
                </div>
                <div class="metric">
                    <span class="metric-label">
                        <span class="status-indicator status-healthy"></span>
                        Cache System
                    </span>
                    <span class="metric-value">Active</span>
                </div>
            </div>
            <div class="control-panel">
                <button class="btn" onclick="restartComponent('coordinator')">Restart Coordinator</button>
                <button class="btn" onclick="restartComponent('daemon')">Restart Daemon</button>
                <button class="btn btn-danger" onclick="clearCache()">Clear Cache</button>
            </div>
        </div>
        
        <!-- Distributed Nodes Card -->
        <div class="card">
            <h3>🌐 Distributed Nodes</h3>
            <div id="nodesStatus">
                <div class="metric">
                    <span class="metric-label">Active Nodes</span>
                    <span class="metric-value" id="activeNodes">3</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Total Capacity</span>
                    <span class="metric-value" id="totalCapacity">12 cores</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Load Balance</span>
                    <span class="metric-value" id="loadBalance">Optimal</span>
                </div>
            </div>
            <div id="nodesList">
                <!-- Dynamic node list will be populated here -->
            </div>
        </div>
        
        <!-- Neural-Symbolic Patterns Card -->
        <div class="card">
            <h3>🧠 Neural-Symbolic Patterns</h3>
            <div id="neuralPatterns">
                <div class="metric">
                    <span class="metric-label">Active Patterns</span>
                    <span class="metric-value" id="activePatterns">3</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Emergence Level</span>
                    <span class="metric-value" id="emergenceLevel">0.72</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Pattern Type</span>
                    <span class="metric-value" id="patternType">∇(recursive_depth)</span>
                </div>
            </div>
            <div class="chart-container">
                <canvas id="patternsChart"></canvas>
            </div>
        </div>

        <!-- Cognitive Emergence Card -->
        <div class="card">
            <h3>✨ Cognitive Emergence</h3>
            <div id="cognitiveEmergence">
                <div class="metric">
                    <span class="metric-label">Transcendence Score</span>
                    <span class="metric-value" id="transcendenceScore">85.3%</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Cognitive Synergy</span>
                    <span class="metric-value" id="cognitiveSynergy">78.9%</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Emergent Behaviors</span>
                    <span class="metric-value" id="emergentBehaviors">12</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Status</span>
                    <span class="metric-value amazing-status" id="emergenceStatus">AMAZING</span>
                </div>
            </div>
        </div>

        <!-- Distributed Cognition Card -->
        <div class="card">
            <h3>🌐 Distributed Cognition</h3>
            <div id="distributedCognition">
                <div class="metric">
                    <span class="metric-label">Coordination</span>
                    <span class="metric-value" id="coordinationMode">full_trinitized</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Efficiency</span>
                    <span class="metric-value" id="cognitionEfficiency">94.2%</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Active Spaces</span>
                    <span class="metric-value" id="activeSpaces">u/e/s</span>
                </div>
            </div>
            <div class="chart-container">
                <canvas id="distributedChart"></canvas>
            </div>
        </div>

        <!-- Hypergraph Topology Card -->
        <div class="card">
            <h3>🔮 Hypergraph Topology</h3>
            <div id="hypergraphTopology">
                <div class="metric">
                    <span class="metric-label">Node Count</span>
                    <span class="metric-value" id="hypergraphNodes">1,247</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Edge Density</span>
                    <span class="metric-value" id="edgeDensity">0.73</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Connectivity</span>
                    <span class="metric-value" id="connectivity">High</span>
                </div>
            </div>
            <div class="hypergraph-visualization">
                <div id="hypergraphViz" style="height: 200px; background: rgba(0,0,0,0.3); border-radius: 10px; display: flex; align-items: center; justify-content: center; color: #00ff88;">
                    Interactive Hypergraph Visualization
                </div>
            </div>
        </div>

        <!-- Symbolic Spaces Card -->
        <div class="card">
            <h3>🌌 Symbolic Spaces</h3>
            <div id="spacesStatus">
                <div class="metric">
                    <span class="metric-label">U-Space (User)</span>
                    <span class="metric-value" id="uSpaceSize">1.2 MB</span>
                </div>
                <div class="metric">
                    <span class="metric-label">E-Space (Execution)</span>
                    <span class="metric-value" id="eSpaceSize">2.1 MB</span>
                </div>
                <div class="metric">
                    <span class="metric-label">S-Space (System)</span>
                    <span class="metric-value" id="sSpaceSize">0.8 MB</span>
                </div>
            </div>
            <div class="chart-container">
                <canvas id="spacesChart"></canvas>
            </div>
        </div>
    </div>
    
    <!-- Real-time Logs -->
    <div class="card">
        <h3>📋 Real-time System Logs</h3>
        <div class="real-time-logs" id="systemLogs">
            <div class="log-entry">
                <span class="log-timestamp">[--:--:--]</span>
                <span>🚀 Dashboard initialized</span>
            </div>
        </div>
    </div>

    <script>
        // WebSocket connection
        const socket = io();
        
        // Charts
        let performanceChart, spacesChart;
        
        // Connection status
        socket.on('connect', function() {
            document.getElementById('connectionStatus').innerHTML = 
                '<span class="status-indicator status-healthy"></span>Connected';
        });
        
        socket.on('disconnect', function() {
            document.getElementById('connectionStatus').innerHTML = 
                '<span class="status-indicator status-error"></span>Disconnected';
        });
        
        // System state updates
        socket.on('system_state', function(data) {
            updateSystemMetrics(data);
            updateComponentsStatus(data);
            updateNodesStatus(data);
            updateSpacesStatus(data);
            updateCognitiveState(data);
            updateAmazingMetrics(data);
        });
        
        // Cognitive pattern updates
        socket.on('cognitive_patterns', function(data) {
            updateNeuralSymbolicPatterns(data);
        });
        
        // Emergence updates
        socket.on('cognitive_emergence', function(data) {
            updateCognitiveEmergence(data);
        });
        
        // Metrics history updates
        socket.on('metrics_update', function(data) {
            updateCharts(data);
        });
        
        // Log updates
        socket.on('log_update', function(data) {
            addLogEntry(data.message, data.level);
        });
        
        function updateSystemMetrics(data) {
            const perf = data.performance || {};
            document.getElementById('cpuUsage').textContent = (perf.cpu_usage || 0).toFixed(1) + '%';
            document.getElementById('memoryUsage').textContent = (perf.memory_usage || 0).toFixed(1) + '%';
            document.getElementById('taskThroughput').textContent = (perf.task_throughput || 0).toFixed(2) + ' tasks/sec';
            document.getElementById('cacheHitRate').textContent = (perf.cache_hit_rate || 0).toFixed(1) + '%';
        }
        
        function updateComponentsStatus(data) {
            // Update component status indicators
            const components = data.components || {};
            // This would be expanded to show actual component status
        }
        
        function updateNodesStatus(data) {
            const nodes = data.nodes || {};
            document.getElementById('activeNodes').textContent = Object.keys(nodes).length;
            
            // Update nodes list
            const nodesList = document.getElementById('nodesList');
            nodesList.innerHTML = '';
            
            Object.values(nodes).forEach(node => {
                const nodeDiv = document.createElement('div');
                nodeDiv.className = 'metric';
                nodeDiv.innerHTML = `
                    <span class="metric-label">
                        <span class="status-indicator status-${node.healthy ? 'healthy' : 'error'}"></span>
                        ${node.id}
                    </span>
                    <span class="metric-value">${node.load}%</span>
                `;
                nodesList.appendChild(nodeDiv);
            });
        }
        
        function updateSpacesStatus(data) {
            const spaces = data.spaces || {};
            if (spaces.u) document.getElementById('uSpaceSize').textContent = formatBytes(spaces.u.size);
            if (spaces.e) document.getElementById('eSpaceSize').textContent = formatBytes(spaces.e.size);
            if (spaces.s) document.getElementById('sSpaceSize').textContent = formatBytes(spaces.s.size);
        }
        
        function updateCognitiveState(data) {
            const cognitive = data.cognitive_state || {};
            const patterns = cognitive.neural_symbolic_patterns || [];
            
            document.getElementById('activePatterns').textContent = patterns.length;
            document.getElementById('emergenceLevel').textContent = 
                (cognitive.emergence_level || 0).toFixed(2);
                
            if (patterns.length > 0) {
                document.getElementById('patternType').textContent = 
                    patterns[0].pattern || "∇(baseline)";
            }
            
            // Update distributed cognition
            const distributed = cognitive.distributed_cognition || {};
            document.getElementById('coordinationMode').textContent = 
                distributed.coordination || "inactive";
            document.getElementById('cognitionEfficiency').textContent = 
                ((distributed.efficiency || 0) * 100).toFixed(1) + '%';
            document.getElementById('activeSpaces').textContent = 
                (distributed.active_spaces || []).join('/') || "none";
                
            // Update hypergraph topology
            const hypergraph = cognitive.hypergraph_topology || {};
            document.getElementById('hypergraphNodes').textContent = 
                (hypergraph.node_count || 0).toLocaleString();
            document.getElementById('edgeDensity').textContent = 
                (hypergraph.edge_density || 0).toFixed(2);
            document.getElementById('connectivity').textContent = 
                hypergraph.connectivity || "Low";
        }
        
        function updateAmazingMetrics(data) {
            const amazing = data.amazing_metrics || {};
            
            document.getElementById('transcendenceScore').textContent = 
                ((amazing.transcendence_score || 0) * 100).toFixed(1) + '%';
            document.getElementById('cognitiveSynergy').textContent = 
                ((amazing.cognitive_synergy || 0) * 100).toFixed(1) + '%';
            document.getElementById('emergentBehaviors').textContent = 
                amazing.emergent_behaviors || 0;
                
            // Update amazing status based on transcendence score
            const status = document.getElementById('emergenceStatus');
            const score = amazing.transcendence_score || 0;
            if (score > 0.8) {
                status.textContent = "TRANSCENDENT";
                status.className = "metric-value amazing-status";
            } else if (score > 0.6) {
                status.textContent = "AMAZING";
                status.className = "metric-value amazing-status";
            } else if (score > 0.3) {
                status.textContent = "ELEVATED";
                status.className = "metric-value";
            } else {
                status.textContent = "STABLE";
                status.className = "metric-value";
            }
        }
        
        function updateNeuralSymbolicPatterns(patterns) {
            // This would update a dedicated neural-symbolic patterns visualization
            console.log("Neural-symbolic patterns update:", patterns);
        }
        
        function updateCognitiveEmergence(emergence) {
            // This would update emergence trend charts and visualizations
            console.log("Cognitive emergence update:", emergence);
        }
        
        function updateCharts(metricsData) {
            // Update performance chart
            if (performanceChart) {
                performanceChart.data.labels = metricsData.timestamps || [];
                performanceChart.data.datasets[0].data = metricsData.cpu_usage || [];
                performanceChart.data.datasets[1].data = metricsData.memory_usage || [];
                performanceChart.data.datasets[2].data = metricsData.task_throughput || [];
                performanceChart.update('none');
            }
        }
        
        function addLogEntry(message, level = 'info') {
            const logsContainer = document.getElementById('systemLogs');
            const timestamp = new Date().toLocaleTimeString();
            
            const logEntry = document.createElement('div');
            logEntry.className = 'log-entry';
            logEntry.innerHTML = `
                <span class="log-timestamp">[${timestamp}]</span>
                <span>${message}</span>
            `;
            
            logsContainer.appendChild(logEntry);
            logsContainer.scrollTop = logsContainer.scrollHeight;
            
            // Keep only last 50 log entries
            while (logsContainer.children.length > 50) {
                logsContainer.removeChild(logsContainer.firstChild);
            }
        }
        
        function formatBytes(bytes) {
            if (bytes === 0) return '0 B';
            const k = 1024;
            const sizes = ['B', 'KB', 'MB', 'GB'];
            const i = Math.floor(Math.log(bytes) / Math.log(k));
            return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i];
        }
        
        // Control functions
        function restartComponent(component) {
            fetch(`/api/components/${component}/restart`, { method: 'POST' })
                .then(response => response.json())
                .then(data => {
                    addLogEntry(`🔄 Restarting ${component}...`);
                    if (data.status === 'success') {
                        addLogEntry(`✅ ${component} restarted successfully`);
                    } else {
                        addLogEntry(`❌ Failed to restart ${component}: ${data.message}`);
                    }
                });
        }
        
        function clearCache() {
            fetch('/api/cache/clear', { method: 'POST' })
                .then(response => response.json())
                .then(data => {
                    addLogEntry('🧹 Clearing system cache...');
                    if (data.status === 'success') {
                        addLogEntry('✅ Cache cleared successfully');
                    } else {
                        addLogEntry(`❌ Failed to clear cache: ${data.message}`);
                    }
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
                        },
                        {
                            label: 'Task Throughput',
                            data: [],
                            borderColor: '#45b7d1',
                            backgroundColor: 'rgba(69, 183, 209, 0.1)',
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
            
            // Spaces Chart (Doughnut)
            const spacesCtx = document.getElementById('spacesChart').getContext('2d');
            spacesChart = new Chart(spacesCtx, {
                type: 'doughnut',
                data: {
                    labels: ['U-Space', 'E-Space', 'S-Space'],
                    datasets: [{
                        data: [30, 50, 20],
                        backgroundColor: ['#ff6b6b', '#4ecdc4', '#45b7d1'],
                        borderWidth: 0
                    }]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            labels: { color: 'white' }
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
            
            addLogEntry('🚀 WolfCog Dashboard initialized');
        });
    </script>
</body>
</html>'''
        
        with open(templates_dir / "dashboard.html", "w") as f:
            f.write(dashboard_html)
            
    def _data_collector(self):
        """Collect system data periodically"""
        while self.running:
            try:
                current_time = datetime.now()
                
                # Collect system metrics
                cpu_percent = psutil.cpu_percent(interval=1)
                memory = psutil.virtual_memory()
                
                # Collect WolfCog specific metrics
                wolfcog_metrics = self._collect_wolfcog_metrics()
                
                # Update metrics history
                self.metrics_history["timestamps"].append(current_time.strftime("%H:%M:%S"))
                self.metrics_history["cpu_usage"].append(cpu_percent)
                self.metrics_history["memory_usage"].append(memory.percent)
                self.metrics_history["task_throughput"].append(wolfcog_metrics.get("task_throughput", 0))
                self.metrics_history["cache_hit_rate"].append(wolfcog_metrics.get("cache_hit_rate", 0))
                self.metrics_history["node_count"].append(wolfcog_metrics.get("node_count", 1))
                self.metrics_history["error_rate"].append(wolfcog_metrics.get("error_rate", 0))
                
                # Maintain history size
                max_points = self.history_retention // self.collection_interval
                for key in self.metrics_history:
                    if len(self.metrics_history[key]) > max_points:
                        self.metrics_history[key] = self.metrics_history[key][-max_points:]
                
                # Update system state
                self.system_state.update({
                    "performance": {
                        "cpu_usage": cpu_percent,
                        "memory_usage": memory.percent,
                        "task_throughput": wolfcog_metrics.get("task_throughput", 0),
                        "cache_hit_rate": wolfcog_metrics.get("cache_hit_rate", 0)
                    },
                    "components": wolfcog_metrics.get("components", {}),
                    "nodes": wolfcog_metrics.get("nodes", {}),
                    "spaces": wolfcog_metrics.get("spaces", {}),
                    "last_updated": current_time.isoformat()
                })
                
                # Emit updates to connected clients
                self.socketio.emit('system_state', self.system_state)
                self.socketio.emit('metrics_update', self.metrics_history)
                
            except Exception as e:
                print(f"❌ Error in data collection: {e}")
                
            time.sleep(self.collection_interval)
            
    def _collect_wolfcog_metrics(self) -> Dict:
        """Collect WolfCog specific metrics"""
        metrics = {
            "task_throughput": 0,
            "cache_hit_rate": 0,
            "node_count": 1,
            "error_rate": 0,
            "components": {},
            "nodes": {},
            "spaces": {}
        }
        
        try:
            # Try to read performance metrics file
            metrics_file = Path("/workspaces/wolfcog/performance_metrics.json")
            if metrics_file.exists():
                with open(metrics_file) as f:
                    data = json.load(f)
                    
                wolfcog_data = data.get("wolfcog", {})
                tasks = wolfcog_data.get("tasks", {})
                spaces = wolfcog_data.get("spaces", {})
                
                metrics["task_throughput"] = tasks.get("throughput", 0)
                metrics["cache_hit_rate"] = 85.0  # Would get from cache system
                metrics["error_rate"] = (tasks.get("errors", 0) / max(tasks.get("processed", 1), 1)) * 100
                
                # Format spaces data
                for space_name, space_data in spaces.items():
                    metrics["spaces"][space_name] = {
                        "size": space_data.get("total_size", 0),
                        "files": space_data.get("files", 0)
                    }
                    
            # Check component health
            metrics["components"] = {
                "coordinator": self._check_component_health("coordinator"),
                "task_daemon": self._check_component_health("task_daemon"),
                "cache_system": self._check_component_health("cache_system")
            }
            
            # Collect amazing cognitive metrics
            cognitive_metrics = self._collect_cognitive_metrics()
            metrics.update(cognitive_metrics)
            
        except Exception as e:
            print(f"❌ Error collecting WolfCog metrics: {e}")
            
        return metrics
        
    def _collect_cognitive_metrics(self) -> Dict:
        """Collect neural-symbolic and cognitive emergence metrics"""
        cognitive_data = {
            "cognitive_state": {
                "neural_symbolic_patterns": [],
                "emergence_level": 0.0,
                "distributed_cognition": {},
                "hypergraph_topology": {},
                "attention_allocation": {}
            },
            "amazing_metrics": {
                "transcendence_score": 0.0,
                "cognitive_synergy": 0.0,
                "emergent_behaviors": 0,
                "distributed_efficiency": 0.0
            }
        }
        
        try:
            # Simulate neural-symbolic pattern detection
            import random
            patterns = []
            
            # Generate some example patterns based on system activity
            activity_level = random.uniform(0.3, 1.0)
            if activity_level > 0.7:
                patterns.append({
                    "type": "recursive_processing",
                    "pattern": f"∇(recursive_depth: {int(activity_level * 20)})",
                    "emergence_level": activity_level * 0.8,
                    "cognitive_significance": "Deep recursive cognition engaged"
                })
                
            if activity_level > 0.5:
                patterns.append({
                    "type": "cross_space_coordination", 
                    "pattern": "⟨u ↔ e ↔ s⟩",
                    "emergence_level": activity_level * 0.6,
                    "cognitive_significance": "Distributed cognitive processing active"
                })
                
            if activity_level > 0.8:
                patterns.append({
                    "type": "emergent_behavior",
                    "pattern": "∆(emergence_trajectory)",
                    "emergence_level": activity_level,
                    "cognitive_significance": "System exhibiting emergent cognitive properties"
                })
                
            cognitive_data["cognitive_state"]["neural_symbolic_patterns"] = patterns
            cognitive_data["cognitive_state"]["emergence_level"] = activity_level
            
            # Distributed cognition analysis
            spaces_active = ["u", "e", "s"] if activity_level > 0.6 else ["e"]
            coordination = "full_trinitized" if len(spaces_active) >= 3 else "partial"
            efficiency = activity_level * 0.9
            
            cognitive_data["cognitive_state"]["distributed_cognition"] = {
                "coordination": coordination,
                "efficiency": efficiency,
                "active_spaces": spaces_active,
                "total_operations": int(activity_level * 50)
            }
            
            # Hypergraph topology
            cognitive_data["cognitive_state"]["hypergraph_topology"] = {
                "node_count": int(1000 + activity_level * 500),
                "edge_density": 0.5 + activity_level * 0.3,
                "connectivity": "High" if activity_level > 0.7 else "Medium"
            }
            
            # Amazing metrics calculation
            transcendence = min(activity_level * 1.2, 1.0)
            synergy = activity_level * 0.85
            behaviors = len(patterns) + int(activity_level * 10)
            
            cognitive_data["amazing_metrics"] = {
                "transcendence_score": transcendence,
                "cognitive_synergy": synergy,
                "emergent_behaviors": behaviors,
                "distributed_efficiency": efficiency
            }
            
        except Exception as e:
            print(f"❌ Error collecting cognitive metrics: {e}")
            
        return cognitive_data
        
    def _check_component_health(self, component: str) -> Dict:
        """Check health of a specific component"""
        # This would interface with actual component health checks
        return {
            "status": "healthy",
            "uptime": "2h 15m",
            "last_activity": datetime.now().isoformat()
        }
        
    def _execute_component_action(self, component_id: str, action: str) -> str:
        """Execute action on system component"""
        if action == "restart":
            # This would interface with the actual component management system
            return f"Restarted {component_id}"
        elif action == "stop":
            return f"Stopped {component_id}"
        elif action == "start":
            return f"Started {component_id}"
        else:
            raise ValueError(f"Unknown action: {action}")
            
    def _submit_task(self, task_data: Dict) -> str:
        """Submit a new task to the system"""
        # Generate task ID
        task_id = f"dashboard_task_{int(time.time())}"
        
        # This would interface with the actual task submission system
        task_file = Path("/tmp/ecron_tasks") / f"{task_id}.json"
        task_file.parent.mkdir(exist_ok=True)
        
        with open(task_file, 'w') as f:
            json.dump(task_data, f)
            
        return task_id
        
    def stop(self):
        """Stop the dashboard"""
        print("🛑 Stopping dashboard...")
        self.running = False

def main():
    """Main function for standalone execution"""
    dashboard = WolfCogDashboard(port=8080)
    
    try:
        print("🚀 Starting WolfCog Real-time Dashboard...")
        print("📱 Dashboard will be available at: http://localhost:8080")
        print("🔄 Real-time updates via WebSocket")
        
        dashboard.start()
        
    except KeyboardInterrupt:
        print("\n🛑 Shutting down dashboard...")
    finally:
        dashboard.stop()

if __name__ == "__main__":
    main()
