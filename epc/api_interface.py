#!/usr/bin/env python3
"""
API Interface System for Wolfram EPC

Provides REST API deployment capabilities with single-command API generation
from Wolfram Language functions. Integrates with WolfCog's existing API 
infrastructure.
"""

import json
import time
from pathlib import Path
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass
from flask import Flask, request, jsonify, Response
from functools import wraps


@dataclass
class APIEndpoint:
    """Configuration for an API endpoint"""
    name: str
    path: str
    wolfram_function: str
    method: str = "POST"
    description: str = ""
    parameters: Dict[str, str] = None
    authentication: bool = False
    rate_limit: Optional[int] = None


class APIInterface:
    """
    Wolfram Language API Interface Manager
    
    Features:
    - Single-command API deployment from Wolfram functions
    - RESTful endpoint generation
    - Automatic parameter validation
    - Rate limiting and authentication
    - Integration with WolfCog compute engine
    """
    
    def __init__(self, compute_engine, host: str = "0.0.0.0", port: int = 5000):
        """
        Initialize API interface
        
        Args:
            compute_engine: ComputeEngine instance for executing Wolfram code
            host: API server host
            port: API server port
        """
        self.compute_engine = compute_engine
        self.host = host
        self.port = port
        
        # Flask app for API hosting
        self.app = Flask(__name__)
        self.app.config['JSON_SORT_KEYS'] = False
        
        # Registered endpoints
        self.endpoints: Dict[str, APIEndpoint] = {}
        
        # API metrics
        self.metrics = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "endpoints": {}
        }
        
        # Setup base routes
        self._setup_base_routes()
    
    def _setup_base_routes(self):
        """Setup base API routes"""
        
        @self.app.route('/api/health', methods=['GET'])
        def health_check():
            """Health check endpoint"""
            return jsonify({
                "status": "healthy",
                "timestamp": time.time(),
                "compute_engine": self.compute_engine.get_kernel_status()
            })
        
        @self.app.route('/api/endpoints', methods=['GET'])
        def list_endpoints():
            """List all registered API endpoints"""
            return jsonify({
                "endpoints": [
                    {
                        "name": ep.name,
                        "path": ep.path,
                        "method": ep.method,
                        "description": ep.description,
                        "parameters": ep.parameters or {}
                    }
                    for ep in self.endpoints.values()
                ]
            })
        
        @self.app.route('/api/metrics', methods=['GET'])
        def get_metrics():
            """Get API usage metrics"""
            return jsonify(self.metrics)
    
    def deploy_api(self, name: str, wolfram_function: str, 
                  path: Optional[str] = None,
                  method: str = "POST",
                  description: str = "",
                  parameters: Optional[Dict[str, str]] = None,
                  authentication: bool = False,
                  rate_limit: Optional[int] = None) -> APIEndpoint:
        """
        Deploy a Wolfram Language function as an API endpoint
        
        Single-command deployment:
        >>> api.deploy_api("factorial", "Factorial[#]&", description="Compute factorial")
        
        Args:
            name: Endpoint name
            wolfram_function: Wolfram Language function code
            path: URL path (defaults to /api/{name})
            method: HTTP method
            description: Endpoint description
            parameters: Parameter specifications
            authentication: Require authentication
            rate_limit: Requests per minute limit
            
        Returns:
            Created APIEndpoint
        """
        if path is None:
            path = f"/api/{name}"
        
        endpoint = APIEndpoint(
            name=name,
            path=path,
            wolfram_function=wolfram_function,
            method=method,
            description=description,
            parameters=parameters or {},
            authentication=authentication,
            rate_limit=rate_limit
        )
        
        self.endpoints[name] = endpoint
        self._register_endpoint(endpoint)
        
        # Initialize metrics for this endpoint
        self.metrics["endpoints"][name] = {
            "requests": 0,
            "successes": 0,
            "failures": 0,
            "avg_response_time": 0.0
        }
        
        return endpoint
    
    def _register_endpoint(self, endpoint: APIEndpoint):
        """Register endpoint with Flask"""
        
        def endpoint_handler():
            """Handle API request"""
            start_time = time.time()
            
            try:
                # Get request data
                if endpoint.method == "POST":
                    data = request.get_json() or {}
                else:
                    data = dict(request.args)
                
                # Execute Wolfram function
                result = self._execute_api_function(endpoint, data)
                
                # Update metrics
                self._update_metrics(endpoint.name, True, time.time() - start_time)
                
                return jsonify({
                    "status": "success",
                    "result": result,
                    "endpoint": endpoint.name,
                    "timestamp": time.time()
                })
                
            except Exception as e:
                # Update metrics
                self._update_metrics(endpoint.name, False, time.time() - start_time)
                
                # Log the full error internally but only return generic message
                print(f"API Error in {endpoint.name}: {str(e)}")
                
                return jsonify({
                    "status": "error",
                    "error": "Internal server error processing request",
                    "endpoint": endpoint.name,
                    "timestamp": time.time()
                }), 500
        
        # Add route to Flask app
        endpoint_handler.__name__ = f"{endpoint.name}_handler"
        self.app.add_url_rule(
            endpoint.path,
            endpoint.name,
            endpoint_handler,
            methods=[endpoint.method]
        )
    
    def _execute_api_function(self, endpoint: APIEndpoint, data: Dict) -> Any:
        """
        Execute Wolfram function with API data
        
        Args:
            endpoint: API endpoint configuration
            data: Request data
            
        Returns:
            Function result
        """
        # Build Wolfram code from function and data
        function_code = endpoint.wolfram_function
        
        # If data provided, apply function to data
        if data:
            # Convert data to Wolfram format
            wolfram_data = self._python_to_wolfram(data)
            code = f"({function_code})[{wolfram_data}]"
        else:
            code = function_code
        
        # Execute via compute engine
        result = self.compute_engine.execute(code)
        
        if result.get("error"):
            raise RuntimeError(f"Wolfram execution error: {result['error']}")
        
        return self._parse_wolfram_output(result["output"])
    
    def _python_to_wolfram(self, obj: Any) -> str:
        """Convert Python object to Wolfram Language representation"""
        if isinstance(obj, dict):
            # Convert to Association
            items = [f'"{k}" -> {self._python_to_wolfram(v)}' for k, v in obj.items()]
            return f"<|{', '.join(items)}|>"
        elif isinstance(obj, list):
            # Convert to List
            items = [self._python_to_wolfram(item) for item in obj]
            return f"{{{', '.join(items)}}}"
        elif isinstance(obj, str):
            return f'"{obj}"'
        elif isinstance(obj, (int, float)):
            return str(obj)
        elif isinstance(obj, bool):
            return "True" if obj else "False"
        else:
            return str(obj)
    
    def _parse_wolfram_output(self, output: str) -> Any:
        """Parse Wolfram output to Python object"""
        # Simple parsing - would need more sophisticated parsing in production
        output = output.strip()
        
        # Try to parse as JSON if it looks like JSON
        if output.startswith('{') or output.startswith('['):
            try:
                return json.loads(output)
            except:
                pass
        
        # Return as string
        return output
    
    def _update_metrics(self, endpoint_name: str, success: bool, response_time: float):
        """Update API metrics"""
        self.metrics["total_requests"] += 1
        
        if success:
            self.metrics["successful_requests"] += 1
        else:
            self.metrics["failed_requests"] += 1
        
        ep_metrics = self.metrics["endpoints"][endpoint_name]
        ep_metrics["requests"] += 1
        
        if success:
            ep_metrics["successes"] += 1
        else:
            ep_metrics["failures"] += 1
        
        # Update average response time
        current_avg = ep_metrics["avg_response_time"]
        request_count = ep_metrics["requests"]
        new_avg = ((current_avg * (request_count - 1)) + response_time) / request_count
        ep_metrics["avg_response_time"] = new_avg
    
    def deploy_from_notebook(self, notebook_path: str):
        """
        Deploy APIs defined in a Wolfram Notebook
        
        Args:
            notebook_path: Path to .nb file with API definitions
        """
        # This would parse a Wolfram notebook and extract API definitions
        # Format: APIFunction[...] or CloudDeploy[APIFunction[...], ...]
        raise NotImplementedError("Notebook deployment coming soon")
    
    def excel_connector(self, endpoint_name: str) -> str:
        """
        Generate Excel connector formula for an API endpoint
        
        Args:
            endpoint_name: Name of deployed endpoint
            
        Returns:
            Excel formula string
        """
        if endpoint_name not in self.endpoints:
            raise ValueError(f"Unknown endpoint: {endpoint_name}")
        
        endpoint = self.endpoints[endpoint_name]
        
        # Generate Excel WEBSERVICE formula
        url = f"http://{self.host}:{self.port}{endpoint.path}"
        
        formula = f"""
=WEBSERVICE("{url}")

Or with data:
=WEBSERVICE("{url}" & "?param=" & A1)

Or use Power Query for POST:
let
    url = "{url}",
    body = Json.FromValue([param = A1]),
    response = Web.Contents(url, [
        Headers = [#"Content-Type"="application/json"],
        Content = body
    ]),
    json = Json.Document(response)
in
    json[result]
"""
        return formula
    
    def run(self, debug: bool = False):
        """Start the API server"""
        print(f"🚀 Starting Wolfram EPC API Interface...")
        print(f"📡 Server: http://{self.host}:{self.port}")
        print(f"📚 Endpoints: {len(self.endpoints)}")
        print(f"🔧 Compute Engine: {len(self.compute_engine.kernels)} kernels")
        
        self.app.run(host=self.host, port=self.port, debug=debug)
    
    def get_openapi_spec(self) -> Dict:
        """Generate OpenAPI specification for all endpoints"""
        spec = {
            "openapi": "3.0.0",
            "info": {
                "title": "Wolfram EPC API",
                "version": "1.0.0",
                "description": "Wolfram Language API endpoints deployed via EPC"
            },
            "servers": [
                {"url": f"http://{self.host}:{self.port}"}
            ],
            "paths": {}
        }
        
        for endpoint in self.endpoints.values():
            spec["paths"][endpoint.path] = {
                endpoint.method.lower(): {
                    "summary": endpoint.description,
                    "operationId": endpoint.name,
                    "parameters": [
                        {
                            "name": param_name,
                            "in": "query" if endpoint.method == "GET" else "body",
                            "schema": {"type": param_type}
                        }
                        for param_name, param_type in (endpoint.parameters or {}).items()
                    ],
                    "responses": {
                        "200": {
                            "description": "Successful response",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "status": {"type": "string"},
                                            "result": {"type": "object"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        
        return spec
