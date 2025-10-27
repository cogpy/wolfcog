# WolfCog Enhanced Bridge API Documentation

## Overview

The WolfCog Enhanced Bridge provides a production-ready integration between Wolfram Language and OpenCog AtomSpace, enabling seamless symbolic computation across both systems.

## Architecture Components

### 1. Type Registry (`src/type_registry.py`)

Manages bidirectional type mappings between Wolfram Language and OpenCog AtomSpace.

#### Features
- 25+ pre-configured type mappings
- Bidirectional conversion (Wolfram ↔ AtomSpace)
- Expression formatting and parsing
- Custom conversion handlers
- Import/export of type mappings

#### Usage Example

```python
from src.type_registry import get_type_registry

# Get global registry
registry = get_type_registry()

# Convert Wolfram to AtomSpace
wolfram_expr = {"type": "Concept", "name": "MyTerm"}
atomspace_repr = registry.convert_wolfram_to_atomspace(wolfram_expr)
# Result: {"type": "ConceptNode", "name": "MyTerm"}

# Convert AtomSpace to Wolfram
atomspace_atom = {"type": "ConceptNode", "name": "MyNode"}
wolfram_repr = registry.convert_atomspace_to_wolfram(atomspace_atom)
# Result: {"type": "Concept", "name": "MyNode"}

# Format as Wolfram Language code
formatted = registry.format_wolfram_expression(wolfram_expr)
# Result: 'Concept["MyTerm"]'

# Get all mappings
mappings = registry.get_all_mappings()
print(f"Total types: {len(mappings['wolfram_to_atomspace'])}")
```

#### Supported Type Mappings

| Wolfram Type | AtomSpace Type | Description |
|--------------|----------------|-------------|
| Concept | ConceptNode | Basic concept |
| Predicate | PredicateNode | Predicate/relation |
| Variable | VariableNode | Variable reference |
| Number | NumberNode | Numeric value |
| String | StringNode | String value |
| List | ListLink | Ordered collection |
| Set | SetLink | Unordered collection |
| Rule | ImplicationLink | Logical implication |
| Equal | EqualLink | Equality relation |
| And | AndLink | Logical AND |
| Or | OrLink | Logical OR |
| Not | NotLink | Logical NOT |
| Function | LambdaLink | Lambda function |
| Evaluation | EvaluationLink | Predicate evaluation |
| Execution | ExecutionLink | Procedure execution |
| Plus | PlusLink | Addition |
| Times | TimesLink | Multiplication |
| Power | PowerLink | Exponentiation |
| ...and more | | |

---

### 2. Symbolic Patterns (`src/symbolic_patterns.py`)

Provides 8 pre-built symbolic computation patterns for common tasks.

#### Available Patterns

1. **symbolic_solve** - Solve symbolic equations
2. **pattern_matching** - Advanced pattern matching
3. **inference_chain** - Multi-step logical inference
4. **optimization** - Symbolic optimization
5. **simplification** - Expression simplification
6. **differentiation** - Symbolic differentiation
7. **integration** - Symbolic integration
8. **knowledge_extraction** - Extract knowledge from text

#### Usage Example

```python
from src.symbolic_patterns import get_pattern_registry

# Get global pattern registry
registry = get_pattern_registry()

# List available patterns
patterns = registry.list_patterns()
print(f"Available patterns: {patterns}")

# Get a specific pattern
solve_pattern = registry.get_pattern("symbolic_solve")
print(f"Description: {solve_pattern.description}")

# Validate input
data = {"equation": "x^2 - 4 == 0", "variable": "x"}
is_valid = solve_pattern.validate_input(data)

# Execute pattern (requires bridge)
# result = registry.execute_pattern("symbolic_solve", bridge, data)
```

#### Pattern Details

##### symbolic_solve
**Description**: Solve symbolic equations using Wolfram and store results in AtomSpace

**Input**:
```python
{
    "equation": str,     # Equation to solve (e.g., "x^2 - 4 == 0")
    "variable": str      # Variable to solve for (default: "x")
}
```

**Output**:
```python
{
    "success": bool,
    "pattern": "symbolic_solve",
    "equation": str,
    "variable": str,
    "solution": str      # Wolfram solution
}
```

##### differentiation
**Description**: Compute symbolic derivatives

**Input**:
```python
{
    "expression": str,   # Expression to differentiate
    "variable": str,     # Variable to differentiate with respect to
    "order": int         # Order of derivative (default: 1)
}
```

**Output**:
```python
{
    "success": bool,
    "pattern": "differentiation",
    "expression": str,
    "variable": str,
    "order": int,
    "derivative": str
}
```

##### optimization
**Description**: Optimize symbolic expressions using Wolfram

**Input**:
```python
{
    "objective": str,         # Objective function
    "variables": list[str],   # Variables to optimize
    "constraints": list[str], # Optional constraints
    "method": str            # Optimization method (default: "NMinimize")
}
```

**Output**:
```python
{
    "success": bool,
    "pattern": "optimization",
    "objective": str,
    "variables": list[str],
    "result": str
}
```

---

### 3. Bridge Monitor (`src/bridge_monitor.py`)

Monitors bridge health, performance, and provides metrics.

#### Features
- Real-time computation tracking
- Success/failure rate monitoring
- Execution time statistics
- Health status checking
- Automated alerting
- Metric export

#### Usage Example

```python
from src.bridge_monitor import get_bridge_monitor

# Get global monitor
monitor = get_bridge_monitor()

# Start monitoring
monitor.start_monitoring()

# Record a computation
start_time = monitor.record_computation_start()
# ... perform computation ...
monitor.record_computation_end(start_time, success=True)

# Record specific events
monitor.record_wolfram_call()
monitor.record_atomspace_operation()
monitor.record_error("error_type", "error message")

# Get metrics
metrics = monitor.get_metrics()
print(f"Total computations: {metrics['total_computations']}")
print(f"Success rate: {metrics['success_rate']:.1f}%")
print(f"Avg execution time: {metrics['average_execution_time']:.3f}s")
print(f"Throughput: {metrics['throughput']:.2f} ops/sec")

# Check health
health = monitor.get_health_status()
print(f"Status: {health['status']}")  # healthy, degraded, unhealthy
print(f"Issues: {health['issues']}")

# Get full report
report = monitor.get_full_report()

# Export metrics
monitor.export_metrics("/path/to/metrics.json")

# Stop monitoring
monitor.stop_monitoring()
```

#### Metrics Provided

- `total_computations`: Total number of computations
- `successful_computations`: Number of successful computations
- `failed_computations`: Number of failed computations
- `success_rate`: Percentage of successful computations
- `average_execution_time`: Average time per computation (seconds)
- `total_execution_time`: Total time spent in computations
- `wolfram_calls`: Number of Wolfram Language calls
- `atomspace_operations`: Number of AtomSpace operations
- `kernel_restarts`: Number of kernel restarts
- `uptime_seconds`: Total uptime
- `throughput`: Computations per second
- `error_count`: Total number of errors
- `recent_errors`: Last 5 errors

#### Health Status Levels

- **healthy**: System operating normally (< 20% error rate, normal execution times)
- **degraded**: System experiencing issues (20-50% error rate or slow execution)
- **unhealthy**: System critical (> 50% error rate or multiple kernel restarts)

---

### 4. Wolfram-OpenCog Integration (`src/wolfram_opencog_integration.py`)

Main integration manager coordinating all components.

#### Features
- Kernel pool management
- Asynchronous computation processing
- Pattern-based execution
- Automatic monitoring
- Component coordination

#### Usage Example

```python
from src.wolfram_opencog_integration import WolframOpenCogIntegration

# Create integration instance
integration = WolframOpenCogIntegration()

# Start the system
if integration.start():
    print("Integration started successfully")
    
    # Submit computations
    integration.submit_computation("symbolic_solve", {
        "equation": "x^2 - 9 == 0",
        "variable": "x"
    })
    
    # Get result
    result = integration.get_result(timeout=5.0)
    if result and result.get("success"):
        print(f"Solution: {result.get('solution')}")
    
    # Get statistics
    stats = integration.get_integration_stats()
    print(f"Computations completed: {stats['computations_completed']}")
    print(f"Available patterns: {stats['available_patterns']}")
    print(f"Health: {stats['health_status']['status']}")
    
    # Run integration tests
    test_results = integration.test_integration()
    
    # Stop the system
    integration.stop()
```

#### Integration Statistics

The `get_integration_stats()` method returns comprehensive statistics including:

```python
{
    "computations_completed": int,
    "wolfram_calls": int,
    "opencog_operations": int,
    "bridge_errors": int,
    "start_time": float,
    "uptime_seconds": float,
    "running": bool,
    "computation_queue_size": int,
    "result_queue_size": int,
    "symbolic_processor": {...},     # Processor stats
    "wolfram_bridge": {...},         # Bridge stats
    "monitor_metrics": {...},        # Monitor metrics
    "health_status": {...},          # Health status
    "available_patterns": [...],     # Pattern list
    "type_mappings_count": {...}     # Type mapping counts
}
```

---

## Complete Workflow Example

Here's a complete example showing how to use all components together:

```python
from src.wolfram_opencog_integration import WolframOpenCogIntegration
from src.type_registry import get_type_registry
from src.symbolic_patterns import get_pattern_registry
from src.bridge_monitor import get_bridge_monitor

# Initialize integration
integration = WolframOpenCogIntegration()

# Start the system
if integration.start():
    # Example 1: Solve an equation
    integration.submit_computation("symbolic_solve", {
        "equation": "x^2 + 3*x - 4 == 0",
        "variable": "x"
    })
    result = integration.get_result()
    print(f"Equation solution: {result}")
    
    # Example 2: Differentiate a function
    integration.submit_computation("differentiation", {
        "expression": "x^3 + 2*x^2 - 5*x + 1",
        "variable": "x",
        "order": 1
    })
    result = integration.get_result()
    print(f"Derivative: {result}")
    
    # Example 3: Optimize a function
    integration.submit_computation("optimization", {
        "objective": "x^2 + y^2",
        "variables": ["x", "y"],
        "constraints": ["x + y == 1"]
    })
    result = integration.get_result()
    print(f"Optimization result: {result}")
    
    # Check system health
    stats = integration.get_integration_stats()
    health = stats["health_status"]
    print(f"System health: {health['status']}")
    
    if not health['is_healthy']:
        print(f"Issues: {health['issues']}")
    
    # Get performance metrics
    metrics = stats["monitor_metrics"]
    print(f"Success rate: {metrics['success_rate']:.1f}%")
    print(f"Average execution time: {metrics['average_execution_time']:.3f}s")
    print(f"Throughput: {metrics['throughput']:.2f} computations/sec")
    
    # Clean shutdown
    integration.stop()
```

---

## Configuration

### Kernel Pool Size

Adjust the number of Wolfram kernels in the pool:

```python
# In WolframOpenCogBridge initialization
self.wolfram_bridge.start_kernel_pool(pool_size=5)  # Default: 3
```

### Health Check Thresholds

Customize health check parameters:

```python
monitor = get_bridge_monitor()
monitor.config = {
    "max_error_rate": 0.2,              # 20% error rate threshold
    "max_avg_execution_time": 10.0,     # 10 seconds average
    "kernel_restart_threshold": 5,      # 5 restarts trigger warning
    "check_interval": 60                # Check every 60 seconds
}
```

### Custom Type Mappings

Register custom type mappings:

```python
registry = get_type_registry()
registry.register_type_mapping("MyWolframType", "MyAtomSpaceType")

# Register custom conversion handler
def convert_custom_type(wolfram_data):
    # Custom conversion logic
    return {"type": "CustomAtomType", ...}

registry.register_conversion_handler(
    "MyWolframType",
    to_atomspace=convert_custom_type
)
```

### Custom Patterns

Create and register custom computation patterns:

```python
from src.symbolic_patterns import SymbolicPattern

class MyCustomPattern(SymbolicPattern):
    def __init__(self):
        super().__init__(
            "my_pattern",
            "Description of my custom pattern"
        )
    
    def validate_input(self, data):
        return "required_field" in data
    
    def execute(self, bridge, data):
        # Custom execution logic
        return {"success": True, "result": ...}

# Register the pattern
registry = get_pattern_registry()
registry.register_pattern(MyCustomPattern())
```

---

## Error Handling

The bridge provides comprehensive error handling:

```python
try:
    integration = WolframOpenCogIntegration()
    integration.start()
    
    # Submit computation
    integration.submit_computation("symbolic_solve", data)
    result = integration.get_result(timeout=10.0)
    
    if result is None:
        print("Timeout: No result received")
    elif not result.get("success"):
        print(f"Computation failed: {result.get('error')}")
    else:
        print(f"Success: {result}")
        
except Exception as e:
    print(f"Error: {e}")
    # Check error logs
    monitor = get_bridge_monitor()
    metrics = monitor.get_metrics()
    print(f"Recent errors: {metrics['recent_errors']}")
finally:
    integration.stop()
```

---

## Testing

Run the comprehensive test suite:

```bash
python test_enhanced_bridge.py
```

This runs 27 tests covering:
- Type registry (7 tests)
- Symbolic patterns (5 tests)
- Bridge monitor (7 tests)
- Integration (8 tests)

---

## Performance Considerations

### Kernel Pool Sizing

- **Small workloads**: 2-3 kernels
- **Medium workloads**: 4-6 kernels
- **Large workloads**: 7-10 kernels

### Monitoring Overhead

- Monitoring adds < 1% overhead
- Health checks run every 60 seconds by default
- Adjust `check_interval` for different workloads

### Computation Queueing

- Queue size is unlimited by default
- Monitor `computation_queue_size` to detect backlog
- Results are available for 5 seconds by default (adjust timeout)

---

## Simulation Mode

When Wolfram Language or OpenCog are not available, the bridge operates in simulation mode:

- Wolfram calls return simulated responses
- AtomSpace uses in-memory simulation
- All patterns still execute
- Useful for development and testing

Check if simulation mode is active:

```python
stats = integration.get_integration_stats()
bridge_stats = stats.get("wolfram_bridge", {})
print(f"OpenCog available: {bridge_stats.get('opencog_available')}")
print(f"Simulation atoms: {bridge_stats.get('simulated_atoms', 0)}")
```

---

## Support and Resources

- **Source Code**: `/src/` directory
- **Tests**: `test_enhanced_bridge.py`
- **Examples**: See usage examples in this document
- **Architecture**: See `docs/architecture.md` for system design

---

## Version

Enhanced Bridge v1.0.0 - Foundational Architecture Complete

Last Updated: 2025-01-27
