#!/usr/bin/env python3
"""
WolfCog Symbolic State Monitor
Native symbolic monitoring system for WolfCog AGI-OS using AtomSpace-based state representation
No web dependencies - pure symbolic AGI monitoring integrated with the AtomSpace filesystem
"""

import json
import time
import threading
import psutil
from pathlib import Path
from datetime import datetime
import subprocess
import signal
import sys

class SymbolicStateMonitor:
    """
    Native symbolic monitoring system for WolfCog AGI-OS
    Integrates with AtomSpace filesystem for symbolic state representation
    """
    
    def __init__(self):
        self.running = False
        self.symbolic_state = {}
        self.atomspace_cache = {}
        self.space_states = {"u": {}, "e": {}, "s": {}}
        self.agent_states = {}
        self.daemon_states = {}
        self.performance_atoms = {}
        
        # Initialize symbolic state representation
        self.init_symbolic_state()
        
        # Setup signal handlers
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
        
    def init_symbolic_state(self):
        """Initialize symbolic state representation in AtomSpace format"""
        print("🧠 Initializing WolfCog Symbolic State Monitor...")
        print("🌌 Creating AtomSpace-based state representation...")
        
        # Create symbolic nodes for system components using AtomSpace concepts
        self.symbolic_state = {
            "system_node": {
                "type": "ConceptNode", 
                "name": "WolfCogSystem",
                "timestamp": datetime.now().isoformat(),
                "state": "monitoring",
                "atoms": []
            },
            "spaces": {
                "u_space": {
                    "type": "ConceptNode", 
                    "name": "UserSpace",
                    "state": "active",
                    "memory_atoms": [],
                    "symbolic_flows": []
                },
                "e_space": {
                    "type": "ConceptNode", 
                    "name": "ExecutionSpace", 
                    "state": "active",
                    "execution_atoms": [],
                    "task_flows": []
                },
                "s_space": {
                    "type": "ConceptNode", 
                    "name": "SystemSpace",
                    "state": "active", 
                    "system_atoms": [],
                    "agent_flows": []
                }
            },
            "agents": {
                "admin_agent": {
                    "type": "AgentNode",
                    "name": "AdminAgent", 
                    "state": "monitoring",
                    "cognitive_atoms": [],
                    "reasoning_links": []
                },
                "director_agent": {
                    "type": "AgentNode",
                    "name": "DirectorAgent",
                    "state": "reasoning", 
                    "logic_atoms": [],
                    "coordination_links": []
                },
                "conversational_agent": {
                    "type": "AgentNode", 
                    "name": "ConversationalAgent",
                    "state": "listening",
                    "interaction_atoms": [],
                    "dialogue_links": []
                }
            },
            "daemons": {
                "scheduler": {
                    "type": "ProcessNode", 
                    "name": "SchedulerDaemon",
                    "state": "active",
                    "scheduling_atoms": [],
                    "flow_links": []
                },
                "reflex": {
                    "type": "ProcessNode",
                    "name": "ReflexDaemon", 
                    "state": "monitoring",
                    "reactive_atoms": [],
                    "trigger_links": []
                },
                "task_processor": {
                    "type": "ProcessNode",
                    "name": "TaskProcessor",
                    "state": "processing",
                    "task_atoms": [],
                    "execution_links": []
                }
            },
            "performance": {
                "type": "PerformanceNode",
                "metrics_atoms": [],
                "health_links": [],
                "optimization_atoms": []
            }
        }
        
        print("✅ Symbolic state structure initialized")
        self._display_symbolic_topology()
        
    def _display_symbolic_topology(self):
        """Display the symbolic topology of the system"""
        print("\n🌐 WolfCog Symbolic Topology:")
        print("=" * 50)
        
        for category, items in self.symbolic_state.items():
            if category == "system_node":
                print(f"📡 {items['name']} ({items['type']})")
            elif category == "performance":
                print(f"📊 Performance Monitor ({items['type']})")
            else:
                print(f"\n🔸 {category.upper()}:")
                for item_name, item_data in items.items():
                    state_icon = self._get_state_icon(item_data.get('state', 'unknown'))
                    print(f"  {state_icon} {item_data['name']} ({item_data['type']})")
                    
    def _get_state_icon(self, state):
        """Get icon for component state"""
        state_icons = {
            "active": "🟢",
            "monitoring": "👁️",
            "reasoning": "🧠", 
            "listening": "👂",
            "processing": "⚙️",
            "healthy": "💚",
            "warning": "🟡",
            "error": "🔴",
            "unknown": "❓"
        }
        return state_icons.get(state, "❓")
        
    def start_monitoring(self):
        """Start symbolic state monitoring"""
        print("\n🚀 Starting WolfCog Symbolic State Monitor")
        print("🧠 Monitoring AtomSpace-based system state...")
        
        self.running = True
        
        # Start monitoring threads
        monitor_thread = threading.Thread(target=self._symbolic_monitor_loop)
        monitor_thread.daemon = True
        monitor_thread.start()
        
        atomspace_thread = threading.Thread(target=self._atomspace_sync_loop)
        atomspace_thread.daemon = True
        atomspace_thread.start()
        
        display_thread = threading.Thread(target=self._display_loop)
        display_thread.daemon = True
        display_thread.start()
        
        print("✅ Symbolic monitoring active")
        print("📋 Real-time symbolic state display starting...")
        
        # Keep main thread alive
        try:
            while self.running:
                time.sleep(1)
        except KeyboardInterrupt:
            self.stop_monitoring()
            
    def _symbolic_monitor_loop(self):
        """Main symbolic monitoring loop"""
        while self.running:
            try:
                # Monitor symbolic spaces
                self._monitor_symbolic_spaces()
                
                # Monitor agents
                self._monitor_agents()
                
                # Monitor daemons
                self._monitor_daemons()
                
                # Monitor performance atoms
                self._monitor_performance_atoms()
                
                # Update AtomSpace cache
                self._update_atomspace_cache()
                
            except Exception as e:
                print(f"❌ Error in symbolic monitoring: {e}")
                
            time.sleep(5)  # Monitor every 5 seconds
            
    def _monitor_symbolic_spaces(self):
        """Monitor the three symbolic spaces (u/e/s)"""
        spaces_dir = Path("/workspaces/wolfcog/spaces")
        
        for space_name in ["u", "e", "s"]:
            space_dir = spaces_dir / space_name
            if space_dir.exists():
                # Count symbolic files
                memory_files = list(space_dir.glob("*.json"))
                symbolic_files = list(space_dir.glob("*.txt"))
                scheme_files = list(space_dir.glob("*.scm"))
                
                # Calculate total symbolic content size
                total_size = sum(f.stat().st_size for f in space_dir.iterdir() if f.is_file())
                
                # Update symbolic space state
                space_data = self.symbolic_state["spaces"][f"{space_name}_space"]
                space_data.update({
                    "memory_files": len(memory_files),
                    "symbolic_files": len(symbolic_files), 
                    "scheme_files": len(scheme_files),
                    "total_size_bytes": total_size,
                    "last_modified": max((f.stat().st_mtime for f in space_dir.iterdir() if f.is_file()), default=0),
                    "atoms_count": self._count_symbolic_atoms(space_dir)
                })
                
                # Create symbolic atoms for this space
                space_data["memory_atoms"] = self._extract_memory_atoms(space_dir)
                space_data["symbolic_flows"] = self._extract_symbolic_flows(space_dir)
                
    def _count_symbolic_atoms(self, space_dir):
        """Count symbolic atoms in a space directory"""
        atoms_count = 0
        
        # Count JSON memory structures
        for json_file in space_dir.glob("*.json"):
            try:
                with open(json_file) as f:
                    data = json.load(f)
                    if isinstance(data, dict):
                        atoms_count += len(data)
                    elif isinstance(data, list):
                        atoms_count += len(data)
            except:
                pass
                
        # Count symbolic expressions in text files
        for txt_file in space_dir.glob("*.txt"):
            try:
                with open(txt_file) as f:
                    content = f.read()
                    # Count symbolic expressions (simplified)
                    atoms_count += content.count("∇") + content.count("∈") + content.count("⊗")
            except:
                pass
                
        return atoms_count
        
    def _extract_memory_atoms(self, space_dir):
        """Extract memory atoms from space directory"""
        memory_atoms = []
        
        for json_file in space_dir.glob("*.json"):
            try:
                with open(json_file) as f:
                    data = json.load(f)
                    memory_atoms.append({
                        "type": "MemoryAtom",
                        "source": json_file.name,
                        "content_type": type(data).__name__,
                        "size": len(str(data)),
                        "timestamp": json_file.stat().st_mtime
                    })
            except:
                pass
                
        return memory_atoms[:10]  # Limit for display
        
    def _extract_symbolic_flows(self, space_dir):
        """Extract symbolic flows from space directory"""
        flows = []
        
        for txt_file in space_dir.glob("*.txt"):
            try:
                with open(txt_file) as f:
                    content = f.read()
                    flows.append({
                        "type": "SymbolicFlow",
                        "source": txt_file.name,
                        "expressions": content.count("∇"),
                        "complexity": len(content),
                        "timestamp": txt_file.stat().st_mtime
                    })
            except:
                pass
                
        return flows[:5]  # Limit for display
        
    def _monitor_agents(self):
        """Monitor persistent agents"""
        agents_dir = Path("/workspaces/wolfcog/agents")
        
        for agent_name in ["admin_agent", "director_agent", "conversational_agent"]:
            agent_file = agents_dir / f"{agent_name}.py"
            
            if agent_file.exists():
                # Check if agent process is running
                agent_running = self._check_process_running(agent_name)
                
                agent_data = self.symbolic_state["agents"][agent_name]
                agent_data.update({
                    "process_running": agent_running,
                    "last_modified": agent_file.stat().st_mtime,
                    "file_size": agent_file.stat().st_size,
                    "cognitive_load": self._calculate_cognitive_load(agent_name)
                })
                
                # Update agent state based on activity
                if agent_running:
                    if agent_name == "admin_agent":
                        agent_data["state"] = "monitoring"
                    elif agent_name == "director_agent": 
                        agent_data["state"] = "reasoning"
                    else:
                        agent_data["state"] = "active"
                else:
                    agent_data["state"] = "inactive"
                    
    def _monitor_daemons(self):
        """Monitor system daemons"""
        daemons_dir = Path("/workspaces/wolfcog/daemons")
        task_daemon = Path("/workspaces/wolfcog/opencog/ecron-task-daemon-enhanced.py")
        
        # Monitor scheduler daemon
        scheduler_dir = daemons_dir / "scheduler"
        if scheduler_dir.exists():
            scheduler_running = self._check_process_running("scheduler")
            self.symbolic_state["daemons"]["scheduler"].update({
                "process_running": scheduler_running,
                "state": "active" if scheduler_running else "inactive"
            })
            
        # Monitor reflex daemon  
        reflex_dir = daemons_dir / "reflex"
        if reflex_dir.exists():
            reflex_running = self._check_process_running("reflex")
            self.symbolic_state["daemons"]["reflex"].update({
                "process_running": reflex_running,
                "state": "monitoring" if reflex_running else "inactive"
            })
            
        # Monitor task processor
        if task_daemon.exists():
            task_running = self._check_process_running("ecron-task-daemon")
            self.symbolic_state["daemons"]["task_processor"].update({
                "process_running": task_running,
                "state": "processing" if task_running else "inactive",
                "last_modified": task_daemon.stat().st_mtime
            })
            
    def _monitor_performance_atoms(self):
        """Monitor system performance as symbolic atoms"""
        try:
            # System performance metrics
            cpu_percent = psutil.cpu_percent(interval=1)
            memory = psutil.virtual_memory()
            
            # WolfCog specific metrics
            wolfcog_metrics = self._collect_wolfcog_metrics()
            
            # Create performance atoms
            self.performance_atoms = {
                "cpu_atom": {
                    "type": "PerformanceAtom",
                    "metric": "cpu_usage",
                    "value": cpu_percent,
                    "unit": "percent",
                    "timestamp": time.time()
                },
                "memory_atom": {
                    "type": "PerformanceAtom", 
                    "metric": "memory_usage",
                    "value": memory.percent,
                    "unit": "percent",
                    "timestamp": time.time()
                },
                "task_throughput_atom": {
                    "type": "PerformanceAtom",
                    "metric": "task_throughput", 
                    "value": wolfcog_metrics.get("task_throughput", 0),
                    "unit": "tasks_per_second",
                    "timestamp": time.time()
                },
                "space_size_atom": {
                    "type": "PerformanceAtom",
                    "metric": "total_space_size",
                    "value": wolfcog_metrics.get("total_space_size", 0),
                    "unit": "bytes",
                    "timestamp": time.time()
                }
            }
            
            # Update system performance state
            self.symbolic_state["performance"]["metrics_atoms"] = list(self.performance_atoms.values())
            
        except Exception as e:
            print(f"❌ Error monitoring performance atoms: {e}")
            
    def _collect_wolfcog_metrics(self):
        """Collect WolfCog specific metrics"""
        metrics = {"task_throughput": 0, "total_space_size": 0}
        
        try:
            # Read performance metrics if available
            metrics_file = Path("/workspaces/wolfcog/performance_metrics.json")
            if metrics_file.exists():
                with open(metrics_file) as f:
                    data = json.load(f)
                    wolfcog_data = data.get("wolfcog", {})
                    tasks = wolfcog_data.get("tasks", {})
                    metrics["task_throughput"] = tasks.get("throughput", 0)
                    
            # Calculate total space size
            spaces_dir = Path("/workspaces/wolfcog/spaces")
            if spaces_dir.exists():
                total_size = 0
                for space_dir in spaces_dir.iterdir():
                    if space_dir.is_dir():
                        for file_path in space_dir.iterdir():
                            if file_path.is_file():
                                total_size += file_path.stat().st_size
                metrics["total_space_size"] = total_size
                
        except Exception as e:
            print(f"❌ Error collecting WolfCog metrics: {e}")
            
        return metrics
        
    def _check_process_running(self, process_name):
        """Check if a process is running"""
        try:
            for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
                if process_name.lower() in ' '.join(proc.info['cmdline'] or []).lower():
                    return True
        except:
            pass
        return False
        
    def _calculate_cognitive_load(self, agent_name):
        """Calculate cognitive load for an agent"""
        # Simplified cognitive load calculation
        base_load = 0.1
        
        # Check recent task activity
        task_dir = Path("/tmp/ecron_tasks")
        if task_dir.exists():
            recent_tasks = len([f for f in task_dir.glob("*.processed") 
                              if time.time() - f.stat().st_mtime < 300])  # Last 5 minutes
            base_load += recent_tasks * 0.05
            
        return min(base_load, 1.0)  # Cap at 100%
        
    def _atomspace_sync_loop(self):
        """Synchronize with AtomSpace structures"""
        while self.running:
            try:
                # Update AtomSpace cache with current symbolic state
                self.atomspace_cache = {
                    "timestamp": datetime.now().isoformat(),
                    "symbolic_nodes": self._generate_atomspace_nodes(),
                    "symbolic_links": self._generate_atomspace_links(),
                    "evaluation_atoms": self._generate_evaluation_atoms()
                }
                
                # Write AtomSpace representation to cache
                self._write_atomspace_cache()
                
            except Exception as e:
                print(f"❌ Error in AtomSpace sync: {e}")
                
            time.sleep(10)  # Sync every 10 seconds
            
    def _generate_atomspace_nodes(self):
        """Generate AtomSpace nodes from symbolic state"""
        nodes = []
        
        for category, items in self.symbolic_state.items():
            if category in ["spaces", "agents", "daemons"]:
                for item_name, item_data in items.items():
                    nodes.append({
                        "type": item_data["type"],
                        "name": item_data["name"],
                        "category": category,
                        "state": item_data["state"]
                    })
                    
        return nodes
        
    def _generate_atomspace_links(self):
        """Generate AtomSpace links between symbolic components"""
        links = []
        
        # Create hierarchical links
        for space_name in ["u_space", "e_space", "s_space"]:
            links.append({
                "type": "InheritanceLink",
                "source": self.symbolic_state["spaces"][space_name]["name"],
                "target": "WolfCogSystem"
            })
            
        # Create agent-space relationships
        links.extend([
            {"type": "MemberLink", "source": "AdminAgent", "target": "SystemSpace"},
            {"type": "MemberLink", "source": "DirectorAgent", "target": "ExecutionSpace"}, 
            {"type": "MemberLink", "source": "ConversationalAgent", "target": "UserSpace"}
        ])
        
        return links
        
    def _generate_evaluation_atoms(self):
        """Generate evaluation atoms for system metrics"""
        evaluations = []
        
        for atom_name, atom_data in self.performance_atoms.items():
            evaluations.append({
                "type": "EvaluationLink",
                "predicate": atom_data["metric"],
                "subject": "WolfCogSystem",
                "value": atom_data["value"],
                "unit": atom_data["unit"]
            })
            
        return evaluations
        
    def _write_atomspace_cache(self):
        """Write AtomSpace cache to filesystem"""
        try:
            cache_dir = Path("/workspaces/wolfcog/opencog/atomspace-mirror")
            cache_dir.mkdir(exist_ok=True)
            
            # Write symbolic state cache
            state_file = cache_dir / "symbolic_state.json"
            with open(state_file, 'w') as f:
                json.dump(self.symbolic_state, f, indent=2, default=str)
                
            # Write AtomSpace cache
            atomspace_file = cache_dir / "atomspace_cache.json" 
            with open(atomspace_file, 'w') as f:
                json.dump(self.atomspace_cache, f, indent=2, default=str)
                
        except Exception as e:
            print(f"❌ Error writing AtomSpace cache: {e}")
            
    def _display_loop(self):
        """Display real-time symbolic state"""
        while self.running:
            try:
                self._display_current_state()
                time.sleep(15)  # Update display every 15 seconds
            except Exception as e:
                print(f"❌ Error in display loop: {e}")
                time.sleep(5)
                
    def _display_current_state(self):
        """Display current symbolic state"""
        print("\n" + "="*70)
        print("🐺 WolfCog Symbolic State Monitor - Real-time Display")
        print(f"🕒 {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*70)
        
        # Display spaces
        print("\n🌌 SYMBOLIC SPACES:")
        for space_name, space_data in self.symbolic_state["spaces"].items():
            state_icon = self._get_state_icon(space_data["state"])
            atoms_count = space_data.get("atoms_count", 0)
            size_mb = space_data.get("total_size_bytes", 0) / 1024 / 1024
            print(f"  {state_icon} {space_data['name']}: {atoms_count} atoms, {size_mb:.2f} MB")
            
        # Display agents
        print("\n🤖 PERSISTENT AGENTS:")
        for agent_name, agent_data in self.symbolic_state["agents"].items():
            state_icon = self._get_state_icon(agent_data["state"])
            cognitive_load = agent_data.get("cognitive_load", 0) * 100
            running_status = "🟢" if agent_data.get("process_running") else "🔴"
            print(f"  {state_icon} {agent_data['name']}: {running_status} Load: {cognitive_load:.1f}%")
            
        # Display daemons
        print("\n⚙️ SYSTEM DAEMONS:")
        for daemon_name, daemon_data in self.symbolic_state["daemons"].items():
            state_icon = self._get_state_icon(daemon_data["state"])
            running_status = "🟢" if daemon_data.get("process_running") else "🔴"
            print(f"  {state_icon} {daemon_data['name']}: {running_status}")
            
        # Display performance atoms
        print("\n📊 PERFORMANCE ATOMS:")
        for atom_name, atom_data in self.performance_atoms.items():
            metric_name = atom_data["metric"].replace("_", " ").title()
            value = atom_data["value"]
            unit = atom_data["unit"]
            print(f"  📈 {metric_name}: {value:.2f} {unit}")
            
        # Display AtomSpace sync status
        print(f"\n🧠 ATOMSPACE CACHE: Updated {datetime.now().strftime('%H:%M:%S')}")
        nodes_count = len(self.atomspace_cache.get("symbolic_nodes", []))
        links_count = len(self.atomspace_cache.get("symbolic_links", []))
        print(f"  🔗 Nodes: {nodes_count}, Links: {links_count}")
        
        print("-" * 70)
        
    def _update_atomspace_cache(self):
        """Update the local AtomSpace cache"""
        try:
            # This would interface with the actual OpenCog AtomSpace
            # For now, we simulate AtomSpace operations
            cache_data = {
                "last_update": datetime.now().isoformat(),
                "total_atoms": sum(space.get("atoms_count", 0) for space in self.symbolic_state["spaces"].values()),
                "active_agents": sum(1 for agent in self.symbolic_state["agents"].values() if agent["state"] != "inactive"),
                "system_health": self._calculate_system_health()
            }
            
            # Update symbolic state with cache information
            self.symbolic_state["system_node"]["atomspace_cache"] = cache_data
            
        except Exception as e:
            print(f"❌ Error updating AtomSpace cache: {e}")
            
    def _calculate_system_health(self):
        """Calculate overall system health"""
        health_score = 0.0
        total_components = 0
        
        # Check spaces
        for space_data in self.symbolic_state["spaces"].values():
            total_components += 1
            if space_data["state"] == "active":
                health_score += 1.0
                
        # Check agents
        for agent_data in self.symbolic_state["agents"].values():
            total_components += 1
            if agent_data["state"] in ["monitoring", "reasoning", "active"]:
                health_score += 1.0
                
        # Check daemons
        for daemon_data in self.symbolic_state["daemons"].values():
            total_components += 1
            if daemon_data["state"] in ["active", "processing", "monitoring"]:
                health_score += 1.0
                
        if total_components > 0:
            health_percentage = (health_score / total_components) * 100
            if health_percentage >= 90:
                return "excellent"
            elif health_percentage >= 75:
                return "good"
            elif health_percentage >= 50:
                return "warning"
            else:
                return "critical"
        
        return "unknown"
        
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals"""
        print(f"\n🛑 Received signal {signum}, shutting down symbolic monitor...")
        self.stop_monitoring()
        
    def stop_monitoring(self):
        """Stop symbolic monitoring"""
        print("🛑 Stopping WolfCog Symbolic State Monitor...")
        self.running = False
        
        # Write final state to cache
        self._write_atomspace_cache()
        
        print("✅ Symbolic monitor stopped")
        sys.exit(0)

def main():
    """Main function for standalone execution"""
    monitor = SymbolicStateMonitor()
    
    try:
        print("🚀 Starting WolfCog Symbolic State Monitor")
        print("🧠 Native AGI-OS monitoring with AtomSpace integration")
        print("🌌 No web dependencies - pure symbolic monitoring")
        print("📡 Press Ctrl+C to stop")
        
        monitor.start_monitoring()
        
    except KeyboardInterrupt:
        print("\n🛑 Keyboard interrupt received")
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        monitor.stop_monitoring()

if __name__ == "__main__":
    main()
