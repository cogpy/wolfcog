#!/usr/bin/env python3
"""
Scheduler Daemon - Main scheduling engine for WolfCog AGI-OS
Runs ecron and schedules symbolic flows across the AGI system
"""

import time
import threading
import subprocess
import json
from pathlib import Path

class SchedulerDaemon:
    def __init__(self):
        self.running = False
        self.ecron_process = None
        self.task_queue = []
        self.active_flows = {}
        self.priorities = {}
        
    def start(self):
        """Start the scheduler daemon"""
        print("⏰ Starting Scheduler Daemon...")
        self.running = True
        
        # Start ecron scheduler thread
        ecron_thread = threading.Thread(target=self.run_ecron_scheduler)
        ecron_thread.daemon = True
        ecron_thread.start()
        
        # Start flow management thread
        flow_thread = threading.Thread(target=self.manage_flows)
        flow_thread.daemon = True
        flow_thread.start()
        
        # Start priority management thread
        priority_thread = threading.Thread(target=self.manage_priorities)
        priority_thread.daemon = True
        priority_thread.start()
        
        print("📅 Scheduler Daemon managing symbolic flows...")
    
    def run_ecron_scheduler(self):
        """Run the ecron symbolic scheduler"""
        while self.running:
            try:
                print("🔄 Running Ecron symbolic scheduler cycle...")
                
                # Simulate ecron execution (in real implementation would call Wolfram)
                self.process_symbolic_tasks()
                
                # Check for new tasks from ecron output
                self.collect_scheduled_tasks()
                
            except Exception as e:
                print(f"❌ Ecron scheduler error: {e}")
            
            time.sleep(5)  # Scheduler cycle every 5 seconds
    
    def process_symbolic_tasks(self):
        """Process symbolic tasks through ecron"""
        task_path = Path("/tmp/ecron_tasks")
        if task_path.exists():
            for task_file in task_path.glob("*.json"):
                try:
                    with open(task_file, 'r') as f:
                        task_data = json.load(f)
                    
                    # Schedule the task
                    self.schedule_task(task_data)
                    
                except Exception as e:
                    print(f"❌ Error processing task {task_file}: {e}")
    
    def schedule_task(self, task_data):
        """Schedule a symbolic task"""
        task_id = f"task_{len(self.task_queue)}"
        priority = self.calculate_priority(task_data)
        
        scheduled_task = {
            "id": task_id,
            "data": task_data,
            "priority": priority,
            "scheduled_time": time.time(),
            "status": "scheduled"
        }
        
        self.task_queue.append(scheduled_task)
        self.priorities[task_id] = priority
        
        print(f"📋 Scheduled task {task_id} with priority {priority}")
    
    def calculate_priority(self, task_data):
        """Calculate task priority based on space and type"""
        space = task_data.get("space", "e")
        flow_type = task_data.get("flow", "default")
        
        # Priority matrix
        space_priority = {"s": 1, "e": 2, "u": 3}  # System > Execution > User
        
        base_priority = space_priority.get(space, 2)
        
        # Adjust based on flow type
        if "critical" in str(flow_type):
            base_priority -= 1
        elif "background" in str(flow_type):
            base_priority += 1
            
        return max(1, base_priority)  # Ensure priority is at least 1
    
    def collect_scheduled_tasks(self):
        """Collect tasks that have been scheduled by ecron"""
        # Sort tasks by priority
        self.task_queue.sort(key=lambda x: x["priority"])
        
        # Process high priority tasks first
        while self.task_queue and self.task_queue[0]["priority"] <= 2:
            task = self.task_queue.pop(0)
            self.execute_task(task)
    
    def execute_task(self, task):
        """Execute a scheduled task"""
        task_id = task["id"]
        print(f"⚡ Executing task: {task_id}")
        
        task["status"] = "executing"
        task["execution_time"] = time.time()
        
        # Add to active flows
        self.active_flows[task_id] = task
        
        # Simulate task execution
        execution_thread = threading.Thread(target=self.simulate_execution, args=(task,))
        execution_thread.daemon = True
        execution_thread.start()
    
    def simulate_execution(self, task):
        """Simulate task execution"""
        task_id = task["id"]
        
        # Simulate variable execution time based on complexity
        execution_time = 1 + (task["priority"] * 0.5)
        time.sleep(execution_time)
        
        # Mark as completed
        task["status"] = "completed"
        task["completion_time"] = time.time()
        
        print(f"✅ Completed task: {task_id}")
        
        # Remove from active flows
        if task_id in self.active_flows:
            del self.active_flows[task_id]
    
    def manage_flows(self):
        """Manage symbolic flow coordination"""
        while self.running:
            try:
                # Monitor active flows
                self.monitor_active_flows()
                
                # Coordinate with other daemons
                self.coordinate_flows()
                
            except Exception as e:
                print(f"❌ Flow management error: {e}")
            
            time.sleep(3)  # Flow management cycle every 3 seconds
    
    def monitor_active_flows(self):
        """Monitor active symbolic flows"""
        if self.active_flows:
            print(f"🌊 Monitoring {len(self.active_flows)} active flows")
            
            # Check for stalled flows
            current_time = time.time()
            for task_id, task in list(self.active_flows.items()):
                execution_time = current_time - task.get("execution_time", current_time)
                if execution_time > 30:  # 30 second timeout
                    print(f"⚠️ Flow {task_id} appears stalled")
                    self.handle_stalled_flow(task_id)
    
    def handle_stalled_flow(self, task_id):
        """Handle a stalled flow"""
        print(f"🔄 Handling stalled flow: {task_id}")
        
        if task_id in self.active_flows:
            task = self.active_flows[task_id]
            task["status"] = "stalled"
            
            # Reschedule with lower priority
            task["priority"] += 1
            self.task_queue.append(task)
            
            del self.active_flows[task_id]
    
    def coordinate_flows(self):
        """Coordinate flows with other system components"""
        # Placeholder for coordination with OpenCog, agents, etc.
        pass
    
    def manage_priorities(self):
        """Manage task priorities and dependencies"""
        while self.running:
            try:
                # Adjust priorities based on system state
                self.adjust_priorities()
                
                # Handle dependencies
                self.resolve_dependencies()
                
            except Exception as e:
                print(f"❌ Priority management error: {e}")
            
            time.sleep(10)  # Priority adjustment every 10 seconds
    
    def adjust_priorities(self):
        """Adjust task priorities based on system state"""
        # Increase priority of old tasks
        current_time = time.time()
        for task in self.task_queue:
            age = current_time - task["scheduled_time"]
            if age > 60:  # Tasks older than 1 minute
                task["priority"] = max(1, task["priority"] - 1)
                print(f"⬆️ Increased priority for aged task: {task['id']}")
    
    def resolve_dependencies(self):
        """Resolve task dependencies"""
        # Placeholder for dependency resolution
        pass
    
    def get_status(self):
        """Get scheduler daemon status"""
        return {
            "running": self.running,
            "queue_size": len(self.task_queue),
            "active_flows": len(self.active_flows),
            "total_priorities": len(self.priorities)
        }
    
    def stop(self):
        """Stop the scheduler daemon"""
        print("🛑 Stopping Scheduler Daemon...")
        self.running = False
        
        if self.ecron_process:
            self.ecron_process.terminate()

if __name__ == "__main__":
    daemon = SchedulerDaemon()
    try:
        daemon.start()
        # Keep daemon running
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        daemon.stop()
        print("👋 Scheduler Daemon stopped.")