#!/usr/bin/env python3
"""
WolfCog Memory Evolution Tracker
Implements symbolic diff and GitLink snapshots of memory graphs
Allows rollback, lineage tracing, and symbolic mutation evaluation
"""

import os
import json
import subprocess
import threading
import hashlib
from pathlib import Path
from datetime import datetime
import shutil

class MemoryEvolutionTracker:
    def __init__(self):
        self.running = False
        self.memory_repo_path = Path("/tmp/wolfcog_memory_evolution")
        self.snapshots_path = self.memory_repo_path / "snapshots"
        self.diffs_path = self.memory_repo_path / "diffs"
        self.lineage_path = self.memory_repo_path / "lineage"
        self.mutations_path = self.memory_repo_path / "mutations"
        self.update_interval = 10  # seconds
        self.ensure_directories()
        self.initialize_git_repo()
        
    def ensure_directories(self):
        """Ensure required directories exist"""
        self.memory_repo_path.mkdir(exist_ok=True)
        self.snapshots_path.mkdir(exist_ok=True)
        self.diffs_path.mkdir(exist_ok=True)
        self.lineage_path.mkdir(exist_ok=True)
        self.mutations_path.mkdir(exist_ok=True)
        
    def initialize_git_repo(self):
        """Initialize Git repository for memory evolution tracking"""
        if not (self.memory_repo_path / ".git").exists():
            print("🔧 Initializing memory evolution Git repository...")
            subprocess.run(["git", "init"], cwd=self.memory_repo_path, capture_output=True)
            subprocess.run(["git", "config", "user.name", "WolfCog-MemoryTracker"], 
                         cwd=self.memory_repo_path, capture_output=True)
            subprocess.run(["git", "config", "user.email", "memory@wolfcog.ai"], 
                         cwd=self.memory_repo_path, capture_output=True)
            print("✅ Memory evolution repository initialized")
        
    def capture_memory_snapshot(self):
        """Capture current state of symbolic memory"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        snapshot = {
            "timestamp": timestamp,
            "spaces": {},
            "checksum": "",
            "metadata": {
                "total_files": 0,
                "total_size": 0,
                "evolution_step": self.get_evolution_step()
            }
        }
        
        # Capture each symbolic space
        for space in ["u", "e", "s"]:
            space_snapshot = self.capture_space_snapshot(space)
            snapshot["spaces"][space] = space_snapshot
            snapshot["metadata"]["total_files"] += space_snapshot["file_count"]
            snapshot["metadata"]["total_size"] += space_snapshot["total_size"]
            
        # Calculate overall checksum
        snapshot["checksum"] = self.calculate_snapshot_checksum(snapshot)
        
        # Save snapshot
        snapshot_file = self.snapshots_path / f"snapshot_{timestamp}.json"
        with open(snapshot_file, 'w') as f:
            json.dump(snapshot, f, indent=2)
            
        print(f"📸 Memory snapshot captured: {timestamp}")
        return snapshot
        
    def capture_space_snapshot(self, space):
        """Capture snapshot of a specific symbolic space"""
        space_path = Path(f"spaces/{space}")
        space_snapshot = {
            "space": space,
            "files": {},
            "file_count": 0,
            "total_size": 0,
            "last_modified": None
        }
        
        if not space_path.exists():
            return space_snapshot
            
        # Capture all files in the space
        for file_path in space_path.rglob("*"):
            if file_path.is_file():
                relative_path = str(file_path.relative_to(space_path))
                file_info = {
                    "size": file_path.stat().st_size,
                    "modified": datetime.fromtimestamp(file_path.stat().st_mtime).isoformat(),
                    "checksum": self.calculate_file_checksum(file_path)
                }
                
                space_snapshot["files"][relative_path] = file_info
                space_snapshot["file_count"] += 1
                space_snapshot["total_size"] += file_info["size"]
                
                # Track latest modification
                if not space_snapshot["last_modified"] or file_info["modified"] > space_snapshot["last_modified"]:
                    space_snapshot["last_modified"] = file_info["modified"]
                    
        return space_snapshot
        
    def calculate_file_checksum(self, file_path):
        """Calculate MD5 checksum of a file"""
        try:
            with open(file_path, 'rb') as f:
                content = f.read()
                return hashlib.md5(content).hexdigest()
        except:
            return "unknown"
            
    def calculate_snapshot_checksum(self, snapshot):
        """Calculate checksum of entire snapshot"""
        # Create deterministic string representation
        snapshot_str = json.dumps(snapshot["spaces"], sort_keys=True)
        return hashlib.md5(snapshot_str.encode()).hexdigest()
        
    def get_evolution_step(self):
        """Get current evolution step number"""
        snapshot_files = list(self.snapshots_path.glob("snapshot_*.json"))
        return len(snapshot_files)
        
    def create_symbolic_diff(self, snapshot1, snapshot2):
        """Create symbolic diff between two snapshots"""
        diff = {
            "timestamp": datetime.now().isoformat(),
            "from_snapshot": snapshot1["timestamp"],
            "to_snapshot": snapshot2["timestamp"],
            "changes": {
                "added": {},
                "modified": {},
                "deleted": {},
                "moved": {}
            },
            "summary": {
                "files_added": 0,
                "files_modified": 0,
                "files_deleted": 0,
                "spaces_changed": []
            }
        }
        
        # Compare each space
        for space in ["u", "e", "s"]:
            space_diff = self.create_space_diff(
                snapshot1["spaces"].get(space, {}),
                snapshot2["spaces"].get(space, {})
            )
            
            if space_diff["has_changes"]:
                diff["changes"]["added"][space] = space_diff["added"]
                diff["changes"]["modified"][space] = space_diff["modified"]
                diff["changes"]["deleted"][space] = space_diff["deleted"]
                diff["summary"]["spaces_changed"].append(space)
                
                diff["summary"]["files_added"] += len(space_diff["added"])
                diff["summary"]["files_modified"] += len(space_diff["modified"])
                diff["summary"]["files_deleted"] += len(space_diff["deleted"])
                
        return diff
        
    def create_space_diff(self, space1, space2):
        """Create diff between two space snapshots"""
        files1 = space1.get("files", {})
        files2 = space2.get("files", {})
        
        diff = {
            "added": [],
            "modified": [],
            "deleted": [],
            "has_changes": False
        }
        
        # Find added files
        for file_path in files2:
            if file_path not in files1:
                diff["added"].append({
                    "file": file_path,
                    "size": files2[file_path]["size"],
                    "checksum": files2[file_path]["checksum"]
                })
                
        # Find deleted files
        for file_path in files1:
            if file_path not in files2:
                diff["deleted"].append({
                    "file": file_path,
                    "size": files1[file_path]["size"],
                    "checksum": files1[file_path]["checksum"]
                })
                
        # Find modified files
        for file_path in files1:
            if file_path in files2:
                if files1[file_path]["checksum"] != files2[file_path]["checksum"]:
                    diff["modified"].append({
                        "file": file_path,
                        "old_size": files1[file_path]["size"],
                        "new_size": files2[file_path]["size"],
                        "old_checksum": files1[file_path]["checksum"],
                        "new_checksum": files2[file_path]["checksum"]
                    })
                    
        diff["has_changes"] = bool(diff["added"] or diff["modified"] or diff["deleted"])
        return diff
        
    def record_symbolic_mutation(self, mutation_type, description, affected_files=None):
        """Record a symbolic mutation event"""
        mutation = {
            "timestamp": datetime.now().isoformat(),
            "type": mutation_type,
            "description": description,
            "affected_files": affected_files or [],
            "evolution_step": self.get_evolution_step(),
            "mutation_id": hashlib.md5(f"{mutation_type}_{description}_{datetime.now().isoformat()}".encode()).hexdigest()[:8]
        }
        
        # Save mutation record
        mutation_file = self.mutations_path / f"mutation_{mutation['mutation_id']}.json"
        with open(mutation_file, 'w') as f:
            json.dump(mutation, f, indent=2)
            
        print(f"🧬 Recorded mutation: {mutation_type} - {description}")
        return mutation
        
    def create_git_commit(self, snapshot, message=None):
        """Create Git commit for memory snapshot"""
        # Copy current space files to repo
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            repo_space_path = self.memory_repo_path / "spaces" / space
            
            # Ensure directory exists
            repo_space_path.mkdir(parents=True, exist_ok=True)
            
            if space_path.exists():
                # Copy files
                for file_path in space_path.rglob("*"):
                    if file_path.is_file():
                        relative_path = file_path.relative_to(space_path)
                        dest_path = repo_space_path / relative_path
                        dest_path.parent.mkdir(parents=True, exist_ok=True)
                        shutil.copy2(file_path, dest_path)
                        
        # Copy snapshot metadata
        snapshot_file = self.memory_repo_path / "current_snapshot.json"
        with open(snapshot_file, 'w') as f:
            json.dump(snapshot, f, indent=2)
            
        # Git add and commit
        subprocess.run(["git", "add", "."], cwd=self.memory_repo_path, capture_output=True)
        
        commit_message = message or f"Memory evolution snapshot {snapshot['timestamp']}"
        commit_result = subprocess.run(
            ["git", "commit", "-m", commit_message], 
            cwd=self.memory_repo_path, 
            capture_output=True, 
            text=True
        )
        
        if commit_result.returncode == 0:
            print(f"📝 Git commit created: {commit_message}")
            return True
        else:
            print(f"⚠️ Git commit failed: {commit_result.stderr}")
            return False
            
    def get_memory_lineage(self, depth=10):
        """Get memory evolution lineage"""
        try:
            # Get Git log
            log_result = subprocess.run(
                ["git", "log", "--oneline", f"-{depth}"],
                cwd=self.memory_repo_path,
                capture_output=True,
                text=True
            )
            
            if log_result.returncode == 0:
                lineage = []
                for line in log_result.stdout.strip().split('\n'):
                    if line:
                        parts = line.split(' ', 1)
                        if len(parts) == 2:
                            lineage.append({
                                "commit_hash": parts[0],
                                "message": parts[1],
                                "position": len(lineage)
                            })
                return lineage
            else:
                return []
        except:
            return []
            
    def rollback_to_snapshot(self, snapshot_timestamp):
        """Rollback memory to a specific snapshot"""
        snapshot_file = self.snapshots_path / f"snapshot_{snapshot_timestamp}.json"
        
        if not snapshot_file.exists():
            print(f"❌ Snapshot {snapshot_timestamp} not found")
            return False
            
        try:
            # Load snapshot
            with open(snapshot_file, 'r') as f:
                snapshot = json.load(f)
                
            print(f"🔄 Rolling back to snapshot {snapshot_timestamp}...")
            
            # Find corresponding Git commit
            log_result = subprocess.run(
                ["git", "log", "--grep", snapshot_timestamp, "--format=%H"],
                cwd=self.memory_repo_path,
                capture_output=True,
                text=True
            )
            
            if log_result.returncode == 0 and log_result.stdout.strip():
                commit_hash = log_result.stdout.strip().split('\n')[0]
                
                # Git checkout
                checkout_result = subprocess.run(
                    ["git", "checkout", commit_hash],
                    cwd=self.memory_repo_path,
                    capture_output=True,
                    text=True
                )
                
                if checkout_result.returncode == 0:
                    print(f"✅ Rollback successful to {commit_hash}")
                    return True
                else:
                    print(f"❌ Git checkout failed: {checkout_result.stderr}")
                    return False
            else:
                print(f"❌ Git commit for snapshot {snapshot_timestamp} not found")
                return False
                
        except Exception as e:
            print(f"❌ Rollback failed: {e}")
            return False
            
    def monitoring_loop(self):
        """Main monitoring loop for memory evolution"""
        print("🔄 Starting memory evolution monitoring...")
        last_snapshot = None
        
        while self.running:
            try:
                # Capture current snapshot
                current_snapshot = self.capture_memory_snapshot()
                
                # Create Git commit
                self.create_git_commit(current_snapshot)
                
                # Create diff if we have a previous snapshot
                if last_snapshot:
                    diff = self.create_symbolic_diff(last_snapshot, current_snapshot)
                    
                    if diff["summary"]["files_added"] > 0 or diff["summary"]["files_modified"] > 0 or diff["summary"]["files_deleted"] > 0:
                        # Save diff
                        diff_file = self.diffs_path / f"diff_{last_snapshot['timestamp']}_to_{current_snapshot['timestamp']}.json"
                        with open(diff_file, 'w') as f:
                            json.dump(diff, f, indent=2)
                            
                        # Record mutation
                        mutation_desc = f"Memory evolution: {diff['summary']['files_added']} added, {diff['summary']['files_modified']} modified, {diff['summary']['files_deleted']} deleted"
                        self.record_symbolic_mutation("evolution", mutation_desc, diff["summary"]["spaces_changed"])
                        
                        print(f"🔍 Memory diff created: {mutation_desc}")
                        
                last_snapshot = current_snapshot
                
                # Wait for next cycle
                import time
                time.sleep(self.update_interval)
                
            except KeyboardInterrupt:
                print("\n🛑 Memory evolution monitoring stopped by user")
                break
            except Exception as e:
                print(f"❌ Error in monitoring loop: {e}")
                import time
                time.sleep(self.update_interval)
                
    def get_evolution_status(self):
        """Get current evolution status"""
        return {
            "evolution_step": self.get_evolution_step(),
            "lineage_depth": len(self.get_memory_lineage()),
            "recent_mutations": len(list(self.mutations_path.glob("mutation_*.json"))),
            "total_snapshots": len(list(self.snapshots_path.glob("snapshot_*.json"))),
            "total_diffs": len(list(self.diffs_path.glob("diff_*.json")))
        }
        
    def start(self):
        """Start memory evolution tracking"""
        print("🧬 Starting Memory Evolution Tracker...")
        self.running = True
        
        # Start monitoring in background
        monitor_thread = threading.Thread(target=self.monitoring_loop)
        monitor_thread.daemon = True
        monitor_thread.start()
        
        print("✨ Memory evolution tracking active!")
        return monitor_thread
        
    def stop(self):
        """Stop memory evolution tracking"""
        print("🛑 Stopping memory evolution tracking...")
        self.running = False

def main():
    """Main function for standalone execution"""
    tracker = MemoryEvolutionTracker()
    
    try:
        # Start monitoring
        monitor_thread = tracker.start()
        
        # Keep running until interrupted
        while tracker.running:
            import time
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\n🛑 Shutting down memory evolution tracker...")
        tracker.stop()
        
if __name__ == "__main__":
    main()