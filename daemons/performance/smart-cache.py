#!/usr/bin/env python3
"""
WolfCog Smart Caching Layer
Multi-level caching system for symbolic operations and task results
"""

import json
import time
import hashlib
import threading
import pickle
import gzip
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, Any, Optional, List, Tuple
from dataclasses import dataclass, asdict
import statistics

@dataclass
class CacheEntry:
    """Cache entry with metadata"""
    key: str
    value: Any
    created_at: datetime
    last_accessed: datetime
    access_count: int
    size_bytes: int
    ttl_seconds: Optional[int]
    space: str
    complexity: float

class LRUCache:
    """LRU (Least Recently Used) cache implementation"""
    
    def __init__(self, max_size: int = 1000):
        self.max_size = max_size
        self.cache: Dict[str, CacheEntry] = {}
        self.access_order: List[str] = []
        self.lock = threading.RLock()
        
    def get(self, key: str) -> Optional[Any]:
        """Get value from cache"""
        with self.lock:
            if key in self.cache:
                entry = self.cache[key]
                
                # Check TTL
                if entry.ttl_seconds:
                    if (datetime.now() - entry.created_at).total_seconds() > entry.ttl_seconds:
                        self._remove(key)
                        return None
                
                # Update access info
                entry.last_accessed = datetime.now()
                entry.access_count += 1
                
                # Move to end (most recently used)
                if key in self.access_order:
                    self.access_order.remove(key)
                self.access_order.append(key)
                
                return entry.value
            return None
            
    def put(self, key: str, value: Any, ttl_seconds: Optional[int] = None, 
            space: str = "e", complexity: float = 1.0):
        """Put value in cache"""
        with self.lock:
            # Calculate size
            size_bytes = len(pickle.dumps(value))
            
            # Create entry
            entry = CacheEntry(
                key=key,
                value=value,
                created_at=datetime.now(),
                last_accessed=datetime.now(),
                access_count=1,
                size_bytes=size_bytes,
                ttl_seconds=ttl_seconds,
                space=space,
                complexity=complexity
            )
            
            # Remove existing entry if present
            if key in self.cache:
                self._remove(key)
                
            # Add new entry
            self.cache[key] = entry
            self.access_order.append(key)
            
            # Evict if over capacity
            while len(self.cache) > self.max_size:
                oldest_key = self.access_order[0]
                self._remove(oldest_key)
                
    def _remove(self, key: str):
        """Remove key from cache"""
        if key in self.cache:
            del self.cache[key]
        if key in self.access_order:
            self.access_order.remove(key)
            
    def clear(self):
        """Clear all cache entries"""
        with self.lock:
            self.cache.clear()
            self.access_order.clear()
            
    def stats(self) -> Dict[str, Any]:
        """Get cache statistics"""
        with self.lock:
            total_size = sum(entry.size_bytes for entry in self.cache.values())
            avg_complexity = statistics.mean([e.complexity for e in self.cache.values()]) if self.cache else 0
            
            return {
                "entries": len(self.cache),
                "total_size_bytes": total_size,
                "average_complexity": avg_complexity,
                "spaces": list(set(entry.space for entry in self.cache.values()))
            }

class SmartCacheLayer:
    """Multi-level smart caching system for WolfCog"""
    
    def __init__(self, 
                 l1_size: int = 500,      # Memory cache
                 l2_size: int = 2000,     # Persistent cache
                 cache_dir: str = "/tmp/wolfcog_cache"):
        
        # Cache levels
        self.l1_cache = LRUCache(l1_size)  # Hot data in memory
        self.l2_cache_dir = Path(cache_dir)  # Warm data on disk
        self.l2_cache_dir.mkdir(exist_ok=True)
        self.l2_size = l2_size
        
        # Cache policies
        self.default_ttl = 3600  # 1 hour
        self.symbolic_ttl = 7200  # 2 hours for symbolic operations
        self.task_result_ttl = 1800  # 30 minutes for task results
        
        # Performance tracking
        self.stats = {
            "l1_hits": 0,
            "l1_misses": 0,
            "l2_hits": 0,
            "l2_misses": 0,
            "total_requests": 0,
            "cache_generations": 0,
            "evictions": 0
        }
        
        # Background maintenance
        self.maintenance_interval = 300  # 5 minutes
        self.running = False
        
        # Key patterns for different data types
        self.key_patterns = {
            "symbolic": "sym:{space}:{hash}",
            "task_result": "task:{task_id}:{hash}",
            "validation": "val:{expr_type}:{hash}",
            "memory_state": "mem:{space}:{timestamp}"
        }
        
    def start(self):
        """Start the caching system"""
        print("🚀 Starting WolfCog Smart Caching Layer...")
        self.running = True
        
        # Start maintenance thread
        maintenance_thread = threading.Thread(target=self._maintenance_worker)
        maintenance_thread.daemon = True
        maintenance_thread.start()
        
        print("✅ Smart caching layer started")
        
    def get_symbolic_result(self, expression: str, space: str) -> Optional[Any]:
        """Get cached symbolic operation result"""
        key = self._generate_key("symbolic", expression, space=space)
        return self._get_multi_level(key)
        
    def cache_symbolic_result(self, expression: str, space: str, result: Any, 
                            complexity: float = 1.0):
        """Cache symbolic operation result"""
        key = self._generate_key("symbolic", expression, space=space)
        self._put_multi_level(key, result, ttl=self.symbolic_ttl, 
                             space=space, complexity=complexity)
        
    def get_task_result(self, task_id: str, task_data: Dict) -> Optional[Any]:
        """Get cached task result"""
        key = self._generate_key("task_result", task_id, extra_data=task_data)
        return self._get_multi_level(key)
        
    def cache_task_result(self, task_id: str, task_data: Dict, result: Any,
                         complexity: float = 1.0):
        """Cache task processing result"""
        key = self._generate_key("task_result", task_id, extra_data=task_data)
        space = task_data.get("space", "e")
        self._put_multi_level(key, result, ttl=self.task_result_ttl,
                             space=space, complexity=complexity)
        
    def get_validation_result(self, expression: str, expr_type: str) -> Optional[bool]:
        """Get cached validation result"""
        key = self._generate_key("validation", expression, expr_type=expr_type)
        return self._get_multi_level(key)
        
    def cache_validation_result(self, expression: str, expr_type: str, is_valid: bool):
        """Cache validation result"""
        key = self._generate_key("validation", expression, expr_type=expr_type)
        self._put_multi_level(key, is_valid, ttl=self.default_ttl,
                             space="validation", complexity=0.1)
        
    def get_memory_state(self, space: str, timestamp_range: Tuple[datetime, datetime]) -> Optional[Dict]:
        """Get cached memory state"""
        # For memory states, use a time-based key
        time_key = f"{timestamp_range[0].isoformat()}_{timestamp_range[1].isoformat()}"
        key = self._generate_key("memory_state", time_key, space=space)
        return self._get_multi_level(key)
        
    def cache_memory_state(self, space: str, timestamp: datetime, state: Dict):
        """Cache memory state snapshot"""
        key = self._generate_key("memory_state", timestamp.isoformat(), space=space)
        self._put_multi_level(key, state, ttl=self.default_ttl * 2,  # Longer TTL for memory
                             space=space, complexity=2.0)
        
    def _generate_key(self, pattern_type: str, primary_data: str, **kwargs) -> str:
        """Generate cache key based on pattern and data"""
        # Create hash of primary data
        data_hash = hashlib.sha256(primary_data.encode()).hexdigest()[:16]
        
        # Add extra data to hash if provided
        if "extra_data" in kwargs:
            extra_str = json.dumps(kwargs["extra_data"], sort_keys=True)
            extra_hash = hashlib.sha256(extra_str.encode()).hexdigest()[:8]
            data_hash += "_" + extra_hash
            
        # Build key based on pattern
        if pattern_type == "symbolic":
            space = kwargs.get("space", "e")
            return f"sym:{space}:{data_hash}"
        elif pattern_type == "task_result":
            return f"task:{primary_data}:{data_hash}"
        elif pattern_type == "validation":
            expr_type = kwargs.get("expr_type", "generic")
            return f"val:{expr_type}:{data_hash}"
        elif pattern_type == "memory_state":
            space = kwargs.get("space", "e")
            return f"mem:{space}:{data_hash}"
        else:
            return f"generic:{data_hash}"
            
    def _get_multi_level(self, key: str) -> Optional[Any]:
        """Get value from multi-level cache"""
        self.stats["total_requests"] += 1
        
        # Try L1 cache first (memory)
        value = self.l1_cache.get(key)
        if value is not None:
            self.stats["l1_hits"] += 1
            return value
        else:
            self.stats["l1_misses"] += 1
            
        # Try L2 cache (disk)
        value = self._get_l2_cache(key)
        if value is not None:
            self.stats["l2_hits"] += 1
            # Promote to L1 cache
            self.l1_cache.put(key, value)
            return value
        else:
            self.stats["l2_misses"] += 1
            
        return None
        
    def _put_multi_level(self, key: str, value: Any, ttl: int = None,
                        space: str = "e", complexity: float = 1.0):
        """Put value in multi-level cache"""
        ttl = ttl or self.default_ttl
        
        # Always put in L1 cache
        self.l1_cache.put(key, value, ttl, space, complexity)
        
        # Put in L2 cache if complexity is high (expensive to recompute)
        if complexity > 2.0 or space in ["s", "u"]:  # System/User space operations
            self._put_l2_cache(key, value, ttl, space, complexity)
            
    def _get_l2_cache(self, key: str) -> Optional[Any]:
        """Get value from L2 (disk) cache"""
        cache_file = self.l2_cache_dir / f"{key}.cache.gz"
        
        if not cache_file.exists():
            return None
            
        try:
            with gzip.open(cache_file, 'rb') as f:
                cache_data = pickle.load(f)
                
            # Check TTL
            created_at = cache_data["created_at"]
            ttl = cache_data["ttl_seconds"]
            
            if ttl and (datetime.now() - created_at).total_seconds() > ttl:
                cache_file.unlink()  # Remove expired file
                return None
                
            # Update access time
            cache_data["last_accessed"] = datetime.now()
            cache_data["access_count"] += 1
            
            # Write back updated metadata
            with gzip.open(cache_file, 'wb') as f:
                pickle.dump(cache_data, f)
                
            return cache_data["value"]
            
        except Exception as e:
            print(f"❌ Error reading L2 cache {key}: {e}")
            if cache_file.exists():
                cache_file.unlink()
            return None
            
    def _put_l2_cache(self, key: str, value: Any, ttl: int,
                     space: str, complexity: float):
        """Put value in L2 (disk) cache"""
        cache_file = self.l2_cache_dir / f"{key}.cache.gz"
        
        try:
            cache_data = {
                "key": key,
                "value": value,
                "created_at": datetime.now(),
                "last_accessed": datetime.now(),
                "access_count": 1,
                "ttl_seconds": ttl,
                "space": space,
                "complexity": complexity
            }
            
            with gzip.open(cache_file, 'wb') as f:
                pickle.dump(cache_data, f)
                
        except Exception as e:
            print(f"❌ Error writing L2 cache {key}: {e}")
            
    def _maintenance_worker(self):
        """Background maintenance worker"""
        while self.running:
            try:
                self._cleanup_expired_l2_cache()
                self._manage_l2_cache_size()
                time.sleep(self.maintenance_interval)
            except Exception as e:
                print(f"❌ Error in cache maintenance: {e}")
                
    def _cleanup_expired_l2_cache(self):
        """Clean up expired L2 cache files"""
        current_time = datetime.now()
        removed_count = 0
        
        for cache_file in self.l2_cache_dir.glob("*.cache.gz"):
            try:
                with gzip.open(cache_file, 'rb') as f:
                    cache_data = pickle.load(f)
                    
                ttl = cache_data.get("ttl_seconds")
                created_at = cache_data.get("created_at")
                
                if ttl and created_at:
                    if (current_time - created_at).total_seconds() > ttl:
                        cache_file.unlink()
                        removed_count += 1
                        
            except Exception:
                # Remove corrupted cache files
                cache_file.unlink()
                removed_count += 1
                
        if removed_count > 0:
            print(f"🧹 Cleaned up {removed_count} expired cache entries")
            
    def _manage_l2_cache_size(self):
        """Manage L2 cache size by removing least recently used files"""
        cache_files = list(self.l2_cache_dir.glob("*.cache.gz"))
        
        if len(cache_files) <= self.l2_size:
            return
            
        # Get file access times
        file_info = []
        for cache_file in cache_files:
            try:
                with gzip.open(cache_file, 'rb') as f:
                    cache_data = pickle.load(f)
                last_accessed = cache_data.get("last_accessed", datetime.min)
                file_info.append((cache_file, last_accessed))
            except Exception:
                # Remove corrupted files
                cache_file.unlink()
                
        # Sort by last access time (oldest first)
        file_info.sort(key=lambda x: x[1])
        
        # Remove oldest files
        files_to_remove = len(file_info) - self.l2_size
        for i in range(files_to_remove):
            file_info[i][0].unlink()
            self.stats["evictions"] += 1
            
        if files_to_remove > 0:
            print(f"📦 Evicted {files_to_remove} L2 cache entries")
            
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get comprehensive cache statistics"""
        l1_stats = self.l1_cache.stats()
        
        # Calculate hit rates
        total_l1_requests = self.stats["l1_hits"] + self.stats["l1_misses"]
        total_l2_requests = self.stats["l2_hits"] + self.stats["l2_misses"]
        
        l1_hit_rate = (self.stats["l1_hits"] / max(total_l1_requests, 1)) * 100
        l2_hit_rate = (self.stats["l2_hits"] / max(total_l2_requests, 1)) * 100
        overall_hit_rate = ((self.stats["l1_hits"] + self.stats["l2_hits"]) / 
                           max(self.stats["total_requests"], 1)) * 100
        
        # L2 cache info
        l2_files = len(list(self.l2_cache_dir.glob("*.cache.gz")))
        
        return {
            "l1_cache": l1_stats,
            "l2_cache_files": l2_files,
            "hit_rates": {
                "l1_hit_rate": l1_hit_rate,
                "l2_hit_rate": l2_hit_rate,
                "overall_hit_rate": overall_hit_rate
            },
            "performance": {
                "total_requests": self.stats["total_requests"],
                "l1_hits": self.stats["l1_hits"],
                "l2_hits": self.stats["l2_hits"],
                "cache_misses": self.stats["l1_misses"] + self.stats["l2_misses"]
            },
            "maintenance": {
                "evictions": self.stats["evictions"],
                "cache_generations": self.stats["cache_generations"]
            }
        }
        
    def clear_cache(self, level: str = "all"):
        """Clear cache at specified level"""
        if level in ["all", "l1"]:
            self.l1_cache.clear()
            print("🧹 L1 cache cleared")
            
        if level in ["all", "l2"]:
            for cache_file in self.l2_cache_dir.glob("*.cache.gz"):
                cache_file.unlink()
            print("🧹 L2 cache cleared")
            
    def stop(self):
        """Stop the caching system"""
        print("🛑 Stopping smart caching layer...")
        self.running = False

def main():
    """Main function for standalone testing"""
    cache = SmartCacheLayer()
    
    try:
        cache.start()
        
        # Test symbolic operation caching
        print("🧪 Testing symbolic operation caching...")
        
        symbolic_expr = "∇(cognitive_pattern + memory_flow)"
        space = "e"
        
        # Simulate expensive computation
        start_time = time.time()
        time.sleep(0.1)  # Simulate computation time
        result = {"transformed": "∇²(cognitive_pattern)", "complexity": 5.2}
        computation_time = time.time() - start_time
        
        # Cache the result
        cache.cache_symbolic_result(symbolic_expr, space, result, complexity=5.2)
        print(f"✅ Cached symbolic result (computed in {computation_time:.3f}s)")
        
        # Retrieve from cache
        start_time = time.time()
        cached_result = cache.get_symbolic_result(symbolic_expr, space)
        cache_time = time.time() - start_time
        
        if cached_result:
            print(f"🎯 Retrieved from cache in {cache_time:.6f}s (speedup: {computation_time/cache_time:.0f}x)")
        
        # Test task result caching
        print("\n🧪 Testing task result caching...")
        
        task_id = "test_task_001"
        task_data = {"flow": "analysis", "space": "u", "symbolic": "Φ(user_intent)"}
        task_result = {"status": "completed", "output": "analyzed_intent", "score": 0.85}
        
        cache.cache_task_result(task_id, task_data, task_result, complexity=3.0)
        retrieved_result = cache.get_task_result(task_id, task_data)
        
        if retrieved_result:
            print("✅ Task result caching successful")
        
        # Test validation caching
        print("\n🧪 Testing validation caching...")
        
        expression = "∇(invalid_symbolic_expr"
        cache.cache_validation_result(expression, "symbolic", False)
        is_valid = cache.get_validation_result(expression, "symbolic")
        
        if is_valid is False:
            print("✅ Validation result caching successful")
        
        # Show cache statistics
        print("\n📊 Cache Statistics:")
        stats = cache.get_cache_stats()
        
        print(f"  L1 Cache: {stats['l1_cache']['entries']} entries")
        print(f"  L2 Cache: {stats['l2_cache_files']} files")
        print(f"  Overall Hit Rate: {stats['hit_rates']['overall_hit_rate']:.1f}%")
        print(f"  Total Requests: {stats['performance']['total_requests']}")
        
        # Test some more operations to see hit rate improve
        print("\n🔄 Testing cache hit performance...")
        
        for i in range(5):
            start_time = time.time()
            cached_result = cache.get_symbolic_result(symbolic_expr, space)
            cache_time = time.time() - start_time
            print(f"  Request {i+1}: {cache_time:.6f}s")
            
        # Final statistics
        print("\n📊 Final Cache Statistics:")
        final_stats = cache.get_cache_stats()
        print(f"  Overall Hit Rate: {final_stats['hit_rates']['overall_hit_rate']:.1f}%")
        print(f"  L1 Hit Rate: {final_stats['hit_rates']['l1_hit_rate']:.1f}%")
        
        print("\n✨ Smart caching layer demo complete!")
        
    except KeyboardInterrupt:
        print("\n🛑 Shutting down...")
    finally:
        cache.stop()

if __name__ == "__main__":
    main()
