# Scheduler Daemon

Runs ecron and schedules symbolic flows across the AGI system.

## Functionality

- Execute Ecron symbolic scheduler
- Manage task priorities and dependencies
- Schedule symbolic flow executions
- Coordinate with other system daemons

## Components

- `ecron-scheduler.py` - Main scheduling engine
- `flow-manager.scm` - Symbolic flow coordination
- `priority-queue.lisp` - Task priority management

## Integration

The scheduler daemon integrates with:
- Ecron symbolic task specifications
- Wolf kernel execution contexts
- OpenCog AtomSpace for memory coordination
- Reflex daemon for reactive scheduling