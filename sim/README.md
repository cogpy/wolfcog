# WolfCog Simulation and Mock Components

This directory contains simulation and mock components that have been separated from the real implementation as part of the recursive cognitive flowchart implementation (Layer 1: Foundation - Bifurcating Mock vs Reality).

## Directory Structure

- `mock_features/` - Mock and prototype features that are not production-ready
- `simulation/` - Simulation environments for testing cognitive scenarios
- `prototypes/` - Early prototype implementations for experimental features

## Implementation Notes

Real features migrate to Layer 2 (Symbolic Processing Core) and beyond. Mock features terminate at this layer as specified in the cognitive flowchart.

## Scheme Integration

This directory supports the mock filtering function:
```scheme
(define (filter-mock-features feature-list)
  (filter (lambda (feature)
            (not (mock-feature? feature)))
          feature-list))
```