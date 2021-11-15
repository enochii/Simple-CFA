## Implementation Details

- Points-to relation representation
- transfer function for several instruction types
- a worklist algorithm for whole program analysis
  - also need to add some mechanisms to trigger intra-procedural fixed point(via function entry node)

## Optimization

a call inst in caller may change the ret node as a result, should we continue caller process or do the callee first?