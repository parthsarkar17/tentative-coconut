# Background
This week we read [A Unified Theory of Garbage Collection](https://dl.acm.org/doi/10.1145/1028976.1028982), 
from OOPSLA 2004.  

The two main garbage collection algorithms are tracing (also known as mark-and-sweep) and reference counting. They each present distinct performance tradeoffs -- a tracing collector will pause execution of the program to scan the entire heap at once, whereas a reference counting collector incurs shorter pause times by incrementally tracking objects. However, reference counting collectors require some extra machinery for tracking cycles. 

The paper's main argument is that these two algorithms, while traditionally viewed as entirely distinct, are actually algorithmic duals. The authors claim that they share a deep underlying structure that becomes apparent when optimizing each type of collector. 

# Contributions

## Qualitative analysis
By presenting tracing and RC as duals, the authors introduce a novel mental model for approaching garbage collectors. They identify several key characteristics they use to describe each algorithm:
| | Tracing | RC |
| --- | --- | --- |
| Starting point | Roots | Anti-roots |
| Graph traversal | Fwd from roots | Fwd from anti-roots | 
| Objects traversed | Live | Dead |

With this formulation, the parallels between each algorithm become clear.

## Hybrid collectors

## Cost model
The authors also present a formal cost model for comparing collectors. Specifically, they introduce:
- $\kappa$, the time overhead for a single garbage collection
- $\sigma$, the space overhead for a single garbage collection
- $\phi$, the frequency of collection
- $\mu$, the mutation overhead
- $\tau$, the total time overhead for an entire program
Using these quantities, the authors are able to compare the hybrid collectors they developed.

# Merits

# Shortcomings
Part of our discussion focused on two aspects of the authors' cost model: (1) its accuracy, and (2) its utility. Regarding (1), we decided we would have liked to see a quantitative analysis of the cost model with actual benchmarks. While it seems like an elegant abstraction, we can't know for sure if the introduced model accounts for all variations between collectors, or at least the important ones. Additionally, we were interested about the impact of the model's assumptions -- it treats the allocation rate and the garbage fraction of the heap as constants. We would have liked to know how accurate the cost model is in cases where these assumptions don't hold. Evaluating various hybrid collectors on a set of benchmarks with varying allocation rates and garbage fractions would have answered these questions.

Regarding (2), we wondered how useful an abstract cost model is given that programmers would likely benchmark their application with different collectors if they were really concerned about garbage collection as a performance bottleneck.

Both of these concerns make us doubtful about the practicality of the proposed cost model. While it is an elegant abstraction and we appreciated the way the authors used it to compare their collectors, it would have been nice to see quantitative support for their comparisons.

# Connections to state of the art