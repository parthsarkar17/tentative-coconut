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
An interesting thread of the discussion led some folks to the idea that the programmer
could play a bigger role in garbage collection, instead of the more abstract interface
that the mainstream paradigm provides. If a language implementation breaks
down some of the existing abstractions, collection could be more "domain-specific",
letting the compiler know which algorithms and hybrids to use on particular data structures
or sections of memory, how often to collect, or generally providing useful compile-time
guarantees.

A great example was the idea that standard library data structures could come
with programmer-facing guarantees on the GC's behavior. In this language, the user
might explicitly choose to use specific data structures because they have the 
guarantee of being reference-counted, for example, avoiding larger pauses. 

One other instance of breaking existing abstractions, this time relying slightly more on 
the programmer, is the idea that there should be a fundamental separation between 
"regions of memory" and "objects to free". In particular, the user should be able to 
operate on some allocated space with the guarantee that the collecter will not 
collect objects in that space until directed by the programmer. For example,
if the program is operating on a graph, and all nodes are constantly active,
it would be wasteful to increment and decrement reference counts for every
change in the graph (or, indeed, mark-and-sweeping every so often); instead, 
there could be pointers into and out of the _space as a whole_, clearly outlining
when to free this large chunk of space. 

(Side note: a while after this point was brought up, it occurred to me that this 
is close to what a programmer would do to manually manage memory. It seems buggy
to free individual nodes during processing, so they might just free the whole
section when they're sure they've finished their computation. This goes back to
a meta-discussion-point about how there's still GC research to be done to fill the gap
between automatic, GC-managed memory and manual memory management).