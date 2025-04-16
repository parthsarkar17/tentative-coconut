# Background
This week, we read [A Unified Theory of Garbage Collection](https://dl.acm.org/doi/10.1145/1028976.1028982) by Bacon et al. from OOPSLA 2004.  

Garbage collection, a form of automatic memory management, frees programmers from needing to deallocate memory themselves. 
The two main garbage collection algorithms are tracing (also known as mark-and-sweep) and reference counting (RC). They each present distinct performance tradeoffs -- a tracing collector (e.g. the [Java Virtual Machine](https://stackoverflow.com/questions/65312024/why-are-jvm-garbage-collectors-trace-based)'s collector) will pause execution of the program to scan the entire heap at once, whereas a reference counting collector (e.g. [CPython](https://github.com/python/cpython/blob/main/InternalDocs/garbage_collector.md)'s collector) incurs shorter pause times by incrementally tracking objects. However, reference counting collectors require some extra machinery for tracking cycles. 

The paper's central thesis is that tracing and RC, although traditionally viewed as being entirely distinct, are actually algorithmic duals. Moreover, the authors demonstrate how various (more sophisticated) GC strategies, such as generational garbage collection, can be viewed as hybrids of tracing and RC. The authors argue that this notion of duality allows one to "systematically" explore the 
design space for GCs and better select the best GC strategy for a particular application. 

# Contributions

## Qualitative analysis
By presenting tracing and RC as duals, the authors introduce a novel mental model for approaching garbage collectors. 
Specifically, tracing operates on live objects (“matter”), while RC operates on dead objects (“anti-matter”). 

Tracing initializes reference counts to 0 (an underestimate of the true count), incrementing them during graph traversal until they reach the true count. On the other hand, RC initializes reference counts to an overestimate of the true count, decrementing them during graph traversal until they reach the true count (ignoring cycles). 

The authors also identify several key characteristics they use to describe each algorithm:
| | Tracing | RC |
| --- | --- | --- |
| Starting point | Roots | Anti-roots |
| Graph traversal | Forward from roots | Forward from anti-roots | 
| Objects traversed | Live | Dead |

With this formulation, the parallels between each algorithm become clear.

## Hybrid collectors
TODO

## Cost model
The authors also present a formal cost model for comparing collectors. Specifically, they introduce:
- $\kappa$, the time overhead for a single garbage collection
- $\sigma$, the space overhead for a single garbage collection
- $\phi$, the frequency of collection
- $\mu$, the mutation overhead
- $\tau$, the total time overhead for an entire program

Notably, the authors mention that these quantities represent "real costs with implied coefficients", as opposed to 
idealized big-Oh notation. Using these quantities, the authors are able to compare the hybrid collectors they developed.

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