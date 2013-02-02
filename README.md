
# lparallel

lparallel is a library for parallel programming in Common Lisp, featuring

  * a simple model of task submission with receiving queue
  * constructs for expressing fine-grained parallelism
  * asynchronous condition handling across thread boundaries
  * parallel versions of map, reduce, sort, remove, and many others
  * promises, futures, and delayed evaluation constructs
  * computation trees for parallelizing interconnected tasks
  * bounded and unbounded FIFO queues
  * high and low priority tasks
  * task killing by category
  * integrated timeouts

See http://lparallel.org for documentation and examples.

### Running

lparallel should run on any Common Lisp implementation supported by
bordeaux-threads. The following implementations successfully pass the
test suite:

  * ABCL
  * Allegro
  * Clozure
  * LispWorks
  * SBCL

To run tests, load `lparallel-test.asd` and call `(lparallel-test:execute)`.

To run benchmarks, load `lparallel-bench.asd` and call
`(lparallel-bench:execute N)` where `N` is the number of worker threads.

### Author

James M. Lawrence <llmjjmll@gmail.com>
