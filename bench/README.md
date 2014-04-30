
You may need to unthrottle your CPUs in order to see significant
speedup.

### Unthrottling on Linux

First run

    $ sh governor.sh

to see a list of available governors along with the current status.

A governor called "performance" will presumably be available. To
switch to the performance governor,

    $ sudo sh governor.sh performance

Each CPU should now report "performance".

After benchmarking you may wish to switch back. If the original
setting was "ondemand" then

    $ sudo sh governor.sh ondemand

### Hyperthreading

Hyperthreading may or may not negatively impact benchmarks.

If you have hyperthreading enabled, using twice as many workers as
CPUs (or some intermediate value) may or may not improve benchmarks.
