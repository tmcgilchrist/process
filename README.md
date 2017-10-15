Process
=====

Specification
-----

Several nodes continuously send messages to other nodes in such a way that every message reaches
every node. *You are free to use any model of communication.* Each messages contains a deterministic
random number n ∈ (0, 1]. That happens for some time, afterwards, during the grace period, each
node prints out the following tuple: `( |m|, sum from i to |m| of i * mi) `

where `m` is the list of all messages sent by all nodes, ordered by sending time,
and `mi` is the i-th messages sent by some node. *You are most welcome to print out debu information
to stderr.*

The larger your score with a given sequence of random numbers is, the better. You code will be run
under different network failure scenarios. For new we don't reveal `the failure maps`, please use
your best judgement of the most applicable communication model. You are free to tell us under which
assumptions you chose it.

See [spec](doc/CH_OTP_Test_Task.pdf) for more details and better rendering of formulas.

Cli
-----

``` shell
process --send-for <k> \
        --wait-for <l> \
        --with-seed <seed>
```

Cluster configuration
-----

At the moment, we run submissions manually by patching node lists in submissions.
That poses a requirement of submission having either source file or configuration
file where we can put the list of nodes of our testing cluster. Please refer to
this file in README.

 * Initially as a documented location in main/process.hs.

Building
-----

``` shell
# Builds the project
./mafia build

# Runs any tests
./mafia test

```

This project uses `mafia` which is a thin wrapper around a cabal sandbox workflow. It should also
build with regular cabal ideally with sandboxes. See [mafia](https://github.com/ambiata/mafia)
