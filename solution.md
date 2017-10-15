Solution
=====

Discussion of the solution.

Initial solution
-----

The solution I have chosen to implement is to broadcast messages between nodes without an explicit reply message.
This imposes an eventual ordering as messages arrive at a node and are placed into an ordered buffer.
If there is a requirement for each message to be explicitly agreed upon between all nodes we would need to
look at a solution involving consensus protocols (eg PAXOS, RAFT)

Each node needs to be available during the initial discovery phase. There is explicitly no consideration given to
adding more nodes after this phase.

Each node has an internal buffer of received messages ordered by a timestamp. If messages are received outside
our time window we drop them.

Problems
-----

1. I'm not happy with the mixing of pure and impure code in this solution, some functions could be
   made more pure. I found the Process monad tricky to extact pure functions from in places and it
   didn't end up working well with the property testing I wanted to do. See `test/Test/Process/Sender.hs`

2. Selective receives like `expect` and `receiveWait` is not ideal and where possible I have used the
   timeout variant.

   I am concerned about how `distributed-process` handles selective receives on the underlying mailbox.
   If the messages are stored in received order in the mailbox, this would mean every time you match a message
   it would begin with the oldest one first. This has obvious performance problems plus newer messages
   get delayed behind older messages you are choosing to ignore. I haven't looked in detail into the implementation
   but I am familiar with the problem from Erlang.

3. Using a Seq for the buffer might not be the best data structure in this case, it was convenient for this quick
   use case.
