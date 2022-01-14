# Spanning Tree

2 states:
* Forwarding
* Blocking

Note that STP predates LAN switches, hence mention of bridges

Failed/shutdown interfaces are placed into an STP disabled state.

The bridge ID is an 8-byte value, unique to the switch. 2 byte priority field,
and 6-byte for the MAC address.

Root switch is whatever has lowest priority, and in a tie, the lowest bridge ID.

If a switch hears a Hello with a lower BID, it stops advertising and forwards
the superior Hello.

For best root cost tiebreakers,
1. lowest neighbor bridge ID.
2. lowest neighbor port priority.
3. lowest neighbor internal port number.

STP root switch sends a new Hello BPDU every 2 seconds by default. When a switch
hasn't received a Hello (MaxAge is 10xHello, so 20 seconds by default) or gets
different details, it reacts to the topology change. When transitioning from
blocking to forwarding, it goes through Listening state (no forwarded frames,
removes stale MAC table entries), then Learning (no forwarded frames, but learns
MAC addresses of frames sent to interface). Forward delay state changes are 15
seconds each (so 30 seconds from blocking to forwarding). In summary, a topology
change could lead to a 50 second delay using STP.

RSTP (IEEE 802.1w originally, 802.1Q today) is an improvement of STP, where
network convergence can happen in a few seconds (10 seconds in worst case). It
allows switches to replace their root parts without the blocking->forwarding
transition wait time in some cases, the ability to replace a designated port
without waiting for forwarding state, and lower wait times on the timers. MaxAge
for Hello is 3 times the Hello timer. There are also messages that can be sent
to neighboring switches asking if problems are occuring, reducing wait times.
There is a concept of Alternate port (which can replace the root port when
failing), and a Backup port (when the designated port is failing)


## Bridge Protocol Data Units

### Hello BPDU

Has root bridge ID, sender's bridge ID, sender's root cost, and timers on the
root switch

## Spanning Tree Algorithm

Elect a root switch, all ports in forwarding

Non-root switches determine which port has least cost to root switch (root
cost). That "root port" is put in forwarding state.

With two switches on a link, the one with the lowest root cost is placed in a
forwarding state. That switch is the "designated switch," and the interface the
"designated port"

Any leftover interfaces are put in a blocking state.

## Standards

Spanning Tree - IEEE 802.1D
Rapid Spanning Tree - IEEE 802.1w
Multiple Spanning Tree - IEEE 802.1s
all incorporated into 802.1Q-2014
