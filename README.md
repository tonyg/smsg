# Recursive Message Broker

This is a *sketch* (executed in [Racket](http://racket-lang.org/)) of
a recursive messaging protocol, broker, and client library, inspired
by AMQP 0-91, [PubSubHubBub](http://code.google.com/p/pubsubhubbub/),
[STOMP](http://stomp.github.com/) and
[reversehttp](http://reversehttp.net/). It's quite different to AMQP
1.0 (but it may be instructive to compare the two approaches).

## A sketch?

Honestly, not meant to be production software. This is to demonstrate
a point.

## Background

Messaging à la AMQP 0-91 can be broken down into a few core pieces:

 - transmission and receipt of messages (publishes, deliveries, and gets)

 - subscription management (subsuming enrollment, bindings, consumers and relays)

 - directory (naming of resources in the broker)

 - object management (creation and destruction of remote resources)

AMQP itself, being a first mover in its space, isn't as orthogonal as
it could be. It can be greatly simplified without losing anything of
value. This experiment is intended to demonstrate one possible way of
paring each of the core pieces of AMQP-style messaging back to their
essences.

### More detail

TBD.

 - what recursive means in this context
 - doing things this way gives you shovels (relays) for free
 - and effortless interop with legacy messaging networks (including UDP, SMTP, IMAP, HTTP etc)
 - and effortless federation

 - relays (including active *client* connections!) are just nodes in
   the network, addressable like any other - so `(post! somerelay
   (post! someremotenode ...))` and so on is the way to cause things
   to happen remotely.

## Run it

Install [Racket](http://racket-lang.org/). Open three terminals. Run
one of the following in each, in order:

 - mzscheme -t main.rkt
 - mzscheme -t test1.rkt
 - mzscheme -t test3.rkt

This starts (a) the server, (b) a test consumer and (c) a test
publisher. A million messages are sent through a queue. Read
`test1.rkt` and `test3.rkt` to get some idea of how the system works
from an API-ish perspective.

## Wire protocol

Obviously the wire protocol itself here is the simplest thing that
could possibly work, and you'd never use anything like this
inefficient in a real system. That said, this is what's there right
now:

### Message transfer

`(post! <routing-key> <message> <subscription-id-or-false>)` -
Instructs the receiving node to route (or process) the given `message`
according to the given `routing-key`. Different kinds of nodes will do
different things here, and in particular, will interpret the routing
key differently. Queues, for example, will ignore the routing key and
will deliver the message to only one of their active subscribers,
whereas exchanges will generally match the routing key against their
active subscriptions and will deliver the message on to all matches.

### Subscription management

`(subscribe! <routing-key-filter> <target-node> <target-routing-key>
<reply-node-or-false> <reply-routing-key>)` - Instructs the receiving
node to create a new subscription. The new subscription will only
route messages matching the `routing-key-filter`, which is interpreted
on a per-node-type basis as above for `routing-key`. Matching messages
will be sent to `target-node` using `post!`, with a routing key of
`target-routing-key`. The `reply-node-or-false` parameter, if
nonfalse, instructs the receiving node to send confirmation of
subscription (along with a token that can be used with `unsubscribe!`
below) to the given address and routing key. If `reply-node-or-false`
is false, no confirmation of subscription is sent.

`(unsubscribe! <token>)` - Instructs the receiving node to delete a
previously established subscription. The `token` comes from the
`subscribe-ok!` messages sent to `reply-node-or-false` after a
successful `subscribe!` operation.

### Object management

`(create! <class-name> <argument> <reply-node-or-false>
<reply-routing-key>)` - Instructs the receiving object factory node to
construct a new instance of the given `class-name`, with the given
`argument` supplied to the constructor. The `reply-node-or-false` and
`reply-routing-key` are used to send confirmation of completion to
some waiting node.

## Copyright and licensing

`smsg` is Copyright 2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>.

`smsg` is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

`smsg` is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
License for more details.

You should have received a copy of the GNU Affero General Public
License along with `smsg` (it should be available [here](agpl.txt)).
If not, see <http://www.gnu.org/licenses/>.
