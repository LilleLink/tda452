-------------------------------------------------------------------------------
-- Data structures --
-- Data types : a model of something we want to represent
-- Data structure : a way of storing data efficiently

-- Abstract data type : abstract specification of something we want to represent
-- to represent in our program. An API, list of instructions.
-- Data structure : particular implementation of a data type.

-- We will look at queues and lookup tables

-------------------------------------------------------------------------------
-- A Slow Queue --
-- FIFO

import SlowQueue () -- Contains the API and functionality for a queue

-- Why is it slow?
-- Pretty slow when adding, bad complexity (linear)

-- We can try to optimize by storing the back and the front
-- seperately.
-- This makes it more efficient to remove and add.
-- The complexity becomes amortized though since we "split" the list of queues
-- and have to maintain it.

-------------------------------------------------------------------------------
-- Fast Queue Implementation --

-- Implementation in Queue.hs
-- An invariant to keep in mind, if the front is empty we move the
-- back to the front. Fixed via smart constructor.

-- Still same complexity since we only move back to front when the
-- front is empty, which is not often. Amortized complexity.

-------------------------------------------------------------------------------
-- Lookup table --
-- Mapping keys to value
-- type Table k v = [(k,v)]

-- Invariant - keep the table sorted to utilise binary search
-- we need to be able to break the table up fast
data Table k v = Join (Table k v) (k v) (Table k v) | Empty