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
