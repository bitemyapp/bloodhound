Bloodhound [![TravisCI](https://travis-ci.org/bitemyapp/bloodhound.svg)](https://travis-ci.org/bitemyapp/bloodhound) [![Hackage](https://img.shields.io/hackage/v/bloodhound.svg?style=flat)](https://hackage.haskell.org/package/bloodhound)
==========

![Bloodhound (dog)](./bloodhound.jpg)

Elasticsearch client and query DSL for Haskell
==============================================

Why?
----

Search doesn't have to be hard. Let the dog do it.

Endorsements
------------

"Bloodhound makes Elasticsearch almost tolerable!" - Almost-gruntled user

"ES is a nightmare but Bloodhound at least makes it tolerable." - Same user, later opinion.

Version compatibility
---------------------

As of version 0.13.0.0, Bloodhound has 2 separate module trees for
Elasticsearch versions 1 and 5. Import the module that is appropriate
for your use case. If you would like to add support for another major
version, open a ticket expressing your intend and follow the pattern
used for other versions. We weighed the idea of sharing code between
versions but it just got too messy, especially considering the
instability of the Elasticsearch API. We switched to a model which
would allow the persons responsible for a particular protocol version
to maintain that version while avoiding conflict with other versions.

See our [TravisCI](https://travis-ci.org/bitemyapp/bloodhound) for a
listing of Elasticsearch version we test against.


Stability
---------

Bloodhound is stable for production use. I will strive to avoid breaking API compatibility from here on forward, but dramatic features like a type-safe, fully integrated mapping API may require breaking things in the future.

Testing
---------

The TravisCI tests are run using [Stack](http://docs.haskellstack.org/en/stable/README.html). You should use Stack instead of `cabal` to build and test Bloodhound to avoid compatibility problems. You will also need to have an Elasticsearch instance running at `localhost:9200` in order to execute some of the tests. See the "Version compatibility" section above for a list of Elasticsearch versions that are officially validated against in TravisCI.

Steps to run the tests locally:
  1. Dig through the [past releases] (https://www.elastic.co/downloads/past-releases) section of the Elasticsearch download page and install the desired Elasticsearch versions.
  2. Install [Stack] (http://docs.haskellstack.org/en/stable/README.html#how-to-install)
  3. In your local Bloodhound directory, run `stack setup && stack build`
  4. Start the desired version of Elasticsearch at `localhost:9200`, which should be the default.
  5. Run `stack test` in your local Bloodhound directory.
  6. The unit tests will pass if you re-execute `stack test`. If you want to start with a clean slate, stop your Elasticsearch instance, delete the `data/` folder in the Elasticsearch installation, restart Elasticsearch, and re-run `stack test`.


Hackage page and Haddock documentation
======================================

<http://hackage.haskell.org/package/bloodhound>

Elasticsearch Tutorial
======================

It's not using Bloodhound, but if you need an introduction to or overview of Elasticsearch and how to use it, you can use [this screencast](https://vimeo.com/106463167).

Examples
========

See the [examples](htts://github.com/bitemyapp/bloodhound/tree/master/examples) directory for example code.


Contributors
============

* [Chris Allen](https://github.com/bitemyapp)
* [Liam Atkinson](https://github.com/latkins)
* [Christopher Guiney](https://github.com/chrisguiney)
* [Curtis Carter](https://github.com/ccarter)
* [Michael Xavier](https://github.com/MichaelXavier)
* [Bob Long](https://github.com/bobjflong)
* [Maximilian Tagher](https://github.com/MaxGabriel)
* [Anna Kopp](https://github.com/annakopp)
* [Matvey B. Aksenov](https://github.com/supki)
* [Jan-Philip Loos](https://github.com/MaxDaten)

Possible future functionality
=============================

Span Queries
------------

Beginning here: <https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-span-first-query.html>

Function Score Query
--------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-function-score-query.html>

Node discovery and failover
---------------------------

Might require TCP support.

Support for TCP access to Elasticsearch
---------------------------------------

Pretend to be a transport client?

Bulk cluster-join merge
-----------------------

Might require making a lucene index on disk with the appropriate format.

GeoShapeQuery
-------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geo-shape-query.html>

GeoShapeFilter
--------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geo-shape-filter.html>

Geohash cell filter
-------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geohash-cell-filter.html>

HasChild Filter
---------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-has-child-filter.html>

HasParent Filter
----------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-has-parent-filter.html>

Indices Filter
--------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-indices-filter.html>

Query Filter
------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-filter.html>

Script based sorting
--------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/search-request-sort.html#_script_based_sorting>

Collapsing redundantly nested and/or structures
-----------------------------------------------

The Seminearring instance, if deeply nested can possibly produce nested structure that is redundant. Depending on how this affects ES performance, reducing this structure might be valuable.

Runtime checking for cycles in data structures
----------------------------------------------

check for n \> 1 occurrences in DFS:

<http://hackage.haskell.org/package/stable-maps-0.0.5/docs/System-Mem-StableName-Dynamic.html>

<http://hackage.haskell.org/package/stable-maps-0.0.5/docs/System-Mem-StableName-Dynamic-Map.html>

Photo Origin
============

Photo from HA! Designs: <https://www.flickr.com/photos/hadesigns/>
