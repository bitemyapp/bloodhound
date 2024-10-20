Bloodhound 
![compatbuild](https://github.com/bitemyapp/bloodhound/actions/workflows/compat.yml/badge.svg)
![haskell-ci](https://github.com/bitemyapp/bloodhound/actions/workflows/haskell-ci.yml/badge.svg)
![nix](https://github.com/bitemyapp/bloodhound/actions/workflows/nix.yml/badge.svg)
![ormolu](https://github.com/bitemyapp/bloodhound/actions/workflows/ormolu.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/bloodhound.svg?style=flat)](https://hackage.haskell.org/package/bloodhound)
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

See the [Github compatibility workflow](./.github/workflows/compat.yml) for a
listing of Elasticsearch and OpenSearch versions we test against.

The workflow executions can be seen in the [Github actions
view](https://github.com/bitemyapp/bloodhound/actions/workflows/compat.yml).


Stability
---------

Bloodhound is stable for production use. I will strive to avoid breaking API compatibility from here on forward, but dramatic features like a type-safe, fully integrated mapping API may require breaking things in the future.

Testing
-------

The TravisCI tests are run using [Stack](http://docs.haskellstack.org/en/stable/README.html). You should use Stack instead of `cabal` to build and test Bloodhound to avoid compatibility problems. You will also need to have an Elasticsearch instance running at `localhost:9200` in order to execute some of the tests. See the "Version compatibility" section above for a list of Elasticsearch versions that are officially validated against in TravisCI.

Steps to run the tests locally:
  1. Dig through the [past releases] (https://www.elastic.co/downloads/past-releases) section of the Elasticsearch download page and install the desired Elasticsearch versions.
  2. Install [Stack] (http://docs.haskellstack.org/en/stable/README.html#how-to-install)
  3. In your local Bloodhound directory, run `stack setup && stack build`
  4. Start the desired version of Elasticsearch at `localhost:9200`, which should be the default.
  5. Run `stack test` in your local Bloodhound directory.
  6. The unit tests will pass if you re-execute `stack test`. If you want to start with a clean slate, stop your Elasticsearch instance, delete the `data/` folder in the Elasticsearch installation, restart Elasticsearch, and re-run `stack test`.

Contributions
-------------

Any contribution is welcomed, for consistency reason [`ormolu`](https://github.com/tweag/ormolu) is used.

Hackage page and Haddock documentation
======================================

<http://hackage.haskell.org/package/bloodhound>

Elasticsearch Tutorial
======================

It's not using Bloodhound, but if you need an introduction to or overview of Elasticsearch and how to use it, you can use [this screencast](https://vimeo.com/106463167).

Examples
========

See the [examples](htts://github.com/bitemyapp/bloodhound/tree/master/examples) directory for example code.

Index a document
----------------

```haskell
indexDocument testIndex defaultIndexDocumentSettings exampleTweet (DocId "1")
{-
IndexedDocument
  { idxDocIndex = "twitter"
  , idxDocType = "_doc"
  , idxDocId = "1"
  , idxDocVersion = 3
  , idxDocResult = "updated"
  , idxDocShards =
      ShardResult
        { shardTotal = 1
        , shardsSuccessful = 1
        , shardsSkipped = 0
        , shardsFailed = 0
        }
  , idxDocSeqNo = 2
  , idxDocPrimaryTerm = 1
  }
-}
```

Fetch documents
---------------

```haskell
let query = TermQuery (Term "user" "bitemyapp") boost
let search = mkSearch (Just query) boost
searchByIndex @_ @Tweet testIndex search
{-
SearchResult
    { took = 1
    , timedOut = False
    , shards =
            ShardResult
                { shardTotal = 1
                , shardsSuccessful = 1
                , shardsSkipped = 0
                , shardsFailed = 0
                }
    , searchHits =
            SearchHits
                { hitsTotal = HitsTotal { value = 2 , relation = HTR_EQ }
                , maxScore = Just 0.18232156
                , hits =
                        [ Hit
                                { hitIndex = IndexName "twitter"
                                , hitDocId = DocId "1"
                                , hitScore = Just 0.18232156
                                , hitSource =
                                        Just
                                            Tweet
                                                { user = "bitemyapp"
                                                , postDate = 2009-06-18 00:00:10 UTC
                                                , message = "Use haskell!"
                                                , age = 10000
                                                , location = LatLon { lat = 40.12 , lon = -71.3 }
                                                }
                                , hitSort = Nothing
                                , hitFields = Nothing
                                , hitHighlight = Nothing
                                , hitInnerHits = Nothing
                                }
                        , Hit
                                { hitIndex = IndexName "twitter"
                                , hitDocId = DocId "2"
                                , hitScore = Just 0.18232156
                                , hitSource =
                                        Just
                                            Tweet
                                                { user = "bitemyapp"
                                                , postDate = 2009-06-18 00:00:10 UTC
                                                , message = "Use haskell!"
                                                , age = 10000
                                                , location = LatLon { lat = 40.12 , lon = -71.3 }
                                                }
                                , hitSort = Nothing
                                , hitFields = Nothing
                                , hitHighlight = Nothing
                                , hitInnerHits = Nothing
                                }
                        ]
                }
    , aggregations = Nothing
    , scrollId = Nothing
    , suggest = Nothing
    , pitId = Nothing
    }
-}
```


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
* [Gautier DI FOLCO](https://github.com/blackheaven)

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
