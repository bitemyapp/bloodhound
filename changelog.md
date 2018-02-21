0.15.0.2
========
- @michaelxavier
  - Allow http-types 0.12

0.15.0.1
========
- @bitemyapp
  - Allow http-types 0.11
  - Removed unused `oPath` function

0.15.0.0
===================

- @jagare
  - #204 Fix bug where Location was used instad of LatLon
  - #207 Add instructions on how to run example code

- @andrewthad
  - #200 Add function for deleting alias
  - #201 Add BulkCreateEncoding, which allows use of aeson's Encoding for faster bulk creates
  - #203 Add support for total fields limit

- @chrissound
  - #196 Add missing import to example

- @tmcgilchrist
  - #186 Fix warnings and drop redundant constraints

- @23Skidoo
  - #183 Bump dependencies to support GHC 8.2

- @bermanjosh
  - #182 Support for Suggesters and Template Queries

- @dawei-dev
  - #181 Fix typo


0.14.0.0
===================

- @bitemyapp
  - Aeson 1.2

- @shmish111
  - #177 Added stats aggregation

- @justinwhite
  - #176 Fixed typo

- @WraithM
  - #175 Sorted version of getInitialScroll for V5 client

0.13.0.0
===================

- @michaelxavier
  - #174 doctests are bad
  - #171 Added Semigroup instances
  - #165 Aeson 1.1

- @bermanjosh
  - #168 Sub-aggregation support
  - #167 Limit QuickCheck to 500 checks

0.12.1.0
===================
* @michaelxavier
  - #162 Allow Aeson 1.1
* @bermanjosh
  - #159 Fix bug with tie breaker json instance for MultiMatchQuery

0.12.0.0
===================
This release contains several new APIs, documentation fixes, and some minor type updates, and steps towards ES 2.0 support. Thanks to all of our contributors!

* @bermanjosh
  - #139 Make `nodeOSLoad` record a `Maybe` for Windows compatibility
  - #138 Change the `nodePluginVersion` record to deal with plugins which report `NA` as their version.
* @phadej
  - #155 Shed `derive` dependency in test.
  - #154 Switch to `Simple` build type.
  - #153 Loosen deps on aeson, hspec.
* @andrewthad
  - #151 Add `waitForYellowIndex`
* @michaelxavier
  - #150 Fix some failing QuickCheck tests.
  - #147 Drop dependency on deprecated doctest-prop.
  - #137 Add node stats API.
  - #128 Add nodes API.
  - #127 Add haddock sections to break up API.
  - #126 Add snapshot/restore API.
  - #125 Set lower bound for http-client
  - #123 Allow replica count of 0 in smart constructor.
* @23Skidoo
  - #143 Fix an example.
  - #142 Docs cleanup.
* @alistair
  - #135 Changes date formatting to have leading zeroes to bring us closer to ES 2.0 support.
  - #133 Fix parent child tests for ES 2.0.
  - #132 Parse status API from ES 2.0.
  - #131 Add cardinality aggregations.
* @dinnu93
  - #130 Add OverloadedStrings to first example.
* @MaxGabriel
  - #117 Fix haddocks italicizing forward slashes.

0.11.1.0
===================
* @bitemyapp
  - Add http-client-0.5.0 support

0.11.0.0
===================

Thanks to the following people, Bloodhound 0.10.0.0 is being released! This one gets a bit messy due to the Aeson 0.11 changeover, but this should be good to go now. Note that Aeson 0.11 returned to Aeson 0.9's behavior and semantics.

* @MichaelXavier
  - #112 List indices support
  - #94 Implement index optimization
  - #91 Make `respIsTwoHunna` more semantic
    - More detail: This is actually the cause of a bug in real code. If you happen to be
      using parseEsResponse (which uses respIsTwoHunna) to parse the result of
      certain operations such as creating an index, those operations return a
      201 and unjustly are deemed to be a failure.
  - Cleaned up errant Haskell tokens in README
  - #84 Added request auth hooks

* @dzhus / @MailOnline
  - #85 Add updateDocument

* @ReadmeCritic
  - #108 Update README URLs based on HTTP redirects

* @MHova
  - #105 Add helper data types and functions for Missing Aggregations
  - Removed unused server versions from the tests
  - Updated readme to reflect actual ES versions supported and tested
  - Added support for parsing results of Missing Aggregations
  - #104 Export BucketValue
  - #102 Add local testing instructions to the README
  - #89 Support Bool and Numeric keys in TermsResults
  - Added Missing Aggregation support
  - #98 Improve EsProtocolException documentation for human error
  - Updated README to warn about 2.0 compatibility
  - Fix docs specifying an incorrect terminating condition

* @bitemyapp
  - Merge monkey, puzzled over spurious local doctest failures

0.10.0.0
===================

Thanks to the following people, Bloodhound 0.10.0.0 is being released! This one gets a bit messy due to the Aeson 0.10 upgrade, so you may want to wait for the dust to settle. YMMV.

* @MichaelXavier
  - #77: Add test for error parsing
  - #76/#78: Support for updating (modifying) index settings
  - #79/#80: Index aliases
  - #81: Low-level scroll API support
  - #82: Date range aggregation

* @bitemyapp
  - Fucked around with dependencies and broke things in order to upgrade to Aeson 0.10
  - Please forgive me.

0.9.0.0
===================

Thanks to the following people, Bloodhound 0.9.0.0 is being released!

* @MichaelXavier
  - #75: A more explicit type for errors
  - #74: Add readme and changelog to extra source files

* @MaxDaten
  - #38/#73 Provide safety by using URL-encoding

* @centromere
  - #72 Added parent support to documentExists


0.8.0.0
===================

Thanks to the following people, Bloodhound 0.8.0.0 is being released!

* @MichaelXavier
  - #67: Deriving Monad(Throw|Catch|Mask)
  - #64: Export BH constructor
  - #61: Filter aggregation support
  - #60: Add value_count aggregation support
  - #58: Eliminate partiality in EsResult

* @centromere
  - #59: Fixed bug with IndexSettings serialization
  - #56: Added fields support to Search
  - #55: Added ability to specify a parent document
  - #54: Fixed IndexTemplate serialization bug
  - #52: Added ability to manipulate templates
  - #51: Fixed mapping API
  - #50: Fixed problem with put sending POST

* @bermanjosh
  - #63: Url query encoding bug
  - #53: Scan type

* @sjakobi
  - #69: Replace Control.Monad.Error with CM.Except via mtl-compat
  - #70: Silence redundant import warning with base-4.8.*
  - #71: Use "newManager" instead of deprecated "withManager"

0.7.0.0
===================

* Added QueryFilter thanks to Bjørn Nordbø!

* Support for optimistic concurrency control thanks again to @MichaelXavier!

0.6.0.1
===================

* Allow Aeson 0.9

0.6.0.0
===================

* Moved to BHMonad, thanks to @MichaelXavier! Now there's a reader of config information and IO is lifted.

* SearchHits have a Monoid now, makes combining search results nicer, allows for defaulting when a search cannot be performed.

0.5.0.0
===================

* Fixed and changed TermsQuery (This caused the major bump)

* Removed benchmarks from travis.yml

* Added doctests, examples for Database.Bloodhound.Client. Haddocks should be much nicer.

* Various fixes, reformatting

0.4.0.0
===================

* Term and date aggregation - thanks to Christopher Guiney! (@chrisguiney)

Following three thanks to Liam Atkins (@latkins)

* omitNulls changed to exclude empty lists and null values

* BoolQuery must/mustNot/Should changed from Maybe (Query|[Query]) to [Query] thanks to @latkins

* Added vector dependency so we can check for V.null/V.empty on JSON arrays

* Highlighting, thanks to @latkins! See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-highlighting.html and http://www.elasticsearch.org/guide/en/elasticsearch/guide/current/highlighting-intro.html for more

* Added 1.4.0 support and CI integration

* Can generate individual bulk operations, https://github.com/bitemyapp/bloodhound/issues/17, bulk requests should be more efficient now too - Vector instead of List.

0.3.0.0
===================

* Status "ok" changed from Bool to Maybe Bool thanks to @borisyukd

* Elasticsearch 1.3.x compatibility fixed with changes to geo bounding boxes - thanks to Curtis Carter! (@ccarter)

* CI coverage expanded to 1.0.x -> 1.3.x

0.2.0.1
===================

* Killed off maybeJson/mField/catMaybes in favor of omitNulls

* Experimenting with RecordWildcards

* Merged Types and Instances module into Types to prevent possibility of orphans and cull orphan instance warnings.

* Added note about current supported Elasticsearch version.

0.2.0.0
===================

* Added TermFilter

* Renamed createMapping to putMapping

* Fixed and rebuilt documentation

* RegexpFlags changed to a sum type instead of Text, thanks to @MichaelXavier!
