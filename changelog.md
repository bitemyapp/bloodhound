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
