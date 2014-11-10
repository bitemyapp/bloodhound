0.4.0.0
===================

* Term and date aggregation - thanks to Christopher Guiney! (@chrisguiney)

Following three thanks to Liam Atkins (@latkins)
* omitNulls changed to exclude empty lists and null values

* BoolQuery must/mustNot/Should changed from Maybe (Query|[Query]) to [Query]

* Added vector dependency so we can check for V.null/V.empty on JSON arrays

0.3.0.0
===================

* Status "ok" changed from Bool to Maybe Bool

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
