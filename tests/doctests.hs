import Test.DocTest

main :: IO ()
main = doctest ["-i src", "Database.Bloodhound"]
