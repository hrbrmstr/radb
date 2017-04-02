context("basic functionality")
test_that("we can do something", {

  rdb <- radb_open()

  rdb <- radb_routes_by_origin(rdb, "5650")

  rdb <- radb_route_set(rdb, "as-google")

  rdb <- radb_route_set(rdb, "as-google", TRUE)

  rdb <- radb_route_set(rdb, "AS-MEEBO")

  rdb <- radb_match(rdb, "mntner", "maint-as237")

  rdb <- radb_route_search(rdb, "108.0.0.0/16", "l")

  rdb <- radb_close(rdb)

  message(str(rdb))

})
