load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "SeatChart",
   srcs = ["src/main/scala/Dec11/SeatChart.scala",
           "src/main/scala/Dec11/SeatingChart.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-SeatChart",
    srcs = ["src/test/scala/Dec11/SeatChartTest.scala"],
    deps = ["SeatChart"]
)