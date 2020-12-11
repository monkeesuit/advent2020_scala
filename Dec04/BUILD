load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Records",
   srcs = ["src/main/scala/Dec4/Records.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Records",
    srcs = ["src/test/scala/Dec4/RecordsTest.scala"],
    deps = ["Records"]
)