load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Backpack",
   srcs = ["src/main/scala/Dec7/Backpack.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Backpack",
    srcs = ["src/test/scala/Dec7/BackpackTest.scala"],
    deps = ["Backpack"]
)