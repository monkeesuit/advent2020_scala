load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Adapter",
   srcs = ["src/main/scala/Dec10/Adapter.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Adapter",
    srcs = ["src/test/scala/Dec10/AdapterTest.scala"],
    deps = ["Adapter"]
)