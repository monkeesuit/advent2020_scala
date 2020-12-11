load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Tobogan",
   srcs = ["src/main/scala/Dec3/Tobogan.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Tobogan",
    srcs = ["src/test/scala/Dec3/ToboganTest.scala"],
    deps = ["Tobogan"]
)