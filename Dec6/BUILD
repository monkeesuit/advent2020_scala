load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Customs",
   srcs = ["src/main/scala/Dec6/Customs.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Customs",
    srcs = ["src/test/scala/Dec6/CustomsTest.scala"],
    deps = ["Customs"]
)