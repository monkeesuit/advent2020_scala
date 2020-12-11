load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "BoardingPass",
   srcs = ["src/main/scala/Dec5/BoardingPass.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-BoardingPass",
    srcs = ["src/test/scala/Dec5/BoardingPassTest.scala"],
    deps = ["BoardingPass"]
)