load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Instructions",
   srcs = ["src/main/scala/Dec8/Instructions.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Instructions",
    srcs = ["src/test/scala/Dec8/InstructionsTest.scala"],
    deps = ["Instructions"]
)