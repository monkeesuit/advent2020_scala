load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "TwentyTwenty",
   srcs = ["src/main/scala/Dec1/TwentyTwenty.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-TwentyTwenty",
    srcs = ["src/test/scala/Dec1/TwentyTwentyTest.scala"],
    deps = ["TwentyTwenty"]
)


