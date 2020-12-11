load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Xmas",
   srcs = ["src/main/scala/Dec9/Xmas.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Xmas",
    srcs = ["src/test/scala/Dec9/XmasTest.scala"],
    deps = ["Xmas"]
)