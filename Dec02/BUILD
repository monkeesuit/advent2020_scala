load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary",  "scala_library", "scala_test")

scala_library(
   name = "Password1",
   srcs = ["src/main/scala/Dec2/Password1.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Password1",
    srcs = ["src/test/scala/Dec2/Password1Test.scala"],
    deps = ["Password1"]
)

scala_library(
   name = "Password2",
   srcs = ["src/main/scala/Dec2/Password2.scala"],
   visibility = ["//visibility:public"]
)

scala_test(
    name = "test-Password2",
    srcs = ["src/test/scala/Dec2/Password2Test.scala"],
    deps = ["Password2"]
)
