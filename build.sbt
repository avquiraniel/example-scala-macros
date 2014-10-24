name := "testMacros"

version := "1.0"

lazy val testMacros = project in file(".") settings(Common.settings: _*) aggregate (main, macros)

lazy val main = project in file("./main") settings (Common.settings: _*) dependsOn macros

lazy val macros = project in file("./macros") settings (Common.settings: _*)
    