name := "tests"

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

libraryDependencies += "junit" % "junit" % "4.8.2"

libraryDependencies += "org.scalatest" %% "scalatest" % CddBuild.scalaTestVersionNo % "test->*" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.28.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-chrome-driver" % "2.35.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-htmlunit-driver" % "2.35.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

            